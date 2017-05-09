{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, TypeFamilies #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TQueue as TQ

import Control.Exception (bracket, catch, SomeException)
import Control.Monad (join, forever, (<=<))
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)

import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import Data.Either (rights)
import Data.Maybe (catMaybes)
import Data.Serialize.Text ()
import Data.String (fromString)
import Data.Typeable (Typeable)
import Data.Word (Word16)

import qualified Data.Acid as AS
import qualified Data.Aeson as J
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.SafeCopy as SC
import qualified Data.Yaml as Y
import qualified Data.Yaml.Include as YI

import GHC.Generics
import GHC.IO.Exception (IOException(IOError))

import Network.HTTP.Types (status200, status400, status401, status500)
import Network.HTTP.Types.URI (Query)
import Network.Wai (Application, responseLBS, queryString)

import qualified Network.Wai.Handler.Warp as Warp

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Error (eofErrorType)

import qualified Nexus.Socket as NS
import qualified Nexus.DNS as DNS
import qualified Nexus.Process as P

data UpdateInfo = UpdateInfo
    { uiUsername :: T.Text
    , uiPassword :: T.Text
    , uiDomain :: DNS.FQDN
    , uiIPv4Address :: Maybe DNS.IPv4
    , uiIPv6Address :: Maybe DNS.IPv6
    } deriving Show

newtype ZoneDatabase = ZoneDatabase (M.Map DNS.FQDN DNS.Zone)
    deriving (Show, Typeable)

$(SC.deriveSafeCopy 0 'SC.base ''ZoneDatabase)

data UserInfo = UserInfo
    { password :: T.Text
    , domains :: [T.Text]
    } deriving (Generic, Show)

instance J.FromJSON UserInfo
instance J.ToJSON UserInfo

type Credentials = HM.HashMap T.Text UserInfo

data ConnectionConfig = ConnectionConfig
    { host :: T.Text
    , port :: Word16
    , device :: Maybe String
    } deriving (Generic, Show)

instance J.FromJSON ConnectionConfig
instance J.ToJSON ConnectionConfig

data MasterConfig = MasterConfig
    { credentials :: Credentials
    , stateDir :: FilePath
    , nameservers :: [T.Text]
    , slaves :: [ConnectionConfig]
    , email :: T.Text
    } deriving (Generic, Show)

instance J.FromJSON MasterConfig
instance J.ToJSON MasterConfig

data SlaveConfig = SlaveConfig
    { writeZoneCommand :: FilePath
    } deriving (Generic, Show)

instance J.FromJSON SlaveConfig
instance J.ToJSON SlaveConfig

type ZoneQueue = TQ.TQueue (DNS.FQDN, BL.ByteString)

parseQuery :: Query -> Maybe UpdateInfo
parseQuery q = UpdateInfo
    <$> fmap TE.decodeUtf8 (lu "username")
    <*> fmap TE.decodeUtf8 (lu "password")
    <*> (either (const Nothing) Just . DNS.parseDomain =<< lu "domain")
    <*> pure (DNS.mkPublicIPv4 =<< lu "ipaddr")
    <*> pure (DNS.mkPublicIPv6 =<< lu "ip6addr")
  where lu = join . flip lookup q

authenticate :: Credentials -> UpdateInfo -> Maybe UpdateInfo
authenticate cred ui =
    case HM.lookup (uiUsername ui) cred of
         Just UserInfo {
             password = passwd,
             domains  = doms
         } | uiPassword ui == passwd && hasDomain doms -> Just ui
         _                                             -> Nothing
       where hasDomain d = uiDomain ui `elem` (rights $ unText <$> d)
             unText = DNS.parseDomain . TE.encodeUtf8

getAllZones :: AS.Query ZoneDatabase [DNS.Zone]
getAllZones = do
    ZoneDatabase db <- ask
    return $ snd <$> M.toList db

updateZone :: DNS.FQDN
           -> [DNS.FQDN]
           -> DNS.FQDN
           -> Maybe DNS.IPv4
           -> Maybe DNS.IPv6
           -> AS.Update ZoneDatabase DNS.Zone
updateZone domain ns mail newV4 newV6 = do
    ZoneDatabase db <- get
    let oldZone = M.lookup domain db
    let newZone = mkUpdate oldZone
    put . ZoneDatabase $ M.insert domain newZone db
    return newZone
  where
    initialZone = DNS.mkTinyZone domain mail $
        DNS.mkRR . DNS.Nameserver . DNS.toRRName <$> ns
    newRecords = catMaybes [ DNS.mkRR . DNS.IPv4Address <$> newV4
                           , DNS.mkRR . DNS.IPv6Address <$> newV6
                           ]
    mkUpdate Nothing = DNS.updateRecords newRecords initialZone
    mkUpdate (Just zone) = DNS.updateRecords newRecords zone

$(AS.makeAcidic ''ZoneDatabase ['getAllZones, 'updateZone])

httpApp :: MasterConfig -> AS.AcidState ZoneDatabase -> TC.TChan DNS.Zone
        -> Application
httpApp cfg state workChan request respond =
    case parseQuery (queryString request) >>= authenticate (credentials cfg) of
         Nothing ->
             respondText status401 "User data wrong or incomplete."
         Just UpdateInfo {
             uiIPv4Address = Nothing,
             uiIPv6Address = Nothing
         } -> respondText status400 "IP address info wrong or incomplete."
         Just ui -> handleUZ $ tryUpdateZone ui
  where
    handleUZ (Left err) = respondText status500 $ BL.pack err
    handleUZ (Right uz) = do
        updated <- AS.update state $ uz
        atomically $ TC.writeTChan workChan updated
        respondText status200 "DNS entry queued for update."
    tryUpdateZone ui = UpdateZone
        <$> pure (uiDomain ui)
        <*> sequenceA (DNS.parseDomain . TE.encodeUtf8 <$> nameservers cfg)
        <*> DNS.emailToFQDN (TE.encodeUtf8 $ email cfg)
        <*> pure (uiIPv4Address ui)
        <*> pure (uiIPv6Address ui)
    respondText s = respond . responseLBS s [("Content-Type", "text/plain")]

masterWorker :: AS.AcidState ZoneDatabase
             -> TC.TChan DNS.Zone
             -> ConnectionConfig
             -> IO ()
masterWorker state workChan connCfg = retry $ \conn -> do
    workQueue <- atomically $ TC.dupTChan workChan
    existing <- AS.query state GetAllZones
    mapM_ (throwIfFalse <=< NS.send conn) existing
    forever $ do
        newZone <- atomically $ TC.readTChan workQueue
        logBSLn ["Zone update: ", BC.pack $ show newZone]
        throwIfFalse =<< NS.send conn newZone
  where
    errDesc = "Connection closed by the remote side."
    eofError = IOError Nothing eofErrorType "connect" errDesc Nothing Nothing
    throwIfFalse False = ioError eofError
    throwIfFalse True  = return ()
    sHost = fromString . T.unpack $ host connCfg
    sPort = show $ port connCfg
    handleError :: SomeException -> IO ()
    handleError err = do
        logBSLn [ "Connection to slave ", BC.pack sHost, ":", BC.pack sPort
                , " has failed (", BC.pack $ show err
                , "), retrying in one second..."
                ]
        threadDelay 1000000
    retry handler = do
        catch (NS.connect sHost sPort (device connCfg) handler) handleError
        retry handler

defaultMasterConfig :: MasterConfig
defaultMasterConfig = MasterConfig
    { credentials = HM.empty
    , stateDir = "/tmp/dyndns.state"
    , nameservers = []
    , slaves = []
    , email = "unconfigured@example.org"
    }

defaultSlaveConfig :: SlaveConfig
defaultSlaveConfig = SlaveConfig "false"

mergeConfig :: J.Value -> J.Value -> J.Value
mergeConfig (J.Object x) (J.Object y) = J.Object $ HM.unionWith mergeConfig x y
mergeConfig _ x = x

loadConfigAndRun :: (J.ToJSON a, J.FromJSON b)
                 => FilePath -> a -> (b -> IO (Either ByteString ()))
                 -> IO (Either ByteString ())
loadConfigAndRun fp defcfg fun = do
    cfg <- YI.decodeFileEither fp
    case cfg of
         Left err  -> return . Left $ BC.pack $ Y.prettyPrintParseException err
         Right val -> do
             let val' = mergeConfig (J.toJSON defcfg) val
             case J.fromJSON val' of
                  J.Error s -> return . Left $ BC.concat
                      ["Could not convert to settings: ", BC.pack s]
                  J.Success settings -> fun settings

serveManyWarps :: Application -> IO [A.Async ()]
serveManyWarps app =
    mapM A.async . fmap listenTo <=< NS.getSocketsFor $ Just "http"
  where
    listenTo :: NS.Socket -> IO ()
    listenTo sock = do
        NS.setNonBlocking sock
        Warp.runSettingsSocket Warp.defaultSettings sock app

startMaster :: MasterConfig -> IO (Either ByteString ())
startMaster MasterConfig { nameservers = [] } =
    return $ Left "No nameservers defined in config"
startMaster cfg = bracket openAcidState AS.closeAcidState $ \state -> do
    workChan <- TC.newBroadcastTChanIO
    slaveWorkers <- mapM (A.async . masterWorker state workChan) $ slaves cfg
    warps <- serveManyWarps $ httpApp cfg state workChan
    snd <$> A.waitAnyCancel (slaveWorkers ++ warps)
    return $ Right ()
  where
    openAcidState = AS.openLocalStateFrom (stateDir cfg) (ZoneDatabase M.empty)

logBSLn :: [ByteString] -> IO ()
logBSLn = BC.hPutStrLn stderr . BC.concat

logBS :: [ByteString] -> IO ()
logBS = BC.hPutStr stderr . BC.concat

updateZoneFile :: FilePath -> DNS.FQDN -> BL.ByteString -> IO ()
updateZoneFile cmd zone zoneData = do
    logBSLn ["Updating zone ", DNS.toByteString zone, "..."]
    P.pipeToStdin cmd [DNS.toByteString zone] zoneData
        (logBSLn ["Updating of zone ", DNS.toByteString zone, " done."])
        (\code -> logBSLn [ "Failed to update zone ", DNS.toByteString zone
                  , " with exit code ", BC.pack $ show code, "."
                  ])

slaveZoneUpdater :: FilePath -> ZoneQueue -> IO ()
slaveZoneUpdater cmd zoneQueue = forever $
    atomically (TQ.readTQueue zoneQueue) >>= uncurry (updateZoneFile cmd)

slaveHandler :: ZoneQueue -> FilePath -> DNS.Zone -> IO ()
slaveHandler _ _ zone | Just err <- DNS.checkZone zone = logBSLn [BC.pack err]
slaveHandler zoneQueue cmd zone = do
    logBS
        ["Scheduling update of zone ", DNS.toByteString $ DNS._zoneDomain zone
        , " with serial ", BC.pack . show . DNS._soaSerial $ DNS._zoneSOA zone
        , " using command ", BC.pack cmd
        , "..."
        ]
    atomically . TQ.writeTQueue zoneQueue $
        (DNS._zoneDomain zone, toLazyByteString $ DNS.renderZone zone)
    BC.hPutStrLn stderr " done."

startSlave :: SlaveConfig -> IO (Either ByteString ())
startSlave SlaveConfig { writeZoneCommand = cmd } = do
    NS.serve (Just "master") scc
    return $ Right ()
  where process zq c = do
            result <- NS.recv c
            case result of
                 Just newZone -> do
                     slaveHandler zq cmd newZone
                     process zq c
                 Nothing -> return ()
        scc c = do
            logBSLn ["New connection from master on ", BC.pack $ show c, "."]
            zoneQueue <- TQ.newTQueueIO
            let updater = slaveZoneUpdater cmd zoneQueue
            A.race_ updater $ process zoneQueue c

realMain :: [String] -> IO (Either ByteString ())
realMain ["--master", cfgFile] =
    loadConfigAndRun cfgFile defaultMasterConfig startMaster
realMain ["--slave", cfgFile] =
    loadConfigAndRun cfgFile defaultSlaveConfig startSlave
realMain _ = do
    prog <- getProgName
    return . Left . BC.concat $
        ["Usage: ", BC.pack prog, " {--master|--slave} configfile.yaml"]

main :: IO ()
main = do
    args <- getArgs
    result <- realMain args
    case result of
         Left s -> BC.hPutStrLn stderr s >> exitFailure
         Right r -> return r
