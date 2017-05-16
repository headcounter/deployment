{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.String (IsString(fromString))
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (empty)
import Data.ByteString.Builder (toLazyByteString)
import Data.Serialize (Serialize)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitWith, ExitCode(ExitFailure))
import System.IO (stderr, hPutStrLn)

import GHC.Generics

import qualified Data.ByteString.Char8 as BC

import qualified Nexus.Socket as NS
import qualified Nexus.DNS as DNS
import qualified Nexus.Process as P

data Command = CmdInstall DNS.FQDN ByteString
             | CmdRemove DNS.FQDN
             | Sync
             deriving (Show, Generic)

instance Serialize Command

data ServerConfig = ServerConfig
    { updateCmd   :: FilePath
    , nameservers :: [DNS.FQDN]
    } deriving Show

process :: ServerConfig -> Command -> IO ()
process _ Sync = return ()
process cfg (CmdInstall fqdn challenge) =
    P.pipeToStdin (updateCmd cfg) args zoneData (return ()) onFailure
  where
    args = [DNS.toByteString $ DNS._zoneDomain zone]
    onFailure code = hPutStrLn stderr $ "Zone update for " ++ show fqdn
                                     ++ " failed with exit code " ++ show code
                                     ++ "."

    txtRR = DNS.mkRR $ DNS.TextRecord challenge
    nsRRs = DNS.mkRR . DNS.Nameserver . DNS.toRRName <$> nameservers cfg
    zone = DNS.mkTempZone acmeZone dummyMail (txtRR : nsRRs)

    acmeZone = mappend fqdn "_acme-challenge"
    dummyMail = mappend fqdn "temporary"
    zoneData = toLazyByteString $ DNS.renderZone zone
process cfg (CmdRemove fqdn) =
    P.pipeToStdin (updateCmd cfg) args empty (return ()) onFailure
  where
    realFQDN = mappend fqdn "_acme-challenge"
    args = ["--delete", DNS.toByteString realFQDN]
    onFailure code = hPutStrLn stderr $ "Removal of zone for " ++ show fqdn
                                     ++ " failed with exit code " ++ show code
                                     ++ "."

server :: ServerConfig -> NS.Connection Command -> IO ()
server cfg conn =
    maybe (return ()) onData =<< NS.recv conn
  where
    onData cmd = process cfg cmd >> void (NS.send conn Sync)

parseOrFail :: String -> (DNS.FQDN -> a) -> Maybe a
parseOrFail d f = either (const Nothing) (Just . f) parsed
            where parsed = DNS.parseDomain $ BC.pack d

parseClientArgs :: [String] -> Maybe Command
parseClientArgs ["challenge-dns-start", fqdn, _, txtValue] =
    parseOrFail fqdn $ \f -> CmdInstall f (BC.pack txtValue)
parseClientArgs ["challenge-dns-stop",  fqdn, _, _] =
    parseOrFail fqdn CmdRemove
parseClientArgs _ = Nothing

realMain :: [String] -> IO ()
realMain ("--server" : cmd : ns) =
    NS.serve Nothing . server $ ServerConfig cmd (fromString <$> ns)
realMain ("--client" : host : port : rest) =
    case parseClientArgs rest of
         Nothing  -> exitWith (ExitFailure 42)
         Just cmd -> NS.connect host port Nothing handler
               where handler conn = do
                         True <- NS.send conn cmd
                         Just Sync <- NS.recv conn
                         return ()
realMain _ = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage: " ++ prog ++ " --server COMMAND"
    hPutStrLn stderr $ "       " ++ prog ++ " --client HOST PORT COMMAND..."
    exitFailure

main :: IO ()
main = getArgs >>= realMain
