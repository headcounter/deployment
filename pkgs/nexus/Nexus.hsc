{-|
This module is for simple TCP connections, very similar to the "network-simple"
package. However it differs in the following aspects:

  * __Only__ supports systemd socket activation.
  * Allows the client to send using a specific device only (@SO_BINDTODEVICE@).
  * Has a way to easily broadcast to all clients for the server.
  * Can serve on several sockets at once.

-}
{-# LANGUAGE TemplateHaskell #-}
module Nexus
    ( Connection
    , Handler
    , connect
    , connectUnix
    , serve
    , recv
    , send
    , broadcast

    , getSocketsFor
    , setNonBlocking
    , NS.Socket
    ) where

import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, finally)
import Control.Monad (void, when, forM, (<=<))
import Data.ByteString (ByteString)
import Foreign.C.String (withCStringLen)
import Foreign.C.Types (CInt(..), CChar)
import Foreign.Ptr (Ptr)
import GHC.IO.Exception (IOException(IOError), IOErrorType(TimeExpired))
import Network.Socket.Internal (throwSocketErrorIfMinus1_)
import System.IO (stderr, fixIO)
import System.Posix.Internals (setNonBlockingFD)
import System.Timeout (timeout)

import qualified Control.Concurrent.Async as A
import qualified Control.Concurrent.STM.TChan as TC
import qualified Control.Concurrent.STM.TQueue as TQ
import qualified Control.Concurrent.STM.TVar as TV
import qualified Data.ByteString.Char8 as BC
import qualified Data.Serialize as S
import qualified Language.Haskell.TH as TH
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified System.Systemd.Daemon as SD

#include <sys/socket.h>

foreign import ccall unsafe "setsockopt"
    setsockopt :: CInt -> CInt -> CInt -> Ptr CChar -> CInt -> IO CInt

type ReceiveQueue     = TQ.TQueue ByteString
type BroadcastQueue a = TC.TChan (Maybe (CInt, a))

-- | An established connection to another network entity.
data Connection a = Connection
    { recvQueue      :: ReceiveQueue
    , broadcastQueue :: Maybe (BroadcastQueue a)
    , connSock       :: NS.Socket
    , sockAddr       :: NS.SockAddr
    }

instance Show (Connection a) where
    show = show . sockAddr

-- | A handler function which can 'recv', 'send' and 'broadcast' to the
--   'Connection'.
--
-- As soon as the handler function returns, the 'Connection' is closed.
type Handler a = Connection a -> IO ()

thisModule :: String
thisModule = $(TH.stringE =<< TH.loc_module <$> TH.location)

setDevice :: NS.Socket -> String -> IO ()
setDevice sock dev =
    withCStringLen dev $ \(optval, optlen) ->
        throwSocketErrorIfMinus1_ (thisModule ++ ".setDevice") $
            setsockopt fd level optname optval (fromIntegral optlen)
      where level = #const SOL_SOCKET
            optname = #const SO_BINDTODEVICE
            fd = NS.fdSocket sock

clientHints :: NS.AddrInfo
clientHints = NS.defaultHints
    { NS.addrFlags = [NS.AI_ALL]
    , NS.addrSocketType = NS.Stream
    }

-- | Helper to set the @O_NONBLOCK@ flag on a 'NS.Socket'.
setNonBlocking :: NS.Socket -> IO ()
setNonBlocking sock = setNonBlockingFD (NS.fdSocket sock) True

-- | Return the sockets that are available for the given name.
--
-- If the name isn't given and is 'Nothing' instead, return __all__ sockets
-- passed by systemd.
getSocketsFor :: Maybe String -> IO [NS.Socket]
getSocketsFor Nothing     = concat <$> SD.getActivatedSockets
getSocketsFor (Just name) =
    filterSocks . concat <$> SD.getActivatedSocketsWithNames
  where
    filterSocks :: [(NS.Socket, String)] -> [NS.Socket]
    filterSocks = fmap fst . filter ((==) name . snd)

startConnection :: NS.Socket -> NS.SockAddr -> IO (Connection a)
startConnection sock sockaddr = do
    recvQ <- TQ.newTQueueIO
    return $ Connection recvQ Nothing sock sockaddr

startBroadcaster :: S.Serialize a
                 => NS.Socket
                 -> BroadcastQueue a
                 -> TV.TVar Bool
                 -> IO ()
startBroadcaster sock queue finisher = do
    result <- atomically $ do
        shouldExit <- TV.readTVar finisher
        if shouldExit then return Nothing else TC.readTChan queue
    case result of
         Just (sockfd, msg) -> do
             when (sockfd /= NS.fdSocket sock) $
                 NSB.sendAll sock . S.runPut $ S.put msg
             startBroadcaster sock queue finisher
         Nothing -> return ()

acceptHandler :: S.Serialize a => Handler a -> BroadcastQueue a
              -> NS.Socket -> NS.SockAddr -> IO ()
acceptHandler handlerFun bcastQ sock sockaddr = do
    bcastReadQ <- atomically $ TC.dupTChan bcastQ
    rconn <- startConnection sock sockaddr
    let conn = rconn { broadcastQueue = Just bcastQ }
    finisher <- TV.newTVarIO False
    broadcaster <- A.async $ startBroadcaster sock bcastReadQ finisher
    handler <- A.async . finally (handlerFun conn) $
        atomically $ TV.writeTVar finisher True
    void $ A.waitCatch broadcaster
    void $ A.waitCatch handler

runAcceptor :: S.Serialize a
            => Handler a -> BroadcastQueue a -> NS.Socket -> A.Async ()
            -> IO ()
runAcceptor handler bcastQ lsock self = do
    (sock, sockaddr) <- NS.accept lsock
    A.link2 self <=< A.async $
        finally (acceptHandler handler bcastQ sock sockaddr) (NS.close sock)
    runAcceptor handler bcastQ lsock self

runPool :: S.Serialize a => [NS.Socket] -> Handler a -> IO ()
runPool sockets handler = do
    mapM_ setNonBlocking sockets
    bcastQ <- TC.newBroadcastTChanIO
    acceptors <- forM sockets $ \sock ->
        fixIO (A.async . runAcceptor handler bcastQ sock)
    finally (void $ A.waitAnyCancel acceptors) $ do
        atomically $ TC.writeTChan bcastQ Nothing
        mapM A.uninterruptibleCancel acceptors

spawnClient :: NS.Socket -> NS.SockAddr -> Handler a -> IO ()
spawnClient sock sockaddr =
    bracket startup (NS.close . connSock)
  where
    errDesc = "Timed out after 5 seconds"
    connectErr = IOError Nothing TimeExpired "connect" errDesc Nothing Nothing
    startup = do
        maybeTimedOut <- timeout 5000000 $ NS.connect sock sockaddr
        case maybeTimedOut of
             Nothing -> ioError connectErr
             Just _  -> startConnection sock sockaddr

-- | Wait for data on the current endpoint of the 'Connection'.
recv :: S.Serialize a => Connection a -> IO (Maybe a)
recv conn =
    decodeLoop . S.Partial $ S.runGetPartial S.get
  where
    putRest :: ByteString -> IO ()
    putRest bs | BC.null bs = return ()
               | otherwise  = atomically $ TQ.writeTQueue (recvQueue conn) bs

    getRest :: IO (Maybe ByteString)
    getRest = atomically . TQ.tryReadTQueue $ recvQueue conn

    decodeLoop :: S.Result a -> IO (Maybe a)
    decodeLoop (S.Partial cont) = do
        new <- maybe (NSB.recv (connSock conn) 4096) return =<< getRest
        if BC.null new then return Nothing else decodeLoop $ cont new
    decodeLoop (S.Fail err _) = do
        BC.hPutStrLn stderr . BC.pack $ "Error in decoding data: " ++ err
        return Nothing
    decodeLoop (S.Done result rest) =
        putRest rest >> return (Just result)

-- | Send the packet to the current endpoint of the 'Connection'.
send :: S.Serialize a => Connection a -> a -> IO ()
send conn = NSB.sendAll (connSock conn) . S.runPut . S.put

-- | Broadcast the packet to all of the connected clients.
broadcast :: S.Serialize a => Connection a -> a -> IO ()
broadcast conn@Connection { broadcastQueue = Just bcastQ } val = do
    atomically . TC.writeTChan bcastQ $
        Just (NS.fdSocket $ connSock conn, val)
    send conn val
broadcast conn val = send conn val

-- | Serve connections for the specified socket and run 'Handler' for every
--   single connection.
serve :: S.Serialize a
      => Maybe String -- ^ Systemd socket name or all systemd sockets
      -> Handler a    -- ^ Connection handler
      -> IO ()
serve sockname handler = NS.withSocketsDo $ do
    maybeSockets <- getSocketsFor sockname
    case maybeSockets of
         []      -> error $ sockErr sockname
         sockets -> runPool sockets handler
  where sockErr (Just sn) = "Unable to find systemd sockets for " ++ sn ++ "."
        sockErr Nothing   = "Couldn't find any systemd sockets."

-- | Connect to a specific host and run the 'Handler' function
--   if the connection has been successful.
connect :: S.Serialize a
        => NS.HostName    -- ^ Host name or IP address
        -> NS.ServiceName -- ^ Port or service name
        -> Maybe String   -- ^ Specific device to bind to
        -> Handler a      -- ^ Connection handler
        -> IO ()
connect host port maybeDev handler = NS.withSocketsDo $
    maybeSpawn =<< NS.getAddrInfo (Just clientHints) (Just host) (Just port)
  where
    maybeSetDevice :: NS.Socket -> Maybe String -> IO ()
    maybeSetDevice _    Nothing    = return ()
    maybeSetDevice sock (Just dev) = setDevice sock dev

    maybeSpawn :: [NS.AddrInfo] -> IO ()
    maybeSpawn [] = error $
        "Couldn't get address info for " ++ host ++ ":" ++ port ++ "."
    maybeSpawn (addr:_) = do
        sock <- NS.socket (NS.addrFamily addr)
                          (NS.addrSocketType addr)
                          (NS.addrProtocol addr)
        maybeSetDevice sock maybeDev
        spawnClient sock (NS.addrAddress addr) handler

-- | Same as 'connect' but use an UNIX Domain Socket instead of a TCP/IP
--   connection.
connectUnix :: String -> Handler a -> IO ()
connectUnix path handler = NS.withSocketsDo $ do
    sock <- NS.socket NS.AF_UNIX NS.Stream 0
    spawnClient sock (NS.SockAddrUnix path) handler
