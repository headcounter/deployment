{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TSem
import Control.Monad (void, unless)
import Data.List (intercalate)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import System.IO (stderr)
import System.Posix.Env (setEnv)
import System.Posix.Process (getProcessID)

import qualified Data.ByteString.Char8 as B
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import Nexus

data TestData = TestData Int String deriving (Show, Generic)
instance Serialize TestData

data Result = Result String deriving Show
type ResultChan = TChan Result

mkSocket :: Int -> IO Int
mkSocket expect = do
    sock <- NS.socket NS.AF_UNIX NS.Stream 0
    NS.bind sock . NS.SockAddrUnix $ "test" ++ show expect ++ ".sock"
    NS.listen sock NS.maxListenQueue
    let fd = fromIntegral $ NS.fdSocket sock
    unless (expect == fd) . error $ "Expected socket FD to be " ++ show expect
                                 ++ " but it is " ++ show fd ++ " instead."
    return fd

setupSystemdFds :: IO ()
setupSystemdFds = do
    sockfds <- mapM mkSocket [3, 4]
    writeEnv "LISTEN_PID" . show =<< getProcessID
    writeEnv "LISTEN_FDS" . show $ length sockfds
    writeEnv "LISTEN_FDNAMES" . intercalate ":" $ const "unknown" <$> sockfds
  where writeEnv n v = setEnv n v True

writeResults :: ResultChan -> String -> IO ()
writeResults chan result = do
    B.hPutStrLn stderr . B.pack $ "WRITING RESULT: " ++ result
    atomically . writeTChan chan $ Result result

serverHandler :: ResultChan -> TSem -> Connection TestData -> IO ()
serverHandler results sync conn = do
    val <- Nexus.recv conn
    case val of
        Just (TestData 1 "sent first") -> do
            writeResults results "server got first"
            Nexus.broadcast conn $ TestData 2 "to all"
            writeResults results "sent to all"
            void $ Nexus.recv conn
        Just (TestData 3 "client after bogus") -> do
            writeResults results "packet after bogus client"
            atomically $ signalTSem sync
        Just (TestData 4 "client 2 ready") -> do
            writeResults results "client signalled ready"
            Nexus.send conn (TestData 5 "client 2 go")
            void $ Nexus.recv conn
        Just _ -> writeResults results "unexpected data from client"
        Nothing -> do
            writeResults results "bogus packet from client"
            atomically $ signalTSem sync

handler1 :: ResultChan -> TSem -> Connection TestData -> IO ()
handler1 results sync conn = do
    atomically $ waitTSem sync
    Nexus.send conn $ TestData 1 "sent first"
    writeResults results "sent first"
    atomically $ waitTSem sync

handler2 :: ResultChan -> TSem -> Connection TestData -> IO ()
handler2 results sync conn = do
    Nexus.send conn $ TestData 4 "client 2 ready"
    Just (TestData 5 "client 2 go") <- Nexus.recv conn
    atomically $ signalTSem sync
    Just (TestData 2 "to all") <- Nexus.recv conn
    atomically $ signalTSem sync
    writeResults results "got broadcast"

checkResults :: ResultChan -> IO ()
checkResults results = do
    expect "client signalled ready"
    expect "sent first"
    expect "server got first"
    expect "sent to all"
    expect "got broadcast"
    expect "bogus packet from client"
    expect "packet after bogus client"
  where
    expect x = do
        Result val <- atomically (readTChan results)
        if val == x
           then B.hPutStrLn stderr . B.pack $
               "Got " ++ show val ++ " as expected (" ++ show x ++ ")."
           else error $ "Expected " ++ show x ++ " but got "
                     ++ show val ++ " instead."

task :: B.ByteString -> IO a -> IO ()
task desc fun = do
    B.hPutStrLn stderr $ B.append "+++ BEGIN: " desc
    void fun
    B.hPutStrLn stderr $ B.append "--- END: " desc

main :: IO ()
main = do
    results <- newTChanIO
    sync <- atomically $ newTSem 0

    task "Set up systemd sockets" setupSystemdFds

    task "Fork off server" $
        void . forkIO . Nexus.serve Nothing $ serverHandler results sync

    task "Run first wave of clients" $ do
        concurrently_ (Nexus.connectUnix "test3.sock" $ handler1 results sync)
                      (Nexus.connectUnix "test4.sock" $ handler2 results sync)

    task "Run bogus client" $ do
        bogusSocket <- NS.socket NS.AF_UNIX NS.Stream 0
        NS.connect bogusSocket (NS.SockAddrUnix "test3.sock")
        NSB.sendAll bogusSocket "bogus"
        NS.close bogusSocket
        atomically $ waitTSem sync

    task "Run non-bogus client" $
        Nexus.connectUnix "test3.sock" $ \conn -> do
            Nexus.send conn $ TestData 3 "client after bogus"
            atomically $ waitTSem sync

    task "Collect results" $
        checkResults results
