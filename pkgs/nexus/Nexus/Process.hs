-- | Functions for interacting with system processes.
module Nexus.Process (pipeToStdin) where

import Control.Exception (bracket)
import Data.ByteString.Char8 (ByteString, unpack)
import System.Exit (ExitCode(..))
import System.IO (hClose)
import System.Process

import qualified Data.ByteString.Lazy as BL

-- | Run a command and pipe a lazy 'BL.ByteString' into stdin.
pipeToStdin :: FilePath       -- ^ The command to execute
            -> [ByteString]   -- ^ Arguments to pass to the command
            -> BL.ByteString  -- ^ Data to write to the command
            -> IO a           -- ^ Handler to call on success
            -> (Int -> IO a)  -- ^ Handler to call on failure (with exit code)
            -> IO a
pipeToStdin cmd args d onSuccess onFailure = do
    exitCode <- bracket (createProcess procCmd) cleanup process
    case exitCode of
         ExitSuccess -> onSuccess
         ExitFailure code -> onFailure code
  where
    procCmd = (proc cmd (unpack <$> args))
        { std_in = CreatePipe
        , close_fds = True
        }
    cleanup (Just i, _, _, p) = hClose i >> terminateProcess p
    cleanup _                 = return ()

    process (Just i, _, _, p) = BL.hPutStr i d >> hClose i >> waitForProcess p
    process _                 = return $ ExitFailure 126
