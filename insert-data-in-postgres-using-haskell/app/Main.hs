module Main where

import Control.Monad.Trans.Except
import qualified Data.Vector as V
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    close,
    connect,
    defaultConnectInfo,
  )
import Debug.Trace
import Reader
import System.Environment
import Types
import Writer

connectionInfo :: ConnectInfo
connectionInfo =
  defaultConnectInfo
    { connectHost = "localhost",
      connectPort = 5432,
      connectUser = "postgres",
      connectPassword = "example",
      connectDatabase = "haskell_test"
    }

-- Why ExceptT? See: https://stackoverflow.com/a/53427527/2420718
readAndWriteStocks :: FilePath -> Connection -> IO (Either ErrorMsg (V.Vector FinancialInstrument))
readAndWriteStocks filePath conn = runExceptT $ do
  stocks <- ExceptT $ readStocks filePath
  ExceptT $ insertStocks conn stocks

-- How to run?
-- stack run "resources/US_LIST_OF_SYMBOLS.csv"
main :: IO ()
main = do
  args <- getArgs
  conn <- connect connectionInfo
  createStocksTable conn
  let csvPath = head args
  readAndWriteStocks csvPath conn >>= print
  close conn
