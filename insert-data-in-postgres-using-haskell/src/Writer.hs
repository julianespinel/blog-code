{-# LANGUAGE OverloadedStrings #-}

module Writer (createStocksTable, insertStocks) where

import qualified Data.Vector as V
import Database.PostgreSQL.Simple
import Types

createStocksTable :: Connection -> IO Int
createStocksTable conn = do
  modifiedRows <- execute_ conn sql
  return (fromIntegral modifiedRows)
  where
    sql =
      "CREATE TABLE IF NOT EXISTS stocks (\
      \ code VARCHAR(128),\
      \ name VARCHAR(255),\
      \ country VARCHAR(128),\
      \ exchange VARCHAR(128),\
      \ currency VARCHAR(128),\
      \ instrumentType VARCHAR(128)\
      \);"

insertStocks :: Connection -> V.Vector FinancialInstrument -> IO (Either ErrorMsg (V.Vector FinancialInstrument))
insertStocks conn instruments = do
  executeMany conn sql (V.toList instruments)
  return (Right instruments)
  where
    sql =
      "INSERT INTO stocks\
      \ (code, name, country, exchange, currency, instrumentType)\
      \ values (?,?,?,?,?,?);"
