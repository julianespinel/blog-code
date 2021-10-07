{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( ErrorMsg,
    FinancialInstrument (..),
  )
where

import Database.PostgreSQL.Simple (ToRow)
import GHC.Generics (Generic)

type ErrorMsg = String

-- data type to model a FinancialInstrument
data FinancialInstrument = FinancialInstrument
  { code :: String,
    name :: String,
    country :: String,
    exchange :: String,
    currency :: String,
    instrumentType :: String
  }
  deriving (Generic, Show, Eq, ToRow)
