{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson
import GHC.Generics

-- TODO: create datatypes for the calculationresult(s)
data KubarResult = KubarResult
  { id   :: Int
  , text :: T.Text
  } deriving (Show, Generic)

instance ToJSON KubarResult
