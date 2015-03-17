{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson

-- TODO: create datatypes for the calculationresult(s)
data Todo = Todo
  { id   :: Int
  , text :: T.Text
  }

instance ToJSON Todo where
  toJSON (Todo id text) = object [ "id" .= id, "text" .= text ]
