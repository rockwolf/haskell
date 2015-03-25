{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.KubarService where

import Api.Types
import Control.Lens
import Control.Monad.State.Class
import Data.Aeson
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

data KubarService = KubarService { }

makeLenses ''KubarService

kubarRoutes :: [(B.ByteString, Handler b KubarService ())]
kubarRoutes = [("/", method GET getTodos),
               ("/status", method GET getStatus)]

getTodos :: Handler b KubarService ()
getTodos = do
  let todos = [Todo 1 "test1"] ++ [Todo 2 "test2"] ++ [Todo 3 "test3"]

  -- TODO: the above works, for api/kubar.
  --
  modifyResponse $ setHeader "Content-Type" "application/json;charset=utf-8"
  writeLBS . encode $ (todos :: [Todo])

getStatus :: Handler b KubarService ()
getStatus = do
  let todos = "Status ok."

  -- TODO: the above works, for api/kubar.
  --
  modifyResponse $ setHeader "Content-Type" "application/json;charset=utf-8"
  writeLBS . encode $ (todos :: String)



kubarServiceInit :: SnapletInit b KubarService
kubarServiceInit = makeSnaplet "kubar" "Kubar Service" Nothing $ do
  addRoutes kubarRoutes
  return $ KubarService
