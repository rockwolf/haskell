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
kubarRoutes = [("/", method GET getKubarResults),
               ("/status", method GET getStatus)]

getKubarResults :: Handler b KubarService ()
getKubarResults = do
  let results = [KubarResult 1 "test1"] ++ [KubarResult 2 "test2"] ++ [KubarResult 3 "test3"]

  -- TODO: the above works, for api/kubar.
  --
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (results :: [KubarResult])

getStatus :: Handler b KubarService ()
getStatus = do
  let results = "Status ok."

  -- TODO: the above works, for api/kubar.
  --
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (results :: String)



kubarServiceInit :: SnapletInit b KubarService
kubarServiceInit = makeSnaplet "kubar" "Kubar Service" Nothing $ do
  addRoutes kubarRoutes
  return $ KubarService
