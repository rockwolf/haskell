{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Api.Services.KubarService
import Control.Lens
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

data Api = Api { _kubarService :: Snaplet KubarService }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk), ("/", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = do
  modifyResponse . setResponseCode $ 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
        ts <- nestSnaplet "kubar" kubarService kubarServiceInit
        addRoutes apiRoutes
        return $ Api ts
