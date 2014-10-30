{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import Database.PostgreSQL.Simple.FromRow
------------------------------------------------------------------------------
import           Application

------------------------------------------------------------------------------
-- | Database related functions
instance FromRow Project where
    fromRow = Project <$> field <*> field
  
instance Show Project where
    show (Project title description) =
      "Project { title: " ++ T.unpack title ++ ", description: " ++ T.unpack description ++ " }n"
------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"


------------------------------------------------------------------------------
-- | Handle leverage submit
-- TODO: write function with accompanying form to calculate leverage contracts:
-- Number of contracts = n
-- Thus the leveraged number of contracts to use, becomes:
-- n - 3 <= 0 -> round (0 / 3) 
-- n - 3 = X -> round (Y / 3), Y TBD
-- e.g. 0 -> 0
-- 
--handleLoginSubmit :: Handler App (AuthManager App) ()
--handleLoginSubmit =
--    loginUser "Number of contracts" "password" Nothing
--              (\_ -> handleLogin err) (redirect "/")
--  where
--    err = Just "Unknown user or password"

------------------------------------------------------------------------------
-- | Handle drawdown submit
--TODO: invent this

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/leverage", with auth handleLeverageSubmit)
         , ("/drawdown", with auth handleDrawDownSubmit)
         --, ("/reports", with auth handleReports)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Area91." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    pg <- nestSnaplet "pg" pg pgsInit
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a pg

