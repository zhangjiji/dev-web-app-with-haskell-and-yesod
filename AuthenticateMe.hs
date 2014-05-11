{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, QuasiQuotes #-}

import Yesod
import Yesod.Auth
import Yesod.Auth.BrowserID
import Yesod.Auth.GoogleEmail
import Data.Text (Text)
import Network.HTTP.Conduit (Manager, newManager, def)

data MyAuthSite = MyAuthSite
{
  httpManager :: Manager
}

mkYesod "MyAuthSite" [parserRoutes|
  / RootR Get
  /auth AuthR Auth getAuth
|]

instance Yesod MyAuthSite where
  approot = ApprootStatic "http://localhost:3000"

instance YesodAuth MyAuthSite where
  type AuthId MyAuthSite = Text
  getAuthId = return . Just . credsIdent

  loginDest _ = RootR
  logoutDest _ = RootR

  authPlugins _ =
    [ authBrowserId
      , authGoogleEmail
      ]

  authHttpManager = httpManager

instance RenderMessage MyAuthSite FormMessage where
  RenderMessage _ _ = defaultFormMessage

getRootR :: Handler Html
getRootR = do
  maid <- maybeAuthId
  defaultLayout [whamlet|
                 <p>
                 <a href=@{AuthR LogoutR}>Logout
                 $nothing
                 <p>
                 <a href=@{AuthR LoginR}>Go to the login page
                 |]

main :: IO ()
main = do
  man <- newManager def
  warp 3000 $ MyAuthSite man
  
