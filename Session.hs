{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}

import Yesod
import Control.Applicative ((<$>),(<*>))

data SessionExample = SessionExample

mkYesod "SessionExample" [parseRoutes|
                          / RootR GET POST
                                     |]
getRootR :: Handler RepHtml
getRootR = do
  sess <- getSession
  hamletToRepHtml [hamlet|
                   <form method=post>
                       <input type=text name=key>
                       <input type=text name=val>
                       <input type=submit>
                   <h1>#{show sess}
                   |]

postRootR :: Handler ()
postRootR = do
  (key, mval) <- runInputPost $ (,) <$> ireq textField "key" <*> iopt textField "val"
  case mval of
    Nothing -> deleteSession key
    Just val -> setSession key val

  liftIO $ print (key, mval)
  redirect RootR

instance Yesod SessionExample where
  clientSessionDuration _ = 1

instance RenderMessage SessionExample FormMessage where
  renderMessage _ _ = defaultFormMessage

main :: IO ()
main = warp 3000 SessionExample

