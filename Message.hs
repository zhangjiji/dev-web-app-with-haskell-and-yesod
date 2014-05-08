{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings #-}

import Yesod

data Message = Message

mkYesod "Message" [parseRoutes|
                   / RootR GET
                   /set-message SetMessageR POST
                   |]

instance Yesod Message where
  defaultLayout widget = do
    pc <- widgetToPageContent widget
    mmsg <- getMessage
    giveUrlRenderer [hamlet|
        $doctype 5
        <html>
            <head>
                <title>#{pageTitle pc}
                ^{pageHead pc}
            <body>
                $maybe msg <- mmsg
                    <p>Your message was: #{msg}
                ^{pageBody pc}
                |]

instance RenderMessage Message FormMessage where
  renderMessage _ _ = defaultFormMessage

getRootR :: Handler Html
getRootR = defaultLayout [whamlet|
    <form method=post action=@{SetMessageR}>
        My message is: #
        <input type=text name=message>
        <input type=submit>
|]

postSetMessageR :: Handler ()
postSetMessageR = do
  msg <- runInputPost $ ireq textField "message"
  setMessage $ toHtml msg
  redirect RootR

main :: IO ()
main = warp 3000 Message
