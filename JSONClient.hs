{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class  (liftIO)
import Data.Aeson              (Value (Object, String))
import Data.Aeson              (encode, object, (.=))
import Data.Aeson.Parser       (json)
import Data.Conduit            (($$+-))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl,
                                          requestBody, withManager)

main :: IO ()
main = withManager $ \manager -> do
  value <- liftIO makeValue
  let valueBS = encode value
  req' <- liftIO $ parseUrl "http://localhost:3000/"
  let req = req' {method = "POST", requestBody = RequestBodyLBS valueBS}
  res <- http req manager
  resValue <- responseBody res $$+- sinkParser json
  liftIO $ handleResponse resValue

makeValue :: IO Value
makeValue = return $ object
  [("foo" .= ("bar" :: String))]

handleResponse :: Value -> IO ()
handleResponse = print
