{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Data.Time

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
                                                       Person
                                                         name String
                                                         age Int Maybe
                                                         created UTCTime default=CURRENT_TIME
                                                         deriving Show
                                                      |]

main = withPostgresqlConn "host=localhost port=5432 user=zhangjiji dbname=exampledb password=zhangzheno1" $ runSqlConn $ do
--  time <- liftIO getCurrentTime
  runMigration migrateAll
--  mId <- insert $ Person "Michael" (Just 26) time
--  gId <- insert $ Person "Greg" Nothing time
--  m <- get mId
--  g <- get gId
--  liftIO $ print m
--  liftIO $ print g
--  list <- selectList [PersonAge >=. 10] []
--  liftIO $ print list
