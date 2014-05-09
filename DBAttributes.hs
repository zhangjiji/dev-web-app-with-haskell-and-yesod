{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.Sqlite
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

main = runSqlite ":memory:" $ do
  time <- liftIO getCurrentTime
  runMigration migrateAll
  mId <- insert $ Person "Michael" (Just 26) time
  gId <- insert $ Person "Greg" Nothing time
  m <- get mId
  g <- get gId
  liftIO $ print m
  liftIO $ print g
  
