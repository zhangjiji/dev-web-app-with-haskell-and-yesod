{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
                                                       Person
                                                         name String
                                                         age Int
                                                         deriving Show
                                                      |]

main = runSqlite ":memory:" $ do
  runMigration migrateAll
  michaelId <- insert $ Person "Michael" 26
  michael <- get michaelId
  liftIO $ print michael

