{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, GADTs, FlexibleContexts #-}

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name String
    deriving Show
  Car
    ownerId PersonId Eq
    name String
    deriving Show
|]

main = runSqlite ":memory:" $ do
  runMigration migrateAll
  bruce <- insert $ Person "Bruce Wayne"
  insert $ Car bruce "Bat Mobile"
  insert $ Car bruce "Porsche"
  cars <- selectList [CarOwnerId ==. bruce] []
  liftIO $ print cars
