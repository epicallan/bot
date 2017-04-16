module App.Types where
import Control.Monad.Eff.Ref (Ref)
import Data.Either (Either)
import Control.Monad.Eff.Exception (Error)
import Database.Mongo.Mongo (Database)

type AppDb = Either Error Database

type DbRef = Ref AppDb
