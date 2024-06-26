module AirGQL.Types.PragmaConf (
  PragmaConf (..),
  getSQLitePragmas,
  defaultConf,
)
where

import Protolude (
  Bool (True),
  IO,
  Int,
  Integer,
  pure,
  show,
  ($),
  (<>),
 )

import Database.SQLite.Simple qualified as SS


data PragmaConf = PragmaConf
  { maxPageCount :: Int
  , hardHeapLimit :: Integer
  , allowRecursTrig :: Bool
  }


defaultConf :: PragmaConf
defaultConf =
  PragmaConf
    { maxPageCount = 4096
    , hardHeapLimit = 500_000_000 -- Bytes
    , allowRecursTrig = True
    }


-- | Get the SQLite pragmas to use for a database
getSQLitePragmas :: PragmaConf -> IO [SS.Query]
getSQLitePragmas pragConf = do
  let
    getPrag key value =
      SS.Query $ "PRAGMA " <> key <> " = " <> value

  pure
    [ getPrag "case_sensitive_like" "True"
    , getPrag "foreign_keys" "True"
    , -- TODO: Check if this really works
      getPrag "hard_heap_limit" $ show @Integer pragConf.hardHeapLimit
    , getPrag "max_page_count" $ show @Int pragConf.maxPageCount
    , getPrag "recursive_triggers" $ show @Bool pragConf.allowRecursTrig
    , -- TODO: Reactivate after https://sqlite.org/forum/forumpost/d7b9a365e0
      --       (Also activate in SqlQuery.hs)
      -- , getPrag "trusted_schema" "False"
      getPrag "writable_schema" "False"
    ]
