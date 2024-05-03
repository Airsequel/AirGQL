module AirGQL.Config (
  Config (..),
  maxGraphqlResultCount,
  defaultConfig,
)
where

import Data.Bool (Bool (False))
import Data.Int (Int)


-- | The maximum number of results allowed for the GraphiQL playground
maxGraphqlResultCount :: Int
maxGraphqlResultCount = 10000


data Config = Config
  { maxTablesPerDb :: Int
  , maxColumnsPerTable :: Int
  , maxRowsPerTable :: Int
  , maxVisibleCellsPerTable :: Int
  , maxDbSize :: Int -- Bytes
  , maxCellSize :: Int -- Bytes
  , hardHeapLimit :: Int -- Bytes
  , sqlTimeoutTime :: Int -- Seconds
  , allowRecursiveTriggers :: Bool
  }


defaultConfig :: Config
defaultConfig =
  Config
    { maxTablesPerDb = 100
    , maxColumnsPerTable = 500
    , maxRowsPerTable = 100_000
    , maxVisibleCellsPerTable = 0 -- Not used currently
    , maxDbSize = 100_000_000 -- Bytes
    , maxCellSize = 10_000_000 -- Bytes
    , hardHeapLimit = 500_000_000 -- Bytes
    , sqlTimeoutTime = 20
    , allowRecursiveTriggers = False
    }
