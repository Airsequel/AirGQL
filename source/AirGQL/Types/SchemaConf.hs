module AirGQL.Types.SchemaConf (
  SchemaConf (..),
  defaultSchemaConf,
) where

import Protolude (Integer, Text)

import AirGQL.Lib (AccessMode, readAndWrite)
import AirGQL.Types.PragmaConf (PragmaConf, defaultConf)


data SchemaConf = SchemaConf
  { accessMode :: AccessMode
  , pragmaConf :: PragmaConf
  , maxRowsPerTable :: Integer
  , upgradeTarget :: Text
  -- ^ Plan to suggest in upgrade prompts when limits are exceeded.
  -- Includes the article so the prompt reads naturally,
  -- e.g. \"an Enterprise\" or \"a Pro\".
  }


-- | Default schema configuration
defaultSchemaConf :: SchemaConf
defaultSchemaConf =
  SchemaConf
    { accessMode = readAndWrite
    , pragmaConf = AirGQL.Types.PragmaConf.defaultConf
    , maxRowsPerTable = 1_000_000
    , upgradeTarget = "an Enterprise"
    }
