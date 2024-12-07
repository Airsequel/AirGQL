module AirGQL.Types.SchemaConf (
  SchemaConf (..),
  defaultSchemaConf,
) where

import Protolude (Integer)

import AirGQL.Lib (AccessMode, readAndWrite)
import AirGQL.Types.PragmaConf (PragmaConf, defaultConf)


data SchemaConf = SchemaConf
  { accessMode :: AccessMode
  , pragmaConf :: PragmaConf
  , maxRowsPerTable :: Integer
  }


-- | Default schema configuration
defaultSchemaConf :: SchemaConf
defaultSchemaConf =
  SchemaConf
    { accessMode = readAndWrite
    , pragmaConf = AirGQL.Types.PragmaConf.defaultConf
    , maxRowsPerTable = 100_000
    }
