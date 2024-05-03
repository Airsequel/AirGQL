module AirGQL.Types.SchemaConf (
  SchemaConf (..),
  defaultSchemaConf,
) where

import Protolude (Integer)

import AirGQL.Lib (AccessMode (ReadAndWrite))
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
    { accessMode = ReadAndWrite
    , pragmaConf = AirGQL.Types.PragmaConf.defaultConf
    , maxRowsPerTable = 100_000
    }
