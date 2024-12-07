module AirGQL.Types.SchemaConf (
  SchemaConf (..),
  defaultSchemaConf,
) where

import Protolude (Bool (True), Integer)

import AirGQL.Lib (AccessMode, mkAccessMode)
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
    { accessMode = mkAccessMode True True True
    , pragmaConf = AirGQL.Types.PragmaConf.defaultConf
    , maxRowsPerTable = 100_000
    }
