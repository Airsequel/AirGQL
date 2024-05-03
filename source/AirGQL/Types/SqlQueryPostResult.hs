module AirGQL.Types.SqlQueryPostResult (
  SqlQueryPostResult (..),
  resultWithErrors,
)
where

import Protolude (
  Generic,
  Show,
  Text,
  foldMap,
  fromMaybe,
  ($),
  (&),
  (<>),
 )

import Control.Arrow ((>>>))
import Data.Aeson (
  FromJSON,
  Object,
  ToJSON,
  Value (Null, Number),
 )
import Data.Aeson.Encoding (list, pair, pairs)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (toEncoding, (.=))
import Data.Fixed (Pico)
import Servant.Docs (ToSample (toSamples), singleSample)


data SqlQueryPostResult = SqlQueryPostResult
  { affectedTables :: [Text]
  , rows :: [Object]
  , columns :: [Text] -- Only necessary for order of columns in the result
  , runtimeSeconds :: Pico -- Precision contained by `NominalDiffTime`
  , errors :: [Text]
  }
  deriving (Show, Generic)


instance FromJSON SqlQueryPostResult


{-| Even though JSON objects are unordered by definition,
the fields (columns) must be returned in the requested order
as Elm relies on it for decoding.
-}
instance ToJSON SqlQueryPostResult where
  toEncoding sqlQueryPostResult =
    pairs $
      "affectedTables" .= sqlQueryPostResult.affectedTables
        <> "rows"
          `pair` ( sqlQueryPostResult.rows
                    & list
                      ( \(row :: Object) ->
                          -- Apply order of columns
                          sqlQueryPostResult.columns
                            & foldMap
                              ( Key.fromText
                                  >>> ( \col ->
                                          col
                                            .= ( row
                                                  & KeyMap.lookup col
                                                  & fromMaybe Null
                                               )
                                      )
                              )
                            & pairs
                      )
                 )
        <> "runtimeSeconds" .= sqlQueryPostResult.runtimeSeconds
        <> "errors" .= sqlQueryPostResult.errors


instance ToSample SqlQueryPostResult where
  toSamples _ =
    singleSample $
      SqlQueryPostResult
        { affectedTables = ["users"]
        , rows =
            [ KeyMap.fromList
                [ (Key.fromText "id", Number 1)
                , (Key.fromText "name", "John")
                ]
            , KeyMap.fromList
                [ (Key.fromText "id", Number 2)
                , (Key.fromText "name", "Jane")
                ]
            ]
        , columns = ["id", "name"]
        , runtimeSeconds = 0.05
        , errors = []
        }


-- | Construct a result for a failed sql query execution.
resultWithErrors :: Pico -> [Text] -> SqlQueryPostResult
resultWithErrors runtimeSeconds errors =
  SqlQueryPostResult
    { affectedTables = []
    , rows = []
    , columns = []
    , runtimeSeconds = runtimeSeconds
    , errors = errors
    }
