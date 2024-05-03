module AirGQL.Types.Utils (
  encodeToText,
)
where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import Protolude (Text, (.))


encodeToText :: (ToJSON a) => a -> Text
encodeToText =
  decodeUtf8 . toStrict . encode
