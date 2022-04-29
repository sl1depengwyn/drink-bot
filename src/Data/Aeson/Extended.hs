module Data.Aeson.Extended
  ( module Data.Aeson
  , customOptions
  ) where

import           Data.Aeson

customOptions :: Options
customOptions = defaultOptions {fieldLabelModifier = camelTo2 '_' . tail, sumEncoding = UntaggedValue}
