module Data.Time.Extended
    (
        module Data.Time,
        nowFormatted

    ) where

import Data.Time


nowFormatted :: IO String
nowFormatted = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime