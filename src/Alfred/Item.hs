module Alfred.Item
    ( Item (..)
    , ToAlfredItem (..)
    ) where

import Data.Csv
import qualified Data.Text as T

data Item = Item
    { aiName :: T.Text
    , aiUrl :: T.Text
    , aiVisits :: Int
    } deriving (Eq, Show)

instance ToNamedRecord Item where
    toNamedRecord (Item name url visits) =
        namedRecord [ "name" .= name
                    , "url" .= url
                    , "visits" .= visits
                    ]

instance FromNamedRecord Item where
    parseNamedRecord m = Item
                          <$> m .: "name"
                          <*> m .: "url"
                          <*> m .: "visits"

instance DefaultOrdered Item where
    headerOrder _ = header ["name", "url", "visits"]

class ToAlfredItem a where
    toAlfredItem :: a -> Item
