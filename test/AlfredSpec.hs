module AlfredSpec (main, spec) where

import Data.Foldable (find, toList)
import System.Directory (setCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)

import Alfred
import Alfred.Search

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "writeItems" $ do
        it "saves items to a cache file" $ do
            let items =
                    [ makeItem { aiName = "Example One" }
                    , makeItem { aiName = "Example Two" }
                    ]
            cachedItems <- inTestDirectory $ do
                writeItems "cache" items
                getItems "cache"
            toList cachedItems `shouldBe` items
    describe "incrementVisits" $ do
        it "adds one visit to the given URL" $ do
            let incrementedUrl = "http://example.com/increment"
                items =
                    [ makeItem
                    , makeItem
                        { aiUrl = incrementedUrl
                        , aiVisits = 2
                        }
                    , makeItem
                    ]
            cachedItems <- inTestDirectory $ do
                writeItems "cache" items
                incrementVisits "cache" incrementedUrl
                getItems "cache"
            let incrementedItem =
                    (find ((== incrementedUrl) . aiUrl) cachedItems)
            aiVisits <$> incrementedItem `shouldBe` Just 3

makeItem :: Item
makeItem = Item
    { aiName = "Example"
    , aiUrl = "http://example.com"
    , aiVisits = 1
    }

inTestDirectory :: IO a -> IO a
inTestDirectory f = withSystemTempDirectory "test" $ \dir -> do
    setCurrentDirectory dir
    f
