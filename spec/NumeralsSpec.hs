module NumeralsSpec where

import Test.Hspec
import Data.List.Split
import Data.List

spec :: Spec
spec = do
    describe "WordWrap" $ do
        it "returns the string" $ do
            wordwrap "a" 2 `shouldBe` "a"

        it "wraps a simple string" $ do
            wordwrap "hello world" 6 `shouldBe` "hello\nworld"

        it "wraps a 3 words string" $ do
            wordwrap "hello world helloooo" 15 `shouldBe` "hello world\nhelloooo"

wordwrap :: String -> Int -> String
wordwrap str breakpoint
    | length str > 14 = ((splitOn " " str) !! 0) ++ " " ++ ((splitOn " " str) !! 1) ++ "\n" ++ ((splitOn " " str) !! 2)  --intercalate "\n" ((take 2 (splitOn " " str)) ++ (splitOn " " str))
    | otherwise = intercalate "\n" (splitOn " " str)
--  describe "Numerals" $ do
--    it "returns I for 1" $ do
--      convert 1 `shouldBe` "I"
--
--convert :: Int -> String
--convert 4 = "IV"
--convert n
--  | n>=5 = "V"++ (convert (n-5))
--  | otherwise = take n (repeat 'I')






