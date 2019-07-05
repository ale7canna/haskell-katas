import Test.Hspec

main = hspec $ do
  describe "Static tests" $ do
    it "Static test: empty" $ likes [] `shouldBe` "no one likes this"
    it "Static test: 1 name" $ likes ["Peter"] `shouldBe` "Peter likes this"
    it "Static test: 2 names" $ likes ["Jacob", "Alex"] `shouldBe` "Jacob and Alex like this"
    it "Static test: 3 names" $ likes ["Max", "John", "Mark"] `shouldBe` "Max, John and Mark like this"
    it "Static test: 4 names" $ likes ["Alex", "Jacob", "Mark", "Max"] `shouldBe` "Alex, Jacob and 2 others like this"

likes :: [String] -> String
likes x = accounts x ++ " " ++  message x

accounts :: [String] -> String
accounts [] = "no one"
accounts [a] = a
accounts [a, b] = a ++ " and " ++ b
accounts [a, b, c] = a ++ ", " ++ b ++ " and " ++ c
accounts (a : b : _ : remainings) = a ++ ", " ++ b ++ " and " ++ show (1 + length remainings) ++ " others"

message :: [String] -> String
message x
  | length x < 2 = "likes this"
  | otherwise    = "like this"
