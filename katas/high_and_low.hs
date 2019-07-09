import Test.Hspec

main :: IO ()
main = hspec $
  describe "Example Tests" $ do
    it "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" $ 
      highAndLow "4 5 29 54 4 0 -214 542 -64 1 -3 6 -6" `shouldBe`  "542 -214"

highAndLow :: String -> String
highAndLow input = mx values ++ " " ++ mn values
  where
    values = map (\x -> read x :: Int) $ words input
    mx = show . maximum
    mn = show . minimum
