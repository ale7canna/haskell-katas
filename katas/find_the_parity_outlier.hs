import Test.Hspec

main = hspec $ do
  describe "Find Outlier" $ do
    it "more tests" $ do
      findOutlier [2,6,8,-10,3] `shouldBe` 3
      findOutlier [206847684,1056521,7,17,1901,21104421,7,1,35521,1,7781] `shouldBe` 206847684
      findOutlier [2147483647,0,1] `shouldBe` 0

findOutlier :: [Int] -> Int
findOutlier x = head $ lonely evenAndOdds
  where evenAndOdds = foldr (\item acc -> move item acc) ([], []) x

lonely :: ([a], [a]) -> [a]
lonely (x, y)
  | length x == 1 = x
  | otherwise = y

move :: Int -> ([Int], [Int]) -> ([Int], [Int])
move x evenAndOdds
  | isEven x = ((x : fst evenAndOdds), snd evenAndOdds)
  | otherwise = (fst evenAndOdds, (x : snd evenAndOdds))

isEven :: Integral a => a -> Bool
isEven a = a `mod` 2 == 0
