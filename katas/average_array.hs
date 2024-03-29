import Test.Hspec

spec :: Spec
spec = do
  it "averages a list of lists into a list of averages" $ do
    avgArray [[1, 2, 3, 4], [5, 6, 7, 8]]                                                     `shouldBe` [3, 4, 5, 6]
    avgArray [[2, 3, 9, 10, 7], [12, 6, 89, 45, 3], [9, 12, 56, 10, 34], [67, 23, 1, 88, 34]] `shouldBe` [22.5, 11, 38.75, 38.25, 19.5]
    avgArray [[2, 5, 4, 3, 19],[2, 5, 6, 7, 10]]                                              `shouldBe` [2, 5, 5, 5, 14.5]
    avgArray [[2,5,-4,3,-19],[-2,-5,6,7,10]]                                                  `shouldBe` [0,0,1,5,-4.5]
    avgArray [[-2,-18,-45,-10],[0,-45,-20,-34]]                                               `shouldBe` [-1,-31.5,-32.5,-22]
    
main = hspec spec


avgArray :: [[Int]] -> [Double]
avgArray x = map (\n -> n / fromIntegral (length x)) sums
  where sums = foldr1 (zipWith (\x y -> x + y)) $ map (\inner -> map fromIntegral inner) x
