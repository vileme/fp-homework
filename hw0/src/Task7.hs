module Task7
  ( firstExpression
  , secondExpression
  , thirdExpression
  ) where

import Data.Either (lefts, rights)

-- | Every term describe sub-expression's type of the
-- null . head $ map (uncurry id) [((++) "Dorian ", " Grey")]
firstExpression :: Bool
firstExpression =
  let term0 = " Grey" :: String in
    let term1 = "Dorian " :: String in
      let term2 = (++) :: String -> String -> String in
        let term3 = term2 term1 :: String -> String  in
          let term4 = (term3, term0) :: (String -> String, String) in
            let term5 = [term4] :: [(String -> String, String)] in
              let term6 = id :: (String -> String) -> String -> String in
                let term7 = uncurry term6 :: (String -> String, String) -> String in
                  let term8 = map term7 :: [(String -> String, String)] -> [String] in
                    let term9 = term8 term5 :: [String] in
                      let term10 = (null . head) :: [String] -> Bool in
                        term10 term9 :: Bool

-- | Every term describes sub-expression's type of the
-- (\x -> zip (lefts x) (rights x)) [Left (1 + 2), Right (2 ^ 6)]
secondExpression :: [(Integer, Integer)]
secondExpression =
  let term0 = 1 :: Integer  in
    let term1 = 2 :: Integer in
      let term2 = (+) :: Integer -> Integer -> Integer in
        let term3 = 2 :: Integer in
          let term4 = 6 :: Integer  in
            let term5 = (^) :: (Num a, Integral b) => a -> b -> a in
              let term6 = term0 `term2` term1 :: Integer in
                let term7 = term3 `term5` term4 :: Integer in
                  let term8 = Left term6 :: Either Integer b in
                    let term9 = Right term7 :: Either a Integer in
                      let term10 = [term8,term9] :: [Either Integer Integer] in
                        let term11 = lefts :: [Either Integer b] -> [Integer] in
                          let term12 = rights :: [Either a Integer] -> [Integer] in
                            let term13 = zip :: [Integer] -> [Integer] -> [(Integer,Integer)] in
                              let term14 = term11 term10 :: [Integer] in
                                let term15 = term12 term10 :: [Integer] in
                                    term13 term14 term15 :: [(Integer,Integer)]

-- | Every term describes sub-expression's type of the
-- let impl = \x y -> not x || y in
   --    let isMod2 = \x -> x `mod` 2 == 0 in
   --    let isMod4 = \x -> x `mod` 4 == 0 in
   --    \x -> (isMod4 x) `impl` (isMod2 x)
thirdExpression :: Integer -> Bool
thirdExpression x =
  let term0 = not :: Bool -> Bool in
    let term1 = (||) :: Bool -> Bool -> Bool in
      let term2 a b = term0 (term1 a b) :: Bool in
        let term3 = mod :: Integer -> Integer -> Integer in
          let term4 a b = term3 a b :: Integer in
            let term5 = (==) :: Integer -> Integer -> Bool in
              let term6  a = term4 a 2 `term5` 0 :: Bool in
                let term7 a = term4 a 4 `term5` 0 :: Bool in
                  term2 (term6 x) (term7 x) :: Bool
