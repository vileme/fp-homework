module Task6
  ( secondWhnf
  , whnf
  ) where

import Data.Maybe (mapMaybe)
import Task1      (distributivity)

-- | This expression is in weak head normal form due to pair constructor
whnf :: (Either String b, Either String c)
whnf = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | This expression isn't in weak head normal form
secondWhnf :: Bool
secondWhnf = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  if char == 'o'
    then Just $ exp pi
    else Nothing
