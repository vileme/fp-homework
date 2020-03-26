{-# LANGUAGE InstanceSigs #-}

module Block4Task1
  ( stringSum
  ) where

import Text.Read (readMaybe)

-- | Counts the sum of integers in list, returns Nothing if meets non-integer
stringSum :: String -> Maybe Int
stringSum = fmap sum . traverse readMaybe . words
