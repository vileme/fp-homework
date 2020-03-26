{-# LANGUAGE InstanceSigs #-}
module Block4Task1 where

import           Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum  = fmap sum . traverse readMaybe . words


