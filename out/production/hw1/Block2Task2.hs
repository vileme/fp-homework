module Block2Task2 where

import           Data.List.NonEmpty as NonEmpty

splitOn :: (Eq a) => a -> [a] -> NonEmpty [a]
splitOn delim list = NonEmpty.fromList $ foldr (\a (n:ns) -> if delim == a then [] : n : ns else (a : n) : ns) [[]] list