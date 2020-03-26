module Block3Task1
  ( maybeConcat
  ) where

-- | Concats the lists of Maybe's into one list
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat =
  foldr
    (\x l ->
       case x of
         Nothing -> l
         Just a -> a ++ l)
    []