module Block3Task1 where


maybeConcat :: [Maybe[a]] -> [a]
maybeConcat =
  foldr
    (\x l ->
       case x of
         Nothing -> l
         Just a -> a ++ l)
    []
   