{-# LANGUAGE BangPatterns #-}

module Task1
  ( Point(..)
  , plus
  , perimeter
  , distance
  , doubleArea
  ) where

data Point =
  Point
    { x :: Int
    , y :: Int
    }
  deriving (Show)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

distance :: Point -> Point -> Double
distance p1 p2 = sqrt $ fromIntegral $ scalarProduct r r
  where
    r :: Point
    r = minus p1 p2

perimeter :: [Point] -> Double
perimeter [] = 0
perimeter (c:cs) = myFold c 0 (c : cs)
  where
    myFold :: Point -> Double -> [Point] -> Double
    myFold _ _ [] = 0
    myFold start !acc [lastP] = acc + distance start lastP
    myFold start !acc list@(cx:cxs) = myFold start newAcc (tail list)
      where
        !newAcc = acc + distance cx (head cxs)

doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea (c:cs) = abs $ myFold c 0 (c : cs)
  where
    myFold :: Point -> Int -> [Point] -> Int
    myFold _ _ [] = 0
    myFold start !acc [lastP] = acc + crossProduct start lastP
    myFold start !acc list@(cx:cxs) = myFold start newAcc (tail list)
      where
        !newAcc = acc + crossProduct cx (head cxs)