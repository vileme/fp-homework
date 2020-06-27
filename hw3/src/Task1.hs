{-# LANGUAGE BangPatterns #-}

module Task1
  ( Point(..)
  , crossProduct
  , doubleArea
  , doubleAreaSlow
  , perimeter
  , perimeterSlow
  , plus
  , scalarProduct
  ) where


-- | Data type for the two dimensional point with the integer coordinates.
data Point =
  Point
    { x :: !Int  -- ^ x - axis
    , y :: !Int  -- ^ y - axis
    }
  deriving (Show)

-- | Adding two points.
plus 
  :: Point  -- ^ first point
  -> Point  -- ^ second point
  -> Point  -- ^ sum result
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Subtraction of two points.
minus 
  :: Point -- ^ first point 
  -> Point -- ^ second point
  -> Point -- ^ result point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Scalar production of two points.
scalarProduct 
  :: Point -- ^ first point
  -> Point -- ^ second point
  -> Int   -- ^ integer result
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- | Directed area production.
crossProduct 
  :: Point  -- ^ first point 
  -> Point  -- ^ second point
  -> Int    -- ^  result 
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

-- | Distance between two points.
distance 
  :: Point  -- ^ first point
  -> Point  -- ^ second point
  -> Double -- ^ result 
distance p1 p2 = sqrt $ fromIntegral $ scalarProduct r r
  where
    r :: Point
    r = minus p1 p2

-- | Counts the perimeter of 2d - figure. Fast implementation with forcing strict accumulator.
perimeter 
  :: [Point]  -- ^ Clockwised figure representation 
  -> Double   -- ^ result
perimeter [] = 0
perimeter (c:cs) = myFold c 0 (c : cs)
  where
    myFold :: Point -> Double -> [Point] -> Double
    myFold _ _ [] = 0
    myFold start !acc [lastP] = acc + distance start lastP
    myFold start !acc list@(cx:cxs) = myFold start newAcc (tail list)
      where
        newAcc = acc + distance cx (head cxs)


-- | Counts the perimeter of 2d - figure. Slow implementation for tests and comparison.
perimeterSlow :: [Point] -> Double
perimeterSlow [] = 0
perimeterSlow (c:cs) = myFold c 0 (c : cs)
  where
    myFold :: Point -> Double -> [Point] -> Double
    myFold _ _ [] = 0
    myFold start acc [lastP] = acc + distance start lastP
    myFold start acc list@(cx:cxs) = myFold start newAcc (tail list)
      where
        newAcc = acc + distance cx (head cxs)

-- | Counts the area of 2d - figure. Fast implementation with forcing strict accumulator.
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



-- | Counts the area of 2d - figure. Slow implementation for tests and comparison.
doubleAreaSlow :: [Point] -> Int
doubleAreaSlow [] = 0
doubleAreaSlow (c:cs) = abs $ myFold c 0 (c : cs)
  where
    myFold :: Point -> Int -> [Point] -> Int
    myFold _ _ [] = 0
    myFold start acc [lastP] = acc + crossProduct start lastP
    myFold start acc list@(cx:cxs) = myFold start newAcc (tail list)
      where
        newAcc = acc + crossProduct cx (head cxs)