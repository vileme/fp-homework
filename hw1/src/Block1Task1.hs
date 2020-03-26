{-# LANGUAGE InstanceSigs #-}

module Block1Task1
  ( Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where
  
  
-- | Data class which illustrates days 
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Day where
  toEnum :: Int -> Day
  toEnum x
    | x < 0 = error "No state for negative numbers"
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum x = toEnum $ mod x 7
  fromEnum :: Day -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

instance Eq Day where
  (==) :: Day -> Day -> Bool
  (==) a b = fromEnum a == fromEnum b

-- | Takes a Day and returns the Day after it with help of Enum Instance
nextDay :: Day -> Day
nextDay = toEnum . succ . fromEnum

-- | Takes a Day and Int (n) , return the Day after n days from current
afterDays :: Day -> Int -> Day
afterDays day n = toEnum $ n + fromEnum day

-- | Checks whether the day is wee(d)kend or not
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | Counts how many days to friday
daysToParty :: Day -> Int
daysToParty today = (fromEnum Friday - fromEnum today + 7) `mod` 7
