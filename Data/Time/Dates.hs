-- |
-- Module    :  Data.Time.Dates
-- Author    :  Jared Tobin, jared@jtobin.ca
-- License   :  BSD3
--
-- An Integer-free modification of some of the stuff found in Data.Time.
--
-- I've literally just assembled the required functions and changed all relevant
-- type signatures from Integer to Int.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Time.Dates (
    Day(..)
  , addDays
  , diffDays
  , toGregorian
  , fromGregorian
  , addGregorianMonthsClip 
  , addGregorianMonthsRollOver
  , addGregorianYearsClip 
  , addGregorianYearsRollOver
  ) where

import Control.DeepSeq
import Control.Monad
import Data.Typeable
import qualified Data.Time as Time
import Test.QuickCheck
import Test.QuickCheck.All (forAllProperties)

-- Data ------------------------------------------------------------------------

newtype Day = ModifiedJulianDay { 
  toModifiedJulianDay :: Int 
  } deriving (Eq, Ord, Typeable)

instance NFData Day where
  rnf (ModifiedJulianDay d) = rnf d

instance Enum Day where
  succ (ModifiedJulianDay a)     = ModifiedJulianDay (succ a)
  pred (ModifiedJulianDay a)     = ModifiedJulianDay (pred a)
  toEnum                         = ModifiedJulianDay . toEnum
  fromEnum (ModifiedJulianDay a) = fromEnum a
  enumFrom (ModifiedJulianDay a) = fmap ModifiedJulianDay (enumFrom a)

  enumFromThen (ModifiedJulianDay a) (ModifiedJulianDay b)
    = fmap ModifiedJulianDay (enumFromThen a b)

  enumFromTo (ModifiedJulianDay a) (ModifiedJulianDay b) 
    = fmap ModifiedJulianDay (enumFromTo a b)

  enumFromThenTo (ModifiedJulianDay a) (ModifiedJulianDay b) 
                                       (ModifiedJulianDay c) 
    = fmap ModifiedJulianDay (enumFromThenTo a b c)

instance Show Day where
  show = showGregorian

-- Exported functions ----------------------------------------------------------

addDays :: Int -> Day -> Day
addDays n (ModifiedJulianDay d) = ModifiedJulianDay (d + n)

diffDays :: Day -> Day -> Int
diffDays (ModifiedJulianDay a) (ModifiedJulianDay b) = a - b

toGregorian :: Day -> (Int, Int, Int)
toGregorian date = (year, month, day) 
  where (year,  yd)  = toOrdinalDate date
        (month, day) = dayOfYearToMonthAndDay (isLeapYear year) yd

fromGregorian :: Int -> Int -> Int -> Day
fromGregorian year month day = 
  fromOrdinalDate year (monthAndDayToDayOfYear (isLeapYear year) month day)

addGregorianMonthsClip :: Int -> Day -> Day
addGregorianMonthsClip n day = fromGregorian y m d where
 (y,m,d) = addGregorianMonths n day

addGregorianMonthsRollOver :: Int -> Day -> Day
addGregorianMonthsRollOver n day = addDays (d - 1) 
                                           (fromGregorian y m 1) where
  (y,m,d) = addGregorianMonths n day

addGregorianYearsClip :: Int -> Day -> Day
addGregorianYearsClip n = addGregorianMonthsClip (n * 12)

addGregorianYearsRollOver :: Int -> Day -> Day
addGregorianYearsRollOver n = addGregorianMonthsRollOver (n * 12)

-- Tests -----------------------------------------------------------------------

instance Arbitrary Day where
  arbitrary = fmap ModifiedJulianDay (choose (1900, 2000000) :: Gen Int)

instance Arbitrary Time.Day where
  arbitrary = fmap Time.ModifiedJulianDay 
                (choose (1900, 2000000) :: Gen Integer)

newtype Small = Small { getInt :: Int } deriving Show

instance Arbitrary Small where
  arbitrary = fmap Small (choose (1900, 2000000) :: Gen Int)

baseDate :: Day
baseDate = fromGregorian 2013 6 23

altBaseDate :: Time.Day
altBaseDate = Time.fromGregorian 2013 6 23

prop_addDaysEquivalence :: Small -> Bool
prop_addDaysEquivalence (Small n) = 
     (\(a, b, c) -> (fromIntegral a, b, c)) 
       (Time.toGregorian (Time.addDays (fromIntegral n) altBaseDate))
  == toGregorian (addDays n baseDate)

prop_diffDaysEquivalence :: Day -> Day -> Bool
prop_diffDaysEquivalence day0 day1 = 
     diffDays day0 day1
  == let (y0, m0, d0) = toGregorian day0
         (y1, m1, d1) = toGregorian day1

         day0n = Time.fromGregorian (fromIntegral y0) m0 d0
         day1n = Time.fromGregorian (fromIntegral y1) m1 d1

     in  fromIntegral $ Time.diffDays day0n day1n

prop_gregorianEquivalence :: Day -> Bool
prop_gregorianEquivalence d = 
  let (y0, m0, d0) = toGregorian d
      dAlt         = Time.fromGregorian (fromIntegral y0) m0 d0
      (y1, m1, d1) = Time.toGregorian dAlt
  in  (fromIntegral y0 == fromIntegral y1) && m0 == m1 && d0 == d1

prop_addGregorianMonthsClipEquivalence :: Small -> Bool
prop_addGregorianMonthsClipEquivalence (Small n) =
  let d            = addGregorianMonthsClip n baseDate
      (y0, m0, d0) = toGregorian d
      dAlt         = Time.fromGregorian (fromIntegral y0) m0 d0
  in  dAlt == Time.addGregorianMonthsClip (fromIntegral n) altBaseDate

prop_addGregorianMonthsRollOverEquivalence :: Small -> Bool
prop_addGregorianMonthsRollOverEquivalence (Small n) =
  let d            = addGregorianMonthsRollOver n baseDate
      (y0, m0, d0) = toGregorian d
      dAlt         = Time.fromGregorian (fromIntegral y0) m0 d0
  in  dAlt == Time.addGregorianMonthsRollOver (fromIntegral n) altBaseDate

prop_addGregorianYearsClipEquivalence :: Small -> Bool
prop_addGregorianYearsClipEquivalence (Small n) =
  let d            = addGregorianYearsClip n baseDate
      (y0, m0, d0) = toGregorian d
      dAlt         = Time.fromGregorian (fromIntegral y0) m0 d0
  in  dAlt == Time.addGregorianYearsClip (fromIntegral n) altBaseDate

prop_addGregorianYearsRollOverEquivalence :: Small -> Bool
prop_addGregorianYearsRollOverEquivalence (Small n) =
  let d            = addGregorianYearsRollOver n baseDate
      (y0, m0, d0) = toGregorian d
      dAlt         = Time.fromGregorian (fromIntegral y0) m0 d0
  in  dAlt == Time.addGregorianYearsRollOver (fromIntegral n) altBaseDate

runTestSuite :: IO Bool
runTestSuite = $forAllProperties 
                 (quickCheckWithResult (stdArgs { maxSuccess = 1000 }))

main :: IO ()
main = void runTestSuite

-- Internal --------------------------------------------------------------------

type NumericPadOption = Maybe Char

isLeapYear :: Int -> Bool
isLeapYear year =  (mod year 4   == 0) 
               && ((mod year 400 == 0) || not (mod year 100 == 0))

toOrdinalDate :: Day -> (Int, Int)
toOrdinalDate (ModifiedJulianDay mjd) = (year, yd) 
  where a        = mjd + 678575
        quadcent = div a 146097
        b        = mod a 146097
        cent     = min (div b 36524) 3
        c        = b - (cent * 36524)
        quad     = div c 1461
        d        = mod c 1461
        y        = min (div d 365) 3
        yd       = (d - (y * 365) + 1)
        year     = quadcent * 400 + cent * 100 + quad * 4 + y + 1

dayOfYearToMonthAndDay :: Bool -> Int -> (Int, Int)
dayOfYearToMonthAndDay isLeap yd = 
  findMonthDay (monthLengths isLeap) (clip 1 (if isLeap then 366 else 365) yd)

findMonthDay :: [Int] -> Int -> (Int, Int)
findMonthDay (n:ns) yd | yd > n = (\(m, d) -> (m + 1, d)) 
                                    (findMonthDay ns (yd - n))
findMonthDay _ yd               = (1, yd)

monthLengths :: Bool -> [Int]
monthLengths isleap = 
  [31, if isleap then 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x         = x

fromOrdinalDate :: Int -> Int -> Day
fromOrdinalDate year day = ModifiedJulianDay mjd where
  y   = year - 1
  mjd = (clip 1 (if isLeapYear year then 366 else 365) day) 
          + (365 * y) + (div y 4) - (div y 100) + (div y 400) - 678576

monthAndDayToDayOfYear :: Bool -> Int -> Int -> Int
monthAndDayToDayOfYear isLeap month day = 
    (div (367 * month'' - 362) 12) + k + day' 
  where month' = clip 1 12 month
        day' = (clip 1 (monthLength' isLeap month') day)
        month'' = month'
        k = if month' <= 2 then 0 else if isLeap then -1 else -2

monthLength' :: Bool -> Int -> Int
monthLength' isLeap month' = (monthLengths isLeap) !! (month' - 1)

rolloverMonths :: (Int, Int) -> (Int, Int)
rolloverMonths (y,m) = (y + (div (m - 1) 12), (mod (m - 1) 12) + 1)

addGregorianMonths :: Int -> Day -> (Int, Int, Int)
addGregorianMonths n day = (y',m',d) where
  (y, m, d) = toGregorian day
  (y',m') = rolloverMonths (y, m + n)

showGregorian :: Day -> String
showGregorian date = (show4 (Just '0') y) 
                  ++ "-" ++ (show2 (Just '0') m) 
                  ++ "-" ++ (show2 (Just '0') d) 
  where (y, m, d) = toGregorian date

show4 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show4 = showPaddedMin 4

show2 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show2 = showPaddedMin 2

show3 :: (Num t,Ord t,Show t) => NumericPadOption -> t -> String
show3 = showPaddedMin 3

showPaddedMin :: (Num t,Ord t,Show t) => Int -> NumericPadOption -> t -> String
showPaddedMin _ Nothing i = show i
showPaddedMin pl opt i | i < 0 = '-':(showPaddedMin pl opt (negate i))
showPaddedMin pl (Just c) i =
  let s = show i in 
    padN (pl - (length s)) c s

padN :: Int -> Char -> String -> String
padN i _ s | i <= 0 = s
padN i c s = (replicate i c) ++ s

