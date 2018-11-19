{- Assignment 4 Tests
 - Name: Dhruv Bhavsar
 - Date: Nov, 18 2018
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
main = do print "Performing polyListValue Test: "
          quickCheck propValue
          print "Performing polyListDegree Test:"
          quickCheck propDegree
          print "Performing polyListDeriv Test:"
          quickCheck propDeriv
          print "Performing polyListSum Test 1:"
          quickCheck propSum
          print "Performing polyListSum Test 2:"
          quickCheck propSum2
          print "Performing polyListProd Test 1:"
          quickCheck propProd
          print "Performing polyListProd Test 2:"
          quickCheck propProd2

{-
Function: polyListValue
Property: Sum of coefficients is equal to value at 1
Actual Test Result: Pass
-}

propValue :: [Integer] -> Bool
propValue xs = sum xs == polyListValue (PolyList xs) 1

{-
Function: polyListDegree
Property: Length of list will always be bigger than degree of list
Actual Test Result: Pass
-}
propDegree :: [Integer] -> Bool
propDegree [] = True
propDegree [0] = True
propDegree xs = toInteger (length xs) > polyListDegree (PolyList xs)

{-
Function: polyListDeriv
Property: Degree of derivative will be less than or equal to degree of the list
Actual Test Result: Pass
-}
propDeriv :: [Integer] -> Bool
propDeriv [] = True
propDeriv [x] = True
propDeriv xs = polyListDegree (polyListDeriv (PolyList xs)) <= polyListDegree (PolyList xs)

{-
Function: polyListSum
Property: Commutative Law a + b == b + a Used the polyListValue to see if it is the same
Actual Test Result: Pass
-}
propSum :: [Integer] -> [Integer] -> Bool
propSum xs ys = polyListValue (polyListSum (PolyList xs) (PolyList ys)) 3 == polyListValue (polyListSum (PolyList ys) (PolyList xs)) 3

{-
Function: polyListSum
Property: Associative Law (x+y) + z == (z+y) + z Used the polyListValue to see if it is the same
Actual Test Result: Pass
-}
propSum2 :: [Integer] -> [Integer] -> [Integer] -> Bool
propSum2 xs ys zs = polyListValue (polyListSum (polyListSum (PolyList xs) (PolyList ys)) (PolyList zs)) 3 == polyListValue (polyListSum (polyListSum (PolyList zs) (PolyList ys)) (PolyList xs)) 3

{-
Function: polyListProd
Property: Commutative Law (a * b) == (b * a) Used the polyListValue to see if it is the same
Actual Test Result: Pass
-}
propProd :: [Integer] -> [Integer] -> Bool
propProd xs ys = polyListValue (polyListProd (PolyList xs) (PolyList ys)) 3 == polyListValue (polyListProd (PolyList ys) (PolyList xs)) 3

{-
Function: polyListProd
Property: Associative Law (x*y) * z == (z*y) * z Used the polyListValue to see if it is the same
Actual Test Result: Pass
-}
propProd2 :: [Integer] -> [Integer] -> [Integer] -> Bool
propProd2 xs ys zs = polyListValue (polyListProd (polyListProd (PolyList xs) (PolyList ys)) (PolyList zs)) 3 == polyListValue (polyListProd (polyListProd (PolyList zs) (PolyList ys)) (PolyList xs)) 3
