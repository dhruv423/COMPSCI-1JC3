{- Assignment 5 Tests
 - Name: Dhruv Bhavsar
 - Date: November 30, 2018
 -}

import Assign_5

import Test.QuickCheck

main :: IO ()
main = do print "Performing definiteIntegral Test 1: "
          quickCheck defIntegralProp
          print "Performing definiteIntegral Test 2: "
          quickCheck defIntegralProp2
          print "Performing definiteIntegral Test 3: "
          quickCheck defIntegralProp3
          print "Performing funH Test 1:"
          quickCheck funHProp
          print "Performing funH Test 2:"
          quickCheck funHProp2
          print "Performing funK Test 1:"
          quickCheck funKProp
          print "Performing funK Test 2:"
          quickCheck funKProp2


{- 
Function: definiteIntegral
Propery: The reverse limits of the integral changes the sign of the definite integral
Actual Test Result: Pass
-}
defIntegralProp :: Double -> Double -> Integer -> Bool
defIntegralProp a b n  = if n <= 0
                            then True
                            else abs((definiteIntegral a b (\x -> x^2) n) + (definiteIntegral b a (\x -> x^2) n)) <= 1e-3

{- 
Function: definiteIntegral
Propery: When the upper and lower limits of the integral are the same, the definite integral is 0
Actual Test Result: Pass
-}
defIntegralProp2 :: Double -> Integer -> Bool
defIntegralProp2 a n = if n <= 0
                            then True
                            else definiteIntegral a a (\x -> x^7 + 8*x - 9) n <= 1e-9


{- 
Function: definiteIntegral
Propery: The definite integral of constant times a function is the same as the constant times the definite integral of the function
Actual Test Result: Pass
-}
defIntegralProp3 :: Double -> Double -> Integer -> Bool
defIntegralProp3 a b n = if n <= 0
                            then True
                            else (definiteIntegral a b (\x -> 5*cos x) n) - (5 * definiteIntegral a b (\x -> cos x) n) <= 1e-3


{- 
Function: funH
Propery: The area between the curves will always be less than 1 because of the limit provided in the assignment
Actual Test Result: Pass
-}
funHProp :: Integer -> Bool
funHProp n = if n <= 0
                then True
                else funH n <= 1

{- 
Function: funH
Propery: The higher the value the closer the area to 1 between the curves because when x -> infinity, h(x) -> 1
Actual Test Result: Pass
-}
funHProp2 :: Integer -> Integer -> Bool
funHProp2 a b = if (a > 0 && b > a)
                    then funH b > funH a
                    else True

{- 
Function: funK
Propery: The area under the curve will always be positive because this is an exponential function and it will never go below x axis because n > 0
Actual Test Result: Pass
-}
funKProp :: Double -> Bool
funKProp n = if n <= 0 
                then True
                else funK n > 0

{- 
Function: funK
Propery: The higher the value the higher the area under the curve because when x -> infinity, k(x) -> infinity
Actual Test Result: Pass
-}
funKProp2 :: Double -> Double -> Bool
funKProp2 a b = if (a > 1 && b > a)
                    then funK b > funK a
                    else True
