{- Assignment 5
 - Name: Dhruv Bhavsar
 - Date: November 28, 2018
 -}
module Assign_5 where

macid :: String
macid = "bhavsd1"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: Computes an approximation of the definite integral using the trapezoid rule.
 This function is done recursively and the base case is when n is equal 1 then it is just the
 trapezoid rule with xi - 1 as a and xi as b. The recursive case is when n is greater than 1, it is
 still the trapezoid rule but xi - 1 is a and xi is the addioon of a and deltaX. Then you add with the next
 partition where a will be a plus deltaX because we need the new partition now and n will decrease by 1 each time
 it recurses which will reach the base case.
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g 1 = (g (a) + g (b)) / 2 * (b - a)
definiteIntegral a b g n = (g (a) + g (y)) / 2 * deltaX + definiteIntegral (a + deltaX) b g (n-1)
                  where
                    deltaX = ((b - a) / fromIntegral n)
                    y = a + deltaX

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: Computes the area between the curves x^1/n and x^n from 0 to 1. This uses the definiteIntegral
 function to find the area between the curve for both functions and then subtract the lower function from the upper function.
 The function x^1/n will always be the upper function when n >= 0 and the other function being the lower function. For the
 number of partitions in the definiteIntegral function, I decided to use 10000 because the larger the number the more accurate
 the area. If I were to increase it to 1000000 or greater the computation will take some time but with 10000 it is faster. The
 reason I used ** for the exponents is because ** is for floating point exponentiation, 1 / n makes it floating point. I used
 ^ because its for non-negative integer and n is going to be an integer which is greater than 0. I threw an error undefined
when n less than equal to 0.
 -}
funH :: Integer -> Double
funH n = if n <= 0
            then error "undefined"
            else definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral n)) 10000 - definiteIntegral 0 1 (\x -> x ^ n) 10000

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: Computes the area between n ^ x and the x-axis from -1 to 1. This uses the definiteIntegral function
 to find the area between the curve and x-axis. For the number of partitions in the definiteIntegral function, the same
 reason as above applies here as well. I used ** for the exponent because in the definiteIntegral function the x can be
 floating point because of deltaX. I threw an error undefined when n less than equal to 0.
 -}
funK :: Double -> Double
funK n = if n <= 0
            then error "undefined"
            else definiteIntegral (-1) (1) (\x -> n ** x) 10000

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: definiteIntegral 2 4 (\x -> x^2) 1000
 - - Expected Output: 18.666668
 - - Acutal Output: 18.666668

 - - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: definiteIntegral 4 2 (\x -> x^2) 1000
 - - Expected Output: -18.666668000000065
 - - Acutal Output: -18.666668000000065

 - - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: definiteIntegral 2 2 (\x -> x^2 + 2*x - 5) 1000
 - - Expected Output: 0
 - - Acutal Output: 0

 - - Function: definiteIntegral
 - - Test Case Number: 4
 - - Input: definiteIntegral (-pi) pi (\x -> cos x) 10000
 - - Expected Output: 2.807825586620405e-13
 - - Acutal Output: 2.807825586620405e-13

 - - Function: definiteIntegral
 - - Test Case Number: 5
 - - Input: definiteIntegral (-10) 5 (\x -> x - 5) 100
 - - Expected Output: -112.49999999999986
 - - Acutal Output: -112.49999999999986

 - - Function: funH
 - - Test Case Number: 1
 - - Input: funH 4
 - - Expected Output: 0.5999967923624612
 - - Acutal Output: 0.5999967923624612

 - - Function: funH
 - - Test Case Number: 2
 - - Input: funH 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0

 - - Function: funH
 - - Test Case Number: 3
 - - Input: funH 1000
 - - Expected Output: 0.9979517141629071
 - - Acutal Output: 0.9979517141629071

 - - Function: funH
 - - Test Case Number: 4
 - - Input: funH 100000
 - - Expected Output: 0.9998900010860307
 - - Acutal Output: 0.9998900010860307

 - - Function: funK
 - - Test Case Number: 1
 - - Input: funK 1
 - - Expected Output: 1.9999999999999185
 - - Acutal Output: 1.9999999999999185

 - - Function: funK
 - - Test Case Number: 2
 - - Input: funK 0.5
 - - Expected Output: 2.1640425647992996
 - - Acutal Output: 2.1640425647992996

 - - Function: funK
 - - Test Case Number: 3
 - - Input: funK 2.454
 - - Expected Output: 2.279667973568788
 - - Acutal Output: 2.279667973568788

 - - Function: funK
 - - Test Case Number: 4
 - - Input: funK 100
 - - Expected Output: 21.71255415765715
 - - Acutal Output: 21.71255415765715

 - - Function: funK
 - - Test Case Number: 5
 - - Input: funK 100000
 - - Expected Output: 8685.893474838036
 - - Acutal Output: 8685.893474838036
 - -----------------------------------------------------------------
 -      quickCheck Test Cases
 
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


 -}
