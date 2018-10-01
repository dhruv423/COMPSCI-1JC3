{- Assignment 1
 - Name: Dhruv Bhavsar
 - Date: September 29th 2018
 -}
module Assign_1 where

macid :: String
macid = "bhavsd1"

-- Computes the Cube Root
cubeRoot :: Double -> Double
cubeRoot x = if x < 0
  then -((-x) ** (1/3))
  else x ** (1/3)

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: Takes in 3 values which are doubles and computes the value
 of Q

 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3*a*c - b*b)/(9*a*a)

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: Takes in 4 values a, b, c, d which are doubles and computes
 the value of R
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (9*a*b*c - 27*a*a*d - 2*b*b*b)/(54*a*a*a)

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: Takes in the values of Q and R that are computed in other functions
 and outputs the discriminant
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q*q*q) + (r*r)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: Takes in the values of Q and R that are computed in other functions
 and outputs the S value
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot(r + sqrt(q^3 + r^2))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: Takes in the values of Q and R that are computed in other functions
 and outputs the R value
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot((r - sqrt(q^3 + r^2)))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Takes in 4 values a, b, c, d  and computes real solutions that don't
 require complex math. If the absolute value of discriminant (discriminant equal to zero)
 is less than or equal to the epsilon then it computes 3 solutions, if the discriminant is positive
 then it computes 1 real solution and if it is anything else (discriminant is negative)
 then output empty list because those solutions require complex arithmetic.
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
      | abs (cubicDisc q r) <= 1e-8 = [s + t - b/(3*a), (-(s + t) / 2) - (b/(3*a)) + ((sqrt(3) * (s - t))/2), (-(s + t) / 2) - (b/(3*a)) - ((sqrt(3) * (s - t))/2)]
      | cubicDisc q r > 0           = [s + t - (b/(3*a))]
      | otherwise                   = []
      where
        s = cubicS q r
        t = cubicT q r
        q = cubicQ a b c
        r = cubicR a b c d





{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

{- cubeRoot Function Test

Test 1
cubeRoot 27
Output: 3.0
--

cubeRoot (-2744)
Output: -13.999999999999998
Double Precision Floating Point Numbers are not exact,
they are approximate to the value.
-}

{- cubicRealSolutions Function Test

Test 1
cubicRealSolutions 1 2 3 4
Output: [-1.6506291914393882]
Function returned x1 only because the discriminant was greater than 0
--
Test 2
cubicRealSolutions 1 0 (-3) 0
Output: []
Function returned empty list because the discriminant was negative, to compute
this we require complex arithmetic
--
Test 3
cubicRealSolutions 1 1 0 0
Output: [-1.0,5.551115123125783e-17,5.551115123125783e-17]
Function returned x1, x2, x3 because the discriminant was zero or very close to
zero. x2 and x3 should have been 0 but its not because of floating point approximation


-}
