{- Assignment 1 Extra Credit
 - Name: Dhruv Bhavsar
 - Date: September 30th 2018
 -}
module Assign_1_ExtraCredit where

import Data.Complex
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

macid = "bhavsd1"

-- Computes the Cube Root
cubeRoot :: Double ->  Double
cubeRoot x = if (x) < 0
  then -((-(x)) ** (1/3))
  else (x) ** (1/3)

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: Takes in 3 numbers which are doubles and computes the value
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
 - rootPosNeg
 - -----------------------------------------------------------------
 - Description: This function calculates the square root of a negative number.
It separates the -1 and the number then calculates the square root of the positive
number. For the root of the -1 that turns into an imaginary number which gets added
to the end of answer which takes the form of a complex number.
 -}
rootPosNeg :: Double ->  Double
rootPosNeg a = if a < 0
            then sqrt(a * (-1))
            else sqrt(a)

{- -----------------------------------------------------------------
- cubicComplexS
- -----------------------------------------------------------------
- Description: Takes in the values of Q and R that are computed in other functions
and outputs the S value in a complex number form. Uses the rootPosNeg function
to see if there is a negative under the root and act accordingly.
-}
cubicComplexS :: Double -> Double ->  Complex Double
cubicComplexS q r = cubeRoot(r + rootPosNeg(q**3 + r**2)) :+ 0

{- -----------------------------------------------------------------
- cubicComplexT
- -----------------------------------------------------------------
- Description: Takes in the values of Q and R that are computed in other functions
and outputs the T value in a complex number form. Uses the rootPosNeg function
to see if there is a negative under the root and act accordingly.
-}
cubicComplexT :: Double -> Double ->  Complex Double
cubicComplexT q r = cubeRoot(r - rootPosNeg(q**3 + r**2)) :+ 0

{- -----------------------------------------------------------------
- cubicComplexSolutions
- -----------------------------------------------------------------
- Description: Takes in 4 double values a, b, c, d and computes complex solutions
no matter what the discriminant value is. (b/3*a) was a double type by adding
:+ 0 making it into a complex number type.
-}
cubicComplexSolutions :: Double -> Double -> Double -> Double -> [Complex Double]
cubicComplexSolutions a b c d =  [s + t - (b/(3*a) :+ 0),
                                 (-(s + t) / 2) - ((b/(3*a)) :+ 0) + ((sqrt(3) * (s - t))/2),
                                 (-(s + t) / 2) - ((b/(3*a)) :+ 0) - ((sqrt(3) * (s - t))/2)]
            where
                 q = cubicQ a b c
                 r = cubicR a b c d
                 s = cubicComplexS q r
                 t = cubicComplexT q r


{- Test Cases

cubicComplexSolutions Function Test

Test 1
cubicComplexSolutions 1 2 3 4
Output: [(-1.6506291914393882) :+ 0.0,1.3721834829510904 :+ 0.0,
        (-1.7215542915117021) :+ (-0.0)]
The first value is real root and the other values are non-real complex numbers
--
Test 2
cubicComplexSolutions 1 0 (-3) 0
Output: [0.0 :+ 0.0,1.7320508075688772 :+ 0.0,(-1.7320508075688772) :+ (-0.0)]
All of the values shown are the real roots of the function, verified from Desmos
--
Test 3
cubicComplexSolutions 1 (-1) (-1) 1
Output: [(-1.0) :+ 0.0,1.000000004839294 :+ 0.0,0.9999999951607059 :+ (-0.0)]
The first two values are the real roots of the function verified from Desmos,
the second value is supposed to be 1 but due to floating point numbers it is very
close to 1.
-}
