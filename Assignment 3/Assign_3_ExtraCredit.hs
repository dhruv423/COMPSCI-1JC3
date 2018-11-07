{- Assignment 3 Extra Credit
 - Name: Dhruv Bhavsar
 - Date: November 1, 2018
 -}
module Assign_3_ExtraCredit where
import Assign_3
macid = "bhavsd1"

data PolyAlt a = Monomial a Integer
               | SumAlt (PolyAlt a) (PolyAlt a)
  deriving Show


{- polyAltValue
- Description: Evaluates the polynomial at a specified point.
-}
polyAltValue :: Num a => PolyAlt a -> a -> a
polyAltValue (Monomial c e) y  = c * (y ^ abs(e)) -- abs because you can enter negative but it will be treated as positive
polyAltValue (SumAlt x y) z    = polyAltValue x z + polyAltValue y z -- sum of the values at the specified point


{- polyAltDegree
- Description: Computes the degree of the polynomial
-}
polyAltDegree :: (Num a, Eq a) => PolyAlt a -> Integer
polyAltDegree (Monomial c e) = abs(e) -- This data type makes it easy to display the degree as it is the second arugument
polyAltDegree (SumAlt x y) -- Returns the highest degree of the sum
                   | polyAltDegree x >= polyAltDegree y = polyAltDegree x -- recursively finds the degree of the 2 aruguments
                   | otherwise                          = polyAltDegree y

{- polyAltDeriv
- Description: Computes the derivative of polynomial. The base case is when it
is a constant which will evaluate to 0. Deriving the Monomial is the same way
you would derive something using the power rule. For the SumAlt, I recursively
call polyAltDeriv on the two aruguments and then use SumAlt to add them together
-}
polyAltDeriv :: Num a => PolyAlt a -> PolyAlt a
polyAltDeriv (Monomial c 0) = Monomial 0 0
polyAltDeriv (Monomial c e) =
                    let
                          a = c * abs (fromIntegral e) -- Brings the exponent down and multiply it by the coefficent
                          b = (abs e - 1) -- Reduces the exponent by one
                          in Monomial a b
polyAltDeriv (SumAlt x y)   = SumAlt (polyAltDeriv x) (polyAltDeriv y) -- Calls polyAltDeriv on the two aruguments


{- polyAltProd
Description: Computes the product of two Monomials. The base case here is when
you have two Monomials and the product of them is simply multiplying them. When
there is two polynomials the additive property is applied meaning a(b+c) = ab +ac
The other way is the same thing just the order changed. Its stuck together with
SumAlt and called polyAltProd on a b and a c.

-}
polyAltProd :: Num a => PolyAlt a -> PolyAlt a -> PolyAlt a
polyAltProd (Monomial c e) (Monomial x y) =
                    let
                          a = c * x
                          b = abs e + abs y
                          in Monomial a b
polyAltProd a (SumAlt b c) = SumAlt (polyAltProd a b) (polyAltProd a c) -- Expand the polynomial, applied the a to all of the aruguments
polyAltProd (SumAlt a b) c = SumAlt (polyAltProd a c) (polyAltProd b c) -- Same thing as above just different pattern


{- polyAltNewton
Description: Computes the root of the polynomial based on the seed and tolerance.
To find the next approximation of the root, you have to follow the Newton's method formula which is
x2 = x1 - f(x1)/f'(x1). After you have to check if the polyAltValue at the approximation
is below the tolerance and if it is then that is the next root. If its not the less
than the tolerance, then pass it as the seed using recursion.Used polyAltValue
and polyAltDeriv functions to calculate the approximation of the root.
-}
polyAltNewton :: (Fractional a, Ord a) => PolyAlt a -> a -> a -> a
polyAltNewton p s t =
  let est = s - (polyAltValue p s / polyAltValue (polyAltDeriv p) s) -- Applied Newtons's Method
                  in if abs (polyAltValue p est) < t -- Checks if it is less than tolerance
                        then est
                      else polyAltNewton p est t -- Calls the function again with the approximation as the seed


{- polyToPolyAlt
Description: Converts from Poly to PolyAlt. Starting with the base cases, if its
just X or a constant then it will return X and the constant in Monomial form. For
the Sum, it will recursively call polyToPolyAlt on the two aruguments and then
combine them using SumAlt. For Prod, it will do the same thing as Sum, except multiplying
them together using polyAltProd.
-}
polyToPolyAlt :: (Num a, Eq a) => Poly a -> PolyAlt a
polyToPolyAlt X          = Monomial 1 (1) -- Returns x in Monomial form
polyToPolyAlt (Coef a)   = Monomial a 0 -- Returns a*x^0 = a
polyToPolyAlt (Sum a b)  = SumAlt (polyToPolyAlt a) (polyToPolyAlt b) -- polyAltToPoly on a and b then sticked together with SumAlt
polyToPolyAlt (Prod a b) = polyAltProd (polyToPolyAlt a) (polyToPolyAlt b) -- polyAltToPoly on a and b then used polyAltProd for the product


{- polyAltToPoly
Description: Converts PolyAlt to Poly. Starting wit the base cases, if it just
a constant and aX^n then it returns the coefficent and the product of the
Monomial respectively. For SumAlt, call polyAltToPoly
on the two aruguments and then combine them with Sum.
-}
polyAltToPoly :: (Num a, Eq a) => PolyAlt a -> Poly a
polyAltToPoly (Monomial a 0) = (Coef a)
polyAltToPoly (Monomial a 1) = Prod (polyAltToPoly (Monomial a 0)) X
--polyAltToPoly (Monomial a b) = Prod (
polyAltToPoly (SumAlt a b)   = Sum (polyAltToPoly a) (polyAltToPoly b)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: polyAltValue
 - - Test Case Number: 1
 - - Input: polyAltValue (Monomial 2 (-4)) 2
 - - Expected Output: 32
 - - Acutal Output: 32

 - - Function: polyAltValue
 - - Test Case Number: 2
 - - Input: polyAltValue (SumAlt (Monomial 2.7 (-4)) (Monomial 4 1)) 2
 - - Expected Output: 51.2
 - - Acutal Output: 51.2

 - - Function: polyAltValue
 - - Test Case Number: 3
 - - Input: polyAltValue (polyAltProd (Monomial 2 (-4)) (Monomial 4 1)) 2
 - - Expected Output: 256
 - - Acutal Output: 256

 - - Function: polyAltDegree
 - - Test Case Number: 1
 - - Input: polyAltDegree (Monomial 2 (-4))
 - - Expected Output: 4
 - - Acutal Output: 4

 - - Function: polyAltDegree
 - - Test Case Number: 2
 - - Input: polyAltDegree (polyAltProd (Monomial 2 (-4)) (Monomial 4 1))
 - - Expected Output: 5
 - - Acutal Output: 5

 - - Function: polyAltDegree
 - - Test Case Number: 3
 - - Input: polyAltDegree (polyAltProd (Monomial 2 (-4)) (SumAlt (Monomial 4 1) (Monomial 2 3)))
 - - Expected Output: 7
 - - Acutal Output: 7

 - - Function: polyAltDeriv
 - - Test Case Number: 1
 - - Input: polyAltDeriv (Monomial 2 (-4))
 - - Expected Output: Monomial 8 3
 - - Acutal Output: Monomial 8 3

 - - Function: polyAltDeriv
 - - Test Case Number: 2
 - - Input: polyAltDeriv (SumAlt (Monomial 2 3) (Monomial 2 4))
 - - Expected Output: SumAlt (Monomial 6 2) (Monomial 8 3)
 - - Acutal Output: SumAlt (Monomial 6 2) (Monomial 8 3)

 - - Function: polyAltDeriv
 - - Test Case Number: 3
 - - Input: polyAltDeriv (polyAltProd (Monomial 2 3) (Monomial 2 4))
 - - Expected Output: Monomial 28 6
 - - Acutal Output: Monomial 28 6

 - - Function: polyAltProd
 - - Test Case Number: 1
 - - Input: polyAltProd (Monomial 2 (-4)) (SumAlt (Monomial 4 1) (Monomial 2 3))
 - - Expected Output: SumAlt (Monomial 8 5) (Monomial 4 7)
 - - Acutal Output: SumAlt (Monomial 8 5) (Monomial 4 7)

 - - Function: polyAltProd
 - - Test Case Number: 2
 - - Input: polyAltProd (Monomial 2 (-4)) (Monomial 2 9)
 - - Expected Output: Monomial 4 13
 - - Acutal Output: Monomial 4 13


 - - Function: polyAltProd
 - - Test Case Number: 3
 - - Input: polyAltProd (SumAlt (Monomial 2 (-4)) (Monomial 3 4)) (Monomial 2 9)
 - - Expected Output: SumAlt (Monomial 4 13) (Monomial 6 13)
 - - Acutal Output: SumAlt (Monomial 4 13) (Monomial 6 13)

 - - Function: polyAltNewton
 - - Test Case Number: 1
 - - Input: polyAltNewton (SumAlt (Monomial (0.6) (-3)) (Monomial (-1) 1)) (-0.3) 0.00000005
 - - Expected Output: 0.0 Close to 0
 - - Acutal Output: 4.0359370305411563e-13

 - - Function: polyAltNewton
 - - Test Case Number: 2
 - - Input: polyAltNewton (SumAlt (Monomial (0.1) (3)) (Monomial (5) 4)) (0.3) 0.00000005
 - - Expected Output: 6.693931841311602e-3
 - - Acutal Output: 6.693931841311602e-3

 - - Function: polyAltNewton
 - - Test Case Number: 3
 - - Input: polyAltNewton (Monomial 5 4) (1) 0.000000000000000001
 - - Expected Output: 1.7878373620688885e-5
 - - Acutal Output: 1.7878373620688885e-5


 - - Function: polyToPolyAlt
 - - Test Case Number: 1
 - - Input: polyToPolyAlt (Prod (Coef 25) (Coef (-8)))
 - - Expected Output: Monomial (-200) 0
 - - Acutal Output: Monomial (-200) 0


 - - Function: polyToPolyAlt
 - - Test Case Number: 2
 - - Input: polyToPolyAlt (Prod (Sum X (Coef 6)) (Sum (Coef 11) (Coef (-18))))
 - - Expected Output: SumAlt (SumAlt (Monomial 11 1) (Monomial 66 0)) (SumAlt (Monomial (-18) 1) (Monomial (-108) 0))
 - - Acutal Output: SumAlt (SumAlt (Monomial 11 1) (Monomial 66 0)) (SumAlt (Monomial (-18) 1) (Monomial (-108) 0))

 - - Function: polyToPolyAlt
 - - Test Case Number: 3
 - - Input: polyToPolyAlt (Prod (Prod X X) (Prod X X))
 - - Expected Output: Monomial 1 4
 - - Acutal Output: Monomial 1 4


 - -----------------------------------------------------------------
 -}
