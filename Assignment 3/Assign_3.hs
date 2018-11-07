{- Assignment 3
 - Name: Dhruv Bhavsar
 - Date: October 25 2018
 -}

module Assign_3 where
import Test.QuickCheck


macid :: String
macid = "bhavsd1"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Evaluates the polynomial at a specified point. Since there
 can be 4 different ways to represent a polynomial using the given data type, we
 can achieve this from setting the base cases. The base cases are just X and only
 a constant which output the specified point and constant respectively. We can
 do this recursively for Sum , just call the function on the first part and
 second part of Sum with the point and the just add the values of both, since
 it will reach the base cases eventually. Same method goes for Prod but instead
 of adding, multiply it.
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue X a           = a
polyValue (Coef a) _    = a
polyValue (Sum a b) c   = polyValue a c + polyValue b c
polyValue (Prod a b) c  = polyValue a c * polyValue b c


{- -----------------------------------------------------------------
 - polyDegree
 - -----------------------------------------------------------------
 - Description: Computes the degree of the polynomial. The bases are the constant
 and just X, returning 0 and 1 respectively. For Sum, the degree of the polynomial
 is the highest degree, so I found the highest degree by calling polyDegree on
 the first and second part. For Prod, the highest degree is found by the sum of
 degrees. So, I called polyDegree on a and b, return the sum of those 2 degrees.
 Had this previously : | polyDegree a == polyDegree b =
   if polyValue (Sum a b) 4 == 0 -- 4 is some arbirtary value
     then -1
   else polyDegree a
 in between the guards for the cancellation terms like x-x and would return -1
 for undefined
 -}
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree (Coef a)   = 0
polyDegree  X         = 1
polyDegree (Sum a b)
                | polyDegree a >= polyDegree b = polyDegree a -- Returns a if its highest or equal to
                | otherwise                    = polyDegree b -- Returns b if its highest
polyDegree (Prod a b)                          = polyDegree a + polyDegree b -- Sum of degrees



{- -----------------------------------------------------------------
 - polyDeriv
 - -----------------------------------------------------------------
 - Description: Computes the derivative of the polynomial. The base cases are
 constant and just X, returning 0 and 1 respectively. The Sum will take the
 derivative of the first part and the second part and add it together using Sum.
 Thats how derivatives work if its addition. For Prod, I applied the same method
 of taking a derivative using product rule to the function and its added using
 Sum.
 y = f(x)g(x) y' =  f'(x)g(x) + f(x)g'(x)
 -}
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Coef a)   = Coef 0
polyDeriv  X         = Coef 1
polyDeriv (Sum a b)  = Sum (polyDeriv a) (polyDeriv b)
polyDeriv (Prod a b) = Sum (Prod (polyDeriv a) b) (Prod a (polyDeriv b)) -- Same as Product Rule



{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 1
 - - Input: polyValue X 9
 - - Expected Output: 9
 - - Acutal Output: 9

 - - Function: polyValue
 - - Test Case Number: 2
 - - Input: polyValue (Sum (Sum (Sum X (Coef 13)) (Prod X (Coef 8))) (Prod (Sum (Coef 14) X) (Sum (Coef 9) X))) 2
 - - Expected Output: 207
 - - Acutal Output: 207

 - - Function: polyValue
 - - Test Case Number: 3
 - - Input: polyValue (Prod(Prod (Prod X (Coef 9)) (Coef 34)) (Prod(Prod X X) X)) 2
 - - Expected Output: 4896
 - - Acutal Output: 4896

 - - Function: polyDegree
 - - Test Case Number: 1
 - - Input: polyDegree (Prod (Prod X X) (Prod X X))
 - - Expected Output: 4
 - - Acutal Output: 4

 - - Function: polyDegree
 - - Test Case Number: 2
 - - Input: polyDegree (Prod (Sum X (Coef 6)) (Sum (Coef 11) (Coef (-18))))
 - - Expected Output: 1
 - - Acutal Output: 1

 - - Function: polyDegree
 - - Test Case Number: 3
 - - Input: polyDegree (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum X X) (Sum X X)))
 - - Expected Output: 2
 - - Acutal Output: 2

 - - Function: polyDeriv
 - - Test Case Number: 1
 - - Input: polyDeriv (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum X X) (Sum X X)))
 - - Expected Output: Sum (Prod (Sum (Sum (Coef 1) (Coef 0)) (Sum (Coef 0) (Coef 0))) (Sum (Sum X X) (Sum X X))) (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum (Coef 1) (Coef 1)) (Sum (Coef 1) (Coef 1))))
 - - Acutal Output: Sum (Prod (Sum (Sum (Coef 1) (Coef 0)) (Sum (Coef 0) (Coef 0))) (Sum (Sum X X) (Sum X X))) (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum (Coef 1) (Coef 1)) (Sum (Coef 1) (Coef 1))))

 - - Function: polyDeriv
 - - Test Case Number: 2
 - - Input: polyDeriv (Prod (Prod X X) (Prod X X))
 - - Expected Output: Sum (Prod (Sum (Sum (Coef 1) (Coef 0)) (Sum (Coef 0) (Coef 0))) (Sum (Sum X X) (Sum X X))) (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum (Coef 1) (Coef 1)) (Sum (Coef 1) (Coef 1))))
 - - Acutal Output: Sum (Prod (Sum (Sum (Coef 1) (Coef 0)) (Sum (Coef 0) (Coef 0))) (Sum (Sum X X) (Sum X X))) (Prod (Sum (Sum X (Coef (-27))) (Sum (Coef 10) (Coef (-9)))) (Sum (Sum (Coef 1) (Coef 1)) (Sum (Coef 1) (Coef 1))))

 - - Function: polyDeriv
 - - Test Case Number: 3
 - - Input: polyDeriv (Prod (Coef 25) (Coef (-8)))
 - - Expected Output: Sum (Prod (Coef 0) (Coef (-8))) (Prod (Coef 25) (Coef 0))
 - - Acutal Output: Sum (Prod (Coef 0) (Coef (-8))) (Prod (Coef 25) (Coef 0))


 - -----------------------------------------------------------------
 -}
