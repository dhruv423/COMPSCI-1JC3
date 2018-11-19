{- Assignment 4
 - Name: Dhruv Bhavsar
 - Date: Nov 17, 2018
 -}
module Assign_4 where

macid :: String
macid = "bhavsd1"


data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
            deriving Show

newtype PolyList a = PolyList [a] deriving Show

{- -----------------------------------------------------------------
 - getPolyList
 - -----------------------------------------------------------------
 - Description: This function gets input from the file using the IO
 function. The <- operator extracts the a from IO a and then the
 function returns the PolyList of the coefficients. Lines turns the
 string into a list of strings. The map function uses the read and
 turns into to PolyList Integer.
 -}
getPolyList :: FilePath -> IO (PolyList Integer)
getPolyList file = do
                    inpt <- readFile file
                    return $ PolyList $ map read $ lines inpt


{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: This function utilizes Horner's method to calculate
 the value of the polynomial at a given value. The function is exactly
 like the formula which recurises. The base case is when the PolyList
 is empty and returns 0.
 -}
polyListValue :: Num a => PolyList a -> a -> a
polyListValue (PolyList []) n     = 0
polyListValue (PolyList (x:xs)) n = x + n * polyListValue (PolyList xs) n

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: This function finds the highest degree of PolyList
 by using legnth - 1. Minus 1 because the first value of the list
 is a constant which has no degree. the toInteger converts the
 Integral a to Integer as specified by the type signature. For the zero polynomial
 it is suppose to undefined so I didnt make a case for that.
 -}
polyListDegree :: (Num a, Eq a) => PolyList a -> Integer
polyListDegree (PolyList (x:xs)) = toInteger (length (x:xs)) - 1

{- -----------------------------------------------------------------
 - polyListDeriv
 - -----------------------------------------------------------------
 - Description: Computes the derivative of the PolyList. For the
 base case the derivative of an empty PolyList is empty PolyList.
 For the other cases, I reverse the list because the length after
 the element corresponds to the degree of the element. I use the
 foldr function to take the first element and multiply it by the length
 of the remaining list since that corresponds with the degree while
 concatnating it to the remaining part of the list and keeps doing
 it to the rest of list. The list will turn out to be in the original
 order as before because of the placement of concatnation. The first
 element after the foldr will be 0 because the derivative of the constant
 will be zero but the degree 1 will become a constant after deriving
 so no need of the 0 constant, thats why I drop the first element.

 -}
polyListDeriv :: (Num a, Eq a) => PolyList a -> PolyList a
polyListDeriv (PolyList [])      = PolyList [] -- Base case
polyListDeriv (PolyList (x:xs))  =
  let
     polyRev        = reverse (x:xs) -- To the get the corresponding degree
     polyDeriv      = foldr (\x xs ->  xs ++ [x * fromIntegral (length xs)] ) [] polyRev -- Multiplies the coef by the degree
     polyZeroLess   = drop 1 polyDeriv -- Drops the 0 constant from the front
     in PolyList polyZeroLess

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: Computes the sum of two PolyLists. For the base cases
 it is if one of the list is empty just return the other one unchanged. For
 the other case there is three cases within, the first being if the first list is
 longer than the second, second is just vice versa of first case and the lastly
 if the length of both lists are the same. To add the lists, I use zipWith, but
 one thing about zipWith is that it stops at the end of the shorter list, to fix
 this I append the rest of the longer list by dropping the length of the smaller
 list. If its the same length then zipWith works fine but I need to use the simplfying
 function I made to get rid of the zeros in the front if there are any. They only way
 you are getting zeros in the front is if the lists are same length.
 -}
polyListSum :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList []) p1                  = p1 -- Base case
polyListSum p1 (PolyList [])                  = p1 -- Base case
polyListSum (PolyList(x:xs)) (PolyList(y:ys))
          | length (x:xs) > length (y:ys) = PolyList $ zipWith (+) (x:xs) (y:ys) ++ drop (length ys) (xs) --  Appends the (x:xs) list at the end
          | length (x:xs) < length (y:ys) = PolyList $ zipWith (+) (x:xs) (y:ys) ++ drop (length xs) (ys) --  Appends the (y:ys) list at the end
          | otherwise                     = polySimp $ PolyList $ zipWith (+) (x:xs) (y:ys) -- Same Algorithm but uses the simplfying function I made at the end



{- -----------------------------------------------------------------
- polySimp
- -----------------------------------------------------------------
- Description: Simplfies the given PolyList by removing the zeros in the
front. This function reverses the PolyList and checks if the first element is a
0 if it is then removes it and passes the rest of list recursivly else returns
the list unreversed.
-}
polySimp :: (Num a, Eq a) => PolyList a -> PolyList a
polySimp (PolyList [])     = PolyList [] -- Base Case
polySimp (PolyList (x:xs)) =
  let
      (a:as) = reverse (x:xs) -- To check if the last element is 0
      in if a == 0
        then polySimp (PolyList $ reverse as) -- Passes the unreversed list again to see if it still has zeros
        else
          PolyList $ reverse (a:as) -- If not then returns the unreversed PolyList

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: Computes the multiplication of the two PolyList. The base cases
 are if one the list is empty and return empty list. There are two because of the
 pattern. Then other base case is if first list is just a single element then you
 just map over list with just the single element. If the second list is a single
 item then it will be covered with the next case. For the last one, you add the
 multiplication of first element of xs with ys and the  multiplication of the recursive
 part of the rest of xs and ys while the first element of ys in this part will
 have a 0 in front because this is how you multiply by hand, you shift it by one
 side. You add a zero when you change digits.
 Eg.  24
    x 32
    -----
  +   48
     720  <- This is the zero
  -}
polyListProd :: (Num a, Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd _ (PolyList [])                  = PolyList []
polyListProd (PolyList []) _                  = PolyList []
polyListProd (PolyList [x]) (PolyList (y:ys)) = PolyList $ map (*x) (y:ys)
polyListProd (PolyList (x:xs)) (PolyList ys)  = polyListSum (polyListProd (PolyList [x]) (PolyList ys)) (polyListProd (PolyList xs) (PolyList (0:ys)))


{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: Converts the polylist to Poly. The base cases are empty list and
 list with one element and returns Coef O and Coef x respectively. When its more
 than one element in the list, it will be the sum of the first element ( it is a
 constant so its just coef x) and the Prod of the rest of the list and X. That case
 utilizes Horner's method, same concept but this time just with Poly type.
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList [])     = Coef 0 -- Base Case
polyListToPoly (PolyList [x])    = Coef x -- Base Case
polyListToPoly (PolyList (x:xs)) = Sum (Coef x) $ Prod (polyListToPoly (PolyList xs)) X -- Horner's Method

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: Converts Poly to PolyList. The base case being Coef 0, Coef x,
 X and returning empty list, list with just the constant and list with 0 and 1
 respectively. For Sum a b, I used the made function of polyListSum for a and b
 while calling the function itself of a and b. Same algorithm goes for Prod a b
 but just using polyListProd.
 -}
polyToPolyList :: (Num a, Eq a) => Poly a -> PolyList a
polyToPolyList (Coef 0)   = PolyList [] -- Base Case
polyToPolyList (Coef x)   = PolyList [x] -- Base Case
polyToPolyList X          = PolyList [0,1] -- Base Case
polyToPolyList (Sum a b)  = polyListSum (polyToPolyList a) (polyToPolyList b) -- Using polyListSum
polyToPolyList (Prod a b) = polyListProd (polyToPolyList a) (polyToPolyList b) -- Using polyListProd


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: getPolyList
 - - Test Case Number: 1
 - - Input: getPolyList "empty.txt"
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []

 - - Function: getPolyList
 - - Test Case Number: 2
 - - Input: getPolyList "testCoeffs.txt"
 - - Expected Output: PolyList [1,2,3,4]
 - - Acutal Output: PolyList [1,2,3,4]

 - - Function: getPolyList
 - - Test Case Number: 3
 - - Input: getPolyList "testCoeffs2.txt"
 - - Expected Output: PolyList [-1,-43,3,-4]
 - - Acutal Output: PolyList [-1,-43,3,-4]

 - - Function: polyListValue
 - - Test Case Number: 1
 - - Input: polyListValue (PolyList [1,-2,3]) 0
 - - Expected Output: 1
 - - Acutal Output: 1

 - - Function: polyListValue
 - - Test Case Number: 2
 - - Input: polyListValue (PolyList []) 0
 - - Expected Output: 0
 - - Acutal Output: 0

 - - Function: polyListValue
 - - Test Case Number: 3
 - - Input: polyListValue (PolyList [-33,45,34,3,53]) 9
 - - Expected Output: 353046
 - - Acutal Output: 353046

 - - Function: polyListValue
 - - Test Case Number: 4
 - - Input: polyListValue (PolyList [-3.3,4.5,3.4,0.3,5.3]) 9
 - - Expected Output: 35304.6
 - - Acutal Output: 35304.5999999999

 - - Function: polyListDegree
 - - Test Case Number: 1
 - - Input: polyListDegree (PolyList [])
 - - Expected Output:  -- Suppose to output nothing
 - - Acutal Output:   -- Suppose to output nothing

 - - Function: polyListDegree
 - - Test Case Number: 2
 - - Input: polyListDegree (PolyList [1,2,3,4])
 - - Expected Output: 3
 - - Acutal Output: 3

 - - Function: polyListDegree
 - - Test Case Number: 2
 - - Input: polyListDegree (PolyList [1])
 - - Expected Output: 0
 - - Acutal Output: 0

 - - Function: polyListDeriv
 - - Test Case Number: 1
 - - Input: polyListDeriv (PolyList [1,2,3,4])
 - - Expected Output: PolyList [2,6,12]
 - - Acutal Output: PolyList [2,6,12]

 - - Function: polyListDeriv
 - - Test Case Number: 2
 - - Input: polyListDeriv (PolyList [1])
 - - Expected Output: PolyList []
 - - Acutal Output: PolyList []

 - - Function: polyListDeriv
 - - Test Case Number: 3
 - - Input: polyListDeriv (PolyList [3,4,0,0,6,4])
 - - Expected Output: PolyList [4,0,0,24,20]
 - - Acutal Output: PolyList [4,0,0,24,20]

  - - Function: polyListSum
  - - Test Case Number: 1
  - - Input: polyListSum (PolyList []) (PolyList [])
  - - Expected Output: PolyList []
  - - Acutal Output: PolyList []


  - - Function: polyListSum
  - - Test Case Number: 2
  - - Input: polyListSum (PolyList [1,2,3,4]) (PolyList [-1,-2,-3,-4])
  - - Expected Output: PolyList []
  - - Acutal Output: PolyList []

  - - Function: polyListSum
  - - Test Case Number: 3
  - - Input: polyListSum (PolyList [1,4]) (PolyList [-1,-2,-3,-4])
  - - Expected Output: PolyList [0,2,-3,-4]
  - - Acutal Output: PolyList [0,2,-3,-4]

  - - Function: polyListSum
  - - Test Case Number: 4
  - - Input: polyListSum (PolyList [14,3,54,43,5]) (PolyList [-1,-3])
  - - Expected Output: PolyList [13,0,54,43,5]
  - - Acutal Output: PolyList [13,0,54,43,5]

  - - Function: polyListSum
  - - Test Case Number: 5
  - - Input: polyListSum (PolyList [14,3,54,43,5]) (PolyList [-1,-3,0,0,-5])
  - - Expected Output: PolyList [13,0,54,43]
  - - Acutal Output: PolyList [13,0,54,43]

  - - Function: polyListProd
  - - Test Case Number: 1
  - - Input: polyListProd (PolyList [14,3,54,43,5]) (PolyList [-1,-3,0,0,-5])
  - - Expected Output: PolyList [-14,-45,-63,-205,-204,-30,-270,-215,-25]
  - - Acutal Output: PolyList [-14,-45,-63,-205,-204,-30,-270,-215,-25]


  - - Function: polyListProd
  - - Test Case Number: 2
  - - Input: polyListProd (PolyList [1]) (PolyList [-1,-3,-5])
  - - Expected Output: PolyList [-1,-3,-5]
  - - Acutal Output: PolyList [-1,-3,-5]

  - - Function: polyListProd
  - - Test Case Number: 3
  - - Input: polyListProd (PolyList [1,3,4,5]) (PolyList [-1,-3,-5])
  - - Expected Output: PolyList [-1,-6,-18,-32,-35,-25]
  - - Acutal Output: PolyList [-1,-6,-18,-32,-35,-25]

  - - Function: polyListProd
  - - Test Case Number: 4
  - - Input: polyListProd (PolyList []) (PolyList [-1,-3,-5])
  - - Expected Output: PolyList []
  - - Acutal Output: PolyList []

  - - Function: polyListToPoly
  - - Test Case Number: 1
  - - Input: polyListToPoly (PolyList [-1,-3,-5])
  - - Expected Output: Sum (Coef (-1)) (Prod (Sum (Coef (-3)) (Prod (Coef (-5)) X)) X)
  - - Acutal Output: Sum (Coef (-1)) (Prod (Sum (Coef (-3)) (Prod (Coef (-5)) X)) X)

  - - Function: polyListToPoly
  - - Test Case Number: 2
  - - Input: polyListToPoly (PolyList [])
  - - Expected Output: Coef 0
  - - Acutal Output: Coef 0

  - - Function: polyListToPoly
  - - Test Case Number: 3
  - - Input: polyListToPoly (PolyList [-7])
  - - Expected Output: Coef (-7)
  - - Acutal Output: Coef (-7)


  - - Function: polyToPolyList
  - - Test Case Number: 1
  - - Input: polyToPolyList (Coef (-7))
  - - Expected Output: PolyList [-7]
  - - Acutal Output: PolyList [-7]

  - - Function: polyToPolyList
  - - Test Case Number: 2
  - - Input: polyToPolyList (Coef (0))
  - - Expected Output: PolyList []
  - - Acutal Output: PolyList []

  - - Function: polyToPolyList
  - - Test Case Number: 3
  - - Input: polyToPolyList (X)
  - - Expected Output: PolyList [0,1]
  - - Acutal Output: PolyList [0,1]

  - - Function: polyToPolyList
  - - Test Case Number: 4
  - - Input: polyToPolyList (Sum (Coef (-1)) (Prod (Sum (Coef (-3)) (Prod (Coef (-5)) X)) X))
  - - Expected Output: PolyList [-1,-3,-5]
  - - Acutal Output: PolyList [-1,-3,-5] -- Input for previous function

-----------------------------------------------------------------------------
     QuickCheck Test Cases

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




 - -----------------------------------------------------------------
 -
 -}
