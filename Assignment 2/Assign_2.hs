{- Assignment 2
 - Name: Dhruv Bhavsar
 - Date: October 21, 2018
 -}
module Assign_2 where

macid :: String
macid = "bhavsd1"

type Vector = (Double,Double,Double)

{- -----------------------------------------------------------------
 - vecZero
 - -----------------------------------------------------------------
 - Description: Returns the vector of the origin (0.0,0.0,0.0), used this in vecF
 to output vecZero when it is an empty list
 -}
vecZero :: Vector
vecZero = (0.0, 0.0, 0.0)

{- -----------------------------------------------------------------
 - vecScalarProd
 - -----------------------------------------------------------------
 - Description: Computes the scalar product for the vector by multiplying
 the scalar with each coordinate of the vector
 -}
vecScalarProd :: Double -> Vector -> Vector
vecScalarProd x (a, b, c) = (x * a, x * b, x * c)

{- -----------------------------------------------------------------
 - vecSum
 - -----------------------------------------------------------------
 - Description: Computes the sum of two vectors by adding each coordinate
 respectively, for example it would add the x coordinates of both vectors and
 continue adding the other coordinates
 -}
vecSum :: Vector -> Vector -> Vector
vecSum (a,b,c) (d,e,f) = (a + d, b + e, c + f)

{- -----------------------------------------------------------------
 - vecMagnitude
 - -----------------------------------------------------------------
 - Description: Computes the magnitude of the vector by taking the sum of
 squared coordinates and square rooting it.
 -}
vecMagnitude :: Vector -> Double
vecMagnitude (a,b,c) = sqrt (a^2 + b^2 + c^2)

{- -----------------------------------------------------------------
 - vecInnerProd
 - -----------------------------------------------------------------
 - Description: Computes the inner product of two vectors, which is essentially
 the dot product. So its the sum of the multiplication of x coordinates from both
 vectors and the same for the y and z vectors. For example,
 a · b = ax × bx + ay × by
 Note: · is the dot product
 -}
vecInnerProd :: Vector -> Vector -> Double
vecInnerProd (a,b,c) (x,y,z) = a*x + b*y + c*z

{- -----------------------------------------------------------------
 - vecF
 - -----------------------------------------------------------------
 - Description: Computes the max and min distance between vec1 and members of the
 list. This is a recursive method, so there are 3 base cases I established. Now,
 if the list has 3 or more elements, then it takes the first three elements and
 figures out which one is in the middle and then gets rid of that and figures
 out the max and min and places in this specfic order (min,max). With the max and min
 figured out of the first three element it calls the method again with the
 specfic order (min:max:vs). vs is the rest of the list. It keeps on doing that
 until it reaches to two elements and then outputs based on the third base case.
 NOTE:We already know that the first element is the min and the second element
 is the max but since this is one of the base case it will do the sorting again.
 But this is a quick comparison so wouldn't affect the runtime at all.
 NOTE: When I refer to the element by itself I mean vecDist of the that element.
 Not refering to the value of that element alone.

 -}
vecF :: Vector -> [Vector] -> (Vector,Vector)
vecF _ [] = error "Can't have an empty list"
vecF _ [x] = (x,x) -- Outputs the single element twice in the tuple
vecF vec1 [x,y] = if vecDist vec1 x >= vecDist vec1 y then (y,x) else (x,y) -- Figures out which is max and min and ouputs accordingly
vecF vec1 (x:y:z:vs) -- 3 or more elements
  | (vecDist vec1 x > vecDist vec1 y) = vecF vec1 (y:x:z:vs) -- Places y infront if it smaller than x and calls the method with that order
  | (vecDist vec1 z < vecDist vec1 x) = vecF vec1 (z:y:vs) -- Places z inplace of x and gets rid of x since x is not the smallest, z is and calls the method with out x
  | (vecDist vec1 z > vecDist vec1 y) = vecF vec1 (x:z:vs) -- Places z inplace of y and gets rid of y since y is not the biggest, z is and calls the method with out y
  | otherwise                         = vecF vec1 (x:y:vs) -- Gets rid of z if z is not the max or min and calls the method without z

{- ----------------------------------------------------------------
 - vecDiff
 - ----------------------------------------------------------------
 - Description: Computes the difference of the vectors using the vecSum
 function and vecScalarProd function. Adds (using the vecSum) the first vector and the second vector
 which is the multiplied by (-1) using vecScalarProd.
 -}
vecDiff :: Vector -> Vector -> Vector
vecDiff (a,b,c) (x,y,z) = vecSum (a,b,c) (vecScalarProd (-1) (x,y,z))


{- ----------------------------------------------------------------
 - vecDist
 - ----------------------------------------------------------------
 - Description: Computes the distance between the two inputted vectors by finding
 the magnitude of the difference of those two vectors. This function relies on
 vecMagnitude and vecDiff to compute distance. This function is used in the
 vecF function to find the distances between a vector and a vector from the list.
 -}
vecDist :: Vector -> Vector -> Double
vecDist (a,b,c) (x,y,z) = vecMagnitude (vecDiff (a,b,c) (x,y,z))

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: vecScalarProd
 - - Test Case Number: 1
 - - Input: vecScalarProd (-1) (4.5,6.7,-1.2)
 - - Expected Output: (-4.5,-6.7,1.2)
 - - Acutal Output: (-4.5,-6.7,1.2)

 - - Function: vecScalarProd
 - - Test Case Number: 2
 - - Input: vecScalarProd (-1) (5234.5,2342.4,-345345.4)
 - - Expected Output: (-5234.5,-2342.4,345345.4)
 - - Acutal Output: (-5234.5,-2342.4,345345.4)

 - - Function: vecScalarProd
 - - Test Case Number: 3
 - - Input: vecScalarProd (-56) (45.5,35.4,2.4)
 - - Expected Output: (-2548.0,-1982.4,-134.4)
 - - Acutal Output: (-2548.0,-1982.3999999999999,-134.4)

 - - Function: vecSum
 - - Test Case Number: 1
 - - Input: vecSum (234,42,34) (-23,234,-4)
 - - Expected Output: (211.0,276.0,30.0)
 - - Acutal Output: (211.0,276.0,30.0)

 - - Function: vecSum
 - - Test Case Number: 2
 - - Input: vecSum (-2,-4,-5) (0,-44,-9)
 - - Expected Output: (-2.0,-48.0,-14.0)
 - - Acutal Output: (-2.0,-48.0,-14.0)

 - - Function: vecSum
 - - Test Case Number: 3
 - - Input: vecSum (4.6666,0.3234,8.434234) (1.00001,3.44444,2.44545)
 - - Expected Output: (5.66661,3.76784,10.879684)
 - - Acutal Output: (5.66661,3.76784,10.879684000000001)

 - - Function: vecMagnitude
 - - Test Case Number: 1
 - - Input: vecMagnitude (4.6666,0.3234,8.434234)
 - - Expected Output: 9.644586372 - From Calculator
 - - Acutal Output: 9.644586371989002 - More Precise Answer

 - - Function: vecMagnitude
 - - Test Case Number: 2
 - - Input: vecMagnitude (-64,45,-54)
 - - Expected Output: 95.06313691 - From Calculator
 - - Acutal Output: 95.06313691436866 - More Precise Answer

 - - Function: vecMagnitude
 - - Test Case Number: 3
 - - Input: vecMagnitude (-3,-4,-5)
 - - Expected Output: 7.071067812 - From Calculator
 - - Acutal Output: 7.0710678118654755 - More Precise Answer

 - - Function: vecInnerProd
 - - Test Case Number: 1
 - - Input: vecInnerProd (-3,-4,-5) (1,2,3)
 - - Expected Output: -26.0
 - - Acutal Output: -26.0

 - - Function: vecInnerProd
 - - Test Case Number: 2
 - - Input: vecInnerProd (2,3,4) (0,0,0)
 - - Expected Output: 0.0
 - - Acutal Output: 0.0

 - - Function: vecInnerProd
 - - Test Case Number: 3
 - - Input: vecInnerProd (345.345,435.4,5.6345) (-234,-2.3435,-6.43)
 - - Expected Output: 81867.31974
 - - Acutal Output: 81867.31973500001

 - - Function: vecF
 - - Test Case Number: 1
 - - Input: vecF (1,2,3) [(4,5,6)]
 - - Expected Output: ((4.0,5.0,6.0),(4.0,5.0,6.0))
 - - Acutal Output: ((4.0,5.0,6.0),(4.0,5.0,6.0))

 - - Function: vecF
 - - Test Case Number: 2
 - - Input: vecF (1,2,3) [(2,12,4),(3,10,5),(4,8,6),(5,6,7),(6,4,8),(7,2,9),
                          (8,0,10),(9,-2,11),(10,-4,12),(13,-10,15),(14,-12,16)]
 - - Expected Output: ((5.0,6.0,7.0),(14.0,-12.0,16.0))
 - - Acutal Output: ((5.0,6.0,7.0),(14.0,-12.0,16.0))

 - - Function: vecF
 - - Test Case Number: 3
 - - Input: vecF (1,2,3) [(4,5,6),(-1,-2,-3)]
 - - Expected Output: ((4.0,5.0,6.0),(-1.0,-2.0,-3.0))
 - - Acutal Output: ((4.0,5.0,6.0),(-1.0,-2.0,-3.0))

 - -----------------------------------------------------------------
 -
 -}
