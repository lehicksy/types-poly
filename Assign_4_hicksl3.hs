{-
Lauren Hicks 
2018-11-21
Assignment 4 
-}

module PolynomialList where

    import Test.QuickCheck

    data Poly = X Exp Coeff | Sum Poly Poly
        deriving (Show, Eq)

    type Exp = Integer
    type Coeff = Integer 

    getPolyList :: FilePath -> IO [Integer] -- Retrieves a polynomial list from a file
    getPolyList path = do 
        {item <- readFile path;
        pure(dropZero(strint (lines item)))
        } 

    strint :: [String] -> [Integer] --Converts a string to a list of integers
    strint []     = []
    strint (s:ss) = read s : strint ss

    polyListValue :: [Integer] -> Integer -> Integer -- Evaluates a polynomial given a polynomial list and an integer
    polyListValue [] _= 0
    polyListValue (x:xs) n = x + n*(polyListValue xs n)

    polyListDegree :: [Integer] -> Integer -- Finds the degree of a polynomial list
    polyListDegree [] = undefined 
    polyListDegree xs = if dropZero (xs) == [] then undefined else (toInteger(length(dropZero xs) - 1))

    polyListDeriv :: [Integer] -> [Integer] -- Takes the derivative of a polynomial list
    polyListDeriv [] = [] 
    polyListDeriv (x:xs) = dropZero(zipWith (*) xs [1..])

    polyListSum :: [Integer] -> [Integer] -> [Integer] -- Adds two polynomials 
    polyListSum [] [] = []
    polyListSum pl [] = if dropZero pl == [] then [] else pl
    polyListSum [] ql = if dropZero ql == [] then [] else ql
    polyListSum (p:pl) (q:ql) = [p + q] ++ polyListSum pl ql

    dropZero :: [Integer] -> [Integer] -- Removes zeroes from the end of a polynomial 
    dropZero [] = [] 
    dropZero x  = if last x == 0 then dropZero (init x) else x
    
    polyListProd :: [Integer] -> [Integer] -> [Integer] --Finds the product of two polynomial lists by implementing an inner and outer function
    polyListProd ps qs = drop (abs((length ps)-length(qs))) (insidePolyListProd ps qs)

    insidePolyListProd :: [Integer] -> [Integer] -> [Integer] -- Finds the product of two polynomial lists
    insidePolyListProd _ [] = []
    insidePolyListProd [] _ = []
    insidePolyListProd ps qs = 
        if (length ps) <=(length qs) then polyListSum ((replicate (abs(length qs - (length ps))) 0)++(map (*(head ps)) qs)) (insidePolyListProd (tail ps) qs)
        else insidePolyListProd qs ps    

    polyListToPoly :: [Integer] -> Poly --Converts a list of coefficients to a polynomial
    polyListToPoly [] = (X 0 0)
    polyListToPoly [x] = (X 0 x)
    polyListToPoly xs = if dropZero xs == [] then (X 0 0) else Sum (polyListToPoly (init xs))(X (toInteger(length xs-1)) (last xs))

    polyToPolyList :: Poly -> [Integer] --Converts a polynomial to a list of coefficients 
    polyToPolyList (X _ 0) = [] 
    polyToPolyList (X ex co) = if ex == 0 then [co] else (replicate (fromIntegral ex) 0 ++ [co])
    polyToPolyList (Sum poly1 poly2) = polyListSum (polyToPolyList(poly1)) (polyToPolyList(poly2))

    -- QuickCheck functions --

    prop_Value :: [Integer] -> Integer -> Bool --quickCheck function for polyListValue
    prop_Value x n =((polyListValue x n)) == (polyEval (polyListToPoly(x)) n)

    prop_Degree :: [Integer] -> Bool --quickCheck function for polyListDegree
    prop_Degree x = if dropZero(x)==[] then True
        else(polyListDegree x) == maxDegree(polyListToPoly x)

    prop_Sum :: [Integer] -> [Integer] -> Bool --quickCheck function for polyListSum
    prop_Sum x y = dropZero(polyToPolyList(polyListToPoly(polyListSum x y))) == dropZero(polyToPolyList(polySum (polyListToPoly(x)) (polyListToPoly(y))))

    prop_Deriv :: [Integer] -> Bool --quickCheck function for polyListDeriv
    prop_Deriv x = dropZero(polyToPolyList(polyListToPoly(polyListDeriv x))) == dropZero(polyToPolyList(polyDeriv (polyListToPoly(x))))

    prop_Prod :: [Integer] -> [Integer] -> Bool --quickCheck function for polyListProd
    prop_Prod x y = dropZero(polyToPolyList(polyListToPoly(polyListProd x y))) == dropZero(polyToPolyList(polyProduct (polyListToPoly(x)) (polyListToPoly(y))))

    polyProduct :: Poly -> Poly -> Poly --Multiplies two polynomials together
    polyProduct (X exp1 co1) (X exp2 co2) = (X (exp1+exp2) (co1*co2))
    polyProduct (X exp co) (Sum poly1 poly2) = Sum (polyProduct (X exp co)(poly1))(polyProduct (X exp co)(poly2))
    polyProduct (Sum poly1 poly2)(X exp co) = Sum (polyProduct (X exp co)(poly1))(polyProduct (X exp co)(poly2))
    polyProduct (Sum poly11 poly21) (Sum poly12 poly22) = Sum (Sum (polyProduct poly11 poly12) (polyProduct poly11 poly22)) (Sum (polyProduct poly21 poly12) (polyProduct poly21 poly22))

    polySum :: Poly -> Poly -> Poly -- Adds two polynomials
    polySum (X ex co)(X ex1 co1) = Sum(X ex co)(X ex1 co1)
    polySum (X ex co)(Sum poly1 poly2) = Sum (X ex co)(Sum poly1 poly2) 
    polySum (Sum poly1 poly2)(X ex co) = Sum (X ex co)(Sum poly1 poly2) 
    polySum (Sum poly1 poly2)(Sum poly3 poly4) = Sum(Sum poly1 poly2)(Sum poly3 poly4)

    polyDeriv :: Poly -> Poly --Finds the derivative of a polynomial
    polyDeriv (X exp co) = if exp == 0 then (X 0 0) else (X (exp-1) (co*exp))
    polyDeriv (Sum poly1 poly2) = Sum (polyDeriv poly1) (polyDeriv poly2)

    polyEval :: Poly -> Integer -> Integer --Evaluates a polynomial for any integer value of x
    polyEval (X ex co) y = co*(y^ex)
    polyEval (Sum poly1 poly2) z = (polyEval poly1 z) + (polyEval poly2 z)

    maxDegree:: Poly -> Integer -- Finds the highest degree of a polynomial
    maxDegree (X 0 0) = undefined
    maxDegree (X exp co) = exp
    maxDegree (Sum poly1 poly2) = max (maxDegree poly1) (maxDegree poly2)
     
    {- Test Cases

    Function: polyListValue 
    Test Case Number: 1
    Input: [] 17
    Expected Output: 0 
    Actual Output: 0

    Function: polyListValue
    Test Case Number: 2 
    Input: [1, 2, 3, 4, 5, 6] 5
    Expected Output: 22,461
    Actual Output: 22461

    Function: polyListValue
    Test Case Number: 3 
    Input: [5, 3, 2] 0
    Expected Output: 5 
    Actual Output: 5

    Function: polyListDegree
    Test Case Number:  1
    Input: [1, 2, 3, 0]
    Expected Output: 2
    Actual Output: 2
    
    Function: polyListDegree
    Test Case Number: 2
    Input: [1]
    Expected Output: 0 
    Actual Output: 0
    
    Function: polyListDegree
    Test Case Number:  3
    Input: [17, 3535, 9]
    Expected Output: 2
    Actual Output: 2

    Function: polyListDeriv 
    Test Case Number: 1
    Input: [1]
    Expected Output: []
    Actual Output: []

    Function: polyListDeriv
    Test Case Number:  2
    Input: [1, 2, 3, 4]
    Expected Output: [2, 6, 12]
    Actual Output: [2, 6, 12]

    Function: polyListDeriv
    Test Case Number:  3
    Input: [1, 2, 0, 0]
    Expected Output: [2]
    Actual Output: [2]

    Function: polyListSum
    Test Case Number: 1 
    Input: [1, 2, 3] [1, 2, 3, 4, 5]
    Expected Output: [2, 4, 6, 4, 5]
    Actual Output: [2, 4, 6, 4, 5]

    Function: polyListSum
    Test Case Number: 2 
    Input: [] [2]
    Expected Output: [2]
    Actual Output: [2]

    Function: polyListSum 
    Test Case Number : 3
    Input: [] [0]
    Expected Output: []
    Actual Output: []

    Function: polyListSum
    Test Case Number: 4
    Input: [1, 2, 3] [1, 0, 0, 0]
    Expected Output: [2, 2, 3]
    Actual Output: [2, 2, 3]

    Function: polyListProd 
    Test Case Number: 1 
    Input: [2, 3, 4][1, 2, 3]
    Expected Output: [2, 7, 16, 17, 12]
    Actual Output: [2, 7, 16, 17, 12]

    Function: polyListProd
    Test Case Number: 2 
    Input: [1, 2][1, 2, 3]
    Expected Output: [1, 4, 7, 6]
    Actual Output: [1, 4, 7, 6]

    Function: polyListProd
    Test Case Number: 3
    Input: [1, 2, 3][1, 2]
    Expected Output: [1, 4, 7, 6]
    Actual Output: 

    Function: polyListToPoly
    Test Case Number: 1 
    Input: [0, 0, 0]
    Expected Output: (X 0 0)
    Actual Output: X 0 0

    Function: polyListToPoly
    Test Case Number: 2 
    Input: [1]
    Expected Output: (X 0 1)
    Actual Output: X 0 1

    Function: polyListToPoly
    Test Case Number: 3 
    Input: [2, 3, 4, 5]
    Expected Output: Sum (Sum (Sum (X 0 2) (X 1 3)) (X 2 4)) (X 3 5)
    Actual Output: Sum (Sum (Sum (X 0 2) (X 1 3)) (X 2 4)) (X 3 5)

    Function: polyToPolyList 
    Test Case Number: 1
    Input: (X 0 0)
    Expected Output: []
    Actual Output: []

    Function: polyToPolyList 
    Test Case Number: 2 
    Input: (Sum (Sum (Sum (X 0 2) (X 1 3)) (X 2 4)) (X 3 5))
    Expected Output: [2, 3, 4, 5]
    Actual Output: [2, 3, 4, 5]

    Function: polyToPolyList
    Test Case Number: 3 
    Input:(X 0 1)
    Expected Output: [1]
    Actual Output: [1]
    -}   
