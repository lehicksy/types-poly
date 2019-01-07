{-
Lauren Hicks 
hicksl3
2018-11-05
Assignment 3 
-}

module Polynomial where 
    data Poly = X Exp Coeff | Sum Poly Poly
            deriving (Show)
    type Exp = Integer
    type Coeff = Integer  

    polyEval :: Poly -> Integer -> Integer --Evaluates a polynomial for any integer value of x
    polyEval (X ex co) y = co*(y^ex)
    polyEval (Sum poly1 poly2) z = (polyEval poly1 z) + (polyEval poly2 z) 

    maxDegree:: Poly -> Integer -- Finds the highest degree of a polynomial
    maxDegree (X exp co) = exp
    maxDegree (Sum poly1 poly2) = max (maxDegree poly1) (maxDegree poly2)

    polyCoeff:: Poly -> Integer -> Integer --Finds the coefficient of the term with the exponent of the integer inputted
    polyCoeff (X exp co) i = if i==exp then co else 0
    polyCoeff (Sum poly1 poly2) i = (polyCoeff poly1 i) + (polyCoeff poly2 i)

    polyProduct :: Poly -> Poly -> Poly 
    polyProduct (X exp1 co1) (X exp2 co2) = (X (exp1+exp2) (co1*co2))
    polyProduct (X exp co) (Sum poly1 poly2) = Sum (polyProduct (X exp co)(poly1))(polyProduct (X exp co)(poly2))
    polyProduct (Sum poly1 poly2)(X exp co) = Sum (polyProduct (X exp co)(poly1))(polyProduct (X exp co)(poly2))
    polyProduct (Sum poly11 poly21) (Sum poly12 poly22) = Sum (Sum (polyProduct poly11 poly12) (polyProduct poly11 poly22)) (Sum (polyProduct poly21 poly12) (polyProduct poly21 poly22))

    polyDeriv :: Poly -> Poly --Finds the derivative of a polynomial
    polyDeriv (X exp co) = if exp == 0 then (X 0 0) else (X (exp-1) (co*exp))
    polyDeriv (Sum poly1 poly2) = Sum (polyDeriv poly1) (polyDeriv poly2)

    {- Test Cases

    Function: polyEval
    Test Case Number: 1
    Input: (X 0 3) 1
    Expected Output: 3
    Actual Output: 3

    Function: polyEval
    Test Case Number: 2 
    Input: (Sum (X 6 4) (X 2 3)) 3
    Expected Output: 2943
    Actual Output: 2943

    Function: polyEval
    Test Case Number: 3 
    Input: (Sum(Sum (X 4 5) (X 3 9)) (X 3 4)) 2
    Expected Output: 184
    Actual Output: 184

    Function: maxDegree
    Test Case Number: 1 
    Input: (X 0 4)
    Expected Output: 0
    Actual Output: 0

    Function: maxDegree
    Test Case Number: 2 
    Input: (Sum (X 4 5) (X 2 6))
    Expected Output: 4 
    Actual Output: 4

    Function: maxDegree
    Test Case Number: 3
    Input: (Sum(Sum (Sum (X 3 9) (X 5 4)) (X 4 5)) (X 3 4))
    Expected Output: 5
    Actual Output: 5

    Function: polyCoeff
    Test Case Number: 1 
    Input: (X 0 3) 4
    Expected Output: 0 
    Actual Output: 0

    Function: polyCoeff
    Test Case Number: 2 
    Input: (Sum(Sum (Sum (X 3 9) (X 5 4)) (X 4 5)) (X 3 4)) 4
    Expected Output: 5
    Actual Output: 5

    Function: polyCoeff 
    Test Case Number: 3 
    Input: (Sum(Sum (Sum (X 3 9) (X 5 4)) (X 4 5)) (X 3 4)) 3
    Expected Output: 13
    Actual Output: 13

    Function: polyProduct
    Test Case Number: 1 
    Input: (X 3 4)(X 0 1)
    Expected Output: (X 3 4)
    Actual Output: X 3 4

    Function: polyProduct
    Test Case Number: 2 
    Input: (Sum (Sum (X 0 3) (X 1 4)) (Sum (X 1 3) (X 3 5))) (Sum (X 0 3) (X 1 1))
    Expected Output: (Sum (Sum (X 0 9) (X 1 12)) (Sum (X 1 3) (X 2 4))) (Sum (Sum (X 1 9) (X 3 15)) (Sum (X 2 3) (X 4 5)))
    Actual Output: (Sum (Sum (X 0 9) (X 1 12)) (Sum (X 1 3) (X 2 4))) (Sum (Sum (X 1 9) (X 3 15)) (Sum (X 2 3) (X 4 5)))

    Function: polyProduct
    Test Case Number: 3 
    Input: (X 3 4)(Sum (X 5 6)(X 0 3))
    Expected Output: Sum(X 8 24)(X 3 12)
    Actual Output: Sum (X 8 24) (X 3 12)

    Function: polyDeriv 
    Test Case Number: 1
    Input: (X 0 3)
    Expected Output: (X 0 0)
    Actual Output: X 0 0

    Function: polyDeriv
    Test Case Number: 2 
    Input: (Sum(Sum (Sum (X 3 9) (X 5 4)) (X 4 5)) (X 3 4))
    Expected Output: (Sum(Sum(Sum(X 2 27)(X 4 20))(X 3 20))(X 2 12))
    Actual Output: Sum (Sum (Sum (X 2 27) (X 4 20)) (X 3 20)) (X 2 12)

    Function: polyDeriv
    Test Case Number: 3 
    Input: (Sum(X 1 2)(X 0 4))
    Expected Output: Sum (X 0 2)(X 0 0)
    Actual Output: Sum (X 0 2) (X 0 0)
    -}