-- Ryan Forsyth (11/25/2015)
module Crypto
       (
         -- * Number Theory
         euclidean
       , findXY
       , inverseMod
         -- * Elliptic Curves
       , Point
       , Polynomial(..)
       , Infinity(..)
       , op
       , mult
       ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Foldable (foldlM)

-- | Find the greatest common denominator of two integers
euclidean :: Int -> Int -> Maybe Int
euclidean p q =
  let f y x = 
        let r = y - (y `div` x)*x
        in if r == 0
           then x
           else f x r
  in if (p > 0) && (q > 0)
     then if p > q
          then Just $ f p q
          else Just $ f q p
     else Nothing

-- Find the factors from the Euclidean algorithm
euclideanFactors :: Int -> Int -> Maybe [Int]
euclideanFactors p q =
  if (p > 0) && (q > 0)
  then let (x,y) = if p < q then (p,q) else (q,p)
           f = y `div` x
           r = y - f*x
       in if r == 0
          then Just []
          else euclideanFactors x r >>= return . (\xs -> f:xs)
  else Nothing

-- Run the extended Euclidean algorithm
extendedEuclidean :: Int -> Int -> Maybe [(Int,Int,Int)]
extendedEuclidean p q =
  let factors :: Maybe [Int]
      factors = euclideanFactors p q
      g :: [(Int,Int,Int)] -> Int -> Maybe [(Int,Int,Int)]
      g zs@((d,e,f):(a,b,c):_) n = Just ((a-n*d, b-n*e, c-n*f):zs)
      g _ n = Nothing
  in do
    fs <- factors
    foldlM g [(q,0,1),(p,1,0)] fs 

-- | @'findXY' p q@ will find @(x,y)@ such that @1 = px + qy@ if
-- such @x,y@ exist
findXY :: Int -> Int -> Maybe (Int, Int)
findXY p q =
  let greater = p > q
  in do
    tuples <- if greater then extendedEuclidean p q else extendedEuclidean q p
    case tuples of
     (1,x,y):_ -> if greater then Just (x,y) else Just (y,x)
     _         -> Nothing

-- | @'inverseMod' p n@ will find the inverse of @p@ mod @n@.
-- That is, it will find @q@ such that @pq=1 (mod n)@
inverseMod :: Int -> Int -> Maybe Int
inverseMod p n = findXY p n >>= return . (\x -> x `mod` n) . fst

-- | Point
type Point = (Int,Int)
type Slope = (Int,Int)
type Intercept = (Int,Int)

slope :: Point -> Point -> Slope
slope (x0,y0) (x1,y1) = (y1-y0,x1-x0)

-- y = mx + b
-- b = y - mx
-- b = y - (dy/dx)x
-- b = (dx/dx)y - (dy/dx)x
-- b = (dx*y - dy*x)/dx
find_b :: Point -> Slope -> Intercept
find_b (x,y) (dy,dx) = (dx*y - dy*x, dx)

-- y = (j/k)x + b
-- y = (j/k)x + (n/d)
-- y = (d/d)(j/k)x + (k/k)(n/d)
-- y = (djx/dk) + (kn/dk)
-- dky = djx + kn
-- y = (djl)x - (kln) where l is the inverse of (dk) mod n
lineMod :: Slope -> Intercept -> Int -> Maybe Polynomial
lineMod (j,k) (n,d) p = do
  l <- (d*k) `inverseMod` p
  return $ Polynomial [(k*l*n) `mod` p, d*j*l]

-- | Int list of coefficients for the polynomial,
-- with lowest degree first
data Polynomial = Polynomial [Int] deriving (Show, Eq)
    
degree :: Polynomial -> Int
degree (Polynomial cs) = length cs

evaluate :: Polynomial -> Int -> Int
evaluate (Polynomial cs) x =
  sum $ zipWith (\c d -> c*x^d) cs [0..]

differentiate :: Polynomial -> Polynomial
differentiate p@(Polynomial cs) =
  case zipWith (*) cs [0..] of
   _:zs -> Polynomial zs
   []   -> Polynomial [0]

-- Find the intersection of y=mx+b and
-- y^2=c3*x^3 + c2*x^2 + c1*x + c0
-- (mx+b)^2 \equiv c3*x^3 + c2*x^2 + c1*x + c0
-- m^2x^2 + 2bmx + b^2 \equiv c3*x^3 + c2*x^2 + c1*x + c0
-- 0 \equiv c3*x^3 + (c2-m^2)*x^2 + (c1 - 2bm)*x + (c0 - b^2)
-- (c2-m^2) = -(r + s + t)  where r,s,t are factors of the cubic
-- c2-m^2+r+s = -t
-- t = m^2 - c2 - r - s
intersect :: Int -> Point -> Point -> Polynomial -> Polynomial -> Maybe Point
intersect n (r,_) (s,_) line@(Polynomial [b,m]) (Polynomial [c0,c1,c2,1]) =
  let t = (m^2 - c2 - r - s) `mod` n in
  Just (t, (-(evaluate line t)) `mod` n)
intersect _ _ _ _ _  = Nothing

-- y^2=c3*x^3 + c2*x^2 + c1*x + c0
-- 2yy' = 3c3*x^2 + 2c2x + c1
-- y' = i(3c3*x^2 + 2c2x + c1) where i is the inverse of 2y mod n
tangentIntersect :: Polynomial -> Int -> Point -> Maybe Point
tangentIntersect c n p@(x,y) = do
  i <- (2*y) `inverseMod` n
  let m = ((i * (evaluate (differentiate c) x)) `mod` n,1)
  let b = find_b p m
  l <- lineMod m b n 
  intersect n p p l c

-- | The point "Infinity" found on any elliptic curve mod n
data Infinity = Infinity deriving (Show, Eq)

{-
-- | @'op' c n p0 p1@ will find a third point on the curve @c@ mod @n@
-- given @p0,p1@.
op :: Polynomial -> Int -> Point -> Point -> Either Infinity (Maybe Point)
op c n p0@(x0,y0) p1@(x1, y1) =
  let m = slope p0 p1
  in if snd m == 0
     then if fst m == 0
          then Right $ tangentIntersect c n p0 -- Same point
          else Left Infinity -- Two points on vertical line
     else let b = find_b (x0,y0) m
          in Right (lineMod m b n >>= \l -> intersect n p0 p1 l c)
-}

-- | @'op' c n p0 p1@ will find a third point on the curve @c@ mod @n@
-- given @p0,p1@.
op :: Polynomial -> Int -> Point -> Point -> MaybeT (Either Infinity) Point
op c n p0@(x0,y0) p1@(x1, y1) =
  let m = slope p0 p1
  in if snd m == 0
     then if fst m == 0
          then MaybeT (Right $ tangentIntersect c n p0) -- Same point
          else MaybeT (Left Infinity) -- Two points on vertical line
     else let b = find_b (x0,y0) m
          in MaybeT (Right (lineMod m b n >>= \l -> intersect n p0 p1 l c))

-- | @'mult' c n k p@ will, using the curve @c@ mod @n@, add the
-- point @p@ to itself @k@ times
mult :: Polynomial -> Int -> Int -> Point -> MaybeT (Either Infinity) Point
mult c n k p =
  let o = op c n in
  foldlM o p (replicate (k-1) p)
