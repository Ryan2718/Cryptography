-- Ryan Forsyth (11/25/2015)
module Test (main) where

import Test.HUnit
import Crypto

c :: Polynomial
c = Polynomial [-2,0,0,1]

o :: Point -> Point -> Maybe Point
o = op c 7

w :: Point
w = Point 3 (-2)

x :: Point
x = Point 3 2

y :: Point
y = Point 5 2

z :: Point
z = Point 6 2

tests :: Test
tests =
  test [
      "testEuclidean01" ~: "" ~: Just 7 ~=? (euclidean 21 14)
    , "testEuclidean02" ~: "" ~: Just 1 ~=? (euclidean 21 20)
    , "testEuclidean03" ~: "" ~: Just 5 ~=? (euclidean 20 5)
      
    , "testOp01" ~: "" ~: Just x ~=? o w y
    , "testOp02" ~: "" ~: Just x ~=? o z z -- Tangent Point
    , "testOp03" ~: "" ~: Just Infinity ~=? o x w -- Vertical Line
    , "testOp04" ~: "" ~: Just x ~=? o x Infinity -- Identity Element
    , "testOp05" ~: "" ~: Just x ~=? o Infinity x -- Identity Element
  ]

main :: IO ()
main = runTestTT tests >> return ()
