-- Ryan Forsyth (11/25/2015)
module Test (main) where

import Test.HUnit
import Crypto

c :: Polynomial
c = Polynomial [-2,0,0,1]

o :: Point -> Point -> Either Infinity (Maybe Point)
o = op c 7

tests :: Test
tests =
  test [
      "testEuclidean01" ~: "" ~: Just 7 ~=? (euclidean 21 14)
    , "testEuclidean02" ~: "" ~: Just 1 ~=? (euclidean 21 20)
    , "testEuclidean03" ~: "" ~: Just 5 ~=? (euclidean 20 5)
      
    , "testOp01" ~: "" ~: Right (Just (3,2)) ~=? ((3,-2) `o` (5,2))
    , "testOp02" ~: "" ~: Right (Just (3,2)) ~=? ((6,2) `o` (6,2))
    , "testOp03" ~: "" ~: Left Infinity ~=? ((3,2) `o` (3,-2))
  ]

main :: IO ()
main = runTestTT tests >> return ()
