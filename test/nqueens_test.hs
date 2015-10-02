module NQueens_Test where

import Test.HUnit
import NQueens


testOne = TestCase $ assertEqual 
    "Should get [Point 1 1] from a size 1 chess board" (Just [Point 1 1]) $ solve 1

testTwo = TestCase $ assertEqual 
    "Should get Nothing from a size 2 chess board" Nothing $ solve 2

testThree = TestCase $ assertEqual 
    "Should get Nothing from a size 3 chess board" Nothing $ solve 3

testFour = TestCase $ assertEqual 
    "Should get a valid board from a size 4 chess board" 
    (Just [Point 1 2, Point 2 4, Point 3 1, Point 4 3]) 
    $ solve 4

testEight = TestCase $ assertEqual
    "Should get a valid board from a size 8 chess board"
    (Just [[1,1], [2,5], [3,8], [4,6], [5,3], [6,7], [7,2], [8,4]])
    $ fmap (map ( \(Point x y) -> [x, y])) $ solve 8

main = runTestTT $ TestList [testOne, testTwo, testThree, testFour, testEight]