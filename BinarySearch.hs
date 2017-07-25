binarySearch :: Ord a => [a] -> a -> Int -> Int -> Maybe Int
binarySearch xs find low high
             | find == guess' = Just mid --end goal this is fine.
             | find > guess'  = binarySearch xs find mid high
             | find < guess'  = binarySearch xs find low (mid - 1)
             | otherwise      = Nothing
             where high'  = high - 1
                   mid   = div (low + high') 2
                   guess' = xs !! mid

{- tests
list = [1,2,3,4,5,6,7]

*Main> binarySearch list 4 0 (length list)
Just 3

*Main> binarySearch list 2 0 (length list)
Just 1

*Main> binarySearch list 6 0 (length list)
Just 5
*Main> binarySearch list 0 0 (length list)
*** Exception: Prelude.!!: negative index

BOTTOM:
*Main> binarySearch list 7 0 (length list)
^CInterrupted.
-}
