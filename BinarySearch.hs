binarySearch xs find low high
             | find == guess' = Just mid --end goal this is fine.
             | find > guess'  = binarySearch xs find (mid + 1) high
             | find < guess'  = binarySearch xs find low (mid - 1)
             | otherwise      = Nothing
             where high'  = high - 1
                   mid   = div (low + high') 2
                   guess' = xs !! mid

{- tests
*Main> binarySearch list 4 0 (length list)
Just 3

*Main> binarySearch list 2 0 (length list)
Just 1

-}
