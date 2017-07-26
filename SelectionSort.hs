--O(n^2)
selectionSort [] = []
selectionSort whole@(x:xs) = case filtered of
                         [] -> x : selectionSort xs
                         _  -> selectionSort $ xs ++ [x]
  where filtered = filter ((>) x) xs


--selectionSort findLargest [1,2,3,4,5]

{-
import Data.List
easySort = sort --lol
-}