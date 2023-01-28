module Chapter2.SimpleFunctions where

firstOrEmpty :: [String] -> String
firstOrEmpty lst = 
    if not (null lst)
        then head lst
        else "empty"

(+++) :: [a] -> [a] -> [a]
[] +++ lst = lst
lst1 +++ lst2 = head lst1 : tail lst1 +++ lst2


maxmin :: Ord a => [a] -> (a, a)
maxmin lst =
    let h = head lst
    in if null (tail lst)
        then (h, h)
        else ( if h > t_max then h else t_max
             , if h < t_min then h else t_min)
            where
                t = maxmin (tail lst)
                t_max = fst t
                t_min = snd t

