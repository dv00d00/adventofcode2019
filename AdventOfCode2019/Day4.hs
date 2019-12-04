import Data.List

grows :: (Foldable t, Ord a) => t (a, a) -> Bool
grows pairs = all (\(a,b)-> a <= b) pairs

ok :: Int -> Bool
ok number =
    let 
        digits = show number
        pairs = zip digits (tail digits)
        hasPair pairs = 
            let 
                match = find (\(a,b)-> a == b) pairs
            in case match of
               Nothing -> False
               otherwise -> True
    in
        hasPair pairs && grows pairs

answer1 :: Int
answer1 = length $ filter ok [138241..674034] 

ok2 :: Int -> Bool
ok2 number =
    let digits = show number
        pairs = zip digits (tail digits)
        isGrowing = grows pairs
        pairsOk = 
            let groups = map length $ groupBy (==) digits
            in elem 2 groups
    in isGrowing && pairsOk

answer2 :: Int
answer2 = length $ filter ok2 [138241..674034] 