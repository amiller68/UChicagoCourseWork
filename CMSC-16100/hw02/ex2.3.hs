{-Alex Miller-}
-- | return returns 90000
-- | return2 returns 90000
-- | return3 returns 90000

result = sum
 . take 100
 . filter (divisibleBy 2)
 . filter (divisibleBy 3)
 . filter (not . divisibleBy 4)
 . filter (not . divisibleBy 9)
 $ [0..]



result2 = sum
 . take 100
 . filter (allp [ divisibleBy 2
 , divisibleBy 3
 , not . divisibleBy 4
 , not . divisibleBy 9
 ])
 $ [0..]

result3 = sum
 . take 100
 . filterAll [ divisibleBy 2
 , divisibleBy 3
 , not . divisibleBy 4
 , not . divisibleBy 9
 ]
 $ [0..]

divisibleBy :: Integral n => n -> n -> Bool
divisibleBy y x
 | mod x y == 0 = True
 | x == 0 = True
 | otherwise = False

allp :: [(n -> Bool)] -> n -> Bool
allp [] x = True
allp (p:ps) x 
 | p x == True = allp ps x
 | otherwise = False


filterAll :: [(a -> Bool)] -> [a] -> [a]
filterAll [] (x:xs) = []
filterAll (p:ps) [] = []
filterAll [] [] = []
filterAll (p:ps) (x:xs)
 | allp (p:ps) x       = x : filterAll (p:ps) xs
 | otherwise = filterAll (p:ps) xs
