main = do
    content <- readFile "input.txt"
    putStrLn $ show $ indexAtBasementEntry content 0 0

indexAtBasementEntry :: String -> Int -> Int -> Int
indexAtBasementEntry [] _ _ = (-1)
indexAtBasementEntry _ (-1) charIndex = charIndex
indexAtBasementEntry (x:xs) floorLevel charIndex =
    let deltaFloor =
            if x == '('
                then 1
                else (-1)
     in indexAtBasementEntry xs (floorLevel + deltaFloor) (charIndex + 1)
