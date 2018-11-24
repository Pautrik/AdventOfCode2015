main = do
    content <- readFile "input.txt"
    putStrLn $ show $ resultingFloor content 0


resultingFloor :: String -> Int -> Int
resultingFloor [] floorLevel = floorLevel
resultingFloor (x:xs) floorLevel =
    let
        curriedFloor =
            if x == '('
            then (1 +)
            else (\n -> n - 1)
    in
        resultingFloor xs (curriedFloor floorLevel)
