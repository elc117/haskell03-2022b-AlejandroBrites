-- Atividade 1
add10toall :: [Int] -> [Int]
add10toall x = [x+10 | x <- x] 

-- Atividade 2
multN :: Int -> [Int] -> [Int]
multN x y = [x*y | y <- y]
 
-- Atividade 3 
multN' :: Int -> [Int] -> [Int]
multN x y = map (*x) y

-- Atividade 4
applyExpr :: [Int] -> [Int]
applyExpr x = [3*x+2 | x <- x]

-- Atividade 5
applyExpr' :: [Int] -> [Int]
applyExpr x = map (\x -> 3*x+2) x

-- Atividade 6
addSuffix :: String -> [String] -> [String]
addSuffix x y = [y ++ x | y <- y]

-- Atividade 7
selectgt5 :: [Int] -> [Int]
selectgt5 x = [x | x <- x, x > 5]

-- Atividade 8
sumOdds :: [Int] -> Int***********************************
sumOdds x = foldr1 x <- [x | x <- x, x `mod` 2 /= 0]

-- Atividade 9
sumOdds' :: [Int] -> Int*****************************************************

-- Atividade 10
selectExpr :: [Int] -> [Int]
selectExpr x = [x | x <- x, x > 20 && x < 50 && x `mod` 2 == 0]

-- Atividade 11
countShorts :: [String] -> Int

-- Atividade 12
calcExpr :: [Float] -> [Float]
calcExpr x = [x | x <- [x^2/2 | x <- x], x > 10]

-- Atividade 13
trSpaces :: String -> String

-- Atividade 14
selectSnd :: [(Int,Int)] -> [Int]
selectSnd x = [x | x <- [snd x | x <- x]]

-- Atividade 15
dotProd :: [Int] -> [Int] -> Int
dotProd x y = [zip x | x <- [snd x | x <- x, y <-y]]
