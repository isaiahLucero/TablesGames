module Games.RouletteGOL where 

data BetType = InsideBet Int | Red | Black | Even | Odd  | FirstHalf | SecondHalf | FirstThird | SecondThird | ThirdThird | FirstColumn | SecondColumn | ThirdColumn deriving (Show)

--first starting of with the rules forthe table just for now
--take a users number and do a random spin and see if the numbers line up and multiply by standard gambling rules 
rules :: Int -> BetType -> Int -> Int 
rules bet betType spin = case betType of
  InsideBet guess -> if spin == guess then bet * 35 else 0
  Red             -> if isRed spin then bet * 2 else 0
  Black           -> if isBlack spin then bet * 2 else 0
  Even            -> if spin /= 0 && even spin then bet * 2 else 0
  Odd             -> if spin /= 0 && odd spin then bet * 2 else 0
  FirstHalf       -> if firstEighteen spin then bet * 2 else 0
  SecondHalf      -> if secondEighteen spin then bet * 2 else 0
  FirstThird      -> if firstTwelve spin then bet * 3 else 0
  SecondThird     -> if secondTwelve spin then bet * 3 else 0
  ThirdThird      -> if thirdTwelve spin then bet * 3 else 0
  FirstColumn     -> if firstCol spin then bet * 3 else 0
  SecondColumn    -> if secondCol spin then bet * 3 else 0
  ThirdColumn     -> if thirdCol spin then bet * 3 else 0

--All the predicates for the outside table rules
--these will pay out even money if won
--even and odd will also 
isRed :: Int -> Bool 
isRed spin = spin /= 0 && spin `elem` [1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36]

isBlack :: Int -> Bool 
isBlack spin = spin /= 0 && spin `elem` [2,4,6,8,10,11,13,15,17,19,20,22,24,26,29,31,33,35]

firstEighteen :: Int -> Bool 
firstEighteen spin = spin `elem` [1..18]

secondEighteen :: Int -> Bool 
secondEighteen spin = not (firstEighteen spin)

--these results will pay 2:1 

firstTwelve :: Int -> Bool 
firstTwelve spin = spin `elem` [1..9]

secondTwelve :: Int -> Bool 
secondTwelve spin = spin `elem` [13..24]

thirdTwelve :: Int -> Bool 
thirdTwelve spin = spin `elem` [25..36]

firstCol :: Int -> Bool 
firstCol spin = spin `elem` [1,4..34]

secondCol :: Int -> Bool 
secondCol spin = spin `elem` [2,5..35]

thirdCol :: Int -> Bool 
thirdCol spin = spin `elem` [3,6..36]

--List of test cases to see if one would accurately multiply the cost and set gained money to zero if lost
testCases :: [(Int, BetType, Int)] 
testCases = [(50, InsideBet 4, 4),
           (25, InsideBet 2, 36),
           (15, Red, 18),
           (5, Red, 15),
           (30, Black, 35), 
           (20, Black, 3),
           (5, Even, 4),
           (10, Even, 1),
           (50, Odd, 3),
           (30, Odd, 8),
           (35, FirstHalf, 10),
           (45, FirstHalf, 25),
           (36, SecondHalf, 26),
           (78, SecondHalf, 4), 
           (23, FirstThird, 6), 
           (23, FirstThird, 30), 
           (23, SecondThird, 14), 
           (23, SecondThird, 35), 
           (23, ThirdThird, 29), 
           (48, ThirdThird, 11), 
           (23, FirstColumn, 22), 
           (23, FirstColumn, 36),
           (23, SecondColumn, 20),
           (23, SecondColumn, 0),
           (23, ThirdColumn, 27),
           (23, ThirdColumn, 20)]

--Runs each of the test cases through the rules and the main prints the resulting winnings
runCases :: [Int]
runCases = [rules bet betType spin | (bet, betType, spin) <- testCases]

labelGrid :: [[Int]] -> [[((Int, Int), Int)]] 
labelGrid grid = [[((x, y), cell) | (x , cell) <- zip [0..] row] | (y, row) <- zip [0..] grid]

--the numbering order for the numbers in roulette
rouletteNumbersTest :: [[Int]]
rouletteNumbersTest = [[r, r - 1, r - 2] | r <- [36, 33..3]]

--test
testRoulette :: Bool
testRoulette = labelGrid grid == ans 
  where 
       grid = [[3,2,1],
               [6,5,4],
               [9,8,7]]
       ans = [[((0,0), 3), ((1,0), 2), ((2,0), 1)],
                [((0,1), 6), ((1,1), 5), ((2,1), 4)],
                [((0,2), 9), ((1,2), 8), ((2,2), 7)]]