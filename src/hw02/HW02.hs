{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches actualCode guess = length . filter (\p -> fst p == snd p) $ zip actualCode guess

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (countColor) colors
                  where countColor color = length $ filter (\c -> c == color) code

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches actual guess = sum $ map (\pair -> countMin pair) $ zip (countColors actual) (countColors guess)
                       where countMin countPair = uncurry (min) countPair

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = let exactMatch = exactMatches actual guess in
                       let nonExactMatch = matches actual guess in
                       Move guess exactMatch (nonExactMatch - exactMatch)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent providedMove code = moveExactMatch providedMove == moveExactMatch moveFromCode && moveNonExactMatch providedMove == moveNonExactMatch moveFromCode
                               where moveFromCode = getMove (moveCode providedMove) code

moveCode :: Move -> Code
moveCode (Move code _ _) = code

moveExactMatch :: Move -> Int
moveExactMatch (Move _ x _) = x

moveNonExactMatch :: Move -> Int
moveNonExactMatch (Move _ _ y) = y


-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = map (\color -> color:[]) colors
allCodes len = concatMap (\code -> map (code:) $ allCodes (len - 1)) colors


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined