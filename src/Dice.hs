module Dice where

import Color
import System.Random (randomRIO)
import Control.Monad (forM_)
import Data.List (sort, sortBy)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newArray, readArray, writeArray, getElems)

type Name = String

data Dice = Dice
    { diceColor :: Color
    , diceNum :: Int
    }

instance Show Dice where
    show (Dice c i) = "[" ++ (show c) ++ (show i) ++ "]"

scoreTable :: ST s (STArray s Color ((Int, Int), Color))
scoreTable = newArray (Red, Black) ((0, 0), Colorless)

calcScore :: [Dice] -> [((Int, Int), Color)]
calcScore dices = runST $ do
    arr <- scoreTable
    forM_ (zip [1..] dices) $ \(i, Dice color num) -> do
        ((score, priority), _) <- readArray arr color
        writeArray arr color ((score + 1, i), color)
    getElems arr

winColor :: [Dice] -> Color
winColor = snd . last . sort . calcScore

sampleDice :: [Dice]
sampleDice = [Dice Red 1, Dice Blue 1, Dice Red 1, Dice Green 1, Dice Blue 1]

d6 :: IO Int
d6 = randomRIO (1,6)

nd6 :: Int -> IO [Int]
nd6 n = do sequence $ replicate n d6

rollManyDice :: Color -> Int -> IO [Dice]
rollManyDice c n = do
    nums <- nd6 n
    return $ sortBy (\ a b -> diceNum a `compare` diceNum b) $ map (Dice c) nums

getDiceNumber :: IO Int
getDiceNumber = do
    putStrLn "使用するダイスの出目を数字で入力してください："
    str <- getLine
    let n = read str
    return n
