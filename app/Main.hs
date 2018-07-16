module Main where

import Game
import GameState
import Control.Monad.State

main :: IO ()
main = evalStateT runGame newGame

runGame :: StateT Game IO ()
runGame = do
    lift $ putStrLn "-- 信長のダイ野望 --"
    n <- lift getPlayerNumber
    initGame n
    handleGameTurn
    closeGame

getPlayerNumber :: IO Int
getPlayerNumber = do
    putStrLn "プレイヤー人数を入力してください："
    str <- getLine
    let n = read str
    if n < 1 || 6 < n
        then do
            putStrLn "error: 1-6の数字を入力してください"
            getPlayerNumber
        else return n
