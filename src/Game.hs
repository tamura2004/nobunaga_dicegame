module Game where

import Board
import Castle
import Player
import Samurai
import Control.Monad.State (put, get, StateT)

data Step
    = Prepare
    | March
    | Employ
    | Battle
    | Check
    deriving (Eq)

instance Show Step where
    show Prepare = "準備フェイズ"
    show March = "行軍フェイズ"
    show Employ = "調略フェイズ"
    show Battle = "合戦フェイズ"
    show Check = "終了フェイズ"

data Game = Game
    { gmStep :: Step
    , gmPlayers :: [Player]
    , gmBoards :: [Board]
    , gmSamurai :: [Samurai]
    , gmCastle :: [Castle]
    , gmTurn :: Int
    } deriving (Show)

type GameST = StateT Game IO

type PlayerNumber = Int

printGame' :: Game -> IO ()
printGame' game = do
    putStrLn "-- Action Cards --"
    mapM_ print $ gmBoards game
    putStrLn "-- Turn Player --"
    print $ head $ gmPlayers game

newGame :: Game
newGame = Game Prepare [] [] hatamotoDeck castleDeck 1

newBoard :: (Int, Card) -> Board
newBoard (n, card) = Board { bdCard = card, bdDice = [], bdNum = n}


