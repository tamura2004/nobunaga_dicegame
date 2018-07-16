module BoardState where

import Game
import Dice
import Board
import SamuraiState
import CastleState
import Control.Monad.State (put, get, lift)

prepareBoards :: GameST ()
prepareBoards = do
    lift $ putStrLn "-- アクションカードを配布します"
    samurai <- drawSamuraiN 2
    castle <- drawCastleN 4
    let samuraiCards = map SamuraiCard samurai
    let castleCards = map CastleCard castle
    let board = map newBoard $ zip [1..] $ samuraiCards ++ castleCards
    game <- get
    put game { gmBoards = board }

getBoard :: Int -> GameST Board
getBoard n = do
    game <- get
    let boards = gmBoards game
    return $ boards !! (n-1)

getBoardDice :: Int -> GameST [Dice]
getBoardDice n = do
    board <- getBoard n
    return $ bdDice board

getBoardCard :: Int -> GameST Card
getBoardCard n = do
    board <- getBoard n
    return $ bdCard board


putBoardDice :: Int -> [Dice] -> GameST ()
putBoardDice n dice = do
    game <- get
    let boards = gmBoards game
    let board = boards !! (n-1)
    let board' = board { bdDice = dice }
    let boards' = (take (n-1) boards) ++ [board'] ++ (drop n boards)
    put game { gmBoards = boards' }

addBoardDice :: Int -> [Dice] -> GameST ()
addBoardDice n dice = do
    oldDice <- getBoardDice n
    putBoardDice n (oldDice ++ dice)
