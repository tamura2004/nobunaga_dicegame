module GameState where

import Board
import BoardState
import Castle
import CastleState
import Color
import Dice
import Game
import Player
import PlayerState
import Samurai
import SamuraiState
import Control.Monad.State (put, get, lift)
import System.Random.Shuffle (shuffleM)

printGame :: GameST ()
printGame = do
    game <- get
    lift $ printGame' game

initGame :: PlayerNumber -> GameST ()
initGame n = do
    shuffleDeck
    initPlayer n
    margeSamuraiDeck

shuffleDeck :: GameST ()
shuffleDeck = do
    shuffleSamurai
    shuffleCastle   

handleMarch :: GameST ()
handleMarch = do
    isEmpty <- gmPlayersDiceEmpty
    if isEmpty then
        updateStep Employ
    else do
        handlePlayerMarch

handlePlayerMarch :: GameST ()
handlePlayerMarch = do
    player <- getPlayer
    if plIsDiceEmpty player then
        rotatePlayers
    else do
        n <- lift $ getPlayerDiceNumber player
        let selectDice = selectedPlayerDice player n
        let unselectDice = unselectedPlayerDice player n
        addBoardDice n selectDice
        putPlayerDice unselectDice
        rotatePlayers

updateStep :: Step -> GameST ()
updateStep step = do
    game <- get
    put game { gmStep = step }

employSamurai :: Int -> GameST ()
employSamurai n = do
    dice <- getBoardDice n
    if null dice then do
        return ()
    else do
        SamuraiCard card <- getBoardCard n
        let winner = winColor dice
        putBoardDice n []
        addPlayerSamurai winner card

handleGameTurn :: GameST ()
handleGameTurn = do
    game <- get
    lift $ print $ gmStep game
    case gmStep game of
        Prepare -> do
            prepareBoards
            preparePlayerDice
            updateStep March
        March -> do
            printGame
            handleMarch
        Employ -> do
            employSamurai 1
            employSamurai 2
            updateStep Battle
        Battle -> do
            updateStep Check
        Check -> do
            closeGame

    if gmStep game == Check then return () else handleGameTurn

closeGame :: GameST ()
closeGame = return ()
