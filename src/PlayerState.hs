module PlayerState where

import Color
import Dice
import Game
import Player
import Samurai
import SamuraiState
import Control.Monad.State (put, get, lift)
import System.Random.Shuffle (shuffleM)

getPlayer :: GameST Player
getPlayer = do
    game <- get
    let (p:_) = gmPlayers game
    return p
    
initPlayer :: PlayerNumber -> GameST ()
initPlayer n = do
    lift $ putStrLn "-- プレイヤーに武将を配ります"
    cards <- drawSamuraiN n
    let pl = createPlayersFromSamurai cards
    game <- get
    put game { gmPlayers = pl }
    lift $ printPlayers pl

preparePlayerDice :: GameST ()
preparePlayerDice = do
    game <- get
    players <- lift $ plsPrepareDice $ gmPlayers game
    put game { gmPlayers = players }

gmPlayersDiceEmpty :: GameST Bool
gmPlayersDiceEmpty = do
    game <- get
    let isEmpty = plsIsDiceEmpty $ gmPlayers game
    return isEmpty

rotatePlayers :: GameST ()
rotatePlayers = do
    game <- get
    let (p:ps) = gmPlayers game
    put game { gmPlayers = ps ++ [p] }

putPlayerDice :: [Dice] -> GameST ()
putPlayerDice dice = do
    game <- get
    let (p:ps) = gmPlayers game
    let p' = p { plDice = dice }
    put game { gmPlayers = p':ps }

addPlayerSamurai :: Color -> Samurai -> GameST ()
addPlayerSamurai color samurai = do
    lift $ print "addPlayerSamurai"
    lift $ print color
    lift $ print samurai
    game <- get
    let players = gmPlayers game
    let (left, player, right) = splitPlayersByColor color players
    let player' = player { plSamurai = samurai : (plSamurai player)}
    put game { gmPlayers = left ++ [player'] ++ right }
    lift $ print player'
