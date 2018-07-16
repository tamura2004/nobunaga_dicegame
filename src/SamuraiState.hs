module SamuraiState where

import Game
import Samurai
import Control.Monad.State (put, get, lift)
import System.Random.Shuffle (shuffleM)

shuffleSamurai :: GameST ()
shuffleSamurai = do
    game <- get
    samurai <- lift $ shuffleM $ gmSamurai game
    put game { gmSamurai = samurai }

drawSamurai :: GameST Samurai
drawSamurai = do
    game <- get
    let c:cs = gmSamurai game
    put game { gmSamurai = cs }
    return c

drawSamuraiN :: Int -> GameST [Samurai]
drawSamuraiN n = do
    game <- get
    let c = take n $ gmSamurai game
    let cs = drop n $ gmSamurai game
    put game { gmSamurai = cs }
    return c

margeSamuraiDeck :: GameST ()
margeSamuraiDeck = do
    game <- get
    put $ game { gmSamurai = (gmSamurai game) ++ samuraiDeck}
    shuffleSamurai

