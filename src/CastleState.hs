module CastleState where

import Game
import Castle
import Control.Monad.State (put, get, lift)
import System.Random.Shuffle (shuffleM)

shuffleCastle :: GameST ()
shuffleCastle = do
    game <- get
    castle <- lift $ shuffleM $ gmCastle game
    put game { gmCastle = castle }

drawCastle :: GameST Castle
drawCastle = do
    game <- get
    let c:cs = gmCastle game
    put game { gmCastle = cs }
    return c

drawCastleN :: Int -> GameST [Castle]
drawCastleN n = do
    game <- get
    let c = take n $ gmCastle game
    let cs = drop n $ gmCastle game
    put game { gmCastle = cs }
    return c
