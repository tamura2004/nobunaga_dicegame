module Player where

import Castle
import Color
import Dice
import Samurai

data Player = Player
    { plColor :: Color
    , plSamurai :: [Samurai]
    , plCastle :: [Castle]
    , plDice ::[Dice]
    }

instance Show Player where
    show (Player c ss cs ds) =
        "プレイヤー" ++ (show c) ++ "：" ++
        (unwords $ map show ss) ++
        (unwords $ map show cs) ++
        "：ダイス" ++ (unwords $ map show ds)

newPlayer :: (Samurai, Color) -> Player
newPlayer (s, c) = Player c [s] [] []

printPlayers :: [Player] -> IO ()
printPlayers pls = do mapM_ print pls

createPlayersFromSamurai :: [Samurai] -> [Player]
createPlayersFromSamurai cards = map newPlayer $ zip cards colors

plPrepareDice :: Player -> IO Player
plPrepareDice pl = do
    let color = plColor pl
    let num = plDiceNum pl
    dice <- rollManyDice color num
    return pl { plDice = dice }

plsPrepareDice :: [Player] -> IO [Player]
plsPrepareDice pls = do
    mapM plPrepareDice pls

plIsDiceEmpty :: Player -> Bool
plIsDiceEmpty = null . plDice

plsIsDiceEmpty :: [Player] -> Bool
plsIsDiceEmpty = all plIsDiceEmpty

plDiceNum :: Player -> Int
plDiceNum pl = x + 3 where
    x = min samurai castle
    samurai = length $ plSamurai pl
    castle = length $ plCastle pl

plDiceNums :: Player -> [Int]
plDiceNums = map diceNum . plDice

selectedPlayerDice :: Player -> Int -> [Dice]
selectedPlayerDice pl n = filter cond $ plDice pl
    where
        cond :: Dice -> Bool
        cond = (n ==) . diceNum

unselectedPlayerDice :: Player -> Int -> [Dice]
unselectedPlayerDice pl n = filter cond $ plDice pl
    where
        cond :: Dice -> Bool
        cond = (n /=) . diceNum

getPlayerDiceNumber :: Player -> IO Int
getPlayerDiceNumber pl = do
    n <- getDiceNumber
    if n `elem` dices then
        return n
    else
        getPlayerDiceNumber pl
    where
        dices :: [Int]
        dices = plDiceNums pl

splitPlayersByColor :: Color -> [Player] -> ([Player], Player, [Player])
splitPlayersByColor color players =
    let (left, right) = span ((/= color) . plColor) players
    in (left, head right, tail right)
