module Board where

import Samurai
import Castle
import Dice

data Card = SamuraiCard Samurai | CastleCard Castle

instance Show Card where
    show card = case card of
        SamuraiCard samurai -> show samurai
        CastleCard castle -> show castle

data Board = Board
    { bdCard :: Card
    , bdDice :: [Dice]
    , bdNum :: Int
    }

instance Show Board where   
    show (Board card dice num) =
        "Action[" ++ (show num) ++ "]:" ++ (show card) ++ ":" ++ (unwords $ map show dice)
