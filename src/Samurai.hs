module Samurai where

import Area
import Dice

data Samurai = Samurai Name Ability

data Ability
    = ChangeDice Int Int
    | BridgeHead Area
    | ExchangeSamurai
    | AddGold Int
    | AddBattleDice Int
    | DeleteDice Int

instance Show Samurai where
    show (Samurai n a) = "<" ++ n ++ "> " ++ (show a) 

instance Show Ability where
    show (ChangeDice x y) = "[" ++ (show x) ++ "]/[" ++ (show $ x+1) ++ "]->[" ++ (show $ y) ++ "]"   
    show (BridgeHead a) = show a
    show ExchangeSamurai = "交換：武将"
    show (AddGold x) = "補給：" ++ (show x)
    show (AddBattleDice x) = "合戦：" ++ (show x)
    show (DeleteDice x) = "[" ++ (show x) ++ "]->x"

samuraiDeck :: [Samurai]
samuraiDeck =
    [Samurai "弥助" (ChangeDice 1 6)
    ,Samurai "木下藤吉郎" (ChangeDice 3 1)
    ,Samurai "明智光秀" (ChangeDice 5 1)
    ,Samurai "柴田勝家" (ChangeDice 3 5)
    ,Samurai "前田利家" (ChangeDice 5 3)
    ,Samurai "滝川一益" (ChangeDice 1 4)
    ,Samurai "毛利元就" (ChangeDice 5 4)
    ,Samurai "徳川家康" (ChangeDice 3 2)
    ,Samurai "今川義元" (ChangeDice 5 1)
    ,Samurai "北条氏康" (ChangeDice 1 3)
    ,Samurai "島津義弘" (ChangeDice 3 6)
    ,Samurai "松永久秀" ExchangeSamurai
    ,Samurai "武田信玄" (AddGold 2)
    ,Samurai "上杉謙信" (AddBattleDice 2)
    ,Samurai "伊達政宗" (BridgeHead (East 4))
    ,Samurai "九鬼義隆" (BridgeHead (East 6))
    ,Samurai "伊藤マンショ" (BridgeHead (West 6))
    ,Samurai "足利義昭" (DeleteDice 1)
    ,Samurai "大友宗麟" (BridgeHead (West 4))
    ]

hatamotoDeck :: [Samurai]
hatamotoDeck = take 6 samuraiDeck
