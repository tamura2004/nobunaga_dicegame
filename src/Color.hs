module Color where

import Data.Array.ST (Ix)

data Color = Red | Blue | Green | Yellow | White | Black | Colorless deriving (Enum, Eq, Ord, Ix)

instance Show Color where
    show Red = "赤" 
    show Blue = "青" 
    show Green = "緑" 
    show Yellow = "黄" 
    show White = "白" 
    show Black = "黒"
    show Colorless = "無色"

colors :: [Color]
colors = [Red .. Black]
