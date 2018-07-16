module Area where

data Area = West Int | East Int

instance Show Area where
    show (East n) = "東" ++ (show n)
    show (West n) = "西" ++ (show n)

