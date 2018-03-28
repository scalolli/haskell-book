module Exercises where

    doubleUp :: [a] -> [a]
    doubleUp [] = []
    doubleUp xs@(x:_) = x:xs