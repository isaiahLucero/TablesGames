module Main where

import Games.RouletteGOL (runCases)

main :: IO ()
main = mapM_ print runCases
