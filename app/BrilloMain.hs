module BrilloMain where 

import Brillo
import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Interface.Pure.Simulate

import Data.List

import Games.RouletteGOL

--Display each of the tiles as rectagles or squares on the 
--on the board, and give it a color depening on the number

numberToPicture :: Float -> Int -> Picture 
numberToPicture size number = 
    pictures 
     [translate 0 0 (color fillColor (rectangleSolid size size)),
      translate 0 0 (color yellow (rectangleWire size size)),
      translate (-size / 4) (-size / 6) (scale 0.15 0.15 (color white (text (show number))))]
    where 
      fillColor
       |number == 0 = green 
       |number `elem` [2,4,6,8,10,11,13,15,17,19,20,22,24,26,29,31,33,35] = black
       |otherwise = red

--have a function to make all the labels outside of the grid 
makeLabel :: Float -> Float -> Color -> String -> Picture 
makeLabel rs rh fillColor labelText = 
  pictures 
      [color fillColor (rectangleSolid rs (rs * rh))
      ,color black (rectangleWire rs (rs * rh))
      ,translate 0 (-30) (rotate 270 (scale 0.15 0.15 (color white(text labelText))))]


--then have the rectangles be centered around the top left
--the screen
translate' :: Float -> Float -> Float -> Picture -> Picture
translate' ws x y = translate (x - halfWS) ((-y) + halfWS)
  where 
     halfWS = ws / 2

--A function to create the entire numbers into the grid
--also find size of screen
numbersToPicture :: Float -> [[Int]] -> Picture
numbersToPicture ws grid = pictures (outsideLabels ++ concat (map(map go) (labelGrid grid)))
      where 
            gs = fromIntegral (length grid)
            rs = ws / gs

            outsideLabels = 
               [translate' ws (3.5 * rs) (1.5 * rs + rs / 2) (makeLabel rs 4 green "1st 12")
               ,translate' ws (3.5 * rs) (5.5 * rs + rs / 2) (makeLabel rs 4 green "2nd 12")
               ,translate' ws (3.5 * rs) (9.5 * rs + rs / 2) (makeLabel rs 4 green "3rd 12")
               ,translate' ws (4.5 * rs) (0.5 * rs + rs / 2) (makeLabel rs 2 green "1-18")
               ,translate' ws (4.5 * rs) (2.5 * rs + rs / 2) (makeLabel rs 2 green "Even")
               ,translate' ws (4.5 * rs) (4.5 * rs + rs / 2) (makeLabel rs 2 green "Red")
               ,translate' ws (4.5 * rs) (6.5 * rs + rs / 2) (makeLabel rs 2 green "Black")
               ,translate' ws (4.5 * rs) (8.5 * rs + rs / 2) (makeLabel rs 2 green "Odd")
               ,translate' ws (4.5 * rs) (10.5 * rs + rs / 2) (makeLabel rs 2 green "19-36")
               ]

            go ((x, y), val) = 
                let cx = (fromIntegral x * rs) + (rs / 2)
                    cy = (fromIntegral y * rs) + (rs / 2)
                in translate' ws cx cy (numberToPicture rs val)

rouletteNumbers :: [[Int]]
rouletteNumbers = [[r - 2, r - 1, r] | r <- [3, 6..36]]

showTable :: IO()
showTable = simulate (InWindow "Roulette Table" (600, 600) (100,100)) white 15 rouletteNumbers (numbersToPicture 600) (\_ _ world -> world)