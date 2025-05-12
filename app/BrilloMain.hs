module BrilloMain (run) where 

import Brillo
import Brillo.Data.Color
import Brillo.Data.Picture
import Brillo.Interface.Pure.Simulate
import Brillo.Interface.IO.Interact

import Data.List

import Games.RouletteGOL

data GameState = GameState
  { pot          :: Int 
  , placedBets   :: [((Int, Int), Int)] 
  , selectedChip :: Int
  , boardWidth   :: Float
  , boardCols    :: Int
  , boardRows    :: Int 
  ,randomSeed     :: Int 
  }

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

--draw chips to the right of the roulette table 
numbersToChips :: Float -> Picture 
numbersToChips size = pictures (zipWith drawChip [1, 5, 10, 25, 50] [0..])
   where 
     chipSize = 25
     spacing = 120
     drawChip val i = 
       translate 0 (200 - fromIntegral i * spacing) (pictures [color black (circleSolid chipSize)
                                                    ,translate (-13) (-7) (scale 0.15 0.15 (color green (text ("$" ++ show val))))])

--show the money the player has 
drawPot :: Int -> Picture 
drawPot p = translate 0 250 (scale 0.2 0.2 (color black(text ("Pot : $" ++ show p))))

--draw the game 
drawGame :: GameState -> Picture 
drawGame gs = pictures [numbersToPicture (boardWidth gs) rouletteNumbersTest, numbersToChips (boardWidth gs), drawPot (pot gs), drawPlacedChips gs, drawSelectedChip (selectedChip gs)]

--Need to draw the new betting chips on the board 
drawPlacedChips :: GameState -> Picture 
drawPlacedChips gs = pictures (map draw(placedBets gs))
   where 
      cellW = boardWidth gs / fromIntegral (boardCols gs)
      cellH = boardWidth gs / fromIntegral (boardRows gs)
      draw ((r, c), val) = 
         let x = fromIntegral c * cellW
             y = fromIntegral r * cellH
         in translate' (boardWidth gs) x y (scale 0.1 0.1 (color green(text("$" ++ show val))))

--Shows the user the chip they have selected 
drawSelectedChip :: Int -> Picture
drawSelectedChip val = translate 0 220 (scale 0.15 0.15 (color green(text("$Selected: $" ++ show val))))

--Detects user clicks on a chip 
chipRadius :: Float 
chipRadius = 25 

chipCenterX :: Float 
chipCenterX = 0

chipXMin :: Float 
chipXMin = chipCenterX - chipRadius
chipXMax :: Float 
chipXMax = chipCenterX + chipRadius

chipTopY :: Float 
chipTopY = 225
chipSpacing :: Float 
chipSpacing = 120 
numChips :: Int 
numChips = 5

chipBottomY :: Float 
chipBottomY = chipTopY - chipSpacing * fromIntegral (numChips - 1) - chipRadius * 2

chipValues :: [Int] 
chipValues = [1,5, 10, 25, 50]

--safely indexs the chipValues
safeIndex :: Int -> [a] -> Maybe a 
safeIndex i xs = if i >= 0 && i < length xs then Just (xs !! i) else Nothing

--Helper funtion to check to see if the picked chip is on board 
isClickonChip :: Float -> Float -> Bool 
isClickonChip mx my = mx >= chipXMin && mx <= chipXMax && my <= chipTopY && my >= chipBottomY

--Find y value for chip 
chipIndexFromY :: Float -> Maybe Int 
chipIndexFromY my 
  |idx >= 0 && idx < length chipValues = Just idx 
  |otherwise = Nothing 
  where 
    idx = floor ((chipTopY - chipRadius - my) / chipSpacing)

--Helper Function to place chip on board 
mapClickToGrid :: Float -> Float -> Float -> Int -> Int -> Maybe (Int, Int) 
mapClickToGrid bw mx my cols rows 
   | x < 0 || x >= cols = Nothing 
   | y < 0 || y >= rows = Nothing 
   |otherwise = Just (x, y) 
   where 
    cellW = bw / fromIntegral cols 
    cellH = bw / fromIntegral rows
    x = floor((mx + bw / 2) / cellW)
    y = floor ((-my + bw / 2) / cellH)

--Handle user clicks 
handleEvent :: Event -> GameState -> GameState 
--This will handle the users clicks to the screen 
handleEvent (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs 
    --Checks to see if the user validly chose a chip to place on the board 
    |isClickonChip mx my 
    , Just idx <- chipIndexFromY my 
    , Just val <- safeIndex idx chipValues
    = gs {selectedChip = val}
    --Will handle the users clicks to board and place the chip on the board
    |Just gridCoord <- mapClickToGrid (boardWidth gs) mx my (boardCols gs) (boardRows gs) = 
      let bet = (gridCoord, selectedChip gs) 
      in gs {placedBets = bet : placedBets gs}
{-
--This will handle spin events and will roll once the user presses the enter KeyEnter
--handleEvent (EventKey(SpecialKey KeyEnter) Down _ _) gs
--| null (placedBets gs) = gs
--| otherwise = gs {pot = newPot, placedBets = [], randomSeed = newSeed}
--where 
--bets = convertBetList (placedBets gs)
--(won, _) = runSimulation bets (randomSeed gs) 
--totalBet = sum [v | (_, v) <- placedBets gs]
--newSeed = rand (randomSeed gs) 
--newPot = pot gs - totalBet + won 
-}
handleEvent _ gs = gs

updateGame :: Float -> GameState -> GameState
updateGame _ = id 

--run game
run :: IO()
run = play window white 60 initState drawGame handleEvent updateGame 
  where 
    cols = 3 
    rows = 12 
    initState = GameState 
      {pot = 100
      , placedBets = [] 
      , selectedChip = 1
      , boardWidth = 600
      , boardCols = cols 
      , boardRows = rows 
      , randomSeed = 12345
      }
    window = InWindow "Roulette Table"  (600, 600) (100, 100)