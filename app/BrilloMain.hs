module BrilloMain (run) where 

import Brillo
import Brillo.Data.Color

import RouletteGOL

--Display each of the tiles as rectagles or squares on the 
--on the board, and give it a color depening on the number

numberToPicture :: FLoat -> Float -> Int -> Picture 
numberToPicture wr hr c = 
   case c of 
      0  -> color green (rectanglePath wr hr)
      00 -> color green (rectanglePath wr hr)
      1  -> color red (rectanglePath wr hr)
      2  -> color black (rectanglePath wr hr) 
      3  -> color red (rectanglePath wr hr) 
      4  -> color black (rectanglePath wr hr)
      5  -> color red (rectanglePath wr hr)
      6  -> color black (rectanglePath wr hr)
      7  -> color red (rectanglePath wr hr)
      8  -> color black (rectanglePath wr hr)
      9  -> color red (rectanglePath wr hr)
      10 -> color black (rectanglePath wr hr)
      11 -> color black (rectanglePath wr hr)
      12 -> color red (rectanglePath wr hr)
      13 -> color black (rectanglePath wr hr)
      14 -> color red (rectanglePath wr hr)
      15 -> color black (rectanglePath wr hr)
      16 -> color red (rectanglePath wr hr)
      17 -> color black (rectanglePath wr hr)
      18 -> color red (rectanglePath wr hr)
      19 -> color red (rectanglePath wr hr)
      20 -> color black (rectanglePath wr hr)
      21 -> color red (rectanglePath wr hr)
      22 -> color black (rectanglePath wr hr)
      23 -> color red (rectanglePath wr hr)
      24 -> color black (rectanglePath wr hr)
      25 -> color red (rectanglePath wr hr)
      26 -> color black (rectanglePath wr hr)
      27 -> color red (rectanglePath wr hr)
      28 -> color black (rectanglePath wr hr)
      29 -> color black (rectanglePath wr hr)
      30 -> color red (rectanglePath wr hr)
      31 -> color black (rectanglePath wr hr)
      32 -> color red (rectanglePath wr hr)
      33 -> color black (rectanglePath wr hr)
      34 -> color red (rectanglePath wr hr)
      35 -> color black (rectanglePath wr hr)
      36 -> color red (rectanglePath wr hr)


--then have the rectangles be centered around the top left
--the screen

translate' :: Float -> Float -> Float -> Picture -> Picture
translate' ws x y = translate (x 0 halfWS) ((-y) + halfWS)
  where 
     halfWS = ws / 2


