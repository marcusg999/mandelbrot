module Main where

import Data.Complex
import Data.List (intercalate)
import System.Environment (getArgs)

mandelbrot :: Int -> Int -> Int -> Int -> [[Int]]
mandelbrot width height maxIterations steps =
  [[iter (toComplex x y) maxIterations | x <- [0,steps..width-1]] | y <- [0,steps..height-1]]
  where
    toComplex x y = ((fromIntegral x - halfW) / scale) :+ ((fromIntegral y - halfH) / scale)
    halfW = fromIntegral width / 2
    halfH = fromIntegral height / 2
    scale = min (halfW / 2) (halfH / 2)
    iter c maxIter = go c c maxIter
      where
        go z z' iter
          | iter == 0 = 0
          | magnitude z >= 2 = maxIter - iter
          | otherwise = go (z * z + c) z' (iter - 1)

printMandelbrot :: [[Int]] -> String
printMandelbrot = intercalate "\n" . map (intercalate "" . map pixel)
  where
    pixel iter = let (r,g,b) = color iter in "\x1b[48;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m  \x1b[0m"
    color iter = (iter `mod` 256, (iter * 7) `mod` 256, (iter * 3) `mod` 256)

main :: IO ()
main = do
  args <- getArgs
  let (width, height, maxIterations, steps) = case args of
        [w, h, i, s] -> (read w, read h, read i, read s)
        _ -> (80, 40, 100, 1)
  let mandel = mandelbrot width height maxIterations steps
  putStrLn $ printMandelbrot mandel
