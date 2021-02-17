module Main where

import Data.Complex
import Graphics.Gloss
import qualified Data.ByteString as B
import Data.Word8
import Control.Parallel.Strategies (rdeepseq, parMap)

maxIterations :: Int
maxIterations = 255

imgDim :: Int
imgDim = 1000

f :: RealFloat a => Complex a -> Complex a -> Complex a
f c z = z^2 + c

fractal :: RealFloat a => Complex a -> Complex a -> Int
fractal c init = length . takeWhile (\x -> magnitude x <= 2) . take maxIterations $ iterated
    where iterated = iterate (f c) init

format :: BitmapFormat
format = BitmapFormat TopToBottom PxRGBA

main :: IO ()
main = display window white $ bitmapOfByteString imgDim imgDim format mandelbrotImage False
    where window = InWindow "Mandelbrot" (100, 100) (300, 300)

mandelbrotImage :: B.ByteString
mandelbrotImage = B.pack $ concat $ (parMap rdeepseq) computePixell pixels

pixels :: [(Int, Int)]
pixels = [(x, y) | x <- [0..imgDim-1], y <- [0..imgDim-1]]

scaleInterval :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleInterval (a, b) (c, d) t = c + (((d - c) * (t - a)) / (b - a))

computePixell :: (Int, Int) -> [Word8]
computePixell (x, y) = [grayScale, grayScale, grayScale, 255]
    where
        grayScale = fromIntegral $ fractal (scX :+ scY) (0 :+ 0)
        scY = scaleInterval (0.0, fromIntegral imgDim) (-1.0, 1.0) $ fromIntegral y
        scX = scaleInterval (0.0, fromIntegral imgDim) (-2.0, 0.5) $ fromIntegral x

