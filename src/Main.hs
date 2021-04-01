module Main where

import Data.Complex
import Data.Word8
import Codec.Picture

maxIterations :: Int
maxIterations = 255

imgWidth :: Int
imgWidth = 1920

imgHeight :: Int
imgHeight = 1080

f :: RealFloat a => Complex a -> Complex a -> Complex a
f c z = z^2 + c

fractal :: RealFloat a => Complex a -> Complex a -> Int
fractal c init = length . takeWhile (\x -> magnitude x <= 2) . take maxIterations $ iterated
    where iterated = iterate (f c) init

main :: IO ()
main = writePng "./mandel.png" mandelbrotImage

mandelbrotImage :: Image PixelRGB8
mandelbrotImage = generateImage generatePixel imgWidth imgHeight

generatePixel :: Int -> Int -> PixelRGB8
generatePixel x y = PixelRGB8 iters iters iters
    where
        iters = fromIntegral $ fractal (scX :+ scY) (0 :+ 0)
        scY = scaleInterval (0.0, fromIntegral imgHeight) (-1.0, 1.0) $ fromIntegral y
        scX = scaleInterval (0.0, fromIntegral imgWidth) (-2.0, 0.5) $ fromIntegral x

scaleInterval :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleInterval (a, b) (c, d) t = c + (((d - c) * (t - a)) / (b - a))
