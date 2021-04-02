{-# LANGUAGE TypeOperators   #-}

module Main where

import Data.Complex
import Data.Word8
import Codec.Picture
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM1, DIM2, U, D, Z (..), (:.)(..), (!))


-- Type that represents a single Pixel that will be used by Repa
type RGB8 = (Pixel8, Pixel8, Pixel8)

maxIterations :: Int
maxIterations = 255

imgWidth :: Int
imgWidth = 1920

imgHeight :: Int
imgHeight = 1080

imgPath :: String
imgPath = "./mandel.png"

f :: RealFloat a => Complex a -> Complex a -> Complex a
f c z = z^2 + c

fractal :: RealFloat a => Complex a -> Complex a -> Int
fractal c init = length . takeWhile (\x -> magnitude x <= 2) . take maxIterations $ iterated
    where iterated = iterate (f c) init

scaleInterval :: Fractional a => (a, a) -> (a, a) -> a -> a
scaleInterval (a, b) (c, d) t = c + (((d - c) * (t - a)) / (b - a))

main :: IO ()
main = do
    putStrLn $ "Writing mandelbrot set in " ++ imgPath ++ ", please wait."
    repaVectorImage <- R.computeUnboxedP generateImgRepa
    let mandelbrotImage = repaArrayToImage repaVectorImage
    writePng imgPath mandelbrotImage
    putStrLn "Image successfully written."

generatePixel :: (Z :. Int :. Int) -> RGB8
generatePixel (Z :. x :. y) = (iters, iters, iters)
    where
        iters = fromIntegral $ fractal (scX :+ scY) (0 :+ 0)
        scY = scaleInterval (0.0, fromIntegral imgHeight) (-1.0, 1.0) $ fromIntegral y
        scX = scaleInterval (0.0, fromIntegral imgWidth) (-2.0, 0.5) $ fromIntegral x

generateImgRepa :: Array D DIM2 RGB8
generateImgRepa = R.fromFunction (Z :. imgWidth :. imgHeight) generatePixel

repaArrayToImage :: Array U DIM2 RGB8 -> Image PixelRGB8
repaArrayToImage a = generateImage gen imgWidth imgHeight
  where
    gen x y =
      let (r, g, b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b
