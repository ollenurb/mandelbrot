# Mandelbrot Fractal
A simple program that generates an image of the mandelbrot fractal. It uses
[repa](https://hackage.haskell.org/package/repa) and [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels). 
In order to exploit parallelism provided by the repa package, you need to have `llvm` installed. 

### Running the program
The project uses stack as a build tool, run `stack build` to compile, then `stack run -- +RTS -s
-N4` to run the program using 4 processing cores. The program will then generate an image file 
`mandelbrot.png` in the root directory.

