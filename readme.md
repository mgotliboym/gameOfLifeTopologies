Usage:
run `stack repl` to get into ghci, the haskell interactive interface
(Installing ghc and stack is an adventure, I don't recall how to do that.)
Compiling from source is necessary, I don't have a nice executable that demonstrates anything.

`runGame [microseconds/frame] $ torusFromList $ vertWallBoard [height] [width]` with values filled in for the frame rate (sleep time in microseconds between frames) and the height/width will start a single vertical wall of living cells on a torus.
`runGame [microseconds/frame] $ kleinFromList $ vertWallBoard [height] [width]` will do the same for a klein bottle rather than a torus

After torusFromList or kleinFromList can come any 2d list of booleans. BoardCreation.hs has a whole bunch of various constructors for such boards.

Other nice ones are `diagBoard [height]` which will make a square board with a living diagonal. `padBoardCenter height width [[bool]]` will surround a board of your choice with emptiness. glider is a board which contains a glider.
So together we get a nice command `padBoardCenter 20 20 $ glider`
Variations also exist, and nice ways to put boards together. Replacing `glider` with `rPentomino` above is also neat.

So altogether another example is `runGame 300000 $ torusFromList $ padBoardCenter 20 20 $ glider`

Also, besides torusFromList and kleinFromList there is also infPlaneFromList which plays the game of life as originally intended, on an infinite plane. Sadly I don't have variations for spheres or projective planes.

If you replace `runGame [microseconds/frame]` with `mainFileOutput` you can get haskell to print the output to a file.
This will let you read the output from octave and draw it on an actual torus. In particular,
`mainFileOutput  $ torusFromList $ vertWallBoard 5 n` (with a number filled in for n) will generate nice output.
Then, in octave/lifeOnTorus.m, line 48 reads `lifeOnTorus(129, 5, 8, 300000);` Change the `129` to whichever `n` you would like to see the file output on a torus, by running `octave lifeOnTorus.m`. This could potentially work with gliders also, but would take some work with the matlab.