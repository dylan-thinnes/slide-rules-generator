# slide-rules-generator

A generator for slide rules!

Made with the help of the International Slide Rule Museum: https://www.sliderulemuseum.com

We have online meet-ups every Wednesday and Saturday - come along! [See the schedule here](https://www.sliderulemuseum.com/SRM_WhatsNew.htm).

## Features

- Outputs SVGs, so you can either print or laser cut/engrave slides
- Generates scales in linear, circular, and spiral format
- Determines the best partitioning from a list of possibilities, given a
  "minimum tick distance"
- Many different functions supported, including LogLog, trig, inverted, nth
  root and nth power scales
- Adding new tick functions, transforms, and tick generation schemes is
  straightforward

No UI yet, so this is library is really only for the particularly determined.
Want documentation? File an issue and we can go over what you want to see and
where you can help!

## Getting Started

This library is written in Haskell, a programming language in the ML-family, so
strong familiarity with Haskell and its paradigm is very useful.

### Setting Up

Note: The below commands are intended to run in a bash environment, if on
Windows please either install the GNU/Linux operating system or install WSL
(the Windows Subsystem for Linux), and run the following commands in there.

To set up a development environment, install the Haskell programming language &
the `cabal` package manager, I recommend using `ghcup` for this,
(https://www.haskell.org/ghcup/)[https://www.haskell.org/ghcup/]. Make sure to
have ghc-8.10.1 with base-4.14.0.0 installed.

```sh
$ # Install ghcup
$ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
$ ghcup install 8.10.1
$ ghcup set 8.10.1
```

Then clone a copy of this repository,

```sh
$ git clone https://github.com/dylan-thinnes/slide-rules-generator.git
```

Finally, build the package using `cabal build` in the root, and start an
interactive REPL using `cabal repl`.

### Usage

Some examples are kept in the `SlideRules` module, in `src/SlideRules.hs`,
though I suspect they'd be impenetrable to a newcomer.

This usage example will not teach Haskell - there are a number of free guides
online that do that far better than I ever could - a good example is
[Learn You a Haskell, by Miran Lipovača](http://learnyouahaskell.com/).

#### A Simple Example

Here is a super simple example of specifying a C scale using this library:

```haskell
module Main where

import Data.Foldable

import SlideRules.Generator
import SlideRules.Partitions
import SlideRules.Tick
import SlideRules.Transformations
import SlideRules.Types
import SlideRules.Utils
import SlideRules.Scales
import SlideRules.IO

main :: IO ()
main = writeToFile "out.svg" $ fold $ genRenderScaleSpec simpleCSpec

simpleCSpec :: ScaleSpec
simpleCSpec = ScaleSpec
    { heightMultiplier = 0.02
    , textMultiplier = 1
    , baseTolerance = 0.002
    , tickIdentifier = defaultIdentifier
    , generator =
        postTransform (Log 10) $
        preTransform (Offset 1) $ preTransform (Scale 9) $
            let part9  = Partition 9 0 $ fromInfo $ \info -> info { _end = 1 }
                part2  = Partition 2 0 $ fromInfo $ \info -> info { _end = _end info * 0.75 }
                part5  = Partition 5 0 $ fromInfo $ \info -> info { _end = _end info * 0.66 }
                part10 = Partition 10 0 $ fromInfo $ \info -> info { _end = _end info * 0.66 }
                tree = fillOptionTree [part9] subtrees
                subtrees =
                    [ OptionTree [part2, part5] $ [(0, 9, subtrees)]
                    , OptionTree [part5] []
                    , OptionTree [part2] []
                    ]
             in runOptionTrees (True, True) [tree]
    , circular = Nothing
    }
```

By saving this source to `app/Main.hs`, we can run `cabal run` to generate a
scale `out.svg`. Open with firefox to see:

![Picture of example "simpleCSpec"](/pictures/example-simple-cspec.png)

## Laser Cutting

Because the produced file is a vector graphic, and text / ticks are color-coded
differently, laser cutting & engraving the produced scales is very
straightforward.

Here are a few simple examples:

![Laser-engraved acrylic circular rule, with cursor](/pictures/purple-circular.jpg)
![Laser-engraved wooden circular rule, with cursor](/pictures/wooden-circular-complete.jpg)
![First example of linear scales](/pictures/linear1.jpg)
![Second example of linear scales](/pictures/linear2.jpg)
![First example of circular scales](/pictures/circular1.jpg)

## Historical

There used to be a few major folders in this project, still visible on the
`history` branch. They were as follows:

- `/ts` -- This is where the first attempt was written, it supported scale
  creation, but generally the API was crufty and most importantly rendering was
  very slow, so we rewrote it...
- `/wasm` -- This was a proof of concept using C & emscripten to generate ticks
  faster. Unfortunately, flexibility was lost due to C's lack of
  expressiveness.
- `/rust/sliderules` -- The first version that could be called a real proof of
  concept - generating specs and rendering them was very fast and the API was
  reasonably simple to type out. A live editor and a few examples together made
  for a nice experience!  However, poor separation of concerns /
  modularization, poor coding style, and some newly-found thorns in the API
  leads me to do a heavily inspried rewrite in...
- `/sliderules2` -- The beginning of another rewrite in Rust, but I decided I
  wanted a bit of home, so I quickly changed to
- `/haskell` -- The first Haskell impl, got more sophisticated than the Rust
  version (w/o live update, though), but soon had cruft of its own and now...
- `/sliderules-hs` -- A nicely organized, powerful Haskell impl, the eventual
  ultimate implementation which now resides in `master` at `/`.

You can run either the Rust implementation V1 or the Rust implementation V2 by
navigating to the that implementation's directory, then running:

```bash
wasm-pack build # Builds the rust into wasm in pkg/
cd www/         # Change directory to web directory
npm run start   # Build the typescript, start the server
```

Then navigate to `http://localhost:8080` in a browser.
