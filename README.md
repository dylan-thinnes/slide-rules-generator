# slide-rules-generator

A generator for slide rules!

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

See a live video demo of SlideRules V1 [here.](https://github.com/dylan-thinnes/slide-rules-generator/blob/master/video-demo.mp4?raw=true)
You can run either the Rust implementation V1 or the Rust implementation V2 by
navigating to the that implementation's directory, then running:

```bash
wasm-pack build # Builds the rust into wasm in pkg/
cd www/         # Change directory to web directory
npm run start   # Build the typescript, start the server
```

Then navigate to `http://localhost:8080` in a browser.
