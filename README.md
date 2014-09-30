
# fluent

Haskell-only live sampler, looper etc.

## Requirements

* Portaudio
* Libsndfile
* [Haskell Platform](http://www.haskell.org/platform)

## Installation (OS X with Homebrew)

    brew install portaudio libsndfile
    cabal configure
    cabal install

## Use (TODO sketchy)

* Read/write audio files to RAM buffers
* Move and copy buffers around
* Loop/record/play buffers with arbitrary volume, speed etc. (TODO how to interpolate?)