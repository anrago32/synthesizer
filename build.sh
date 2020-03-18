#!/bin/bash

#Run to generate project executable

cd $(dirname $0)
mkdir -p executable
cd source

ghc program.hs audio.hs gui.hs patch.hs pitch.hs -o program -threaded
mv program ../executable
