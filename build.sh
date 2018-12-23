#!/bin/bash

#Run to generate project executable

cd $(dirname $0)
mkdir -p executable
cd source

ghc program.hs audio-generation.hs patch.hs pitch-conversion.hs -o program
mv program ../executable
