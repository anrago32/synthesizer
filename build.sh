#!/bin/bash

#Run to generate project executable

cd $(dirname $0)
mkdir -p executable
cd source

ghc program.hs pitch-conversion.hs tone-generation.hs -o program
mv program ../executable
