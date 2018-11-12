#!/bin/bash

#Run to generate project executable

cd $(dirname $0)
mkdir -p executable
cd source

ghc program.hs
mv program ../executable/program

rm program.hi
rm program.o
