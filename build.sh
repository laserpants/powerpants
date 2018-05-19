#!/bin/bash
g++ -c -o pp_ginac.o pp_ginac.cpp -lginac -lcln --std=c++11 -Wall \
  && ghc --make Main.hs -o main pp_ginac.o -lginac -lcln -lstdc++
