#!/bin/bash

cd ~/MakahGW/f90/code;

gfortran -fbounds-check -c -w AW-RES7.FOR;

gfortran -fbounds-check AW-RES7.o -w -o aw-res7.app;

# Copy compiled executable files into same directory as *.PAR files.
cp ~/MakahGW/f90/code/aw-res7.app ~/MakahGW/f90/Project
