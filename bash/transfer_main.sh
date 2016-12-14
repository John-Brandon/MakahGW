#!/bin/bash
# Main script -- calls other scripts which do the heavy lifting.
# NOTE: 
#  If running for the first time, you might have to give 
#  executable privelages to the scripts in the ./scripts folder
# e.g. 
# > chmod a+x 'script-name.sh'
# -------------------------------------------------------------------

# Reference directory variable
DIR=~/MakahGW/f90;

cd DIR;

# Copy AEP's files from DropBox to local machine
./copyIWC.sh;

# The directory structure of Andre's input files from DropBox does not play nice with the code. 
# Move some files into more appropriate directories.

# Copy catch data file(s) so they reside in same directory as main executable
cp ./Project/CATCHG.DAT ./code/CATCHG.DAT;

# Move 'manage.dat' files into a folder under '../code' directory
./copy-manage-dat.sh;

# Move Windows *.bat files into folder under ./code directory
./MoveWinBat.sh;

# Copy file with random numbers to same directory as code.
cp ./Project/RANDOM.NUM ./code;

# Copy Runstreams from ./Project to ./code directory.
# These are the *.DDD trials -> copy.dat
cp -r ./Project/Runstreams ./code/runstreams



exit;