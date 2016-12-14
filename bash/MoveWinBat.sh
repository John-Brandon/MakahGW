#!/bin/bash
# Move Andre's Win.bat scripts from his ./Project directory to the ./code directory

# Project Directory
proj_dir=~/MakahGW;

# Go to code directory
cd $proj_dir/f90/code;

# Make a new folder
mkdir WinBatScripts;

# Directory with ./Project/Runstreams (i.e. all the *.DDD -> copy.dat trials)
cd $proj_dir/f90/Project;

# Copy Win.bat files from ./Project to ./code/WinBatScripts
find . -iname '*.bat' -exec cp {} $proj_dir/f90/code/WinBatScripts \;

exit; 
