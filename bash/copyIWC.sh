#!/bin/bash
# Author: John Brandon 
# Script to copy IWC Fortran code from Dropbox to local drive on Mac OS machine

# Set working directory
cd ~/MakahGW/

# Copy directory with code from Andre
ditto ~/Dropbox/AWMP/Code2013\ Match/AllOut ~/MakahGW/f90/AllOut
ditto ~/Dropbox/AWMP/Code2013\ Match/code ~/MakahGW/f90/code
ditto ~/Dropbox/AWMP/Code2013\ Match/Project ~/MakahGW/f90/Project
cp ~/Dropbox/AWMP/Code2013\ Match/ExtractKs.r ~/MakahGW/f90
