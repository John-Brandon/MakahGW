#!/bin/bash
# Notes on compiling Andre Punt's AWMP Fortran code
# Using gfortran compiler on Mac OS 10.11
# For quick reference see: https://gcc.gnu.org/wiki/GFortranUsage

# Compiling source files one at a time to more easily catch any errors

# Can use this command to search files for text returned from compilation error
# grep -l DECL_STKVRS *.FOR
# Where: grep -l [search text] [file extensions] 

# Change to Fortran code directory
cd ~/MakahGW/f90/code

# Remove previously compiled object files
rm *.o

# Compile source without showing warnings
gfortran -c -w COMMON.FOR;  # Should result in *.o object file

gfortran -c -w MATRIX.FOR;  # The *.o object files will be linked into an executable at the end

# This is to be compiled into a separate executable
# gfortran -c -w AW-RES7.FOR;

# Includes DECL_STKVRS, etc. 
gfortran -c -w F2TST9.FOR;

# Expects DECL_STKVRS, so need to compile after F2TST9.FOR
gfortran -c -w AWEXTRD.FOR; 

gfortran -c -w Dmslc.for;

gfortran -c -w Jbslc.for;

gfortran -c GUP2.FOR;

gfortran -c -w Proc.FOR;

gfortran -c -w F2-gup2.for;

# In Andre's files, this is named 'F2-gup2.exe' rather than 'main'
# This command will link the object files into an executable
# Noting that the F2-gup2.FOR code is just a list of 'include' statements to this effect. 
gfortran F2-gup2.o -w -o main;

# Run executable
./main

# Log warnings and errors during compilation process in error.txt file
# NOT RUN INFINITE LOOP POSSIBLE
# ./gfortran_script 2> error.txt 

# Check if any error messages were logged during compilation
# grep 'Error' error.txt 

# gfortran COMMON.FOR MATRIX.FOR AW-RES7.FOR F2TST9.FOR AWEXTRD.FOR Dmslc.for Jbslc.for GUP2.FOR Proc.FOR F2-gup2.for -o main

# gfortran F2-gup2.for Proc.FOR GUP2.FOR Jbslc.for Dmslc.for AWEXTRD.FOR F2TST9.FOR AW-RES7.FOR AW-RES7.FOR MATRIX.FOR COMMON.FOR -o main

# gfortran -fbounds-check -w -o main COMMON.FOR MATRIX.FOR F2TST9.FOR AWEXTRD.FOR Dmslc.for Jbslc.for GUP2.FOR

exit;
