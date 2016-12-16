# Makefile for MakahGW code libraries
#
#  Basic make command organization:
# targetfile: sourcefile
#		command
#
# Author: John Brandon
#
# NOTE: 
# F2TST9.FOR contains the main program, as well as several modules in the code file.
# Compiling this code file results in associated *.mod files for each module of code, including:
#  array_par.mod
#  decl_convrs.mod
#  decl_datvrs.mod
#  decl_pcfg.mod
#  decl_setup.mod
#  decl_stkvrs.mod
# These *.mod files are needed during linking / compilation of main executable, because the main program invokes them with 'use' statements. 
# 
# F2-GUP2.FOR -> F2-GUP.o -> main.app
# F2-GUP2.FOR is simply a list of these include statements
#      INCLUDE 'F2TST9.FOR'
#      INCLUDE 'AWEXTRD.FOR'
#      INCLUDE 'MATRIX.FOR'
#      INCLUDE 'COMMON.FOR'
#      INCLUDE 'GUP2.FOR'
#      INCLUDE 'DMSLC.FOR'
#      INCLUDE 'JBSLC.FOR'
# 

#VPATH = f90/code 
vpath %.for f90/code
vpath %.FOR f90/code
vpath %.o f90/code
vpath %.mod f90/code

# Set alias for code directory
code_dir := ./f90/code

# Copy executable main to Project folder (which contains input files for each trial)
f90/Project/main.app: $(code_dir)/main.app
	cp $(code_dir)/main.app f90/Project
	
# Why not just tell compiler to output executable in desired path ./f90/Project/main.app ?	
$(code_dir)/main.app: F2-gup2.o
	gfortran -fbounds-check $(code_dir)/F2-gup2.o -w -o $(code_dir)/main.app;	

# Main program (and modules) 	
# array_par.mod decl_convrs.mod decl_pcfg.mod decl_setup.mod decl_stkvrs.mod
F2-gup2.o: $(code_dir)/F2-gup2.FOR $(code_dir)/F2TST9.o 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2-gup2.FOR;

# Main program object file
$(code_dir)/F2TST9.o: $(code_dir)/F2TST9.FOR
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR;
	
# Check that compiled modules are up to date
# 1.
$(code_dir)/array_par.mod: $(code_dir)/F2TST9.o $(code_dir)/F2TST9.f90
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR

# 2.    
$(code_dir)/decl_convrs.mod: $(code_dir)/F2TST9.o $(code_dir)/F2TST9.f90
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR

# 3.    
$(code_dir)/decl_datvrs.mod: $(code_dir)/F2TST9.o $(code_dir)/F2TST9.f90
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR

# 4.    
$(code_dir)/decl_pcfg.mod: $(code_dir)/F2TST9.o $(code_dir)/F2TST9.f90
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR

# 5.                
$(code_dir)/decl_setup.mod: $(code_dir)/F2TST9.o $(code_dir)/F2TST9.f90
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR

# 6.
$(code_dir)/decl_stkvrs.mod: $(code_dir)/F2TST9.o $(code_dir)/F2TST9.f90
	cd $(code_dir);\
	gfortran -fbounds-check -c -w F2TST9.FOR 
		
# Delete files resulting from compiling
.PHONY: clean	
clean:
	cd $(code_dir);\
	rm *.o *.mod main.app	
	
