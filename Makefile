# Makefile for MakahGW code libraries
#
#  Basic make command organization:
# targetfile: sourcefile
#		command
#
# Author: John Brandon
# Language: GNU Make 3.81
# Shell: GNU Bash 3.2.57(1)
# OS: Mac OS 10.11.6
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
# DEPRICATED IN FAVOR OF "MODULAR" APPROACH (see ~/MakahGW/README.md)
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
# ---------------------------------------------------------------------------------------

# Set alias for code directory ----------------------------------------------------------
code_dir := ./f90/code
PROJ_DIR := ./f90/Project

# Define variables ----------------------------------------------------------------------
FORTRAN_COMPILER = gfortran
GFFLAGS = -fbounds-check -w -03 # Check array dimensions, do not print compile warnings, max runtime speed
OBJECT_FILES := F2TST9.o AWEXTRD.o MATRIX.o COMMON.o DMSLC.o JBSLC.o GUP2.o 
OBJECT_FILES := $(addprefix $(code_dir)/,$(OBJECT_FILES)) 

# Define search paths -------------------------------------------------------------------
#VPATH = f90/code 
vpath %.for f90/code
vpath %.FOR f90/code
vpath %.o f90/code
vpath %.mod f90/code

# Compile executable using the INCLUDE APPROACH -----------------------------------------
# NOTE: Executable file is written to f90/Project directory		
# $(code_dir)/main.app: F2-gup2.o 
# 	gfortran -fbounds-check $(code_dir)/F2-gup2.o -w -o $(PROJ_DIR)/main.app;	
# 
# Main program (will also output these module files) 	 	
# array_par.mod decl_convrs.mod decl_pcfg.mod decl_setup.mod decl_stkvrs.mod
# F2-gup2.o: $(code_dir)/F2-gup2.FOR $(code_dir)/F2TST9.o 
# 	cd $(code_dir);\
# 	gfortran -fbounds-check -c -w F2-gup2.FOR;

# Compile executable using MODULAR APPROACH ----------------------------------------------
# NOTE: Executable file is written to f90/Project directory		
$(PROJ_DIR)/main.app: F2TST9.o AWEXTRD.o MATRIX.o COMMON.o DMSLC.o JBSLC.o GUP2.o
	gfortran -fbounds-check $(OBJECT_FILES) -w -o $(PROJ_DIR)/main.app;			

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
		
# Compile object files for remaining code files -------------------------		
AWEXTRD.o: $(code_dir)/AWEXTRD.FOR 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w AWEXTRD.FOR;
	
MATRIX.o: $(code_dir)/MATRIX.FOR 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w MATRIX.FOR;	

COMMON.o: $(code_dir)/COMMON.FOR 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w COMMON.FOR;	
	
DMSLC.o: $(code_dir)/DMSLC.FOR 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w DMSLC.FOR;	

JBSLC.o: $(code_dir)/JBSLC.FOR 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w JBSLC.FOR;
		 
GUP2.o: $(code_dir)/GUP2.FOR 
	cd $(code_dir);\
	gfortran -fbounds-check -c -w GUP2.FOR;
		
# Delete files resulting from compiling
.PHONY: clean	
clean:
	rm $(OBJECT_FILES) 
	rm $(PROJ_DIR)/main.app
	
