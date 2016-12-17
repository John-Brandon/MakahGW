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
# Compiling this code file results in associated *.mod files, including:
#  array_par.mod
#  decl_convrs.mod
#  decl_datvrs.mod
#  decl_pcfg.mod
#  decl_setup.mod
#  decl_stkvrs.mod
# These are needed during linking / compilation of main executable. The main program invokes them with 'use' statements. 
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
CODE_DIR := ./f90/code
PROJ_DIR := ./f90/Project

# Define variables ----------------------------------------------------------------------
FORTRAN_COMPILER = gfortran
GFFLAGS = -fbounds-check -w
OBJECT_FILES := F2TST9.o MATRIX.o COMMON.o DMSLC.o JBSLC.o GUP2.o AWEXTRD.o
OBJECT_FILES := $(addprefix $(CODE_DIR)/,$(OBJECT_FILES))

# Define search paths -------------------------------------------------------------------
VPATH = ./f90/code/
# vpath %.for ./f90/code
# vpath %.FOR ./f90/code
# vpath %.o ./f90/code
# vpath %.mod ./f90/code

# Compile executable using the INCLUDE APPROACH -----------------------------------------
# NOTE: Executable file is written to f90/Project directory		
# $(CODE_DIR)/main.app: F2-gup2.o 
# 	gfortran -fbounds-check $(CODE_DIR)/F2-gup2.o -w -o $(PROJ_DIR)/main.app;	
# 
# Main program (will also output these module files) 	 	
# array_par.mod decl_convrs.mod decl_pcfg.mod decl_setup.mod decl_stkvrs.mod
# F2-gup2.o: $(CODE_DIR)/F2-gup2.FOR $(CODE_DIR)/F2TST9.o 
# 	cd $(CODE_DIR);\
# 	gfortran -fbounds-check -c -w F2-gup2.FOR;

# Compile executable using MODULAR APPROACH ----------------------------------------------
# NOTE: Executable file is written to f90/Project directory		
# -O3 flag for max run time speed
$(PROJ_DIR)/main.app: $(OBJECT_FILES)
	gfortran $(GFFLAGS) $(OBJECT_FILES) -O3 -o $(PROJ_DIR)/main.app;

# Main program object file
$(CODE_DIR)/F2TST9.o: $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran -fbounds-check -c -w F2TST9.FOR;

# Check that compiled modules are up to date
# 1.
$(CODE_DIR)/array_par.mod: $(CODE_DIR)/F2TST9.o $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c F2TST9.FOR

# 2.    
$(CODE_DIR)/decl_convrs.mod: $(CODE_DIR)/F2TST9.o $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c F2TST9.FOR

# 3.    
$(CODE_DIR)/decl_datvrs.mod: $(CODE_DIR)/F2TST9.o $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c F2TST9.FOR

# 4.    
$(CODE_DIR)/decl_pcfg.mod: $(CODE_DIR)/F2TST9.o $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c F2TST9.FOR

# 5.                
$(CODE_DIR)/decl_setup.mod: $(CODE_DIR)/F2TST9.o $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c F2TST9.FOR

# 6.
$(CODE_DIR)/decl_stkvrs.mod: $(CODE_DIR)/F2TST9.o $(CODE_DIR)/F2TST9.FOR
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c F2TST9.FOR 
		
# Compile object files for remaining code files -----------------------------------------		
$(CODE_DIR)/AWEXTRD.o: $(CODE_DIR)/AWEXTRD.FOR $(CODE_DIR)/F2TST9.o
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c AWEXTRD.FOR;
	
$(CODE_DIR)/MATRIX.o: $(CODE_DIR)/MATRIX.FOR 
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c MATRIX.FOR;	

$(CODE_DIR)/COMMON.o: $(CODE_DIR)/COMMON.FOR 
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c COMMON.FOR;	
	
$(CODE_DIR)/DMSLC.o: $(CODE_DIR)/DMSLC.FOR 
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c DMSLC.FOR;	

$(CODE_DIR)/JBSLC.o: $(CODE_DIR)/JBSLC.FOR 
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c JBSLC.FOR;
		 
$(CODE_DIR)/GUP2.o: $(CODE_DIR)/GUP2.FOR 
	cd $(CODE_DIR);\
	gfortran $(GFFLAGS) -c GUP2.FOR;
		
# Delete files resulting from compiling -------------------------------------------------
.PHONY: clean	
clean:
	rm $(OBJECT_FILES)
	rm $(CODE_DIR)/*.mod 
	rm $(PROJ_DIR)/main.app
	
