#!/usr/bin RScript
# 
# DataGen_NPCFG.R
# John R. Brandon, PhD
#
# Changes the input *.DDD files for each trial.
# NOAA's alternatives differ from the IWC in that N_MIN is calculated 
#   based on the abundance estimate for the full putative range of the PCFG.
# The IWC IR assumed that N_MIN would be calculated from the abundance estimates
#   corresponding to the OR-SVI sub-area of that putative range. 
# The relevant parameters are on lines 74 and 75 of the input *.DDD file for
#   each trial. The *.DDD files are in: ./f90/Project/Runstreams/*.DDD
# These parameters are described in JCRM 2013 (Suppl). Annex E. Appendix 2. Eqn (B1.1).
# They are implimented in the operating model code in the SUBROUTINE DATGEN of
#   file ./f90/code/F2TST9.FOR
# Note: the Fortran program reads the input file for each trial from file `copy.dat`.
#   It is the shell script ./bash/run.sh that copies individual trial *.DDD files 
#   to the copy.dat file before each trial is run in turn.
# The original ./f90/Project/Runstreams were moved into ./f90/Project/Runstreams_ORSVI_NPCFG

library(tidyverse)  # For data wrangling 
library(readr)      # File IO
library(stringr)    # String manipulation

rm(list = ls())     # Clear workspace environment

# Create vector of input file names
base_dir = "./f90/Project/Runstreams"
file_names = list.files(path = base_dir, pattern = "*.DDD")  # Check

# Need to be careful here not to over-write any bias trials.
# So run a preliminary check before changing any values.
# The relevant parameters are on lines 74 and 75 of the input *.DDD file for each trial.

# Create an empty list, which ultimately will have the file contents as character vectors in each element
file_list = vector(mode = "list", length = length(file_names))
next_file = paste(base_dir, file_names, sep = "/")

# Populate list with text from each input file with OR-SVI bias parameter (for generating abundance estimate)
for (i in seq_along(file_names)){
  file_list[[i]] = read_lines(file = next_file[i])
}  
lapply(X = file_list, FUN = "[", 74:75)  # Check

# Function to re-write the two relevant lines of text in input files
change_bias <- function(input_lines) {
  substr(x = input_lines[74], start = nchar(input_lines[74]) - 4, stop = nchar(input_lines[74])) = "1.000"  # 0.744
  substr(x = input_lines[75], start = nchar(input_lines[75]) - 4, stop = nchar(input_lines[75])) = "0.000"        # 0.110
  input_lines
}

# Revise bias parameter to generate PCFG abundance estimates from OR-SVI to PacNW
updated_files = lapply(X = file_list, FUN = change_bias)   
lapply(X = updated_files, FUN = "[", 74:75)  # Check

# Re-write all of the input files with revised bias parameter
for (i in seq_along(file_names)){
  write_lines(x = updated_files[[i]], path = next_file[i])
} 
