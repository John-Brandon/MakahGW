# Table_Check.R
# John Brandon
#
# Test output from Andre's code against results reported from  
#  IWC Implementation Review: 
#  J Cet Res Manage 14 Suppl p 167 Table 1 Appendix 4
#

#
# Preliminaries ----------------------------------------------------------------
#
library(tidyverse)  # For data wrangling 
library(stringr)    # 
rm(list = ls())     # Clear workspace environment

# Compile Fortran program if that code has been modified
syserr = system("make")
ifelse(syserr == 0, "All good", stop("Exit non-zero! Error!"))

# Load helper functions 'Process' and 'AWExtract'
source(file = "./R/AWTables.R")

#
# Read results from 2012 and select only "SLA 1" (Dec - Apr hunt) --------------
#
#  Also only select 'final' depleption category (else repetition in trial list)
results_2012 = read_csv(file = "./tables/Table_A1_JCRM_2013.csv") %>%
  filter(sla == 1, depl_cat == "final")

# Select vector with trials in 2012 table, and remove leading "G"
trials_2012 = results_2012$trial %>%
  substr(start = 2, stop = 5)

#
# Create list of comparison trials for Bash script and write script ------------
#
ntrials = length(trials_2012); ntrials
cmd1 = rep("source run.sh", ntrials)
sla = rep("1", ntrials)
cmdlines = paste(cmd1, trials_2012, sla)

# Add hash-bang to top of *.sh shell file for Bash scripting
write_lines(x = "#!/bin/bash", path = "./bash/runset_all.sh")

# Write list of commands to Bash script
write_lines(x = cmdlines, path = "./bash/runset_all.sh", append = TRUE)
system("open ./bash/runset_all.sh")
#
# Run all the trials -----------------------------------------------------------
#
# WARNING: THIS WILL TAKE A WHILE. HERE IS TIMING FROM 2016 MACBOOK PRO
# real	47m40.859s
# user	40m21.197s
# sys	0m49.404s

# Clear old output file
system(command = "rm ./f90/Project/AW-ALL.OUT")

# Change mode to executable for shell script we just wrote and run it
system(command = "chmod a+x ./bash/runset_all.sh")
system(command = "cd ./bash; ./runset_all.sh")

# Copy output from this batch to an intermediate file for checking and debugging
system(command = "cp ./f90/Project/AW-ALL.OUT ./f90/Project/AW-ALL-CHECK-AGAINST-JCRM-2013.OUT")

# Introduce leading letter "G" in vector of trial names
trials_check = trials_2012 %>%  vapply(., function(x) paste0("G", x),
                        FUN.VALUE="character", USE.NAMES = F)

# AWExtract distills output and writes tables to file in out.path directory
dat.path = "./f90/Project/AW-ALL.OUT"
out.path = "./f90/Project/out.csv"
AWExtract(trials_check, dat.path, out.path, SLAs=c(1))

# Check
system(command = "open ./f90/Project/out.csv")

# Read output file
# Skip first two rows of file (headers are inconvinient)
results_check = read_csv(file = out.path, skip = 2, col_names = F)

# Extract relevant columns (for 2012 SLA 1, see Table 5 Appendix 2 JCRM Suppl 2013)
# Column: 1 for trial ID; 
#         2 for lower 5th final depletion; 
#         3 for median final depl;
#         8 for lower 5th rescaled depletion (zero catches); 
#         9 for median rescaled depletion;
#         24 for median annual landings; 
#         27 for median struck and lost;
#         30 for median PCFG killed.
results_check = results_check %>% select(1, 2, 3, 8, 9, 24, 30)

# Edit trial names (simplify, no SLA)
results_check[,1] %<>% unlist(use.names = FALSE) %>% substr(start = 1, stop = 5)
names(results_check) = c("trial", "low5th_final", "median_final", "low5th_rescale", "median_rescale", "median_landing","median_PCFGkilled")

# Write `results_2012` and `results_check` to RData file
save(results_2012, results_check, file = "./R/output_tables.RData")

# Output table for text
glimpse(results_check)
write.csv(file = "./tables/Stats.csv", x = results_check, row.names = F)

# 
# Code below was used to check code from Andre against accepted numbers. Found identical.
# 
# table_sla1 = results_2012 %>% filter(sla == 1)
# table_sla1_final = table_sla1 %>% filter(depl_cat == "final")
# table_sla1_rescaled = table_sla1 %>% filter(depl_cat == "rescaled")
# 
# check_sum = function(x1, x2){
#   # take element-wise difference between two vectors and return sum of squares
#   x3 = NULL
#   x3 = x2 - x1
#   x3 = x3 ^ 2
#   ss_x3 = sum(x3)
#   if(ss_x3 == 0){
#     return(ss_x3)
#   } else {
#     stop("WARNING: Sum of Squares not equal to zero")
#   }
# }
# 
# # If results from Andre's code and 2012 runs are the same,
# #  all of these check_sums should be equal to zero.
# check_sum(table_sla1_final$low5th, results_check$low5th_final)
# check_sum(table_sla1_final$median, results_check$median_final)
# 
# check_sum(table_sla1_rescaled$low5th, results_check$low5th_rescale)
# check_sum(table_sla1_rescaled$median, results_check$median_rescale)
# 
