# Test output from Andre's code against results reported from  
#  IWC Implementation Review: 
#  J Cet Res Manage 14 Suppl p 167 Table 1 Appendix 4
#
library(tidyverse)

# Read results from 2012 and select only "SLA 1" (Dec - Apr hunt)
#  Also only select 'final' depleption category (else repetition in trial list)
results_2012 = read_csv(file = "./tables/Table_A1_JCRM_2013.csv") %>% 
  filter(sla == 1, depl_cat == "final")

# Select vector with trials in 2012 table, and remove leading "G"
trials_2012 = results_2012$trial %>% 
  substr(start = 2, stop = 5)

# Create list for Bash script 
ntrials = length(trials_2012); ntrials
cmd1 = rep("source run.sh", ntrials)
sla = rep("1", ntrials)
cmdlines = paste(cmd1, trials_2012, sla)

# Write list of commands to Bash script
write_lines(x = cmdlines, path = "./bash/runset_check.sh")

# Clear old output file
system(command = "rm ./f90/Project/AW-ALL.OUT")

# Run all the trials
# WARNING: THIS WILL TAKE AWHILE. HERE IS TIMING FROM 2016 MACBOOK PRO 
# real	47m40.859s
# user	40m21.197s
# sys	0m49.404s
system(command = "chmod a+x ./bash/runset_check.sh")
system(command = "cd ./bash; ./runset_check.sh")

# Replace leading letter "G" in vector of trial names
trials_check = trials_2012 %>%  vapply(., function(x) paste0("G", x), 
                        FUN.VALUE="character", USE.NAMES = F)
trials_check
# Assuming functions AWExtract() and Process() are loaded --
#  defined in file AWTABLES_.R
dat.path = "./f90/Project/AW-ALL.OUT"
out.path = "./f90/Project/out.csv"
AWExtract(trials_check, dat.path, out.path, SLAs=c(1))

system(command = "open ./f90/Project/out.csv")
