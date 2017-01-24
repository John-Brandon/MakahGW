# Join_Depletion_Tables.R
# John Brandon
#
#   Combine depletion results for Alternating Season SLA with those from 2012 into a 
# single data.frame for plotting comparisons (eg ./R/Fig_compare_depl.R)
# 
#   Assumes that all trials have been run by sourcing `Table_Check.R`, which
# process and writes to results `ouput_tables.RData`. The Alternating Season SLA
# results are loaded from that data set. 

library(devtools)  # For sourcing snippet of code with JB's ggplot custom theme 
library(dplyr)     # Wrangling data
library(ggrepel)   # Prettify text labels in ggplot
library(tidyverse) 

rm(list = ls())    # Clear workspace

# 
# Read results from 2012 SLA1 (and 2) and Alternating Season SLA ---------------
#
# JCRM 2013 Suppl Annex E Appendix 4 Table 1
sla_2012 = read_csv(file = "./tables/Table_A1_JCRM_2013.csv")  # SLA 1 and 2 (all strikes in May)

# Load results_check from Alternating Season SLA batch of evaluation trials
load(file = "./R/output_tables.RDATA")  

# 
# Massage data.frames before doing joining tables ------------------------------
#
# Wrangle current output to have same structure and names as 2012 for joining
make_factor_column = function(text, nrows){
  # Create columns with variables for transforming from wide to long data.frame
  rep(text, nrows)
}

# column_names = c("alt_sla", "final", "rescaled")  # Could use purrr functional approach
sla_check = make_factor_column(text = "alt_sla", nrows = nrow(results_check))
final_depl = make_factor_column(text = "final", nrows = nrow(results_check))
rescaled_depl = make_factor_column(text = "rescaled", nrows = nrow(results_check))

# More wrangling of data to get from wide to long
results_check = cbind(results_check, sla_check, final_depl, rescaled_depl)
glimpse(results_check)  # Check

low5_final_check = results_check %>% 
  select(trial, sla_check, final_depl, low5_final_depl, median_final_depl, median_landed)
low5_relative_check = results_check %>% 
  select(trial, sla_check, rescaled_depl, low5_rescaled_depl, median_rescaled_depl, median_landed)

# rename column headers to be consistent across data.frames and bind together data frames
names(low5_final_check) = c("trial", "sla", "depl_cat", "depl_5th_ptile", "depl_median","landing_median")
names(low5_relative_check) = c("trial", "sla", "depl_cat", "depl_5th_ptile", "depl_median","landing_median")

# Create long data.frame from alt_sla results
results_check = rbind(low5_final_check, low5_relative_check)  

# rename SLAs from 2012
sla_2012$sla = ifelse(sla_2012$sla == "1", "sla1_2012", "sla2_2012")

# Extract trials marked for examination during 2012
trials_marked = sla_2012 %>% filter(sla == "sla1_2012", depl_cat == "final") %>% 
  select(examine, finalist)

# Should be same columns and structure as sla_2012 data.frame now
results_check = cbind(results_check, trials_marked)

# Join 2012 and current results into single data.frame
sla_all = rbind(sla_2012, results_check)

# Translate NA entries to FALSE
sla_all$examine = ifelse(is.na(sla_all$examine), FALSE, TRUE)
sla_all$finalist = ifelse(is.na(sla_all$finalist), FALSE, TRUE) 

# Strip the leading "G" (redundant) off trial ID
sla_all$trial = sla_all$trial %>% 
  sub(x = ., pattern = "G", replace = "", fixed = TRUE)

#
# List trials that were reviewed in detail during 2012 IR ----------------------
# 
# List trials in Appendix 4 Table 1 that were marked for detailed examination.
# Note some trials were marked for detailed examination but had depl > 0.6.
# That examination identified a subset of trials, the finalists, for review.
# The finalist are listed in JCRM Annex E Table 5 p142 as: 
#
# annex_table5 = c("GP01C", "GP02B", "GP02C", "GP08B", "GP09B", "GP10A", "GP10B", "GP13C",
#                  "GB01C", "GB02B",          "GB08B", "GB09B", "GB10A", "GB10B"         ,
#                  "GI01C", "GI02B"                                                       )
# appndx4_marked = sla1_2012 %>% 
#   filter(examine == TRUE, depl_cat == "final") %>%  # depl_cat "final" avoids duplicates
#   select(trial) 
# 
# appndx4_marked %>% unique %>% nrow  # 19 trials intially marked for detailed examination.
# length(annex_table5)  # 16 trials were the 'finalists', some shuffling happened.
# setdiff(annex_table5, appndx4_marked$trial)  # "GPO2C" included in finalists
# setdiff(appndx4_marked$trial, annex_table5)  # "GB08A" "GB09A" "GP08A" "GP09A" excluded.
# intersect(annex_table5, appndx4_marked$trial) 


#
# prepare data.frame for ggplotting --------------------------------------------
#
# Go from long to wide for ggplotting
# This is the initial data.frame for testing plotting below
# delta_low5_final = sla_all %>% 
#   select(trial, sla, finalist, depl_cat, depl_5th_ptile) %>% 
#   filter(sla != "sla2_2012", depl_cat == "final")
delta_low5_final = 
  sla_all %>% 
  select(trial, sla, finalist, depl_cat, depl_5th_ptile) %>% 
  filter(depl_cat == "final") %>% 
  spread(key = sla, value = depl_5th_ptile) 

glimpse(delta_low5_final)



