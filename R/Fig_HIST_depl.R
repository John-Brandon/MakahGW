# Fig_HIST_depl.R
# John Brandon
#
# Plot histograms showing the difference between 5th percentile of depletion. 
# Overlap histograms on same plot to compare multiple pairs of SLAs.

library(tidyverse)  # ggplot, dplyr (for data wrangling and pipes %>%, etc.) 
library(devtools)   # For sourcing snippet of code with JB's ggplot custom theme 

rm(list = ls())     # Clear workspace

# source("./R/Table_Check.R")  # "WARNING! ./bash/runset_all.sh LATELY? THIS COULD TAKE A WHILE "
# Read 2012 depletion results and join into one data.frame with results from current trial batch
source("./R/Join_Depletion_Tables.R")  

#
# ggplotting -------------------------------------------------------------------
#
# Source R code from: https://gist.github.com/John-Brandon/484d152675507dd145fe
# Use package devtools to load plot theme code with `source_gist`
source_gist(id = "484d152675507dd145fe", filename = "mytheme_bw.R") 

# To create histograms, it easier to start with results from the data.frame
# that is in long format. That data.frame is `sla_all`, created by `Join_Depletion_Tables.R`.
glimpse(sla_all)  # Check

#
# Change names of SLA factor levels for plotting facet labels ------------------
#
sla_all$sla = factor(sla_all$sla)
levels(sla_all$sla)[levels(sla_all$sla)=="alt_sla"] <- "Alt Season"
levels(sla_all$sla)[levels(sla_all$sla)=="sla1_2012"] <- "SLA1"
levels(sla_all$sla)[levels(sla_all$sla)=="sla2_2012"] <- "SLA2"

#
# Plot frequency polygon of median PCFG annual landings using facet ------------
#
# A frequency polygon shows the same binned data as hist, but with lines instead of bars. 
sla_all %>% filter(depl_cat == "final") %>% 
  ggplot(data = ., aes(x = landing_median, color = sla)) + # fill = finalist 
  geom_freqpoly(binwidth = 0.2) +
  labs(x = "Median Annual Landings", y = "Number of Trials") +
  mytheme_bw + 
  guides(color = FALSE) +
  facet_grid(sla ~ .)

ggsave(filename = "./figs/freqpoly_PCFG_landings.png", dpi = 500)

#
# Developmental function below for overlapping histograms of depletion. Work in progress. 
#
# Could pass the long data.frame (with final and rescaled depletion) into the
# function for ggplotting histograms, then do `filter` and `select` within 
# function, using .e = environment() approach (see Fig_XY_depl.R for example).
hist_depl = function(dat, sla_x1 = "alt_sla", sla_x2, deplcat){
  # sla_x1 and sla_x2 are the SLAs to be compared, ie one histogram for each (overlapping)
  # deplcat is the depletion category: "final" or "rescaled"
  sla_hist = dat %>% filter(depl_cat == deplcat, sla %in% c(sla_x1, sla_x2)) 
  delta = filter(sla_hist, sla == sla_x1)$depl_5th_ptile - filter(sla_hist, sla == sla_x2)$depl_5th_ptile
  sla_hist = cbind(sla_hist, delta)  # Add column with difference

  # Write to console for checking
  glimpse(sla_hist)  
  cat("Delta vector = ", delta, "\n", "Elements in Delta", "\n", length(delta), "\n")
  cat("Median Delta = ", median(sla_hist$delta), "\n")
  
  # Plotting paramters
  sla1_color = "green"    # Alternatively "#a6cee3"
  sla2_color = "blue"     # Alternatively "#33a02c"
  altsla_color = "black"  # Alternatively "#1f78b4"

  ggplot(data = dat, aes(x = depl_5th_ptile, fill = sla, color = sla)) + 
    # geom_histogram(position = "identity", alpha = 0.75, binwidth = 0.025) +
    geom_freqpoly(binwidth = 0.125) +
    geom_vline(xintercept = 0.6, lty = 2, alpha = 0.6) +
    mytheme_bw +
    labs(x = "Difference in Depletion", y = "Number of Trials") +
    coord_cartesian(xlim = c(-0.25, 0.25))
    # scale_fill_manual(values = c(sla1_color, sla2_color, altsla_color))
    
}

# Write plot files to disk 
hist_depl(dat = sla_all, sla_x1 = "alt_sla", sla_x2 = "sla1_2012", deplcat = "final")
# system("open ./figs/hist_depl_a.png")
ggsave(filename = "./figs/hist_depl_a.png", dpi = 500)

hist_depl(dat = sla_all, sla_x1 = "alt_sla", sla_x2 = "sla2_2012", deplcat = "final")
# system("open ./figs/hist_depl_b.png")
ggsave(filename = "./figs/hist_depl_b.png", dpi = 500)
