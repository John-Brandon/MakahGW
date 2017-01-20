# Fig_compare_depl.R
# John Brandon
#
# Compare results from alternating season SLA with those from 2012
# 
# Plot function assumes all evaluation trials have been run by sourcing `Table_Check.R`, 
#  which
# process and writes to results `ouput_tables.RData` 
# 

library(devtools)  # For sourcing snippet of code with JB's ggplot custom theme 
library(dplyr)     # Wrangling data
library(ggrepel)   # Prettify text labels in ggplot
library(tidyverse) # ggplot2 + most anything else that might be needed in the near future

rm(list = ls())    # Clear workspace

source("./R/Table_Check.R")  # "WARNING! ./bash/runset_all.sh LATELY? THIS COULD TAKE A WHILE "
# Read 2012 depletion results and join into one data.frame with results from current trial batch
source("./R/Join_Depletion_Tables.R")  

#
# ggplotting -------------------------------------------------------------------
#
# Source R code from: https://gist.github.com/John-Brandon/484d152675507dd145fe
# Use package devtools to load plot theme code with `source_gist`
source_gist(id = "484d152675507dd145fe", filename = "mytheme_bw.R")  

# Parameters for plotting
repel_force = 1
repel_nudge_x = -0.1
repel_nudge_y = 0.1  
  
# To run multi-line in RStudio, align cursor on next line and press Ctrl + Enter (Mac OS)
ggplot(data = delta_low5_final, aes(x = sla1_2012, y = alt_sla, labels = trial)) +
  geom_point(aes(fill = finalist), shape = 21, size = 4, alpha = 0.7) + 
  scale_fill_manual(values = c("blue", "orange"),
                    labels = c("2012 Trials", "AWMP Detailed Review")) +
  mytheme_bw + 
  labs(x ="2012 SLA1", y = "Alternating Season SLA") +
  ggtitle("5th Percentile Depletion vs 2012 SLA1") +
  theme(plot.title = element_text(hjust = 0.5)) +  # Center title
  geom_hline(yintercept = 0.6, lty = 2, alpha = 0.6) +
  geom_vline(xintercept = 0.6, lty = 2, alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.8) + 
  coord_cartesian(xlim = c(0.2, 1), ylim = c(0.2, 1)) +
  geom_text_repel(aes(label = trial), 
                  data = subset(delta_low5_final, finalist == TRUE), 
                  force = repel_force,
                  nudge_x = repel_nudge_x,
                  nudge_y = repel_nudge_y) +
  theme(legend.position = c(0.65, 0.20),
        legend.title=element_blank())  
  
  
delta_low5_final %>% arrange()


# Base plot --------------------------------------------------------------------
# base_plt_comp = function(xx, yy){
#   plot(xx, yy, type = "p", xlab = "2012 SLA1", ylab = "Alt Season SLA", main = "5th Percentile of Final Depletion", xlim = c(0,1), ylim = c(0,1), yaxs = 'i', xaxs = 'i')
#   abline(a = 0, b = 1, lty = 1)
#   abline(h = 0.6, lty = 2); abline(v = 0.6, lty = 2)  
# }
# x1 = delta_low5_final$sla1_2012
# y1 = delta_low5_final$alt_sla
# base_plt_comp(xx = x1, yy = y1)  # Base R plot for initial proto-type inspection