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

# source("./R/Table_Check.R")  # "WARNING! ./bash/runset_all.sh LATELY? THIS COULD TAKE A WHILE "
# Read 2012 depletion results and join into one data.frame with results from current trial batch
source("./R/Join_Depletion_Tables.R")  

#
# ggplotting -------------------------------------------------------------------
#
# Source R code from: https://gist.github.com/John-Brandon/484d152675507dd145fe
# Use package devtools to load plot theme code with `source_gist`
source_gist(id = "484d152675507dd145fe", filename = "mytheme_bw.R")  

plot_xy_depl = function(dat, xsla, ysla){
  # Make this more general by processing data given function arguments. 
  dat_plt = dat %>% select_("trial", "finalist", xsla, ysla) # , finalist, xsla, ysla)
  dat_labels = dat_plt %>% filter(finalist == TRUE)
  glimpse(dat_plt)  # Check
  glimpse(dat_labels)

  # Parameters for plotting
  repel_force = 1
  repel_nudge_x = -0.1
  repel_nudge_y = 0.1    
  if(xsla == "sla1_2012"){
    xlabel = "2012 SLA1"
    title_text = "5th Percentile Depletion vs 2012 SLA1"
  }else{
    xlabel = "2012 SLA2"
    title_text = "5th Percentile Depletion vs 2012 SLA2"
  }

  # Here we need to pass function argument variables into `ggplot`. 
  # Gets a little involved because ggplot apparently only sees global environment vars,
  .e <- environment()
  ggplot(data = dat_plt, aes(x = dat_plt[, xsla], 
                             y = dat_plt[, ysla],
                             labels = trial), 
                environment = .e) +
    geom_point(aes(fill = finalist),
               shape = 21,
               size = 4,
               alpha = 0.7) +
    scale_fill_manual(values = c("blue", "orange"),
                      name = "2012 IWC Evaluation Trials:",
                      labels = c("Preliminaries", "Reviewed in Detail")) +
    mytheme_bw +
    labs(x = xlabel, y = "Alternating Season SLA") +
    ggtitle("5th Percentile Depletion vs 2012 SLA2") +
    ggtitle(title_text) +
    theme(plot.title = element_text(hjust = 0.5)) +  # Center title
    geom_hline(yintercept = 0.6, lty = 2, alpha = 0.6) +
    geom_vline(xintercept = 0.6, lty = 2, alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.8) +
    coord_cartesian(xlim = c(0.2, 1), ylim = c(0.2, 1)) +
    geom_text_repel(data = dat_labels,
                    aes(x = dat_labels[, xsla], 
                        y = dat_labels[, ysla], 
                        label = trial),
                    environment = .e,
                    # Add extra padding around each text label.
                    box.padding = unit(0.5, 'lines'),
                    # Color of the line segments.
                    # segment.color = "darkgray",
                    segment.alpha = 0.6,
                    segment.color = "orange",
                    alpha = 0.8,
                    force = repel_force,
                    nudge_x = repel_nudge_x,
                    nudge_y = repel_nudge_y) +
    theme(legend.position = c(0.70, 0.25))  # , legend.title=element_blank()
}

# Compare two SLAs (SLA1 and SLA2) that were found to be acceptable during 2012 IWC IR
delta_low5_final %>% plot_xy_depl(dat = ., xsla = "sla1_2012", ysla = "sla2_2012")
ggsave(filename = "./figs/xy_depl_2012.png", dpi = 500)
system("open ./figs/xy_depl_a.png")

# Compare SLA1 from 2012 to Alternating Season SLA
delta_low5_final %>% plot_xy_depl(dat = ., xsla = "sla1_2012", ysla = "alt_sla")
ggsave(filename = "./figs/xy_depl_a.png", dpi = 500)
system("open ./figs/xy_depl_a.png")

# Compare SLA2 from 2012 to Alternating Season SLA
delta_low5_final %>% plot_xy_depl(dat = ., xsla = "sla2_2012", ysla = "alt_sla")
ggsave(filename = "./figs/xy_depl_b.png", dpi = 500)
system("open ./figs/xy_depl_b.png")


# Base plot --------------------------------------------------------------------
# base_plt_comp = function(xx, yy){
#   plot(xx, yy, type = "p", xlab = "2012 SLA1", ylab = "Alt Season SLA", main = "5th Percentile of Final Depletion", xlim = c(0,1), ylim = c(0,1), yaxs = 'i', xaxs = 'i')
#   abline(a = 0, b = 1, lty = 1)
#   abline(h = 0.6, lty = 2); abline(v = 0.6, lty = 2)  
# }
# x1 = delta_low5_final$sla1_2012
# y1 = delta_low5_final$alt_sla
# base_plt_comp(xx = x1, yy = y1)  # Base R plot for initial proto-type inspection