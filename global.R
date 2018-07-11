#Author: Giuseppe Angele'

rm(list = ls())
### Pepsimulator
### Global options and packages

options(scipen = 999)
options(stringsAsFactors = F)
os_folder_struc <- if(Sys.info()["sysname"]=="Darwin"){"/Volumes/OMDINT/"} else {"S:/"}
#setwd(paste0(os_folder_struc,"OMD Strategy Team/Advanced Analytics"))
library(tidyverse)
#library(shinysky)
library(rhandsontable)
library(plotly)
library(data.table)


# ### Inputs - admin
# 
# exc_var_mass <- 0.01        # side by side boxes 'excability variance'
# exc_var_pers <- 0.01
# exc_var_both <- 0.01
# exc_var_none <- 0.01


#calibration <- 1        # input box


freq_num <- 1:4
freq_cum_exc <- c(0.8, 0.95, 0.99, 1)
freq_dist <- data.frame(freq_num, freq_cum_exc) 
rhandsontable(freq_dist, width = 550, height = 300)
