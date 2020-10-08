# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Jul. 6, 2020                
# Last revised: Oct. 7, 2020                 
# Project: UDP SGC        
# Subproject: Propensity Score Matching
# Re: ACS/Census Crosswalk (2000 - 2010)  
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script crosswalks ACS/Census data from 2000 boundaries to 2010 boundaries
# using LTBD (https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx).
# Crosswalks at the tract level.

# Inputs:
# ACS/Census data with 2000 boundaries
# LTBD crosswalk CSV (2000 - 2010)

# Outputs:
# ACS/Census data crosswalked to 2010 boundaries

# Update log: 
# 10/6/20 - updated import data section
# 10/7/20 - fixed typo in code

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)

# Directories: 
homedir <- "E:/udp_summer2020/"
workdir <- "sgc_data/raw/"
savedir <- "sgc_data/cleaned/"
setwd(homedir)

# Import data: 
# Census/ACS data
census_data <-
  read_csv(paste0(homedir, workdir, "acs_tract_09.csv")) %>%
  # rename Tract ID to trtid00
  rename(trtid00 = stctytrct)

# LTBD crosswalk data 
crosswalk_00_10 <-
  read_csv(paste0(homedir, workdir, "crosswalk_2000_2010.csv"))

# Parameters:
# FIPS filter from Census/ACS data
fips_filter <- paste(census_data$trtid00, collapse = "|")

# List of Census/ACS variables to crosswalk
crosswalk_vars <- c(names(census_data[9:length(census_data)]))

# Main Script -------------------------------------------------------------

# Filter crosswalk data by relevant FIPS (reduces interpolation time)
crosswalk_filter <-
  crosswalk_00_10 %>% 
  filter(str_detect(trtid00, fips_filter))

# Remove full crosswalk data
rm(crosswalk_00_10)

# Interpolate 2000 boundary data to 2010 tracts
census_data_xwalk <-
  census_data %>% 
  # join to filtered crosswalk
  full_join(crosswalk_filter, by = "trtid00") %>% 
  # change weight from character to numeric type
  mutate(weight = as.numeric(weight)) %>% 
  # multiply all data by its weight
  mutate(
    across(
      # select all Census/ACS data
      all_of(crosswalk_vars),
      # find value of data multiplied by weight
      ~ . * weight, 
      # attach name in the following pattern
      .names = "{col}_00"
    )
  ) %>% 
  # select for relevant variables (2010 tracts)
  select(trtid10, ends_with("_00")) %>% 
  # group by 2010 tracts
  group_by(trtid10) %>% 
  # sum up all variables
  summarise_all(sum) 

# remove original census data, filtered crosswalk data
rm(census_data)
rm(crosswalk_filter)

# Save Results ------------------------------------------------------------
write_csv(
  census_data_xwalk,
  path = paste0(homedir, savedir, "acs_tract_09_xwalk.csv")
)

# Clean up environment
rm(list = ls())

