# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Aug. 15, 2020                
# Last revised: Oct. 8, 2020                 
# Project: UDP SGC     
# Subproject: Full Regressions
# Re: Clean and prepare ACS/Census data for full regressions
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script takes all raw (tract-level) ACS/Census data, crosswalks data with 
# 2000 boundaries, and produces a cleaned master dataset to use in the full
# regressions write-up.

# Inputs:
# Various years of ACS/Census data
# LTBD

# Outputs:
# Cleaned and crosswalked master ACS/Census datatable

# Update log: 
# 10/8/20 - converted from RMarkdown to R script for faster processing.

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
census_tract_00 <- read_csv(paste0(homedir, workdir, "census_tract_00.csv"))
acs_tract_09 <- read_csv(paste0(homedir, workdir, "acs_tract_09.csv"))
acs_tract_10 <- read_csv(paste0(homedir, workdir, "acs_tract_10.csv"))
acs_tract_13 <- read_csv(paste0(homedir, workdir, "acs_tract_13.csv"))
acs_tract_16 <- read_csv(paste0(homedir, workdir, "acs_tract_16.csv"))
acs_tract_18 <- read_csv(paste0(homedir, workdir, "acs_tract_18.csv"))

# LTBD crosswalking file
## Brown University Diversity and Disparities
## https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx
crosswalk_00_10 <- read_csv(paste0(homedir, workdir, "crosswalk_2000_2010.csv"))

# Parameters:
# Filter crosswalk to relevant tract IDs
# Create list of FIPS from ACS data to filter on
fips_00_filter <-
  paste(census_tract_00$stctytrct, collapse = "|")

fips_09_filter <-
  paste(acs_tract_09$stctytrct, collapse = "|")

# Main Script ------------------------------------------------------------------

# Read in data

# 2000 Census
## Can't use percentages, need to crosswalk first
census_tract_00 <- 
  census_tract_00 %>% 
  select(
    stctytrct,
    pop_tot,
    pop_whi_nonhis,
    pop_hisp,
    housing_tot,
    housing_occ,
    housing_o_rent,
    h_single,
    housing_vac,
    med_rent,
    pop_pov_tot,
    pop_pov_pov,
    pop_25over,
    pop_college
  ) %>% 
  rename_with(
    ~ paste0(., "_00"),
    .cols = pop_tot:pop_college
  )

# 2009 ACS
## Can't use percentages, need to crosswalk first
acs_tract_09 <- 
  acs_tract_09 %>% 
  select(
    stctytrct,
    pop_tot,
    pop_whi_nonhis,
    pop_hisp,
    housing_tot,
    housing_occ,
    housing_o_rent,
    h_single,
    housing_vac,
    med_rent,
    pop_pov_tot,
    pop_pov_pov,
    pop_25over,
    pop_college
  ) %>% 
  rename_with(
    ~ paste0(., "_09"),
    .cols = pop_tot:pop_college
  )

# 2010 ACS
acs_tract_10 <- 
  acs_tract_10 %>% 
  select(
    GEOID = stctytrct,
    pop_tot,
    pct_whi,
    pct_hisp,
    pct_h_rent,
    pct_h_single,
    vac_rate,
    med_rent,
    pov_rate,
    pct_college
  ) %>% 
  rename_with(
    ~ paste0(., "_10"),
    .cols = pop_tot:pct_college
  )

# 2013 ACS
acs_tract_13 <- 
  acs_tract_13 %>% 
  select(
    GEOID = stctytrct,
    pop_tot,
    pct_whi,
    pct_hisp,
    pct_h_rent,
    pct_h_single,
    vac_rate,
    med_rent,
    pov_rate,
    pct_college
  ) %>% 
  rename_with(
    ~ paste0(., "_13"),
    .cols = pop_tot:pct_college
  )

# 2016 ACS
acs_tract_16 <- 
  acs_tract_16 %>% 
  select(
    GEOID = stctytrct,
    pop_tot,
    pct_whi,
    pct_hisp,
    pct_h_rent,
    pct_h_single,
    vac_rate,
    med_rent,
    pov_rate,
    pct_college
  ) %>% 
  rename_with(
    ~ paste0(., "_16"),
    .cols = pop_tot:pct_college
  )

# 2018 ACS
acs_tract_18 <- 
  acs_tract_18 %>% 
  select(
    GEOID = stctytrct,
    pop_tot,
    pct_whi,
    pct_hisp,
    pct_h_rent,
    pct_h_single,
    vac_rate,
    med_rent,
    pov_rate,
    pct_college
  ) %>% 
  rename_with(
    ~ paste0(., "_18"),
    .cols = pop_tot:pct_college
  )

# Crosswalk 2000 to 2010 Census boundaries

# Filter crosswalk data by relevant FIPS
crosswalk_filter_00 <-
  crosswalk_00_10 %>% 
  filter(str_detect(trtid00, fips_00_filter))

crosswalk_filter_09 <-
  crosswalk_00_10 %>% 
  filter(str_detect(trtid00, fips_09_filter))

# Remove original crosswalk
rm(crosswalk_00_10)

# 2000 crosswalk
## Interpolate 2000 boundary data to 2010 tracts
census_00_xwalk <-
  census_tract_00 %>% 
  # join to filtered crosswalk
  full_join(crosswalk_filter_00, by = c("stctytrct" = "trtid00")) %>% 
  # change weight from character to numeric type
  mutate(weight = as.numeric(weight)) %>% 
  # multiply all data by its weight
  mutate(
    across(
      # select all Census data
      pop_tot_00:pop_college_00,
      # find value of data multiplied by weight
      ~ . * weight
    )
  ) %>% 
  # select for relevant variables (2010 tracts)
  select(trtid10, ends_with("_00")) %>% 
  # group by 2010 tracts
  group_by(trtid10) %>% 
  # sum up all variables
  summarise_all(sum) %>% 
  # rename trtid10 to stctytrct
  rename(GEOID = trtid10) %>% 
  # find percentages
  transmute(
    GEOID = GEOID,
    pop_tot_00 = pop_tot_00,
    pct_whi_00 = pop_whi_nonhis_00 / pop_tot_00,
    pct_hisp_00 = pop_hisp_00 / pop_tot_00,
    pct_h_rent_00 = housing_o_rent_00 / housing_occ_00,
    pct_h_single_00 = h_single_00 / housing_tot_00,
    vac_rate_00 = housing_vac_00 / housing_tot_00,
    med_rent_00 = med_rent_00,
    pov_rate_00 = pop_pov_pov_00 / pop_pov_tot_00,
    pct_college_00 = pop_college_00 / pop_25over_00
  )

# remove original 2000 data
rm(census_tract_00)

# 2009 crosswalk
## Interpolate 2000 boundary data to 2010 tracts
acs_09_xwalk <-
  acs_tract_09 %>% 
  # join to filtered crosswalk
  full_join(crosswalk_filter_09, by = c("stctytrct" = "trtid00")) %>% 
  # change weight from character to numeric type
  mutate(weight = as.numeric(weight)) %>% 
  # multiply all data by its weight
  mutate(
    across(
      # select all Census data
      pop_tot_09:pop_college_09,
      # find value of data multiplied by weight
      ~ . * weight
    )
  ) %>% 
  # select for relevant variables (2010 tracts)
  select(trtid10, ends_with("_09")) %>% 
  # group by 2010 tracts
  group_by(trtid10) %>% 
  # sum up all variables
  summarise_all(sum) %>% 
  # rename trtid10 to GEOID
  rename(GEOID = trtid10) %>% 
  # find percentages
  transmute(
    GEOID = GEOID,
    pop_tot_09 = pop_tot_09,
    pct_whi_09 = pop_whi_nonhis_09 / pop_tot_09,
    pct_hisp_09 = pop_hisp_09 / pop_tot_09,
    pct_h_rent_09 = housing_o_rent_09 / housing_occ_09,
    pct_h_single_09 = h_single_09 / housing_tot_09,
    vac_rate_09 = housing_vac_09 / housing_tot_09,
    med_rent_09 = med_rent_09,
    pov_rate_09 = pop_pov_pov_09 / pop_pov_tot_09,
    pct_college_09 = pop_college_09 / pop_25over_09
  )

# remove original 2009 data
rm(acs_tract_09)

# Combine Census and ACS data to single df, write to CSV
census_merged <-
  census_00_xwalk %>% 
  full_join(acs_09_xwalk, by = "GEOID") %>% 
  full_join(acs_tract_10, by = "GEOID") %>% 
  full_join(acs_tract_13, by = "GEOID") %>%
  full_join(acs_tract_16, by = "GEOID") %>%
  full_join(acs_tract_18, by = "GEOID") 

# remove old data
rm(
  census_00_xwalk, acs_09_xwalk, acs_tract_10, acs_tract_13, 
  acs_tract_16, acs_tract_18, crosswalk_filter_00, crosswalk_filter_09
)

# Save Results ------------------------------------------------------------
# write merged census data
write_csv(census_merged, paste0(homedir, savedir, "all_years_tract_CA.csv"))

# Clean up environment
rm(list = ls())
