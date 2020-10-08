# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Jun. 29, 2020                
# Last revised: Oct. 7, 2020                 
# Project: UDP SGC     
# Subproject: Propensity Score Matching
# Re: Match investment neighborhoods with ACS variables
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script takes matched investment/neighborhood data (from invest_nbr_match.R)
# and appends ACS data from 2009 and 2018. 2009 ACS data was crosswalked to 2010
# boundaries in acs_crosswalk.R

# Inputs:
# Investments matched with Census tracts
# 2009 (crosswalked) and 2018 ACS data

# Outputs:
# Table of matched investments and corresponding ACS measures

# Update log: 
# 10/6/20 - converted from RMarkdown to R script for faster processing.

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)
library(glue)
library(tidycensus)
library(tigris)
library(sf)
options(tigris_use_cache = TRUE)

# Directories: 
homedir <- "E:/udp_summer2020/"
workdir <- "sgc_data/raw/"
savedir <- "sgc_data/cleaned/"
setwd(homedir)

# Import data:
# Matched investment tracts + ACS data
investment_tract <- read_csv(paste0(homedir, savedir, "investment_tract.csv"))

# Crosswalked 2009 ACS data
acs_tract_09_xwalk <- 
  read_csv(paste0(homedir, savedir, "acs_tract_09_xwalk.csv"))

# 2018 ACS data
acs_tract_18 <- read_csv(paste0(homedir, workdir, "acs_tract_18.csv"))

# Parameters:
# Bay Area counties
sf_bay_area <- 
  c(
    "Alameda", "Contra Costa", "Marin", "Napa", "Sacramento", "San Francisco",
    "San Joaquin", "San Mateo", "Santa Clara", "Santa Cruz", "Solano", "Yolo"
  )

# Census boundary year
census_year <- 2010

# Main Script ------------------------------------------------------------------

# Read in all study area tracts (LA, Fresno, SF Bay)
# Use 2010 Census for comparison
study_tracts <-
  # study area depends on year
  tracts(
    state = "CA",
    county = c("Los Angeles", "Fresno", sf_bay_area),
    year = census_year
  ) %>% 
  st_as_sf() %>% 
  select(GEOID = GEOID10, geometry)

# Join investment and study tracts
study_invest_join <-
  study_tracts %>% 
  left_join(investment_tract, by = "GEOID") %>% 
  #turn into tibble for easier manipulation
  tibble() %>% 
  mutate(row_id = row_number())

# Drop study tracts
rm(study_tracts)

# Generate list for investment flag
invest_true <-
  study_invest_join %>%
  select(row_id, contains("investment")) %>% 
  filter(!is.na(investment_1))

# include investment flag, intervention flag, 
# and investment count to create master_df
master_df <-
  study_invest_join %>% 
  mutate(
    # create investment flag
    investment = 
      if_else(
        study_invest_join$row_id %in% invest_true$row_id,
        1,
        0
      ),
    # create intervention type flags
    # count # of investments
    n_invest = 
      case_when(
        !is.na(investment_7) ~ 7,
        !is.na(investment_6) ~ 6,
        !is.na(investment_5) ~ 5,
        !is.na(investment_4) ~ 4,
        !is.na(investment_3) ~ 3,
        !is.na(investment_2) ~ 2,
        !is.na(investment_1) ~ 1,
        TRUE                 ~ 0
      ),
    # determine study location
    location =
      case_when(
        str_extract(GEOID, "(?<=06)\\d{3}") == "037" ~ "LA",
        str_extract(GEOID, "(?<=06)\\d{3}") == "019" ~ "Fresno",
        TRUE                                         ~ "SF_Bay"
      )
  ) %>% 
  # convert the NA's in intervention type flags to 0
  mutate_at(vars(greening:transit), ~replace(., is.na(.), 0)) %>% 
  # do not need geometry anymore, drop here
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  # reorder columns
  select(row_id, GEOID, investment, location, everything()) %>% 
  # Join crosswalked 2009 ACS data
  left_join(acs_tract_09_xwalk, by = c("GEOID" = "trtid10")) %>% 
  # remove the ending "_09"
  rename_with(~ str_remove(., "_09$"), ends_with("_09")) %>% 
  # Filter to study variables, join with 2018 data
  # Generate baseline study variables
  mutate(
    # total pop in 2009
    pop_tot_09 = pop_tot,
    # % nonwhites in 2009
    pct_nonwhi_09 = 1 - pct_whi,
    # pop nonwhites in 2009
    pop_nonwhi_09 = pop_tot_09 - pop_whi_nonhis,
    # pop poverty in 2009
    pop_pov_09 = pop_pov_pov,
    # % poverty in 2009
    pct_pov_09 = pov_rate,
    # median hh income in 2009
    med_inc_09 = med_inc,
    # no. of vacant housing units in 2009
    housing_vac_09 = housing_vac,
    # % vacant housing units in 2009
    pct_housing_vac_09 = vac_rate,
    # no. of renter occupied housing units in 2009
    h_rent_09 = housing_o_rent,
    # % renter occupied in 2009
    pct_h_rent_09 = pct_h_rent,
    # median gross rent in 2009
    med_rent_09 = med_rent,
    # pop college-educated in 2009
    pop_college_09 = pop_college,
    # % college-educated in 2009
    pct_college_09 = pct_college
  ) %>% 
  # filter to only relevant variables
  select(c(GEOID:n_invest, pop_tot_09:pct_college_09)) %>% 
  # join with filtered 2018 ACS data
  left_join(acs_tract_18, by = c("GEOID" = "stctytrct")) %>% 
  # generate change over time study variables
  mutate(
    # pct point change in non-whites (2009 to 2018)
    pct_pnt_nonwhi_chng = (1 - pct_whi) - pct_nonwhi_09,
    # pct population change in non-whites (2009 to 2018)
    pct_pop_nonwhi_chng = 
      ((pop_tot - pop_whi_nonhis) - pop_nonwhi_09) / pop_nonwhi_09,
    # pct point change in poverty rate (2009 to 2018)
    pct_pnt_pov_chng = pov_rate - pct_pov_09,
    # pct population change in poverty (2009 to 2018)
    pct_pop_pov_chng = (pop_pov_pov - pop_pov_09) / pop_pov_09,
    # change in median hh income (2009 to 2018)
    med_inc_chng = med_inc - med_inc_09,
    # pct change in median hh income (2009 to 2018)
    pct_med_inc_chng = (med_inc - med_inc_09) / med_inc_09,
    # pct pnt change in vacancy rate (2009 to 2018)
    pct_pnt_vac_chng = vac_rate - pct_housing_vac_09,
    # pct change in vacant housing units (2009 to 2018)
    pct_vac_chng = (housing_vac - housing_vac_09) / housing_vac_09,
    # pct pnt change in renter-occupied units (2009 to 2018)
    pct_pnt_h_rent_chng = pct_h_rent - pct_h_rent_09,
    # pct change in renter-occupied units (2009 to 2018)
    pct_h_rent_chng = (housing_o_rent - h_rent_09) / h_rent_09,
    # change in median gross rent (2009 to 2018)
    med_rent_chng = med_rent - med_rent_09,
    # % change in median gross rent (2009 to 2018
    pct_med_rent_chng = (med_rent - med_rent_09) / med_rent_09,
    # pct pnt change in college-educated (2009 to 2018)
    pct_pnt_college_chng = pct_college - pct_college_09,
    # pct population change in college-educated (2009 to 2018)
    pct_pop_college_chng = (pop_college - pop_college_09) / pop_college_09
  ) %>%
  select(c(GEOID:pct_college_09, pct_pnt_nonwhi_chng:pct_pop_college_chng)) %>% 
  # Replace all "Inf" values with NA
  mutate(across(.cols = everything(), na_if, Inf))

# Drop objects
rm(study_invest_join)

# Save Results ------------------------------------------------------------
# Write master_09_df (master investments for 2009/change over time) to csv
write_csv(master_df, paste0(homedir, savedir, "master_investments_traits.csv"))

# Clean up environment
rm(list = ls())
