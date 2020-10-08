# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Jun. 29, 2020                
# Last revised: Oct. 8, 2020                 
# Project: UDP SGC     
# Subproject: Shiny Map
# Re: Clean and prepare data for use in Shiny app
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script takes all cleaned data and prepares both a full area and study area
# dataset, saved as a geopackage. Results are used in creating the Shiny app.

# Inputs:
# Total stats (PSM writeup result)
# NOAH data
# Outmigration data
# ACS data

# Outputs:
# Geopackages for full results and study area results, 
# used in creating Shiny app
# Paired GEOIDs CSV

# Update log: 
# 10/8/20 - pulled code out of original markdown document for better organization

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)
library(sf)
library(sp)
library(tigris)
options(tigris_use_cache = TRUE)

# Directories: 
homedir <- "E:/udp_summer2020/"
workdir <- "sgc_data/raw/"
savedir <- "sgc_data/cleaned/"
setwd(homedir)

# Import data:
# Total stats (PSM writeup result)
total_stats <- 
  read_csv(paste0(homedir, savedir, "writeup_results/total_stats_all.csv"))

# NOAH
## 2009 xwalked
noah_09_xwalk <- read_csv(paste0(homedir, savedir, "NOAH_SGC_2009_xwalk.csv"))
## 2016
noah_16 <- read_csv(paste0(homedir, workdir, "NOAH_SGC_2016.csv"))

# Outmigration
outmigration_tract <- 
  read_csv(paste0(homedir, "infogroup_data/outmigration_tract_master.csv"))

# ACS
## 2009 xwalked
acs_tract_09_xwalk <-
  read_csv(paste0(homedir, savedir, "acs_tract_09_xwalk.csv"))

# Parameters:

# County names
sf_bay_counties <- 
  c(
    "Alameda", "Contra Costa", "Marin", "Napa", "Sacramento", "San Francisco", "San Joaquin", "San Mateo", "Santa Clara", "Santa Cruz", "Solano", "Yolo"
  )
la_county <- "Los Angeles"
fresno_county <- "Fresno"

# EPSG Code (use 4326 for WGS84)
epsg <- 4326

# Main Script ------------------------------------------------------------------

# Read in tract geometry
study_tracts <-
  tracts(
    state = "CA", 
    county = c(sf_bay_counties, la_county, fresno_county),
    year = 2010
  ) %>% 
  st_as_sf() %>% 
  st_transform(crs = epsg) %>% 
  as_tibble() %>% 
  select(GEOID10, geometry)

# Filter total stats
total_stats <- 
  total_stats %>% 
  # select for relevant variables
  select(
    GEOID, 
    investment,
    distance,
    greening:transit,
    location,
    perc_noah_tot_change,
    perc_noah_nolihtc_change,
    contains("outmigration")
  )

# Filter 2016 NOAH data
# 2016
noah_16 <- 
  noah_16 %>% 
  # filter down to necessary columns
  select(
    GEOID = stctytrct, 
    noah_housing_tot = housing_tot,
    noah_tot_clean,
    noah_nolihtc_clean,
    d_bay,
    d_la, 
    d_fresno
  )


# Filter ACS 2009 data
acs_tract_09_xwalk <- 
  acs_tract_09_xwalk %>% 
  # create population and percent nonwhite
  mutate(
    # pop nonwhites in 2009
    pop_nonwhi_09 = pop_tot_09 - pop_whi_nonhis_09,
    # % nonwhites in 2009
    pct_nonwhi_09 = pop_nonwhi_09 / pop_tot_09,
    # rename poverty rate 2009
    pct_pov_09 = pov_rate_09,
    # rename housing vacancy rate 2009
    pct_housing_vac_09 = housing_vac_09
  ) %>% 
  # filter to only relevant variables
  select(
    trtid10,
    pct_nonwhi_09, 
    pct_pov_09,
    med_inc_09, 
    pct_housing_vac_09,
    pct_h_rent_09, 
    med_rent_09, 
    pct_college_09
  ) %>% 
  # Replace all "Inf" values with NA
  mutate(across(.cols = everything(), na_if, Inf))

# Join all ACS, NOAH, Outmigration data together
full_area_info <-
  acs_tract_09_xwalk %>% 
  # join with 2016 data
  left_join(noah_16, by = c("trtid10" = "GEOID")) %>%
  # clean joined data
  select(-c(d_bay:d_fresno)) %>% 
  rename(
    noah_tot_clean_16 = noah_tot_clean, 
    noah_nolihtc_clean_16 = noah_nolihtc_clean,
    noah_housing_tot_16 = noah_housing_tot
  ) %>% 
  # join with crosswalked 2009 data
  left_join(noah_09_xwalk, by = c("trtid10" = "GEOID")) %>% 
  # find diff in NOAH units
  mutate(
    noah_tot_change = noah_tot_clean_16 - noah_tot_clean_09,
    noah_nolihtc_change = noah_nolihtc_clean_16 - noah_nolihtc_clean_09,
    perc_noah_tot_change = noah_tot_change / noah_tot_clean_09,
    perc_noah_nolihtc_change = noah_nolihtc_change / noah_nolihtc_clean_09
  ) %>% 
  # convert Inf to NA
  mutate(across(.cols = everything(), na_if, Inf)) %>% 
  # join with outmigration data
  left_join(outmigration_tract, by = c("trtid10" = "tract")) %>%
  # rename tract ID
  rename(GEOID = trtid10) %>% 
  # find change in outmigration rates by tract
  group_by(GEOID) %>% 
  mutate(
    outmigration_all_mean = 
      mean(c_across(starts_with("outmigration_all")), na.rm = TRUE),
    outmigration_LI_mean =
      mean(c_across(starts_with("outmigration_LI_2")), na.rm = TRUE),
    outmigration_r_mean =
      mean(c_across(starts_with("outmigration_r")), na.rm = TRUE),
    outmigration_LI_r_mean =
      mean(c_across(starts_with("outmigration_LI_r")), na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  # Join with tract geometry
  left_join(study_tracts, by = c("GEOID" = "GEOID10")) %>% 
  # add location flag
  mutate(
    location =
      case_when(
        str_detect(GEOID, "^06037") ~ "LA",
        str_detect(GEOID, "^06019") ~ "Fresno",
        TRUE ~ "SF Bay"
      )
  ) %>% 
  st_as_sf() %>% 
  st_transform(crs = epsg)

# Drop unneeded variables
rm(outmigration_tract, noah_09_xwalk, noah_16)

# Join study data with tract geometry and ACS
total_stats_geo <-
  total_stats %>% 
  # calculate outmigration rates by tract
  group_by(GEOID) %>% 
  mutate(
    outmigration_all_mean = 
      mean(c_across(starts_with("outmigration_all")), na.rm = TRUE),
    outmigration_LI_mean =
      mean(c_across(starts_with("outmigration_LI_2")), na.rm = TRUE),
    outmigration_r_mean =
      mean(c_across(starts_with("outmigration_r")), na.rm = TRUE),
    outmigration_LI_r_mean =
      mean(c_across(starts_with("outmigration_LI_r")), na.rm = TRUE),
  ) %>% 
  ungroup() %>% 
  # join with ACS data
  left_join(acs_tract_09_xwalk, by = c("GEOID" = "trtid10")) %>% 
  # join with geometry
  left_join(study_tracts, by = c("GEOID" = "GEOID10")) %>%
  st_as_sf() %>% 
  st_transform(crs = epsg)

# Drop unneeded objects
rm(acs_tract_09_xwalk, total_stats)

# Create df with paired GEOIDS
paired_geoids <- 
  total_stats_geo %>% 
  group_by(investment) %>% 
  arrange(location, investment, distance) %>% 
  # designate paired neighborhood (census tract)
  mutate(geoid_group = row_number()) %>% 
  ungroup() %>% 
  # join geometry by geoid group
  group_by(geoid_group) %>% 
  mutate(geometry = st_union(geometry)) %>% 
  # drop extra variables
  select(GEOID, geoid_group) %>% 
  group_by(geoid_group) %>% 
  # create paired geoids
  mutate(
    geoid_pair = lead(GEOID, order_by = geoid_group),
    geoid_pair = 
      if_else(is.na(geoid_pair), lag(GEOID, order_by = geoid_group), geoid_pair)
  ) %>% 
  st_drop_geometry() %>%  
  slice(1) %>% 
  ungroup() %>% 
  select(geoid_no_investment = GEOID, geoid_investment = geoid_pair)

# Save Results ------------------------------------------------------------

# GEOID pairs
write_csv(paired_geoids, paste0(homedir, savedir, "sgc_geoid_pairs.csv"))

# full area 
st_write(
  full_area_info, 
  paste0(homedir, savedir, "full_area_info.gpkg"), 
  delete_layer = TRUE
)

# total stats geo
st_write(
  total_stats_geo,
  paste0(homedir, savedir, "total_stats_geo.gpkg"),
  delete_layer = TRUE
)

# Clean up environment
rm(list = ls())
