# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Jun. 27, 2020                
# Last revised: Oct. 7, 2020                 
# Project: UDP SGC     
# Subproject: Propensity Score Matching
# Re: Match investments to neighborhoods (Census tracts)
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script matches the geospatial data of SGC investments to 2010 Census
# tract boundaries, and attaches tract IDs to each investment. 

# Inputs:
# Raw investment spatial data
# Lookup tables for investments

# Outputs:
# Investments paired with Census tracts
# Cleaned investment lookup tables

# Update log: 
# 10/6/20 - converted from RMarkdown to R script for faster processing.

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)
library(glue)
library(tidycensus)
library(tigris)
library(sf)

# Directories: 
homedir <- "E:/udp_summer2020/"
workdir <- "sgc_data/raw/"
savedir <- "sgc_data/cleaned/"
spatial_dir <- "Spatial Data 06_26_2020/"
setwd(homedir)

# Import data:
# Create list of investment shapefiles (importation done in main script)
shp_list <- 
  list.files(
    paste0(homedir, workdir, spatial_dir), 
    pattern = "\\.shp$", 
    full.names = TRUE
  )

# Read in lookup table
# investment_lookup <-
#   read_csv(paste0(homedir, workdir, "investment_lookup.csv")) %>%
#   arrange(FolderPath)

# Read in investment database (sheet 1)
investment_database <-
  readxl::read_excel(
    paste0(homedir, 
           workdir, 
           "Investment Selection.xlsx"
    ), 
    sheet = 1
  )


# intervention_flags_file <- "./SGC/intervention_flags.csv"

# Parameters:
## file paths to delete (delete points if polygon version exists)
delete_list <-
  c(
    "Albion_Riverside_Park_Point",
    "Cultural_Arts_District_Park_Point",
    "Ed_Roberts_Campus_Point",
    "Granville_Properties_Points",
    "MacArthur_Transit_Village_Points",
    "Gold_Line_Extension_Points",
    "Willowbrook_Rosa_Parks_Station_Point",
    "Mid_City_Expo_LRT_Points",
    "Rumrill_Park_Point",
    "Salud_Park_Point",
    "T_Third_St_LR_Stations_Points",
    "South_Sacramento_Corridor_LR_Extension_Points",
    "Taylor_Yard_Transit_Village_Points",
    "Neighborhood_Types"
  )

## group of SF Bay counties
sf_bay_area <- 
  c(
    "Alameda", "Contra Costa", "Marin", "Napa", "Sacramento", "san Francisco",
    "San Joaquin", "San Mateo", "Santa Clara", "Santa Cruz", "Solano", "Yolo" 
  )

## path for individual and investment group lookup
# investment_lookup_path <- "./SGC/investment_lookup.csv"
# 
# ## path for intervention group flags
# intervention_flags_path <- "./SGC/intervention_flags.csv"
# 
# ## path for final CSV
# investment_tract_path <- "./SGC/investment_tract.csv"

## Study year for Census geography
census_year <- 2010

# Main Script ------------------------------------------------------------------

# Filter shp_list--------------------------------------------------------------

# delete points from consideration
# definitely not the most elegant solution...
shp_list <-
  shp_list %>% 
  # turn into df to implement filters
  tibble(full = .) %>% 
  mutate(
    # extract just the name
    short = str_remove(full, paste0(homedir, workdir, spatial_dir)),
    short = str_remove(short, "/"),
    short = str_remove(short, ".shp"),
    # create flag if name is in delete list
    delete_flag = short %in% delete_list
  ) %>% 
  # filter out delete_list
  filter(delete_flag == FALSE) %>% 
  # return to list format
  pull(full)

# Read in all shp--------------------------------------------------------------

# write all investments to a list
invest_list <-
  shp_list %>% 
  map(~ read_sf(.))

# create empty "missing" list for abnormal shp
missing <- c()

# Create empty filter df
invest_filter <- 
  tibble(name = c(), group = c(), geometry = st_sfc())

# filter each shp down to name and geometry
for (i in 1:length(invest_list)) {
  # create column names variable
  invest_cols <- colnames(invest_list[i] %>% data.frame())
  # if missing "Name", add index to missing list
  if (!("Name" %in% invest_cols)) {
    missing <- c(missing, i)
    # if not missing "Name", filter to Name, FolderPath, and geometry
  } else {
    # generate filtered investment, bind to one df
    invest_filter <-
      invest_list[i] %>% 
      data.frame() %>%
      dplyr::select(Name, FolderPath, geometry) %>%
      st_as_sf() %>% 
      # use EPSG code for WGS 84
      st_transform(crs = st_crs(4326)) %>% 
      tibble() %>% 
      rbind(invest_filter) %>% 
      st_as_sf()
  }
}

# Investigate "missing" indexes
# see which are missing
shp_list[missing]

# create empty df for missing filtered
missing_filter <- 
  tibble(name = c(), group = c(), geometry = st_sfc())

# loop to create missing_filter df
for (i in 1:length(missing)) {
  missing_filter <-
    invest_list[missing[i]] %>% 
    data.frame() %>%
    mutate(
      # Extract investment name
      Name =
        str_extract(
          shp_list[missing[i]],
          "(?<=Spatial Data 06_26_2020/)\\w+"
        ),
      # replicate investment name for "group name" (ie FolderPath)
      FolderPath = Name
    ) %>% 
    dplyr::select(Name, FolderPath, geometry) %>%
    st_as_sf() %>% 
    # use EPSG code for WGS 84
    st_transform(crs = st_crs(4326)) %>% 
    tibble() %>% 
    rbind(missing_filter) %>% 
    st_as_sf()
}

# Create final investment filtered df-------------------------------------------
# bind invest_filter and missing_filter
final_invest_filter <-
  invest_filter %>%
  # Drop unnecessary z dimension
  st_zm() %>% 
  # Bind "missing" indexes to the rest of the investments
  bind_rows(missing_filter) %>% 
  # convert to tibble for easy manipulation
  tibble() %>% 
  # convert to sf object
  st_as_sf() %>% 
  # rename Crenshaw and Willowbrook
  mutate(
    Name = if_else(Name == "LA County", "Willowbrook_Rosa_Parks_Station", Name),
    Name = 
      if_else(Name == "Los Angeles-B", "Crenshaw_Blvd_Streetscape_Plan", Name),
    FolderPath = 
      if_else(
        FolderPath == "Round 1 Los Angeles TOD/Round 1", 
        "Crenshaw Blvd Streetscape Plan (LA TOD Planning Round 1)",
        FolderPath
      )
  )

# Check to make sure there are 20 investments (ie 20 unique rows)
final_invest_filter %>% 
  count(FolderPath) %>% 
  tibble()

# Check passed! - El Monte doubles up

# Remove unneeded objects
rm(invest_filter, invest_list, missing_filter)

# Create intervention group list -----------------------------------------------

# Create investment lookup table
investment_lookup <-
  final_invest_filter %>% 
  st_drop_geometry() %>% 
  select(Name, FolderPath) %>% 
  distinct() %>% 
  arrange(FolderPath)

# Clean investment database
investment_database_clean <-
  investment_database %>%
  # Filter to relevant investments
  filter(
    Investment %in%
      # No Rumrill or Salud Park
      c(
        "Albion Riverside Park",
        "Concord- Monument Blvd/Meadow Ln Pedestrian Imprvs",
        "Crenshaw Boulevard Streetscape Plan (Los Angeles TOD Planning (Round 1))",
        "Cultural Arts District Park",
        "Ed Roberts Campus",
        "El Monte Transit Village",
        "BRT Improvements",
        "Fulton Mall Reconstruction Project",
        "Metro Gold Line Foothill Extension (Phase 2A)",
        "Granville mixed-use market rate housing developments in downtown",
        "MacArthur Transit Village (Phase I-III)",
        "Mid City/Expo LRT",
        "Midtown Transportation & Streetscape Improvements",
        "San Leandro Downtown-BART Pedestrian Interface",
        "South Sacramento Corridor Light Rail Extension Phase 2",
        "SFMTA Third Street Light Rail",
        "Taylor Yard Village",
        "Metro Willowbrook/Rosa Parks Station Improvements"
      )
  ) %>%
  # select for relevant columns
  select(
    investment = Investment,
    intervention_group = `Intervention Group`,
    intervention_type = `Intervention Type`
  ) %>%
  # mutate to fix names and add in missing parks
  mutate(
    investment =
      if_else(investment == "BRT Improvements", "Fresno BRT Route", investment),
    investment =
      if_else(investment == "Mid City/Expo LRT", "Exposition Line", investment),
    investment =
      if_else(
        investment == "Metro Gold Line Foothill Extension (Phase 2A)",
        "Gold_Line_Extension_Line",
        investment
      ),
    investment =
      if_else(
        investment == "Metro Willowbrook/Rosa Parks Station Improvements",
        "Willowbrook_RosaParks",
        investment
      ),
    investment =
      if_else(
        investment == "South Sacramento Corridor Light Rail Extension Phase 2",
        "The South Line Phase 2 Project/Sac_Light_Rail_Expansion_Phase_2",
        investment
      )
  ) %>%
  add_row(
    # Assign intervention group type "greening" to both parks
    investment = c("Rumrill Park", "Salud Park"),
    intervention_group = c("Greening", "Greening")
  ) %>%
  arrange(investment)

# create new lookup table
investment_lookup_new <-
  investment_lookup %>%
  select(FolderPath) %>%
  distinct() %>%
  arrange() %>%
  # remove duplicated El Monte
  filter(FolderPath != "El Monte Transit Village Polygon") %>%
  # bind to investment database
  bind_cols(investment_database_clean) %>%
  select(-investment)

# fill out original lookup table
investment_lookup_clean <-
  investment_lookup %>%
  # reattach to original lookup table
  left_join(investment_lookup_new, by = "FolderPath") %>%
  # fill in the missing El Monte data
  mutate(
    intervention_group =
      if_else(
        FolderPath == "El Monte Transit Village Polygon",
        "Urban Infill, Greening",
        intervention_group
      ),
    intervention_type =
      if_else(
        FolderPath == "El Monte Transit Village Polygon",
        "mixed use, affordable TOD, market rate TOD, parks/open space",
        intervention_type
      ),
    # convert all intervention groups to the same case
    intervention_group = str_to_lower(intervention_group),
    # create group type flags
    greening = if_else(str_detect(intervention_group, "greening"), 1, 0),
    urban_infill =
      if_else(str_detect(intervention_group, "urban infill"), 1, 0),
    active_transportation =
      if_else(str_detect(intervention_group, "active transportation"), 1, 0),
    transit = if_else(str_detect(intervention_group, "transit"), 1, 0)
  )

# remove unneeded objects
rm(investment_database, investment_database_clean, 
   investment_lookup, investment_lookup_new)

# Join to final investment filter
final_invest_filter <-
  final_invest_filter %>% 
  left_join(
    investment_lookup_clean %>% 
      select(Name, greening, urban_infill, active_transportation, transit),
    by = "Name"
  )

# Read in Census tracts for LA, Fresno, SF Bay Area----------------------------

# Use Census 2010 (will crosswalk ACS data)
study_tracts <-
  tracts(
    state = "CA",
    county = c("Los Angeles", "Fresno", sf_bay_area),
    year = census_year
  ) %>% 
  st_as_sf() %>% 
  select(GEOID = GEOID10, geometry) %>% 
  # set to correct projection
  st_transform(crs = st_crs(final_invest_filter))

# Intersect tracts with investments--------------------------------------------

# join using intersects
invest_join <- 
  st_join(
    final_invest_filter, 
    study_tracts, 
    join = st_intersects
  ) %>% 
  tibble()

# Check invest join
invest_join %>% 
  arrange(FolderPath)

# tidy the data ---------------------------------------------------------------
invest_join_tidy <-
  invest_join %>% 
  select(GEOID, everything()) %>%
  # drop FolderPath, no longer necessary
  select(-FolderPath) %>% 
  # group by tract
  group_by(GEOID) %>%
  arrange(GEOID) %>% 
  # create # of investments in tract
  mutate(investment = glue("investment_{1:n()}")) %>% 
  # pivot to create unique rows (by tract)
  pivot_wider(
    names_from = investment, 
    values_from = Name
  ) %>% 
  # fill in the values of investments to create unique row
  fill(investment_1:length(.), .direction = "up") %>% 
  # replace 0's with NA in GEOID groups
  mutate_at(vars(greening:transit), na_if, y = 0) %>% 
  # fill in NA's with flag if applicable by group
  fill(greening:transit, .direction = "up") %>% 
  # filter out repeated tracts 
  # (equivalent to NA value of investment 1)
  filter(!is.na(investment_1)) %>%
  # Replace the NA's with 0's for the flags
  mutate_at(vars(greening:transit), ~replace(., is.na(.), 0)) %>% 
  # get rid of geometry variable
  st_as_sf() %>% 
  st_drop_geometry() %>% 
  # ungroup from GEOID
  ungroup()

# Remove unneeded objects
rm(study_tracts, investment_lookup_clean, invest_join)

# Save Results ------------------------------------------------------------

# write investment lookup table to csv
write_csv(final_invest_filter, paste0(homedir, savedir, "investment_lookup.csv"))

# write matched investments to new csv
write_csv(invest_join_tidy, paste0(homedir, savedir, "investment_tract.csv"))

# Clean up environment
rm(list = ls())
