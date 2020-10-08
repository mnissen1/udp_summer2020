# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Sept. 11, 2020                
# Last revised: Oct. 8, 2020                 
# Project: UDP SGC     
# Subproject: Shiny Map
# Re: Create Shiny App
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script creates a Shiny app using cleaned data from map_clean.R.

# Inputs:
# Full area and study area geopackages

# Outputs:
# Shiny app (Leaflet map)

# Update log: 
# 10/8/20 - added script header

# Open libraries---------------------------------------------
library(tidyverse)
library(sf)
library(sp)
library(tigris)
library(htmlwidgets)
library(leaflet)
library(shiny)
options(tigris_use_cache = TRUE)

# Files and parameters --------------------------------------
## Matched Stats
total_stats_geo_file <- "total_stats_geo.gpkg"

## Full Area data
full_area_info_file <- "full_area_info.gpkg"

## Spatial data
dir <- "Spatial Data 06_26_2020"
shp_list <- list.files(dir, pattern = "\\.shp$", full.names = TRUE)

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

## EPSG Code (use 4326 for WGS84)
epsg <- 4326

# Main Code -----------------------------------------------------------------

# Read in overlay data ------------------------------------------------------
# Total Stats Geo
total_stats_geo <- 
  st_read(total_stats_geo_file) %>% 
  st_as_sf() %>% 
  st_transform(crs = epsg) %>% 
  rename(geometry = geom)

# Full Area Info
full_area_info <- 
  st_read(full_area_info_file) %>% 
  # create location column
  mutate(
    location = 
      case_when(
        str_detect(GEOID, "^06037") ~ "LA",
        str_detect(GEOID, "^06019") ~ "Fresno",
        TRUE                         ~ "SF_Bay"
      )
  ) %>% 
  st_transform(crs = 3488) %>% # transform to NAD83 to simplify properly
  st_simplify() %>% 
  st_transform(crs = epsg) # return to WGS 84

# Create paired geoids -------------------------------------------------------
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
      if_else(
        is.na(geoid_pair), 
        lag(GEOID, order_by = geoid_group), 
        geoid_pair
      ),
    # add location filter
    location = 
      case_when(
        str_detect(GEOID, "^06037") ~ "LA",
        str_detect(GEOID, "^06019") ~ "Fresno",
        TRUE                         ~ "SF_Bay"
      )
  ) %>% 
  slice(1)

# Read in shapefiles -----------------------------------------------------------

# delete points from consideration
# definitely not the most elegant solution...
shp_list <-
  shp_list %>% 
  # turn into df to implement filters
  tibble(full = .) %>% 
  mutate(
    # extract just the name
    short = str_remove(full, dir),
    short = str_remove(short, "/"),
    short = str_remove(short, ".shp"),
    # create flag if name is in delete list
    delete_flag = short %in% delete_list
  ) %>% 
  # filter out delete_list
  filter(delete_flag == FALSE) %>% 
  # return to list format
  pull(full)

# Read in investments

#Albion Riverside Park
albion_park <- 
  st_read(shp_list[1]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

#Concord Monument Ped Imprv
concord_ped_imprv <- 
  st_read(shp_list[2]) %>% 
  st_transform(crs = epsg) %>% st_zm() %>% st_cast("LINESTRING")

#Crenshaw Blvd Streetscape
crenshaw_blv_st <- 
  st_read(shp_list[3]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

#cultural arts district park
cultural_arts_park <- 
  st_read(shp_list[4]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# ed roberts campus
ed_roberts_campus <- 
  st_read(shp_list[5]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# el monte transit
el_monte_transit <- 
  st_read(shp_list[6]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# fresno BRT
fresno_brt <-
  st_read(shp_list[7]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# fulton mall
fulton_mall <- 
  st_read(shp_list[8]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# gold line extension
gold_line <- 
  st_read(shp_list[9]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# granville properties
granville_properties <-
  st_read(shp_list[10]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# macarthur transit village
mtv <- 
  st_read(shp_list[11]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# mid city expo LRT
midcity_expo_lrt <- 
  st_read(shp_list[12]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# midtown transportation...
midtown_trans_st_imprv_line <-
  st_read(shp_list[13]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# rumrill park
rumrill_park <- 
  st_read(shp_list[14]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# salud park
salud_park <-
  st_read(shp_list[15]) %>%
  st_transform(crs = epsg) %>% st_zm() 

# san leandro bart
san_leandro_bart <- 
  st_read(shp_list[16]) %>%
  st_transform(crs = epsg) %>% st_zm() 

# south sac phase 2 extension
sac_phase_2 <- 
  st_read(shp_list[17]) %>%
  st_transform(crs = epsg) %>% st_zm() 

# third street LR line
third_st_lr <- 
  st_read(shp_list[18]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# taylor yard transit village
taylor_yard_transit_village <- 
  st_read(shp_list[19]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# the exchange at el monte
exchange_el_monte <-
  st_read(shp_list[20]) %>% 
  st_transform(crs = epsg) %>% st_zm() %>% st_cast("POINT") 

# willowbrook
willowbrook_station <- 
  st_read(shp_list[21]) %>% 
  st_transform(crs = epsg) %>% st_zm() 

# Set leaflet bins, labels, and palettes----------------------------------------
## set noah bins
noah_bins <- 
  round(
    BAMMtools::getJenksBreaks(total_stats_geo$perc_noah_tot_change, k = 10), 2
  )

# include 0 break
noah_bins <- c(noah_bins, 0)
noah_bins <- sort(noah_bins)

# Set study NOAH limits
pal_noah <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from minimum to 1500
      c(
        min(total_stats_geo$perc_noah_tot_change, na.rm = TRUE),
        max(total_stats_geo$perc_noah_tot_change, na.rm = TRUE)
      ),
    #set to custom bins
    bins = noah_bins,
    na.color = "grey"
  )

# Set NOAH labels
## don't forget to slice the final NA label
noah_labs <- 
  head(
    paste(
      scales::percent(noah_bins, 4), 
      scales::percent(lead(noah_bins), 4), 
      sep = " - "
    ), 
    -1
  )

# create investment factor to color boundaries 
invest_pal <-
  colorFactor(
    palette = c("red", "green"),
    domain = total_stats_geo$investment
  )

# set investment labels
invest_labs <- c("No Investment", "Investment")


# set outmigration bins 
outmig_bins <- 
  round(
    BAMMtools::getJenksBreaks(total_stats_geo$outmigration_all_mean, k = 10), 2
  )

outmig_bins <- c(outmig_bins, 0, 1)
outmig_bins <- sort(outmig_bins)

# Set outmigration all limits
pal_outmig_all <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from 0 to 1
      c(min(outmig_bins), max(outmig_bins)),
    #set to custom bins
    bins = outmig_bins,
    na.color = "grey"
  )

# Set Outmigration labels
## don't forget to slice the final NA label
outmig_labs <- 
  head(
    paste(
      scales::percent(outmig_bins), 
      scales::percent(lead(outmig_bins)), 
      sep = " - "
    ),
    -1
  )

# set outmigration bins 
nonwhi_bins <- 
  round(
    BAMMtools::getJenksBreaks(full_area_info$pct_nonwhi_09, k = 10), 2
  )

# Set non-white limits
pal_nonwhi <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from 0 to 1
      c(min(nonwhi_bins), max(nonwhi_bins)),
    #set to custom bins
    bins = nonwhi_bins,
    na.color = "grey"
  )

# Set non-white labels
## don't forget to slice the final NA label
nonwhi_labs <- 
  head(
    paste(
      scales::percent(nonwhi_bins), 
      scales::percent(lead(nonwhi_bins)), 
      sep = " - "
    ),
    -1
  )

# set outmigration bins 
college_bins <- 
  round(
    BAMMtools::getJenksBreaks(full_area_info$pct_college_09, k = 10), 2
  )

# Set college limits
pal_college <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from 0 to 1
      c(min(college_bins), max(college_bins)),
    #set to custom bins
    bins = college_bins,
    na.color = "grey"
  )

# Set college labels
## don't forget to slice the final NA label
college_labs <- 
  head(
    paste(
      scales::percent(college_bins), 
      scales::percent(lead(college_bins)), 
      sep = " - "
    ),
    -1
  )

# set outmigration bins 
renter_bins <- 
  round(
    BAMMtools::getJenksBreaks(full_area_info$pct_h_rent_09, k = 10), 2
  )

# Set renter limits
pal_renter <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from 0 to 1
      c(min(renter_bins), max(renter_bins)),
    #set to custom bins
    bins = renter_bins,
    na.color = "grey"
  )

# Set renter labels
## don't forget to slice the final NA label
renter_labs <- 
  head(
    paste(
      scales::percent(renter_bins), 
      scales::percent(lead(renter_bins)), 
      sep = " - "
    ),
    -1
  )

# set outmigration bins
medrent_bins <- 
  round(
    BAMMtools::getJenksBreaks(full_area_info$med_rent_09, k = 10), 0
  )

# Set median rent limits
pal_medrent <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from 0 to 4070
      c(min(medrent_bins), max(medrent_bins)),
    #set to custom bins
    bins = medrent_bins,
    na.color = "grey"
  )

# Set median rent labels
## don't forget to slice the final NA label
medrent_labs <- 
  head(
    paste(
      scales::dollar(medrent_bins), 
      scales::dollar(lead(medrent_bins)), 
      sep = " - "
    ),
    -1
  )

medinc_bins <- 
  round(
    BAMMtools::getJenksBreaks(full_area_info$med_inc_09, k = 10), 0
  )

# Set median income limits
pal_medinc <- 
  colorBin(
    palette = "plasma",
    domain =
      # set domain from 0 to 304041
      c(min(medinc_bins), max(medinc_bins)),
    #set to custom bins
    bins = medinc_bins,
    na.color = "grey"
  )

# Set median income labels
## don't forget to slice the final NA label
medinc_labs <- 
  head(
    paste(
      scales::dollar(medinc_bins), 
      scales::dollar(lead(medinc_bins)), 
      sep = " - "
    ),
    -1
  )

# Set color palattes
## NOAH
pal_area_noah <-
  colorFactor(
    palette = c("white", "white", "white"),
    domain = 
      unite(
        total_stats_geo %>%
          group_by(location) %>%
          summarise(
            noah_change = 
              scales::percent(mean(perc_noah_tot_change, na.rm = TRUE), 4)
          ),
        "location_change",
        location,
        noah_change,
        sep = ": "
      ) %>% 
      st_drop_geometry() %>% 
      mutate(location_change = str_replace(location_change, "_", " ")) %>% 
      pull()
  )

# outmigration all
pal_area_outmigration_all <-
  colorFactor(
    palette = c("white", "white", "white"),
    domain = 
      unite(
        total_stats_geo %>%
          group_by(location) %>%
          summarise(
            outmig_change = 
              scales::percent(mean(outmigration_all_mean, na.rm = TRUE), 4)
          ),
        "location_change",
        location,
        outmig_change,
        sep = ": "
      ) %>% 
      st_drop_geometry() %>% 
      mutate(location_change = str_replace(location_change, "_", " ")) %>% 
      pull()
  )

# outmigration LI
pal_area_outmigration_li <-
  colorFactor(
    palette = c("white", "white", "white"),
    domain = 
      unite(
        total_stats_geo %>%
          group_by(location) %>%
          summarise(
            outmig_change = 
              scales::percent(mean(outmigration_LI_mean, na.rm = TRUE), 4)
          ),
        "location_change",
        location,
        outmig_change,
        sep = ": "
      ) %>% 
      st_drop_geometry() %>% 
      mutate(location_change = str_replace(location_change, "_", " ")) %>% 
      pull()
  )

# outmigration renter
pal_area_outmigration_r <-
  colorFactor(
    palette = c("white", "white", "white"),
    domain = 
      unite(
        total_stats_geo %>%
          group_by(location) %>%
          summarise(
            outmig_change = 
              scales::percent(mean(outmigration_r_mean, na.rm = TRUE), 4)
          ),
        "location_change",
        location,
        outmig_change,
        sep = ": "
      ) %>% 
      st_drop_geometry() %>% 
      mutate(location_change = str_replace(location_change, "_", " ")) %>% 
      pull()
  )

# outmigration LI r
pal_area_outmigration_li_r <-
  colorFactor(
    palette = c("white", "white", "white"),
    domain = 
      unite(
        total_stats_geo %>%
          group_by(location) %>%
          summarise(
            outmig_change = 
              scales::percent(mean(outmigration_LI_r_mean, na.rm = TRUE), 4)
          ),
        "location_change",
        location,
        outmig_change,
        sep = ": "
      ) %>% 
      st_drop_geometry() %>% 
      mutate(location_change = str_replace(location_change, "_", " ")) %>% 
      pull()
  )



# Set leaflet overlay groups ---------------------------------------------------
overlay_groups <-
  c(
    "Census Boundaries", 
    "Investments",
    "Investment Tract Flag",
    "Non-white Population",
    "College Educated Population",
    "Renter-Occupied Housing",
    "Median Rent",
    "Median Income",
    "NOAH Study Areas", 
    "Outmigration All Study Areas",
    "Outmigration LI Study Areas",
    "Outmigration Renter Study Areas",
    "Outmigration LI Renter Study Areas"
  )

# create investment marker icon ------------------------------------------------
dollar_icon <- makeAwesomeIcon(
  icon = "dollar",
  iconColor = "black",
  library = "fa",
  markerColor = "orange"
)

# Create leaflet popups---------------------------------------------------------
# outmigration popups
outmigration_popup <-
  paste(
    # Display Tract number
    "Census Tract:", as.character(total_stats_geo$GEOID), "<br>",
    
    # Display Average Outmigration Rates
    "Average Total Outmigration Rate:",
    scales::percent(total_stats_geo$outmigration_all_mean, 4), "<br>",
    "Average Low Income Outmigration Rate:",
    scales::percent(total_stats_geo$outmigration_LI_mean, 4), "<br>",
    "Average Renter Outmigration Rate:",
    scales::percent(total_stats_geo$outmigration_r_mean, 4), "<br>",
    "Average Low Income Renter Outmigration Rate:",
    scales::percent(total_stats_geo$outmigration_LI_r_mean, 4), "<br>",
    
    # Display investment flags by type
    "Investment Type: ", 
    case_when(
      # all 4
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Transit, Urban Infill, Active Transportation",
      # 3 investment types
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Greening, Transit, Urban Infill",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Transit, Active Transportation",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Urban Infill, Active Transportation",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Transit, Urban Infill, Active Transportation",
      # 2 investment types
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 0 ~
        "Greening, Transit",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Greening, Urban Infill",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Active Transportation",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Transit, Urban Infill",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Transit, Active Transportation",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Urban Infill, Active Transportation",
      # single investments
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 0 ~
        "Greening",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 0 ~
        "Transit",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Urban Infill",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Active Transportation",
      TRUE ~ "No Investments"
    )
  )

# NOAH popups
noah_popup <-
  paste(
    # Display Tract number
    "Census Tract:", as.character(total_stats_geo$GEOID), "<br>",
    
    # Display NOAH change
    "% NOAH Change:", 
    scales::percent(total_stats_geo$perc_noah_tot_change, 4), "<br>",
    
    # Display investment flags by type
    "Investment Type: ", 
    case_when(
      # all 4
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Transit, Urban Infill, Active Transportation",
      # 3 investment types
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Greening, Transit, Urban Infill",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Transit, Active Transportation",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Urban Infill, Active Transportation",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Transit, Urban Infill, Active Transportation",
      # 2 investment types
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 0 ~
        "Greening, Transit",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Greening, Urban Infill",
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Greening, Active Transportation",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Transit, Urban Infill",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Transit, Active Transportation",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 1 ~
        "Urban Infill, Active Transportation",
      # single investments
      total_stats_geo$greening == 1 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 0 ~
        "Greening",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 1 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 0 ~
        "Transit",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 1 &
        total_stats_geo$active_transportation == 0 ~
        "Urban Infill",
      total_stats_geo$greening == 0 &
        total_stats_geo$transit == 0 &
        total_stats_geo$urban_infill == 0 &
        total_stats_geo$active_transportation == 1 ~
        "Active Transportation",
      TRUE ~ "No Investments"
    )
  )

# ACS popups
acs_popup <-
  paste(
    # Display Tract number
    "Census Tract:", as.character(full_area_info$GEOID), "<br>",
    
    # Display non-white population
    "Non-white Population:",
    scales::percent(full_area_info$pct_nonwhi_09, 4), "<br>",
    
    # Display college educated
    "College-Educated Population:",
    scales::percent(full_area_info$pct_college_09, 4), "<br>",
    
    # Display renter occupied housing
    "Renter-Occupied Housing:",
    scales::percent(full_area_info$pct_h_rent_09, 4), "<br>",
    
    # Display median rent
    "Median Rent:",
    scales::dollar(full_area_info$med_rent_09, 4), "<br>",
    
    # Display median income
    "Median Income:",
    scales::dollar(full_area_info$med_inc_09, 4)
  )



# Create input selection choices ----------------------------------------------------
# Create list of study data
study_data_choices <- 
  c(
    "None",
    "NOAH", 
    "Total Outmigration",
    "Low Income Outmigration",
    "Renter Outmigration",
    "Low Income Renter Outmigration"
  )

# Create list of full area data
full_data_choices <-
  c(
    "None",
    "Non-white Population",
    "College Educated Population",
    "Renter-Occupied Housing",
    "Median Rent",
    "Median Income"
  )

# Create lists of layerIds for leafletProxy to clear on ------------------------
# Create a list of NOAH layerIds to clear on
noah_clear <- paste0("noah_", seq(1, nrow(total_stats_geo)))

# Create list of outmigration layerIds to clear on
outmig_all_clear <- paste0("outmig_all_", seq(1, nrow(total_stats_geo)))
outmig_li_clear <- paste0("outmig_li_", seq(1, nrow(total_stats_geo)))
outmig_r_clear <- paste0("outmig_r_", seq(1, nrow(total_stats_geo)))
outmig_li_r_clear <- paste0("outmig_li_r_", seq(1, nrow(total_stats_geo)))

# Create list of ACS layerIds to clear on
nw_clear <- paste0("nw_", seq(1, nrow(full_area_info)))
college_clear <- paste0("college_", seq(1, nrow(full_area_info)))
renter_clear <- paste0("renter_", seq(1, nrow(full_area_info)))
med_rent_clear <- paste0("med_rent_", seq(1, nrow(full_area_info)))
med_inc_clear <- paste0("med_inc_", seq(1, nrow(full_area_info)))

# Create a list of investment LayerIds to clear on
tract_clear <- paste0("tract_", seq(1, nrow(total_stats_geo)))
tract_pair_clear <- paste0("tract_pair_", seq(1, nrow(paired_geoids)))

# Create list of investment shapefiles to clear on
invest_list <- paste0("invest_", seq(1, length(shp_list)))


# Create investment marker functions to filter by area--------------------------
la_markers <- function(prior){
  prior %>% 
    # Add Investment Shapefiles
    ## LA INVESTMENTS
    ## albion park
    addPolygons(
      data = albion_park,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Albion Riverside Park", "<br>",
          "Investment Type: Greening"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[1]
    ) %>%
    ## albion park marker
    addAwesomeMarkers(
      data = albion_park %>% st_centroid(),
      group = "Investments",
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 20),
      clusterId = "LA",
      layerId = invest_list[1],
      popup =
        paste(
          "Albion Riverside Park", "<br>",
          "Investment Type: Greening"
        )
    ) %>%
    
    ## crenshaw blvd streetscape
    addPolygons(
      data = crenshaw_blv_st,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Crenshaw Boulevard Streetscape Plan", "<br>",
          "Investment Type: Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[2]
    ) %>%
    ## crenshaw marker
    addAwesomeMarkers(
      data = crenshaw_blv_st %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[2],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 15),
      clusterId = "LA",
      popup =
        paste(
          "Crenshaw Boulevard Streetscape Plan", "<br>",
          "Investment Type: Active Transportation"
        )
    ) %>%
    
    ## el monte transit village
    addPolygons(
      data = el_monte_transit,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "El Monte Transit Village", "<br>",
          "Investment Type: Greening, Urban Infill"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[3]
    ) %>%
    ## el monte transit
    addAwesomeMarkers(
      data = el_monte_transit %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[3],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "LA",
      popup =
        paste(
          "El Monte Transit Village", "<br>",
          "Investment Type: Greening, Urban Infill"
        )
    ) %>%
    
    ## gold line extension
    addPolylines(
      data = gold_line,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Metro Gold Line Foothill Extension", "<br>",
          "Investment Type: Transit, Urban Infill"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[4]
    ) %>%
    ## gold line marker
    addAwesomeMarkers(
      data = gold_line %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[4],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "LA",
      popup =
        paste(
          "Metro Gold Line Foothill Extension", "<br>",
          "Investment Type: Transit, Urban Infill"
        )
    ) %>%
    
    ## mid city expo lrt
    addPolylines(
      data = midcity_expo_lrt,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Mid City/Expo LRT", "<br>",
          "Investment Type: Transit"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[5]
    ) %>%
    ## mid city expo lrt
    addAwesomeMarkers(
      data = midcity_expo_lrt %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[5],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 15),
      clusterId = "LA",
      popup =
        paste(
          "Mid City/Expo LRT", "<br>",
          "Investment Type: Transit"
        )
    ) %>%
    
    ## salud park
    addPolygons(
      data = salud_park,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Salud Park", "<br>",
          "Investment Type: Greening"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[6]
    ) %>%
    ## salud marker
    addAwesomeMarkers(
      data = salud_park %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[6],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "LA",
      popup =
        paste(
          "Salud Park", "<br>",
          "Investment Type: Greening"
        )
    ) %>%
    
    ## taylor transit yard
    addPolygons(
      data = taylor_yard_transit_village,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Taylor Yard Transit Village Master Plan", "<br>",
          "Investment Type: Urban Infill"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[7]
    ) %>%
    ## taylor transit marker
    addAwesomeMarkers(
      data =
        taylor_yard_transit_village %>%
        filter(Name == "Taylor Yard Transit Village") %>%
        st_centroid(),
      group = "Investments",
      layerId = invest_list[7],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 20),
      clusterId = "LA",
      popup =
        paste(
          "Taylor Yard Transit Village Master Plan", "<br>",
          "Investment Type: Urban Infill"
        )
    ) %>%
    
    ## exchange at el monte
    addCircles(
      data = exchange_el_monte,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "The Exchange at El Monte - El Monte Transit Village",
          "<br>", "Investment Type: Greening, Urban Infill"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[8]
    ) %>%
    
    ## Willowbrook Station
    addPolygons(
      data = willowbrook_station,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Metro Willowbrook/Rosa Parks Station Improvements", "<br>",
          "Investment Type: Transit, Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[9]
    ) %>%
    ## willowbrook marker
    addAwesomeMarkers(
      data = willowbrook_station %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[9],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "LA",
      popup =
        paste(
          "Metro Willowbrook/Rosa Parks Station Improvements", "<br>",
          "Investment Type: Transit, Active Transportation"
        )
    )
}

sf_markers <- function(prior) {
  prior %>% 
    ## SF BAY INVESTMENTS
    ## concord monument blvd
    addPolylines(
      data = concord_ped_imprv,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Monument Corridor Pedestrian Infrastructure Improvement Project",
          "<br>", "Investment Type: Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[10]
    ) %>%
    ## concord monument marker
    addAwesomeMarkers(
      data = concord_ped_imprv %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[10],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "SF",
      popup =
        paste(
          "Monument Corridor Pedestrian Infrastructure Improvement Project",
          "<br>", "Investment Type: Active Transportation"
        )
    ) %>%
    
    ## ed roberts campus
    addPolygons(
      data = ed_roberts_campus,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Ed Roberts Campus", "<br>",
          "Investment Type: Urban Infill, Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[11]
    ) %>%
    ## ed roberts
    addAwesomeMarkers(
      data = ed_roberts_campus %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[11],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 10),
      clusterId = "SF",
      popup =
        paste(
          "Ed Roberts Campus", "<br>",
          "Investment Type: Urban Infill, Active Transportation"
        )
    ) %>%
    
    ## macarthur transit village
    addPolygons(
      data = mtv,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "MacArthur Transit Village", "<br>",
          "Investment Type: Urban Infill, Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[12]
    ) %>%
    ## macarthur transit marker
    addAwesomeMarkers(
      data =
        mtv %>%
        filter(Name == "MTV: Mural Affordable Housing Polygon") %>%
        st_centroid(),
      group = "Investments",
      layerId = invest_list[12],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 10),
      clusterId = "SF",
      popup =
        paste(
          "MacArthur Transit Village", "<br>",
          "Investment Type: Urban Infill, Active Transportation"
        )
    ) %>%
    
    ## midtown transportation and st imprv
    addPolylines(
      data = midtown_trans_st_imprv_line,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Midtown Transportation and Streetscape Improvements", "<br>",
          "Investment Type: Greening, Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[13]
    ) %>%
    ## midtown... marker
    addAwesomeMarkers(
      data = midtown_trans_st_imprv_line %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[13],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "SF",
      popup =
        paste(
          "Midtown Transportation and Streetscape Improvements", "<br>",
          "Investment Type: Greening, Active Transportation"
        )
    ) %>%
    
    ## rumrill park
    addPolygons(
      data = rumrill_park,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Rumrill Park", "<br>",
          "Investment Type: Greening"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[14]
    ) %>%
    ## rumrill marker
    addAwesomeMarkers(
      data = rumrill_park %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[14],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "SF",
      popup =
        paste(
          "Rumrill Park", "<br>",
          "Investment Type: Greening"
        )
    ) %>%
    
    ## san leandro dt bart
    addPolylines(
      data = san_leandro_bart,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "San Leandro Downtown-BART Pedestrian Interface", "<br>",
          "Investment Type: Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[15]
    ) %>%
    ## san leandro bart marker
    addAwesomeMarkers(
      data = san_leandro_bart %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[15],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "SF",
      popup =
        paste(
          "San Leandro Downtown-BART Pedestrian Interface", "<br>",
          "Investment Type: Active Transportation"
        )
    ) %>%
    
    ## south sac lrt extension
    addPolylines(
      data = sac_phase_2,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "South Sacramento Corridor Light Rail Extension", "<br>",
          "Investment Type: Transit"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[16]
    ) %>%
    ## south sac lrt marker
    addAwesomeMarkers(
      data = sac_phase_2 %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[16],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "SF",
      popup =
        paste(
          "South Sacramento Corridor Light Rail Extension", "<br>",
          "Investment Type: Transit"
        )
    ) %>%
    
    # third st lr line
    addPolylines(
      data = third_st_lr,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "SFMTA Third Street Light Rail", "<br>",
          "Investment Type: Transit"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[17]
    ) %>%
    ## 3rd st lrt marker
    addAwesomeMarkers(
      data = third_st_lr %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[17],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "SF",
      popup =
        paste(
          "SFMTA Third Street Light Rail", "<br>",
          "Investment Type: Transit"
        )
    )
}

fresno_markers <- function(prior) {
  prior %>% 
    ## FRESNO INVESTMENTS
    ## cultural arts district park
    addPolygons(
      data = cultural_arts_park,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Cultural Arts District Park", "<br>",
          "Investment Type: Greening"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[18]
    ) %>%
    ## cultural arts marker
    addAwesomeMarkers(
      data = cultural_arts_park %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[18],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 10),
      clusterId = "Fresno",
      popup =
        paste(
          "Cultural Arts District Park", "<br>",
          "Investment Type: Greening"
        )
    ) %>%
    
    ## fresno BRT line
    addPolylines(
      data = fresno_brt,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "BRT Improvements (Line Q)", "<br>",
          "Investment Type: Transit"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[19]
    ) %>%
    ## fresno brt
    addAwesomeMarkers(
      data = fresno_brt %>% st_point_on_surface(),
      group = "Investments",
      layerId = invest_list[19],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 250),
      clusterId = "Fresno",
      popup =
        paste(
          "BRT Improvements (Line Q)", "<br>",
          "Investment Type: Transit"
        )
    ) %>%
    
    ## fulton mall
    addPolygons(
      data = fulton_mall,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          "Fulton Mall Reconstruction Project", "<br>",
          "Investment Type: Active Transportation"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[20]
    ) %>%
    ## fulton mall
    addAwesomeMarkers(
      data = fulton_mall %>% st_centroid(),
      group = "Investments",
      layerId = invest_list[20],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(maxClusterRadius = 10),
      clusterId = "Fresno",
      popup =
        paste(
          "Fulton Mall Reconstruction Project", "<br>",
          "Investment Type: Active Transportation"
        )
    ) %>%
    
    ## Granville properties
    addPolygons(
      data = granville_properties,
      stroke = TRUE,
      opacity = 1,
      fillColor = "orange",
      color = "black",
      fillOpacity = 1,
      weight = 4,
      popup =
        paste(
          str_remove(granville_properties$Name, "Polygon"), "<br>",
          "Granville Properties", "<br>",
          "Investment Type: Urban Infill"
        ),
      highlightOptions =
        highlightOptions(color = "white", bringToFront = TRUE),
      group = "Investments",
      layerId = invest_list[21]
    ) %>%
    ## granville property marker
    addAwesomeMarkers(
      data =
        granville_properties %>%
        filter(Name == "The Lede Polygon") %>%
        st_centroid(),
      group = "Investments",
      layerId = invest_list[21],
      icon = dollar_icon,
      clusterOptions = markerClusterOptions(),
      clusterId = "Fresno",
      popup =
        paste(
          "Granville Properties", "<br>",
          "Investment Type: Urban Infill"
        )
    )
}



# Set up UI -------------------------------------------------------------------
ui <- fluidPage(
  # enable Shinyjs for disabling feature
  shinyjs::useShinyjs(),
  
  # Print title
  titlePanel("SGC Matched Neighborhoods"),
  
  # Set up sidebar panel 
  sidebarLayout(
    sidebarPanel(
      # create location filter
      selectInput(
        inputId = "location",
        label = "Select location",
        choices = 
          c("SF Bay Area" = "SF_Bay", "Fresno" = "Fresno", "Los Angeles" = "LA"),
        multiple = TRUE
      ),
      
      # create full data (matching data) select button
      selectInput(
        inputId = "full_data",
        label = "Select matching data overlay",
        choices = full_data_choices
      ),
      
      # create study data (outcome data) select button
      selectInput(
        inputId = "study_data",
        label = "Select outcome data overlay",
        choices = study_data_choices
      ),
      
      # create investment overlays
      checkboxInput("Investments", "Investment Markers"),
      checkboxInput("invest_tracts", "Paired Tracts")
    ),
    
    mainPanel(
      leafletOutput("mymap", height = 500),
      p()
    )
  )
)


# Set up server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # Set up base map ####
  output$mymap <- renderLeaflet({
    # leaflet setup
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 18)) %>% 
      addTiles(options = tileOptions(minZoom = 6, maxZoom = 18)) %>% 
      
      # Set Provider Tile
      addProviderTiles(
        provider = "CartoDB.Positron",
        # Set min/max zoom to focus on statewide level
        options = tileOptions(minZoom = 6, maxZoom = 18)
      ) %>%
      
      # Set Maximum Bounds for California
      setMaxBounds(
        lat1 = 32.5121,
        lat2 = 42.0126,
        lng1 = -124.6509,
        lng2 = -114.1315
      ) %>%
      
      ## Add button to set zoom level
      addEasyButton(
        easyButton(
          icon = "fa-globe",
          title = "Zoom out to California",
          # Set to min zoom level (match to provider tile)
          onClick = JS("function(btn, map){ map.setZoom(6); }")
        )
      )
  })
  
  # Apply area filters to study data ####
  study_data_react <- reactive({
    total_stats_geo %>% filter(location %in% input$location)
  })
  
  # Apply area filters to full data ####
  full_data_filter <- reactive({
    full_area_info %>% filter(location %in% input$location)  
  })
  ## Add a delay
  full_data_react <- full_data_filter %>%  debounce(1000)
  
  # Apply area filters to paired geoids ####
  paired_geoids_react <- reactive({
    paired_geoids %>% filter(location %in% input$location)
  })
  
  # area filters for investment markers
  marker_react <- reactive({
    input$location
  })
  
  # Disable all overlay layers if no location data selected ####
  observe({
    shinyjs::toggleState("invest_tracts", !is.null(input$location))
    shinyjs::toggleState("Investments", !is.null(input$location))
    shinyjs::toggleState("study_data", !is.null(input$location))
    shinyjs::toggleState("full_data", !is.null(input$location))
  })
  
  # Clear/reset map if no location data is selected
  observeEvent(input$location, {
    if (is.null(input$location)) {
      # Clear map if no location selected
      leafletProxy("mymap") %>%
        clearShapes() %>%
        clearControls() %>% 
        clearMarkerClusters() 
    } else {
      leafletProxy("mymap")
    }
  })
  
  # Reset outcome inputs if no location is selected
  observe({
    if (is.null(input$location)) {
      # Update study data button
      updateSelectInput(
        session,
        inputId = "study_data",
        label = "Select outcome data overlay",
        choices = study_data_choices,
        selected = "None"
      )
    }
  })
  
  # Reset matched inputs if no location is selected
  observe({
    if (is.null(input$location)) {
      # update full data (matching data) select button
      updateSelectInput(
        session,
        inputId = "full_data",
        label = "Select matching data overlay",
        choices = full_data_choices,
        selected = "None"
      )
    }
  })
  
  # Reset investment checkbox if no location is selected
  observe({
    if (is.null(input$location)) {
      # Update study data button
      updateCheckboxInput(
        session, 
        inputId = "Investments", 
        label = "Investment Markers",
        value = FALSE
      )
    } else {
      leafletProxy("mymap")
    }
  })
  
  # Reset investment tracts checkbox if no location is selected
  observe({
    if (is.null(input$location)) {
      # Update study data button
      updateCheckboxInput(
        session, 
        inputId = "invest_tracts", 
        label = "Paired Tracts",
        value = FALSE
      )
    } else {
      leafletProxy("mymap")
    }
  })
  
  # Disable outcome layers if matching layers selected ####
  observe({
    # disable if no location is selected 
    if (is.null(input$location)) {
      shinyjs::disable(id = "study_data")
      
    } else {
      observeEvent(input$full_data, {
        if(input$full_data == "None" & !is.null(input$location)){
          shinyjs::enable(id = "study_data")
        } else {
          shinyjs::disable(id = "study_data")
        }
      })
    }
  })
  
  # Disable matching layers if outcome layers selected ####
  observe({
    # disable if no location is selected 
    if (is.null(input$location)) {
      shinyjs::disable(id = "full_data")
      
    } else {
      observeEvent(input$study_data, {
        if(input$study_data == "None" & !is.null(input$location)){
          shinyjs::enable(id = "full_data")
        } else {
          shinyjs::disable(id = "full_data")
        }
      })
    }
  })
  
  # Add outcome data layers ####
  observe({
    
    # Apply location filter
    total_stats_geo <- study_data_react()
    
    # Create proxy map
    proxy <- leafletProxy("mymap")
    
    #### Clear map if "None" selected ####
    if (input$study_data == "None") {
      proxy %>%
        # Reset controls (need to do one by one)
        removeControl("noah_legend_clear") %>% 
        removeControl("noah_legend_clear2") %>% 
        removeControl("outmig_all_legend") %>% 
        removeControl("outmig_all_legend2") %>% 
        removeControl("outmig_li_legend") %>% 
        removeControl("outmig_li_legend2") %>% 
        removeControl("outmig_r_legend") %>% 
        removeControl("outmig_r_legend2") %>% 
        removeControl("outmig_li_r_legend") %>% 
        removeControl("outmig_li_r_legend2") %>%
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            noah_clear,
            outmig_all_clear,
            outmig_li_clear,
            outmig_r_clear,
            outmig_li_r_clear
          )
        )
    } 
    
    #### Add NOAH layers ####
    else if (input$study_data == "NOAH") {
      proxy %>%
        # Reset controls (need to do one by one)
        removeControl("noah_legend_clear") %>% 
        removeControl("noah_legend_clear2") %>% 
        removeControl("outmig_all_legend") %>% 
        removeControl("outmig_all_legend2") %>% 
        removeControl("outmig_li_legend") %>% 
        removeControl("outmig_li_legend2") %>% 
        removeControl("outmig_r_legend") %>% 
        removeControl("outmig_r_legend2") %>% 
        removeControl("outmig_li_r_legend") %>% 
        removeControl("outmig_li_r_legend2") %>%
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            noah_clear,
            outmig_all_clear,
            outmig_li_clear,
            outmig_r_clear,
            outmig_li_r_clear
          )
        ) %>% 
        
        ## add NOAH % change in study areas polygon ##################
      addPolygons(
        data = total_stats_geo,
        # Set GEOID names and NOAH percentages
        popup = noah_popup,
        # set fill colors and settings
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_noah(perc_noah_tot_change),
        # assign group
        group = "NOAH Study Areas",
        # assign layerId
        layerId = noah_clear,
        # bring shape to front on hover
        highlightOptions = 
          highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add average area NOAH legend###################
      addLegend(
        # add color scheme
        pal = pal_area_noah,
        # set position
        "bottomleft",
        # set values 
        values = 
          unite(
            total_stats_geo %>%
              group_by(location) %>%
              summarise(
                noah_change = 
                  scales::percent(
                    mean(perc_noah_tot_change, na.rm = TRUE), 4
                  )
              ),
            "location_change",
            location,
            noah_change,
            sep = ": "
          ) %>% 
          st_drop_geometry() %>% 
          mutate(
            location_change = str_replace(location_change, "_", " ")
          ) %>% 
          pull(),
        title = "Average NOAH Change Rate </br> 2009 - 2016",
        opacity = 1,
        # Set group
        group = "NOAH Study Areas",
        # set LayerId
        layerId = "noah_legend_clear"
      ) %>% 
        
        ## Add NOAH legend ####
      addLegend(
        "bottomleft",
        pal = pal_noah,
        values = full_area_info$perc_noah_tot_change,
        title = "NOAH Change Rate</br>2009 - 2016",
        labFormat = 
          function(type, cuts, p) {  # Here's the trick
            paste0(noah_labs)
          },
        opacity = 1,
        group = "NOAH Study Areas",
        layerId = "noah_legend_clear2"
      ) 
    }
    
    ## Add Outmigration All layers ####
    else if (input$study_data == "Total Outmigration") {
      proxy %>%
        # Reset controls (need to do one by one)
        removeControl("noah_legend_clear") %>% 
        removeControl("noah_legend_clear2") %>% 
        removeControl("outmig_all_legend") %>% 
        removeControl("outmig_all_legend2") %>% 
        removeControl("outmig_li_legend") %>% 
        removeControl("outmig_li_legend2") %>% 
        removeControl("outmig_r_legend") %>% 
        removeControl("outmig_r_legend2") %>% 
        removeControl("outmig_li_r_legend") %>% 
        removeControl("outmig_li_r_legend2") %>%
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            noah_clear,
            outmig_all_clear,
            outmig_li_clear,
            outmig_r_clear,
            outmig_li_r_clear
          )
        ) %>% 
        
        ## add Total Outmigration study areas polygon ####################
      addPolygons(
        # set outmigration data
        data = total_stats_geo,
        # set popups for geoid, data, investment type
        # Set GEOID names and NOAH percentages
        popup = outmigration_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_outmig_all(outmigration_all_mean),
        group = "Outmigration All Study Areas",
        layerId = outmig_all_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add average area outmigration all legend###################
      addLegend(
        pal = pal_area_outmigration_all,
        "bottomleft",
        values = 
          unite(
            total_stats_geo %>%
              group_by(location) %>%
              summarise(
                outmig_change = 
                  scales::percent(mean(outmigration_all_mean, na.rm = TRUE), 4)
              ),
            "location_change",
            location,
            outmig_change,
            sep = ": "
          ) %>% 
          st_drop_geometry() %>% 
          mutate(location_change = str_replace(location_change, "_", " ")) %>% 
          pull(),
        title = "Average Rate of Total Outmigration</br>2007 - 2018",
        opacity = 1,
        group = "Outmigration All Study Areas",
        layerId = "outmig_all_legend"
      ) %>%
        
        ## Add Outmigration All legend ####################################
      addLegend(
        "bottomleft",
        pal = pal_outmig_all,
        values = full_area_info$outmigration_all_mean,
        title = "Average Rate of Total Outmigration</br>2007 - 2018",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(outmig_labs)
          },
        opacity = 1,
        group = "Outmigration All Study Areas",
        layerId = "outmig_all_legend2"
      ) 
    }
    
    ## Add Outmigration LI layers ####
    else if (input$study_data == "Low Income Outmigration") {
      proxy %>%
        # Reset controls (need to do one by one)
        removeControl("noah_legend_clear") %>% 
        removeControl("noah_legend_clear2") %>% 
        removeControl("outmig_all_legend") %>% 
        removeControl("outmig_all_legend2") %>% 
        removeControl("outmig_li_legend") %>% 
        removeControl("outmig_li_legend2") %>% 
        removeControl("outmig_r_legend") %>% 
        removeControl("outmig_r_legend2") %>% 
        removeControl("outmig_li_r_legend") %>% 
        removeControl("outmig_li_r_legend2") %>%
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            noah_clear,
            outmig_all_clear,
            outmig_li_clear,
            outmig_r_clear,
            outmig_li_r_clear
          )
        ) %>% 
        
        ## add LI Outmigration study areas polygon #########################
      addPolygons(
        # set outmigration data
        data = total_stats_geo,
        # set popups for geoid, data, investment type
        # Set GEOID names and NOAH percentages
        popup = outmigration_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_outmig_all(outmigration_LI_mean),
        group = "Outmigration LI Study Areas",
        layerId = outmig_li_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>% 
        
        ## Add average area outmigration LI legend###################
      addLegend(
        pal = pal_area_outmigration_li,
        "bottomleft",
        values = 
          unite(
            total_stats_geo %>%
              group_by(location) %>%
              summarise(
                outmig_change = 
                  scales::percent(mean(outmigration_LI_mean, na.rm = TRUE), 4)
              ),
            "location_change",
            location,
            outmig_change,
            sep = ": "
          ) %>% 
          st_drop_geometry() %>% 
          mutate(location_change = str_replace(location_change, "_", " ")) %>% 
          pull(),
        title = "Average Rate of Low Income Outmigration</br>2007 - 2018",
        opacity = 1,
        group = "Outmigration LI Study Areas",
        layerId = "outmig_li_legend"
      ) %>%
        
        ## Add Outmigration LI legend ##################################
      addLegend(
        "bottomleft",
        pal = pal_outmig_all,
        values = full_area_info$outmigration_LI_mean,
        title = "Average Rate of Low Income Outmigration</br>2007 - 2018",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(outmig_labs)
          },
        opacity = 1,
        group = "Outmigration LI Study Areas",
        layerId = "outmig_li_legend2"
      ) 
    }
    
    ## Add Outmigration Renters Layers ####
    else if (input$study_data == "Renter Outmigration") {
      proxy %>%
        # Reset controls (need to do one by one)
        removeControl("noah_legend_clear") %>% 
        removeControl("noah_legend_clear2") %>% 
        removeControl("outmig_all_legend") %>% 
        removeControl("outmig_all_legend2") %>% 
        removeControl("outmig_li_legend") %>% 
        removeControl("outmig_li_legend2") %>% 
        removeControl("outmig_r_legend") %>% 
        removeControl("outmig_r_legend2") %>% 
        removeControl("outmig_li_r_legend") %>% 
        removeControl("outmig_li_r_legend2") %>%
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            noah_clear,
            outmig_all_clear,
            outmig_li_clear,
            outmig_r_clear,
            outmig_li_r_clear
          )
        ) %>% 
        
        ## add Renter Outmigration study areas polygon ####################
      addPolygons(
        # set outmigration data
        data = total_stats_geo,
        # set popups for geoid, data, investment type
        # Set GEOID names and NOAH percentages
        popup = outmigration_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_outmig_all(outmigration_r_mean),
        group = "Outmigration Renter Study Areas",
        layerId = outmig_r_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add average area outmigration renter legend###################
      addLegend(
        pal = pal_area_outmigration_r,
        "bottomleft",
        values = 
          unite(
            total_stats_geo %>%
              group_by(location) %>%
              summarise(
                outmig_change = 
                  scales::percent(mean(outmigration_r_mean, na.rm = TRUE), 4)
              ),
            "location_change",
            location,
            outmig_change,
            sep = ": "
          ) %>% 
          st_drop_geometry() %>% 
          mutate(location_change = str_replace(location_change, "_", " ")) %>% 
          pull(),
        title = "Average Rate of Renter Outmigration</br>2007 - 2018",
        opacity = 1,
        group = "Outmigration Renter Study Areas",
        layerId = "outmig_r_legend"
      ) %>%
        
        ## Add Outmigration Renter legend ###################################
      addLegend(
        "bottomleft",
        pal = pal_outmig_all,
        values = full_area_info$outmigration_r_mean,
        title = "Average Rate of Renter Outmigration</br>2007 - 2018",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(outmig_labs)
          },
        opacity = 1,
        group = "Outmigration Renter Study Areas",
        layerId = "outmig_r_legend2"
      ) 
    }
    
    ## Add Outmigration Low Income Renters Layers ####
    else if (input$study_data == "Low Income Renter Outmigration") {
      proxy %>%
        # Reset controls (need to do one by one)
        removeControl("noah_legend_clear") %>% 
        removeControl("noah_legend_clear2") %>% 
        removeControl("outmig_all_legend") %>% 
        removeControl("outmig_all_legend2") %>% 
        removeControl("outmig_li_legend") %>% 
        removeControl("outmig_li_legend2") %>% 
        removeControl("outmig_r_legend") %>% 
        removeControl("outmig_r_legend2") %>% 
        removeControl("outmig_li_r_legend") %>% 
        removeControl("outmig_li_r_legend2") %>%
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            noah_clear,
            outmig_all_clear,
            outmig_li_clear,
            outmig_r_clear,
            outmig_li_r_clear
          )
        ) %>% 
        
        ## add LI Renter Outmigration study areas polygon ######################
      addPolygons(
        # set outmigration data
        data = total_stats_geo,
        # set popups for geoid, data, investment type
        # Set GEOID names and NOAH percentages
        popup = outmigration_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_outmig_all(outmigration_LI_r_mean),
        group = "Outmigration LI Renter Study Areas",
        layerId = outmig_li_r_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add average area outmigration LI renter legend###################
      addLegend(
        pal = pal_area_outmigration_li_r,
        "bottomleft",
        values = 
          unite(
            total_stats_geo %>%
              group_by(location) %>%
              summarise(
                outmig_change = 
                  scales::percent(mean(outmigration_LI_r_mean, na.rm = TRUE), 4)
              ),
            "location_change",
            location,
            outmig_change,
            sep = ": "
          ) %>% 
          st_drop_geometry() %>% 
          mutate(location_change = str_replace(location_change, "_", " ")) %>% 
          pull(),
        title = "Average Rate of Low Income Renter Outmigration</br>2007 - 2018",
        opacity = 1,
        group = "Outmigration LI Renter Study Areas",
        layerId = "outmig_li_r_legend"
      ) %>%
        
        ## Add Outmigration LI Renter legend #####################################
      addLegend(
        "bottomleft",
        pal = pal_outmig_all,
        values = full_area_info$outmigration_LI_r_mean,
        title = "Average Rate of Low Income Renter Outmigration</br>2007 - 2018",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(outmig_labs)
          },
        opacity = 1,
        group = "Outmigration LI Renter Study Areas",
        layerId = "outmig_li_r_legend2"
      ) 
    }
  })
  
  # Add matching data layers ####
  observe({
    
    # Apply location filter
    full_area_info <- full_data_react()
    
    #### Clear map if "None" selected ####
    if (input$full_data == "None") {
      leafletProxy("mymap") %>% 
        # Reset controls (need to do one by one)
        removeControl("nw_legend") %>% 
        removeControl("college_legend") %>% 
        removeControl("renter_legend") %>% 
        removeControl("med_rent_legend") %>% 
        removeControl("med_inc_legend") %>% 
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            nw_clear,
            college_clear,
            renter_clear,
            med_rent_clear,
            med_inc_clear
          )
        )
    } 
    
    #### Add Non-white pop layers ####
    else if (input$full_data == "Non-white Population") {
      leafletProxy("mymap") %>%
        # Reset controls (need to do one by one)
        removeControl("nw_legend") %>% 
        removeControl("college_legend") %>% 
        removeControl("renter_legend") %>% 
        removeControl("med_rent_legend") %>% 
        removeControl("med_inc_legend") %>% 
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            nw_clear,
            college_clear,
            renter_clear,
            med_rent_clear,
            med_inc_clear
          )
        ) %>% 
        
        ## Add Non-white population ##############################
      addPolygons(
        # set population data
        data = full_area_info,
        # set popups for geoid, data
        # Set GEOID names and NOAH percentages
        popup = acs_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_nonwhi(pct_nonwhi_09),
        group = "Non-white Population",
        layerId = nw_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>% 
        
        ## Add Non-white population legend ###############################
      addLegend(
        "bottomleft",
        pal = pal_nonwhi,
        values = full_area_info$pct_nonwhi_09,
        title = "Percent Non-white Population 2009",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(nonwhi_labs)
          },
        opacity = 1,
        group = "Non-white Population",
        layerId = "nw_legend"
      )
    }
    
    #### Add college educated pop layers ####
    else if (input$full_data == "College Educated Population") {
      leafletProxy("mymap") %>%
        # Reset controls (need to do one by one)
        removeControl("nw_legend") %>% 
        removeControl("college_legend") %>% 
        removeControl("renter_legend") %>% 
        removeControl("med_rent_legend") %>% 
        removeControl("med_inc_legend") %>% 
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            nw_clear,
            college_clear,
            renter_clear,
            med_rent_clear,
            med_inc_clear
          )
        ) %>% 
        
        ## Add College population #############################
      addPolygons(
        # set population data
        data = full_area_info,
        # set popups for geoid, data
        # Set GEOID names and NOAH percentages
        popup = acs_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_college(pct_college_09),
        group = "College Educated Population",
        layerId = college_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add college population legend ################################
      addLegend(
        "bottomleft",
        pal = pal_college,
        values = full_area_info$pct_college_09,
        title = "Percent College-Educated Population 2009",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(college_labs)
          },
        opacity = 1,
        group = "College Educated Population",
        layerId = "college_legend"
      ) 
    }
    
    #### Add renter occupied housing layers ####
    else if (input$full_data == "Renter-Occupied Housing") {
      leafletProxy("mymap") %>%
        # Reset controls (need to do one by one)
        removeControl("nw_legend") %>% 
        removeControl("college_legend") %>% 
        removeControl("renter_legend") %>% 
        removeControl("med_rent_legend") %>% 
        removeControl("med_inc_legend") %>% 
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            nw_clear,
            college_clear,
            renter_clear,
            med_rent_clear,
            med_inc_clear
          )
        ) %>% 
        
        ## Add renter-occupied housing ##########################
      addPolygons(
        # set population data
        data = full_area_info,
        # set popups for geoid, data
        # Set GEOID names and NOAH percentages
        popup = acs_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_renter(pct_h_rent_09),
        group = "Renter-Occupied Housing",
        layerId = renter_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add renter occupied legend ###############################
      addLegend(
        "bottomleft",
        pal = pal_renter,
        values = full_area_info$pct_h_rent_09,
        title = "Percent Renter-Occupied Housing 2009",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(renter_labs)
          },
        opacity = 1,
        group = "Renter-Occupied Housing",
        layerId = "renter_legend"
      )
    }
    
    #### Add median rent layers ####
    else if (input$full_data == "Median Rent") {
      leafletProxy("mymap") %>%
        # Reset controls (need to do one by one)
        removeControl("nw_legend") %>% 
        removeControl("college_legend") %>% 
        removeControl("renter_legend") %>% 
        removeControl("med_rent_legend") %>% 
        removeControl("med_inc_legend") %>% 
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            nw_clear,
            college_clear,
            renter_clear,
            med_rent_clear,
            med_inc_clear
          )
        ) %>% 
        
        ## Add median rent ##################################
      addPolygons(
        # set population data
        data = full_area_info,
        # set popups for geoid, data
        # Set GEOID names and NOAH percentages
        popup = acs_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_medrent(med_rent_09),
        group = "Median Rent",
        layerId = med_rent_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add median rent legend ##################################
      addLegend(
        "bottomleft",
        pal = pal_medrent,
        values = full_area_info$med_rent_09,
        title = "Median Rent 2009",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(medrent_labs)
          },
        opacity = 1,
        group = "Median Rent",
        layerId = "med_rent_legend"
      ) 
    }
    
    #### Add median income layers ####
    else if (input$full_data == "Median Income") {
      leafletProxy("mymap") %>%
        # Reset controls (need to do one by one)
        removeControl("nw_legend") %>% 
        removeControl("college_legend") %>% 
        removeControl("renter_legend") %>% 
        removeControl("med_rent_legend") %>% 
        removeControl("med_inc_legend") %>% 
        
        # Reset shapes (remove NOAH/Outmigration variants)
        removeShape(
          c(
            nw_clear,
            college_clear,
            renter_clear,
            med_rent_clear,
            med_inc_clear
          )
        ) %>% 
        
        ## Add median income ##############################
      addPolygons(
        # set population data
        data = full_area_info,
        # set popups for geoid, data
        # Set GEOID names and NOAH percentages
        popup = acs_popup,
        smoothFactor = 0,
        fillOpacity = .7,
        weight = 0,
        color = ~pal_medinc(med_inc_09),
        group = "Median Income",
        layerId = med_inc_clear,
        # bring shape to front on hover
        highlightOptions = highlightOptions(opacity = 0, bringToFront = FALSE)
      ) %>%
        
        ## Add median income legend ##################################
      addLegend(
        "bottomleft",
        pal = pal_medinc,
        values = full_area_info$med_inc_09,
        title = "Median Income 2009",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(medinc_labs)
          },
        opacity = 1,
        group = "Median Income",
        layerId = "med_inc_legend"
      )
    }
  })
  
  # Add investment marker checkbox ####
  observe({
    # Create proxy map
    proxy <- leafletProxy("mymap")
    #marker_area <- marker_react()
    
    if (input$Investments) {
      # filter by location
      ## ALL locations
      if(setequal(marker_react(), c("SF_Bay", "Fresno", "LA"))) {
        proxy %>%
          clearMarkerClusters() %>%
          removeShape(invest_list) %>%
          
          # add SF
          sf_markers() %>%
          #add Fresno
          fresno_markers() %>%
          # add LA
          la_markers()
        
        ## JUST SF AND FRESNO
      } else if (setequal(marker_react(), c("SF_Bay", "Fresno"))) {
        proxy %>%
          clearMarkerClusters() %>%
          removeShape(invest_list) %>%
          
          # add SF
          sf_markers() %>%
          #add Fresno
          fresno_markers()
        
        ## JUST SF AND LA
      } else if (setequal(marker_react(), c("SF_Bay", "LA"))) {
        proxy %>%
          clearMarkerClusters() %>%
          removeShape(invest_list) %>%
          
          # add SF
          sf_markers() %>%
          # add LA
          la_markers()
        
        ## JUST FRESNO AND LA
      } else if (setequal(marker_react(), c("Fresno", "LA"))) {
        proxy %>%
          clearMarkerClusters() %>%
          removeShape(invest_list) %>%
          
          # add Fresno
          fresno_markers() %>%
          # add LA
          la_markers()
        
        ## JUST SF
      } else if (setequal(marker_react(), "SF_Bay")) {
        proxy %>%
          clearMarkerClusters() %>% 
          removeShape(invest_list) %>%
          
          # add SF
          sf_markers()
        
        ## JUST FRESNO
      } else if (setequal(marker_react(), "Fresno")) {
        proxy %>%
          clearMarkerClusters() %>% 
          removeShape(invest_list) %>%
          
          # add Fresno
          fresno_markers()
        
        ## JUST LA
      } else if (setequal(marker_react(), "LA")) {
        proxy %>%
          clearMarkerClusters() %>% 
          removeShape(invest_list) %>%
          
          # add LA
          la_markers()
      }
    }
    
    else {
      leafletProxy("mymap") %>%
        clearMarkerClusters() %>%
        removeShape(invest_list)
    }
    
  })
  
  
  # Add investment tract checkbox ####
  observe({
    # Apply location filter
    paired_geoids <- paired_geoids_react()
    total_stats_geo <- study_data_react()
    
    # Create proxy map
    proxy <- leafletProxy("mymap")
    
    if (input$invest_tracts) {
      proxy %>%
        
        ## add investment boundaries ###############################
      addPolylines(
        data = total_stats_geo,
        # set investment borders
        stroke = TRUE,
        color = ~ invest_pal(investment),
        opacity = 1,
        fillColor = NULL,
        fillOpacity = 0,
        weight = 1.5,
        layerId = tract_clear,
        group = "invest_tracts",
        popup =
          paste(
            # add tract's GEOID
            "Census Tract:", as.character(total_stats_geo$GEOID), "<br>",
            
            # Display investment flags by type
            "Investment Type: ",
            case_when(
              # all 4
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 1 ~
                "Greening, Transit, Urban Infill, Active Transportation",
              # 3 investment types
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 0 ~
                "Greening, Transit, Urban Infill",
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 1 ~
                "Greening, Transit, Active Transportation",
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 1 ~
                "Greening, Urban Infill, Active Transportation",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 1 ~
                "Transit, Urban Infill, Active Transportation",
              # 2 investment types
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 0 ~
                "Greening, Transit",
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 0 ~
                "Greening, Urban Infill",
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 1 ~
                "Greening, Active Transportation",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 0 ~
                "Transit, Urban Infill",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 1 ~
                "Transit, Active Transportation",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 1 ~
                "Urban Infill, Active Transportation",
              # single investments
              total_stats_geo$greening == 1 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 0 ~
                "Greening",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 1 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 0 ~
                "Transit",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 1 &
                total_stats_geo$active_transportation == 0 ~
                "Urban Infill",
              total_stats_geo$greening == 0 &
                total_stats_geo$transit == 0 &
                total_stats_geo$urban_infill == 0 &
                total_stats_geo$active_transportation == 1 ~
                "Active Transportation",
              TRUE ~ "No Investments"
            )
          ),
        highlightOptions = highlightOptions(weight = 4, bringToFront = TRUE)
      ) %>%
        ## add investment pairs
        addPolygons(
          data = paired_geoids,
          # set investment borders
          stroke = TRUE,
          color = NA,
          opacity = 1,
          fillColor = "white",
          fillOpacity = .1,
          weight = 0,
          layerId = tract_pair_clear,
          group = "invest_tracts",
          popup =
            paste(
              # add investment tract's GEOID
              "Investment Tract:", as.character(paired_geoids$GEOID), "<br>",
              # add paired tract
              "Non-Investment Tract:", as.character(paired_geoids$geoid_pair)
            ),
          highlightOptions = highlightOptions(
            fillColor = "green",
            fillOpacity = .8,
            stroke = 4,
            weight = 0,
            bringToFront = TRUE
          )
        ) %>%
        
        ## Add Investment Flag legend #####
      addLegend(
        "bottomright",
        pal = invest_pal,
        values = total_stats_geo$investment,
        title = "Paired Tracts",
        labFormat =
          function(type, cuts, p) {  # Here's the trick
            paste0(invest_labs)
          },
        opacity = 1,
        layerId = "legend_clear",
        group = "invest_tracts"
      )
    }
    
    else {
      proxy %>%
        removeShape(tract_clear) %>%
        removeShape(tract_pair_clear) %>% 
        removeControl("legend_clear")
    }
  })
  
}


# Run shiny app ----------------------------------------------------------------------
shinyApp(ui, server)

