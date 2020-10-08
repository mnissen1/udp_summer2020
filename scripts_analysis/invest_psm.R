# -------------------------------------------------------------------------
# Created by: Matt Alvarez-Nissen                         
# Date created: Jun. 25, 2020                
# Last revised: Oct. 7, 2020                 
# Project: UDP SGC     
# Subproject: Propensity Score Matching
# Re: Carry out Propensity Score Matching and generate matched pairs
# -------------------------------------------------------------------------

# Script Description ------------------------------------------------------

# This script takes the master investments and traits CSV, conducts PSM, and
# produces matched pairs for each study area (LA, SF Bay, and Fresno). It relies 
# on source functions provided by psm_funcs.R.

# Inputs:
# Master investment/traits CSV
# Cov table once created

# Outputs:
# Table of potential covariates and their outcomes (cov table)
# Three separate tables of matched pairs by study area

# Update log: 
# 10/6/20 - converted from RMarkdown to R script for faster processing.

# Setup -------------------------------------------------------------------

# Packages: 
library(tidyverse)
library(MatchIt)
library(gridExtra)
library(tableone)
library(broom)
library(infer)

# Directories: 
homedir <- "E:/udp_summer2020/"
workdir <- "sgc_data/raw/"
savedir <- "sgc_data/cleaned/"
setwd(homedir)

# Import data:
# Master data table
master_investments_traits <-
  read_csv(paste0(homedir, savedir, "master_investments_traits.csv"))

# covariate table
cov_table <-
  read_csv(paste0(homedir, savedir, "cov_table.csv"))

# Parameters:
# Source functions
source(paste0(homedir, "scripts_analysis/psm_funcs.R"))

# Main Script ------------------------------------------------------------------

# Filter by location
sf_neighborhood <-
  master_investments_traits %>% 
  filter(location == "SF_Bay")

la_neighborhood <-
  master_investments_traits %>% 
  filter(location == "LA")

fresno_neighborhood <-
  master_investments_traits %>% 
  filter(location == "Fresno")

# Identify covariates (skip if already done)------------------------------------
# create covariate table to test for lowest absolute standard difference
# cov_table <-
#   tibble(
#     case = 
#       c(
#         "base_case", "base_case_plus", "raw", "raw_plus", 
#         "pct_pnt", "pct_pnt_plus", "pct", "pct_plus", "base_raw", 
#         "base_raw_plus", "base_pct_pnt", "base_pct_pnt_plus", "base_pct", 
#         "base_pct_plus"
#       ),
#     cov =
#       list(
#         # base case
#         c(
#           "pct_nonwhi_09", "med_inc_09", "pct_h_rent_09", "med_rent_09", 
#           "pct_college_09"
#         ),
#         # base case plus
#         c(
#           "pct_nonwhi_09", "pct_pov_09", "med_inc_09", "pct_housing_vac_09", 
#           "pct_h_rent_09", "med_rent_09", "pct_college_09"
#         ),
#         # raw
#         c(
#           "pct_pnt_nonwhi_chng", "med_inc_chng", "pct_pnt_h_rent_chng", 
#           "med_rent_chng", "pct_pnt_college_chng"
#         ),
#         # raw plus
#         c(
#           "pct_pnt_nonwhi_chng", "pct_pnt_pov_chng", "med_inc_chng", 
#           "pct_pnt_vac_chng", "pct_pnt_h_rent_chng", "med_rent_chng", 
#           "pct_pnt_college_chng"
#         ),
#         # pct pnt
#         c(
#           "pct_pnt_nonwhi_chng", "pct_med_inc_chng", "pct_pnt_h_rent_chng",
#           "pct_med_rent_chng", "pct_pnt_college_chng"
#         ),
#         # pct pnt plus
#         c(
#           "pct_pnt_nonwhi_chng", "pct_pnt_pov_chng", "pct_med_inc_chng", 
#           "pct_pnt_vac_chng", "pct_pnt_h_rent_chng", "pct_med_rent_chng", 
#           "pct_pnt_college_chng"
#         ),
#         # pct
#         c(
#           "pct_pop_nonwhi_chng", "pct_med_inc_chng", "pct_h_rent_chng",
#           "pct_med_rent_chng", "pct_pop_college_chng"
#         ),
#         # pct plus
#         c(
#           "pct_pop_nonwhi_chng", "pct_pop_pov_chng", "pct_med_inc_chng",
#           "pct_vac_chng", "pct_h_rent_chng", "pct_med_rent_chng", 
#           "pct_pop_college_chng"
#         ),
#         # base raw
#         c(
#           "pct_nonwhi_09", "med_inc_09", "pct_h_rent_09", "med_rent_09", 
#           "pct_college_09", "pct_pnt_nonwhi_chng", "med_inc_chng", 
#           "pct_pnt_h_rent_chng", "med_rent_chng", "pct_pnt_college_chng"
#         ),
#         # base raw plus
#         c(
#           "pct_nonwhi_09", "pct_pov_09", "med_inc_09", "pct_housing_vac_09", 
#           "pct_h_rent_09", "med_rent_09", "pct_college_09", 
#           "pct_pnt_nonwhi_chng", "pct_pnt_pov_chng", "med_inc_chng", 
#           "pct_pnt_vac_chng", "pct_pnt_h_rent_chng", "med_rent_chng", 
#           "pct_pnt_college_chng"
#         ),
#         # base pct pnt
#         c(
#           "pct_nonwhi_09", "med_inc_09", "pct_h_rent_09", "med_rent_09", 
#           "pct_college_09", "pct_pnt_nonwhi_chng", "pct_med_inc_chng", 
#           "pct_pnt_h_rent_chng", "pct_med_rent_chng", "pct_pnt_college_chng"
#         ),
#         # base pct pnt plus
#         c(
#           "pct_nonwhi_09", "pct_pov_09", "med_inc_09", "pct_housing_vac_09", 
#           "pct_h_rent_09", "med_rent_09", "pct_college_09",
#           "pct_pnt_nonwhi_chng", "pct_pnt_pov_chng", "pct_med_inc_chng", 
#           "pct_pnt_vac_chng", "pct_pnt_h_rent_chng", "pct_med_rent_chng", 
#           "pct_pnt_college_chng"
#         ),
#         # base pct
#         c(
#           "pct_nonwhi_09", "med_inc_09", "pct_h_rent_09", "med_rent_09", 
#           "pct_college_09", "pct_pop_nonwhi_chng", "pct_med_inc_chng", 
#           "pct_h_rent_chng", "pct_med_rent_chng", "pct_pop_college_chng"
#         ),
#         # base pct plus
#         c(
#           "pct_nonwhi_09", "pct_pov_09", "med_inc_09", "pct_housing_vac_09", 
#           "pct_h_rent_09", "med_rent_09", "pct_college_09",
#           "pct_pop_nonwhi_chng", "pct_pop_pov_chng", "pct_med_inc_chng", 
#           "pct_vac_chng", "pct_h_rent_chng", "pct_med_rent_chng", 
#           "pct_pop_college_chng"
#         )
#       )
#   )
# 
# 
# # Find the Mean Absolute Standard Difference across regions
# # (to determine the best overall covariate balance)
# 
# ## Create helper to find AASD for each pairing
# abs_std_diff_finder <- function(data, cov) {
#   # Run PSM
#   matched_table <- psm_results(data, cov)
#   
#   # Find AASD from PSM, print as double
#   aasd(matched_table, cov)
# }
# 
# 
# # Generate cov_table
# # create a list of results for each covariate grouping for LA
# diffs <- c()
# for (i in 1:as.double(count(cov_table))) {
#   diffs <-
#     rbind(diffs, abs_std_diff_finder(la_neighborhood, cov_table$cov[[i]]))
# }
# 
# # add results on to cov_table
# cov_table <-
#   cov_table %>%
#   bind_cols(diffs) %>%
#   rename(abs_std_diff_la = n)
# 
# # Repeat the process for SF Bay
# diffs <- c()
# for (i in 1:as.double(count(cov_table))) {
#   diffs <-
#     rbind(diffs, abs_std_diff_finder(sf_neighborhood, cov_table$cov[[i]]))
# }
# 
# # add results on to cov_table
# cov_table <-
#   cov_table %>%
#   bind_cols(diffs) %>%
#   rename(abs_std_diff_sf = n)
# 
# # Repeat the process for Fresno
# diffs <- c()
# for (i in 1:as.double(count(cov_table))) {
#   diffs <-
#     rbind(diffs, abs_std_diff_finder(fresno_neighborhood, cov_table$cov[[i]]))
# }
# 
# # add results on to cov_table
# cov_table <-
#   cov_table %>%
#   bind_cols(diffs) %>%
#   rename(abs_std_diff_fresno = n)
# 
# # Convert cov column to character
# cov_table <-
#   cov_table %>%
#   mutate(
#     cov = as.character(cov),
#     cov = str_remove(cov, "^c\\("),
#     cov = str_remove(cov, "\\)$")
#   )

# Find best covariate balance (resume here if cov_table available) -------------
# Calculate mean ASD and find the minimum (best covariate balance)
cov_table %>% 
  mutate(
    mean_asd = (abs_std_diff_la + abs_std_diff_sf + abs_std_diff_fresno) / 3 
  ) %>% 
  arrange(mean_asd) %>% 
  select(case, mean_asd, everything()) %>% 
  # select the top 5 cases
  head(5)

# The "raw" covariate sample produces on average the lowest ASD
## The next, from smallest to largest, are base_case_plus, pct, base_pct_pnt, 
## & base_raw

# Extract the list of covariates for use in further analysis
## Use base case plus
neighborhood_cov <-
  cov_table %>% 
  # filter to "raw" (or covariate list of choice)
  filter(case == "base_case") %>% 
  mutate(cov = str_remove_all(cov, '\\"')) %>% 
  # select for list of covariates
  select(cov) %>% 
  pull() 
  
neighborhood_cov <-
  # Convert to proper form for analysis
  unlist(strsplit(neighborhood_cov, split = ", "))

# Drop unneeded objects
rm(master_investments_traits)

# LOS ANGELES -----------------------------------------------------------------

# Run PSM function
psm_results_la <- psm_results(la_neighborhood, neighborhood_cov)

# Investigate PSM results
# Print area of common support plot
common_support_la <- common_support_plot(la_neighborhood, neighborhood_cov)

# Print covariate balance plots
cov_balance_la <- covariate_balance_plot(psm_results_la, neighborhood_cov)

# Clean PSM results
psm_results_la_clean <-
  psm_results_la %>% 
  # turn row name into a column
  rownames_to_column() %>% 
  # match on original df
  inner_join(la_neighborhood) %>% 
  # move around the order of the df
  select(GEOID:n_invest, everything()) %>% 
  select(GEOID, investment, distance, everything()) %>% 
  # arrange by distance (i.e. propensity score) to put matched pairs together
  arrange(distance)

# remove unneeded objects 
rm(psm_results_la, la_neighborhood)

# SF BAY AREA -----------------------------------------------------------------

# Run PSM function
psm_results_sf <- psm_results(sf_neighborhood, neighborhood_cov)

# Investigate PSM results
# Print area of common support plot
common_support_sf <- common_support_plot(sf_neighborhood, neighborhood_cov)

# Print covariate balance plots
cov_balance_sf <- covariate_balance_plot(psm_results_sf, neighborhood_cov)

# Clean PSM results
psm_results_sf_clean <-
  psm_results_sf %>% 
  # turn row name into a column
  rownames_to_column() %>% 
  # match on original df
  inner_join(sf_neighborhood) %>% 
  # move around the order of the df
  select(GEOID:n_invest, everything()) %>% 
  select(GEOID, investment, distance, everything()) %>% 
  # arrange by distance (i.e. propensity score) to put matched pairs together
  arrange(distance)

# remove uncleaned
rm(psm_results_sf, sf_neighborhood)

# FRESNO -----------------------------------------------------------------------

# Run PSM function
psm_results_fresno <- psm_results(fresno_neighborhood, neighborhood_cov)

# Investigate PSM results
# Print area of common support plot
common_support_fresno <- 
  common_support_plot(fresno_neighborhood, neighborhood_cov)

# Print covariate balance plots
cov_balance_fresno <-
  covariate_balance_plot(psm_results_fresno, neighborhood_cov)

# Clean PSM results
psm_results_fresno_clean <-
  psm_results_fresno %>% 
  # turn row name into a column
  rownames_to_column() %>% 
  # match on original df
  inner_join(fresno_neighborhood) %>% 
  # move around the order of the df
  select(GEOID:n_invest, everything()) %>% 
  select(GEOID, investment, distance, everything()) %>% 
  # arrange by distance (i.e. propensity score) to put matched pairs together
  arrange(distance)

# remove unneeded objects
rm(psm_results_fresno, fresno_neighborhood)

# Save Results ------------------------------------------------------------
# Write cov_table 
write_csv(cov_table, paste0(homedir, savedir, "cov_table.csv"))

# Write matched results
write_csv(psm_results_la_clean, paste0(homedir, savedir, "psm_matched_la.csv"))
write_csv(psm_results_sf_clean, paste0(homedir, savedir, "psm_matched_sf.csv"))
write_csv(
  psm_results_fresno_clean, paste0(homedir, savedir, "psm_matched_fresno.csv")
)

# Save plots
## Common support plots
ggsave(
  plot = common_support_la, 
  filename = paste0(homedir, "visualizations/psm/common_support_la.png")
)
ggsave(
  plot = common_support_sf, 
  filename = paste0(homedir, "visualizations/psm/common_support_sf.png")
)
ggsave(
  plot = common_support_fresno, 
  filename = paste0(homedir, "visualizations/psm/common_support_fresno.png")
)

## Covariate balance plots
ggsave(
  plot = cov_balance_la, 
  filename = paste0(homedir, "visualizations/psm/covariate_balance_la.png")
)
ggsave(
  plot = cov_balance_sf, 
  filename = paste0(homedir, "visualizations/psm/covariate_balance_sf.png")
)
ggsave(
  plot = cov_balance_fresno, 
  filename = paste0(homedir, "visualizations/psm/covariate_balance_fresno.png")
)

# Clean up environment
rm(list = ls())
