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

# Directories: 
homedir <- "E:/udp_summer2020/"
workdir <- "sgc_data/raw/"
savedir <- "sgc_data/cleaned/"
spatial_dir <- "Spatial Data 06_26_2020/"
setwd(homedir)

# Import data:

# Main Script ------------------------------------------------------------------

# Save Results ------------------------------------------------------------


# Clean up environment
rm(list = ls())
