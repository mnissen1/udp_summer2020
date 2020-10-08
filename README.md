# Urban Displacement Project Work Summary
This repo contains work done for the Urban Displacement Project (UDP) during the summer of 2020. 

UDP is, in their own words, "a research and action initiative of UC Berkeley. UDP conducts community-centered, data-driven, applied research toward more equitable and inclusive futures for cities. Our research aims to understand and describe the nature of gentrification and displacement, and also to generate knowledge on how policy interventions and investment can respond and support more equitable development." You can find out more about them and their work [here](https://www.urbandisplacement.org/).

My work for UDP contributed to their project with the [Strategic Growth Council](https://sgc.ca.gov/). Focusing on Los Angeles, the San Francisco Bay Area, and Fresno, the project aims to understand the unintended impacts of climate change related investments on neighborhood displacement, and how to best mitigate adverse effects. The end result of this project will be a tool to predict investment-related displacement. The entire project summary can be found [here](https://www.urbandisplacement.org/current-projects#section-191), along with other ongoing UDP projects. 

I was responsible for matching neighborhoods with investments to neighborhoods without investments, in order to appropriately study the impact of climate change mitigation investments. This process made use of the propensity score matching technique and studied outcomes with both full OLS regressions and fixed effects regressions. Finally, I also produced an interactive map demonstrating the selection process and visualizing the outcomes. Impacts were approximated using outmigration measures from InfoGroup (now [Data Axle](https://www.data-axle.com/)) and UDP's Naturally Occuring Affordable Housing (NOAH) dataset.

# Propensity Score Matching
The results of the propensity score matching (PSM) can be found [here](https://github.com/mnissen1/udp_summer2020/tree/master/writeups/sgc_psm_writeup.pdf).

PSM is a statistical matching technique that matches control and treatment observations based on similar "propensity scores", which is a balancing score derived from selected covariates. Covariates in this case were selected after running through the process multiple times and selecting the grouping with the lowest Average Absolute Standardized Difference. Goodness-of-fit for these results can be seen [here](https://github.com/mnissen1/udp_summer2020/tree/master/visualizations/psm_tests).

# Full Regressions
The results of the full regression models can be found [here](https://mnissen1.github.io/udp_summer2020/writeups/sgc_reg_full.html). They are still a work in progress, especially the fixed effects models. These will be updated once a more definite "beginning year" is determined for each variable.

There are both OLS and fixed effects models in the writeup, and at various geographic levels broken down by investment type.

# Map
An interactive geospatial representation of the matching process and results is produced as a Shiny app using Leaflet. The code to produce the Shiny app is available in this repo, and the app itself can be accessed [here](https://matt-nissen.shinyapps.io/SGC_Matched_Neighborhoods_Map/?_ga=2.46349092.2031182488.1602192102-2050714689.1599855133).

Screenshot of the interactive map showcasing some features:
![Map Screenshot](https://github.com/mnissen1/udp_summer2020/blob/master/visualizations/map_screenshot.png)

# Credits
* R packages used in PSM: `tidyverse`, `MatchIt`, `gridExtra`, `tableone`, `broom`, `infer`, `glue`, `tidycensus`, `tigris`, `sf`, `knitr`, `leaps`, `kableExtra`, `gridExtra`, `magick`
* R packages used in regressions: `tidyverse`, `glue`, `stargazer`
* R packages used in mapping: `tidyverse`, `sf`, `sp`, `tigris`, `htmlwidgets`, `leaflet`, `shiny`, `shinycssloaders`
