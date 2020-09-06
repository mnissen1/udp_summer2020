### Helper Functions for PSM Descriptive Stats

# DATA CLEANING FUNCTIONS
## Join investment, NOAH, and outmigration data
area_psm <- function(psm_df) {
  
  total_psm <-
    psm_df %>% 
    # join with 2016 data
    left_join(noah_16, by = "GEOID") %>%
    # clean joined data
    select(-c(d_bay:d_fresno)) %>% 
    rename(
      noah_tot_clean_16 = noah_tot_clean, 
      noah_nolihtc_clean_16 = noah_nolihtc_clean,
      noah_housing_tot_16 = noah_housing_tot
    ) %>% 
    # join with crosswalked 2009 data
    left_join(noah_09_xwalk, by = "GEOID") %>% 
    # find diff in NOAH units
    mutate(
      noah_tot_change = noah_tot_clean_16 - noah_tot_clean_09,
      noah_nolihtc_change = noah_nolihtc_clean_16 - noah_nolihtc_clean_09,
      perc_noah_tot_change = noah_tot_change / noah_tot_clean_09,
      perc_noah_nolihtc_change = noah_nolihtc_change / noah_nolihtc_clean_09
    ) %>% 
    # clean up the data
    select(-c(rowname, pct_pnt_nonwhi_chng:pct_pop_college_chng)) %>% 
    # convert Inf to NA
    mutate(across(.cols = everything(), na_if, Inf)) %>% 
    # join with outmigration data
    left_join(outmigration_tract, by = c("GEOID" = "tract")) 
  
  return(total_psm)
}

## Generate descriptive statistics
area_stats <- function(area_psm) {
  
  total_stats <-
    area_psm %>% 
    # group by investment flag
    group_by(investment) %>% 
    # sum up all the columns to get totals by investment flag
    summarize(
      across(
        .cols = 
          c(
            noah_housing_tot_16:noah_nolihtc_change, 
            num_out_2007:num_fams_2017,
            num_out_LI_2007:num_fams_LI_2017,
            num_out_r_2007:num_fams_r_2017,
            num_out_LI_r_2007:num_fams_LI_r_2017
          ),
        sum,
        na.rm = TRUE
      )
    ) %>% 
    ungroup() %>% 
    # create percent change/rate variables
    mutate(
      perc_noah_tot_change = noah_tot_change / noah_tot_clean_09,
      perc_noah_nolihtc_change = noah_nolihtc_change / noah_nolihtc_clean_09,
      # all outmigration
      outmigration_all_2007 = num_out_2007 / num_fams_2006,
      outmigration_all_2008 = num_out_2008 / num_fams_2007,
      outmigration_all_2009 = num_out_2009 / num_fams_2008,
      outmigration_all_2010 = num_out_2010 / num_fams_2009,
      outmigration_all_2011 = num_out_2011 / num_fams_2010,
      outmigration_all_2012 = num_out_2012 / num_fams_2011,
      outmigration_all_2013 = num_out_2013 / num_fams_2012,
      outmigration_all_2014 = num_out_2014 / num_fams_2013,
      outmigration_all_2015 = num_out_2015 / num_fams_2014,
      outmigration_all_2016 = num_out_2016 / num_fams_2015,
      outmigration_all_2017 = num_out_2017 / num_fams_2016,
      outmigration_all_2018 = num_out_2018 / num_fams_2017,
      # LI outmigration
      outmigration_LI_2007 = num_out_LI_2007 / num_fams_LI_2006,
      outmigration_LI_2008 = num_out_LI_2008 / num_fams_LI_2007,
      outmigration_LI_2009 = num_out_LI_2009 / num_fams_LI_2008,
      outmigration_LI_2010 = num_out_LI_2010 / num_fams_LI_2009,
      outmigration_LI_2011 = num_out_LI_2011 / num_fams_LI_2010,
      outmigration_LI_2012 = num_out_LI_2012 / num_fams_LI_2011,
      outmigration_LI_2013 = num_out_LI_2013 / num_fams_LI_2012,
      outmigration_LI_2014 = num_out_LI_2014 / num_fams_LI_2013,
      outmigration_LI_2015 = num_out_LI_2015 / num_fams_LI_2014,
      outmigration_LI_2016 = num_out_LI_2016 / num_fams_LI_2015,
      outmigration_LI_2017 = num_out_LI_2017 / num_fams_LI_2016,
      outmigration_LI_2018 = num_out_LI_2018 / num_fams_LI_2017,
      # renters outmigration
      outmigration_renters_2007 = num_out_r_2007 / num_fams_r_2006,
      outmigration_renters_2008 = num_out_r_2008 / num_fams_r_2007,
      outmigration_renters_2009 = num_out_r_2009 / num_fams_r_2008,
      outmigration_renters_2010 = num_out_r_2010 / num_fams_r_2009,
      outmigration_renters_2011 = num_out_r_2011 / num_fams_r_2010,
      outmigration_renters_2012 = num_out_r_2012 / num_fams_r_2011,
      outmigration_renters_2013 = num_out_r_2013 / num_fams_r_2012,
      outmigration_renters_2014 = num_out_r_2014 / num_fams_r_2013,
      outmigration_renters_2015 = num_out_r_2015 / num_fams_r_2014,
      outmigration_renters_2016 = num_out_r_2016 / num_fams_r_2015,
      outmigration_renters_2017 = num_out_r_2017 / num_fams_r_2016,
      outmigration_renters_2018 = num_out_r_2018 / num_fams_r_2017,
      # LI renters outmigration
      outmigration_LI_renters_2007 = num_out_LI_r_2007 / num_fams_LI_r_2006,
      outmigration_LI_renters_2008 = num_out_LI_r_2008 / num_fams_LI_r_2007,
      outmigration_LI_renters_2009 = num_out_LI_r_2009 / num_fams_LI_r_2008,
      outmigration_LI_renters_2010 = num_out_LI_r_2010 / num_fams_LI_r_2009,
      outmigration_LI_renters_2011 = num_out_LI_r_2011 / num_fams_LI_r_2010,
      outmigration_LI_renters_2012 = num_out_LI_r_2012 / num_fams_LI_r_2011,
      outmigration_LI_renters_2013 = num_out_LI_r_2013 / num_fams_LI_r_2012,
      outmigration_LI_renters_2014 = num_out_LI_r_2014 / num_fams_LI_r_2013,
      outmigration_LI_renters_2015 = num_out_LI_r_2015 / num_fams_LI_r_2014,
      outmigration_LI_renters_2016 = num_out_LI_r_2016 / num_fams_LI_r_2015,
      outmigration_LI_renters_2017 = num_out_LI_r_2017 / num_fams_LI_r_2016,
      outmigration_LI_renters_2018 = num_out_LI_r_2018 / num_fams_LI_r_2017
    ) 
  
  return(total_stats)
}

## Generate descriptive stats for intervention type
type_stats <- function(area_psm, investment_type) {
  investment_type <- enquo(investment_type)
  
  investment_stats <-
    area_psm %>% 
    # group by investment flags
    group_by(investment, !! investment_type) %>% 
    # sum up all the columns to get totals by investment flag
    summarize(
      across(
        .cols = 
          c(
            noah_housing_tot_16:noah_nolihtc_change, 
            num_out_2007:num_fams_2017,
            num_out_LI_2007:num_fams_LI_2017,
            num_out_r_2007:num_fams_r_2017,
            num_out_LI_r_2007:num_fams_LI_r_2017
          ),
        sum,
        na.rm = TRUE
      )
    ) %>% 
    ungroup() %>% 
    # create percent change/rate variables
    mutate(
      perc_noah_tot_change = noah_tot_change / noah_tot_clean_09,
      perc_noah_nolihtc_change = noah_nolihtc_change / noah_nolihtc_clean_09,
      # all outmigration
      outmigration_all_2007 = num_out_2007 / num_fams_2006,
      outmigration_all_2008 = num_out_2008 / num_fams_2007,
      outmigration_all_2009 = num_out_2009 / num_fams_2008,
      outmigration_all_2010 = num_out_2010 / num_fams_2009,
      outmigration_all_2011 = num_out_2011 / num_fams_2010,
      outmigration_all_2012 = num_out_2012 / num_fams_2011,
      outmigration_all_2013 = num_out_2013 / num_fams_2012,
      outmigration_all_2014 = num_out_2014 / num_fams_2013,
      outmigration_all_2015 = num_out_2015 / num_fams_2014,
      outmigration_all_2016 = num_out_2016 / num_fams_2015,
      outmigration_all_2017 = num_out_2017 / num_fams_2016,
      outmigration_all_2018 = num_out_2018 / num_fams_2017,
      # LI outmigration
      outmigration_LI_2007 = num_out_LI_2007 / num_fams_LI_2006,
      outmigration_LI_2008 = num_out_LI_2008 / num_fams_LI_2007,
      outmigration_LI_2009 = num_out_LI_2009 / num_fams_LI_2008,
      outmigration_LI_2010 = num_out_LI_2010 / num_fams_LI_2009,
      outmigration_LI_2011 = num_out_LI_2011 / num_fams_LI_2010,
      outmigration_LI_2012 = num_out_LI_2012 / num_fams_LI_2011,
      outmigration_LI_2013 = num_out_LI_2013 / num_fams_LI_2012,
      outmigration_LI_2014 = num_out_LI_2014 / num_fams_LI_2013,
      outmigration_LI_2015 = num_out_LI_2015 / num_fams_LI_2014,
      outmigration_LI_2016 = num_out_LI_2016 / num_fams_LI_2015,
      outmigration_LI_2017 = num_out_LI_2017 / num_fams_LI_2016,
      outmigration_LI_2018 = num_out_LI_2018 / num_fams_LI_2017,
      # renters outmigration
      outmigration_renters_2007 = num_out_r_2007 / num_fams_r_2006,
      outmigration_renters_2008 = num_out_r_2008 / num_fams_r_2007,
      outmigration_renters_2009 = num_out_r_2009 / num_fams_r_2008,
      outmigration_renters_2010 = num_out_r_2010 / num_fams_r_2009,
      outmigration_renters_2011 = num_out_r_2011 / num_fams_r_2010,
      outmigration_renters_2012 = num_out_r_2012 / num_fams_r_2011,
      outmigration_renters_2013 = num_out_r_2013 / num_fams_r_2012,
      outmigration_renters_2014 = num_out_r_2014 / num_fams_r_2013,
      outmigration_renters_2015 = num_out_r_2015 / num_fams_r_2014,
      outmigration_renters_2016 = num_out_r_2016 / num_fams_r_2015,
      outmigration_renters_2017 = num_out_r_2017 / num_fams_r_2016,
      outmigration_renters_2018 = num_out_r_2018 / num_fams_r_2017,
      # LI renters outmigration
      outmigration_LI_renters_2007 = num_out_LI_r_2007 / num_fams_LI_r_2006,
      outmigration_LI_renters_2008 = num_out_LI_r_2008 / num_fams_LI_r_2007,
      outmigration_LI_renters_2009 = num_out_LI_r_2009 / num_fams_LI_r_2008,
      outmigration_LI_renters_2010 = num_out_LI_r_2010 / num_fams_LI_r_2009,
      outmigration_LI_renters_2011 = num_out_LI_r_2011 / num_fams_LI_r_2010,
      outmigration_LI_renters_2012 = num_out_LI_r_2012 / num_fams_LI_r_2011,
      outmigration_LI_renters_2013 = num_out_LI_r_2013 / num_fams_LI_r_2012,
      outmigration_LI_renters_2014 = num_out_LI_r_2014 / num_fams_LI_r_2013,
      outmigration_LI_renters_2015 = num_out_LI_r_2015 / num_fams_LI_r_2014,
      outmigration_LI_renters_2016 = num_out_LI_r_2016 / num_fams_LI_r_2015,
      outmigration_LI_renters_2017 = num_out_LI_r_2017 / num_fams_LI_r_2016,
      outmigration_LI_renters_2018 = num_out_LI_r_2018 / num_fams_LI_r_2017
    ) %>% 
    filter(!(investment == "1" & !! investment_type == "0"))
  
  return(investment_stats)
}

# PLOTTING FUNCTIONS
## Create NOAH plots
noah_bar_plot <- function(area_stats, area_psm) {
  df_name <- deparse(substitute(area_psm))
  
  # NOAH  change
  noah_bar <-
    area_stats %>% 
    select(investment, perc_noah_tot_change, perc_noah_nolihtc_change) %>% 
    pivot_longer(cols = c(perc_noah_tot_change, perc_noah_nolihtc_change)) %>% 
    ggplot(aes(x = name, y = value, fill = investment)) +
    geom_col(position = position_dodge2(padding = 0, reverse = TRUE)) +
    scale_x_discrete(labels = c("No LIHTC", "Total")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_discrete(labels = c("No Investment", "Investment")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom") +
    labs(
      title = "NOAH change (2009 - 2016)",
      subtitle =
        str_glue(
          "Area: {area}", 
          area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
        ),
      x = NULL,
      y = "% change",
      fill = NULL
    )
  # save to PNG
  path <-
    str_glue(
      "./SGC/visualizations/bar_plots/noah_{area}.png",
      area = str_extract(df_name, "^\\w+(?=_)")
    )
  ggsave(path, plot = noah_bar)
  
  return(noah_bar)
}

noah_reg <- function(area_stats, area_psm, reg_output = html, font_size = "small") {
  reg_output <- enquo(reg_output)
  df_name <- deparse(substitute(area_stats))
  
  # linear regression on NOAH total change
  total_noah_reg <- lm(noah_tot_change ~ investment, area_psm)
  
  # linear regression on NOAH total change
  nolihtc_noah_reg <- lm(noah_nolihtc_change ~ investment, area_psm)
  
  # Print regression results
  stargazer::stargazer(
    total_noah_reg,
    nolihtc_noah_reg,
    type = quo_name(reg_output),
    title =
      str_glue(
        "NOAH by Investment in Area: {area}",
        area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
      ),
    font.size = font_size,
    column.sep.width = "1pt"
  )
}

## Create outmigration plots
outmigration_time <- function(area_stats, area_psm) {
  df_name <- deparse(substitute(area_stats))
  
  # Print Outmigration by time plot
  time_plot <-
    area_stats %>% 
    # select for investment and outmigration type
    select(investment, starts_with("outmigration")) %>% 
    # change structure of data for plotting
    pivot_longer(
      cols = starts_with("outmigration"),
      names_to = "type",
      values_to = "migration_rate"
    ) %>% 
    # extract the year digits
    mutate(
      year = str_extract(type, "\\d{4}$"),
      type = str_remove(type, "_\\d{4}$"),
      type = str_remove(type, "outmigration"),
      type = str_replace_all(type, "_", " "),
      type = str_to_title(type),
      type = str_replace(type, "Li", "Low Income")
    ) %>%
    # plot outmigration over time
    ggplot(
      aes(x = year, y = migration_rate, color = investment, group = investment)
    ) +
    geom_point() +
    geom_path() +
    geom_smooth(
      aes(fill = investment), 
      alpha = .1, 
      linetype = "dashed",
      show.legend = FALSE
    ) +
    facet_wrap(~ type, ncol = 2) +
    scale_color_discrete(labels = c("No Investment", "Investment")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    guides(color = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom", axis.text.x = element_text(size = 8)) +
    labs(
      title = "Outmigration change (2007 - 2018)",
      subtitle = 
        str_glue(
          "Area: {area}", 
          area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
        ),
      y = "Outmigration Rate",
      x = NULL,
      color = NULL
    )
  # Save to PNG 
  path <-
    str_glue(
      "./SGC/visualizations/time_series/outmigration_time_{area}.png",
      area = str_extract(df_name, "^\\w+(?=_)")
    )
  ggsave(path, plot = time_plot)
  
  return(time_plot)
}

outmigration_bar <- function(area_stats, area_psm) {
  df_name <- deparse(substitute(area_stats))
  
  # Plot investment graph
  bar_plot <-
    area_stats %>% 
    select(investment, contains("outmigration")) %>% 
    pivot_longer(
      cols = contains("outmigration"),
      names_to = "type",
      values_to = "migration_rate"
    ) %>% 
    mutate(
      year = str_extract(type, "\\d{4}$"),
      type = str_remove(type, "_\\d{4}$")
    ) %>% 
    group_by(investment, type) %>% 
    summarise(migration_rate = mean(migration_rate)) %>% 
    ggplot(aes(x = type, y = migration_rate, fill = investment)) +
    geom_col(position = position_dodge2(padding = 0, reverse = TRUE)) +
    scale_x_discrete(
      labels = c("All", "Low Income", "Low Income Renters", "Renters")
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_discrete(labels = c("No Investment", "Investment")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom") +
    labs(
      title = "Average outmigration rates (2007 - 2018)",
      subtitle = 
        str_glue(
          "Area: {area}", 
          area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
        ),
      x = NULL,
      y = "Outmigration rate",
      fill = NULL
    )
  # save to PNG
  path <-
    str_glue(
      "./SGC/visualizations/bar_plots/outmigration_bar_{area}.png",
      area = str_extract(df_name, "^\\w+(?=_)")
    )
  ggsave(path, plot = bar_plot)
  
  return(bar_plot)
}

outmigration_reg <- function(area_stats, area_psm, reg_output = html, font_size = "small") {
  df_name <- deparse(substitute(area_stats))
  reg_output <- enquo(reg_output)
  
  # linear regression on outmigration ALL
  outmig_all_reg <- 
    lm(
      migration_rate ~ investment, 
      area_psm %>% 
        select(investment, starts_with("outmigration_all")) %>% 
        pivot_longer(
          cols = starts_with("outmigration_all"),
          names_to = "year",
          values_to = "migration_rate"
        )
    )
  
  # linear regression on outmigration LI
  outmig_li_reg <- 
    lm(
      migration_rate ~ investment, 
      area_psm %>% 
        select(investment, starts_with("outmigration_LI_2")) %>% 
        pivot_longer(
          cols = starts_with("outmigration_LI_2"),
          names_to = "year",
          values_to = "migration_rate"
        )
    )
  
  # linear regression on outmigration RENTER
  outmig_r_reg <- 
    lm(
      migration_rate ~ investment, 
      area_psm %>% 
        select(investment, starts_with("outmigration_r")) %>% 
        pivot_longer(
          cols = starts_with("outmigration_r"),
          names_to = "year",
          values_to = "migration_rate"
        )
    )
  
  # linear regression on outmigration LOW INCOME RENTER
  outmig_li_r_reg <- 
    lm(
      migration_rate ~ investment, 
      area_psm %>% 
        select(investment, starts_with("outmigration_LI_r")) %>% 
        pivot_longer(
          cols = starts_with("outmigration_LI_r"),
          names_to = "year",
          values_to = "migration_rate"
        )
    )
  
  stargazer::stargazer(
    outmig_all_reg,
    outmig_li_reg,
    outmig_r_reg,
    outmig_li_r_reg,
    type = quo_name(reg_output),
    title = 
      str_glue(
        "Outmigration Rates by Investment in Area: {area}",
        area =  
          str_to_upper(
            str_extract(df_name, "^\\w+(?=_)")
          )
      ),
    font.size = font_size,
    column.labels = c("ALL", "LOW INCOME", "RENTER", "LOW INCOME RENTER"),
    column.sep.width = "1pt"
  ) 
}

## Create NOAH type plots
noah_type_bar_plot <- function(area_stats, area_psm) {
  df_name <- deparse(substitute(area_psm))
  investment_name <- deparse(substitute(area_stats))
  
  investment_type <- str_extract(investment_name, "^.+(?=_stats)")
  
  # NOAH  change
  
  noah_bar <-
    area_stats %>% 
    select(investment, perc_noah_tot_change, perc_noah_nolihtc_change) %>% 
    pivot_longer(cols = c(perc_noah_tot_change, perc_noah_nolihtc_change)) %>% 
    ggplot(aes(x = name, y = value, fill = investment)) +
    geom_col(position = position_dodge2(padding = 0, reverse = TRUE)) +
    scale_x_discrete(labels = c("No LIHTC", "Total")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_discrete(labels = c("No Investment", "Investment")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom") +
    labs(
      title =
        str_glue(
          "{type} ",
          "NOAH change (2009 - 2016)",
          type = 
            str_to_title(str_replace_all(investment_type, "_", " "))
        ),
      subtitle =
        str_glue(
          "Area: {area}", 
          area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
        ),
      x = NULL,
      y = "% change",
      fill = NULL
    )
  # save to PNG
  path <-
    str_glue(
      "./SGC/visualizations/bar_plots/noah_{area}_{invest}.png",
      area = str_extract(df_name, "^\\w+(?=_)"),
      invest = investment_type
    )
  ggsave(path, plot = noah_bar)
  
  return(noah_bar)
}

noah_type_reg <- function(area_stats, area_psm, reg_output = html, font_size = "small") {
  reg_output <- enquo(reg_output)
  df_name <- deparse(substitute(area_psm))
  investment_name <- deparse(substitute(area_stats))
  
  investment_type <- str_extract(investment_name, "^.+(?=_stats)")
  
  # linear regression on NOAH total change
  model_formula <- 
    formula(paste0("noah_tot_change ~", investment_type))
  total_noah_reg <- lm(model_formula, area_psm)
  # linear regression on NOAH no LIHTC change
  model_formula <- 
    formula(paste0("noah_nolihtc_change ~", investment_type))
  nolihtc_noah_reg <- lm(model_formula, area_psm)
  
  stargazer::stargazer(
    total_noah_reg,
    nolihtc_noah_reg,
    type = quo_name(reg_output),
    title =
      str_glue(
        "NOAH by {type} in Area: {area}",
        type = 
          str_to_title(str_replace_all(investment_type, "_", " ")),
        area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
      ),
    font.size = font_size,
    column.labels = c("TOTAL", "NO LIHTC"),
    column.sep.width = "1pt"
  )
}

## Create outmigration plots
outmigration_type_time <- function(area_stats, area_psm) {
  df_name <- deparse(substitute(area_psm))
  investment_name <- deparse(substitute(area_stats))
  
  investment_type <- str_extract(investment_name, "^.+(?=_stats)")
  
  # Print Outmigration by time plot
  time_plot <-
    area_stats %>% 
    # select for investment and outmigration type
    select(investment, starts_with("outmigration")) %>% 
    # change structure of data for plotting
    pivot_longer(
      cols = starts_with("outmigration"),
      names_to = "type",
      values_to = "migration_rate"
    ) %>% 
    # extract the year digits
    mutate(
      year = str_extract(type, "\\d{4}$"),
      type = str_remove(type, "_\\d{4}$"),
      type = str_remove(type, "outmigration"),
      type = str_replace_all(type, "_", " "),
      type = str_to_title(type),
      type = str_replace(type, "Li", "Low Income")
    ) %>%
    # plot outmigration over time
    ggplot(
      aes(x = year, y = migration_rate, color = investment, group = investment)
    ) +
    geom_point() +
    geom_path() +
    geom_smooth(
      aes(fill = investment), 
      alpha = .1, 
      linetype = "dashed",
      show.legend = FALSE
    ) +
    facet_wrap(~ type, ncol = 2) +
    scale_color_discrete(labels = c("No Investment", "Investment")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    guides(color = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom", axis.text.x = element_text(size = 8)) +
    labs(
      title =
        str_glue(
          str_to_title(str_replace_all(investment_type, "_", " ")),
          " outmigration change (2007 - 2018)"
        ),
      subtitle =
        str_glue(
          "Area: {area}",
          area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
        ),
      y = "Outmigration Rate",
      x = NULL,
      color = NULL
    )
  # Save to PNG
  path <-
    str_glue(
      "./SGC/visualizations/time_series/outmigration_{area}_{investment_type}.png",
      area = str_extract(df_name, "^\\w+(?=_)"),
      investment_type = quo_name(investment_type)
    )
  ggsave(path, plot = time_plot)
  
  return(time_plot)
}

outmigration_type_bar <- function(area_stats, area_psm) {
  df_name <- deparse(substitute(area_psm))
  investment_name <- deparse(substitute(area_stats))
  
  investment_type <- str_extract(investment_name, "^.+(?=_stats)")
  
  # Plot investment graph
  bar_plot <-
    area_stats %>% 
    select(investment_type, contains("outmigration")) %>% 
    pivot_longer(
      cols = contains("outmigration"),
      names_to = "type",
      values_to = "migration_rate"
    ) %>% 
    mutate(
      year = str_extract(type, "\\d{4}$"),
      type = str_remove(type, "_\\d{4}$")
    ) %>% 
    # need to group by investment type and outmigration type
    group_by(.dots = investment_type, type) %>% 
    summarise(migration_rate = mean(migration_rate)) %>% 
    ggplot(
      aes_string(x = "type", y = "migration_rate", fill = investment_type)
    ) +
    geom_col(position = position_dodge2(padding = 0, reverse = TRUE)) +
    scale_x_discrete(
      labels = c("All", "Low Income", "Low Income Renters", "Renters")
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_discrete(labels = c("No Investment", "Investment")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme(legend.position = "bottom") +
    labs(
      title =
        str_glue(
          "{investment_type} ",
          "average outmigration rates (2007 - 2018)",
          investment_type =
            str_to_title(str_replace_all(investment_type, "_", " "))
        ),
      subtitle =
        str_glue(
          "Area: {area}",
          area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
        ),
      x = NULL,
      y = "Outmigration rate",
      fill = NULL
    )
  # save to PNG
  path <-
    str_glue(
      "./SGC/visualizations/bar_plots/outmigration_{investment_type}_bar.png"
    )
  ggsave(path, plot = bar_plot)
  
  return(bar_plot)
}

outmigration_type_reg <- function(area_stats, area_psm, outmigration_type, reg_output = html, font_size = "small") {
  outmigration_type <- enquo(outmigration_type)
  df_name <- deparse(substitute(area_psm))
  reg_output <- enquo(reg_output)
  investment_name <- deparse(substitute(area_stats))
  
  investment_type <- str_extract(investment_name, "^.+(?=_stats)")
  
  # Generic model formula
  model_formula <- formula(paste("migration_rate ~ ", investment_type))
  
  # linear regression on outmigration ALL
  outmig_all_reg <-
    lm(
      model_formula,
      area_psm %>%
        select(investment, investment_type, starts_with("outmigration_all")) %>%
        pivot_longer(
          cols = starts_with("outmigration_all"),
          names_to = "year",
          values_to = "migration_rate"
        ) %>%
        filter(!(investment == "1" & !!as.symbol(investment_type) == "0"))
    )
  
  # linear regression on outmigration LI
  outmig_li_reg <-
    lm(
      model_formula,
      area_psm %>%
        select(investment, investment_type, starts_with("outmigration_LI_2")) %>%
        pivot_longer(
          cols = starts_with("outmigration_LI_2"),
          names_to = "year",
          values_to = "migration_rate"
        ) %>%
        filter(!(investment == "1" & !!as.symbol(investment_type) == "0"))
    )
  
  # linear regression on outmigration RENTER
  outmig_r_reg <- 
    lm(
      model_formula, 
      area_psm %>% 
        select(investment, investment_type, starts_with("outmigration_r")) %>% 
        pivot_longer(
          cols = starts_with("outmigration_r"),
          names_to = "year",
          values_to = "migration_rate"
        ) %>%
        filter(!(investment == "1" & !!as.symbol(investment_type) == "0"))
    )
  
  # linear regression on outmigration LOW INCOME RENTER
  outmig_li_r_reg <- 
    lm(
      model_formula, 
      area_psm %>% 
        select(investment, investment_type, starts_with("outmigration_LI_r")) %>% 
        pivot_longer(
          cols = starts_with("outmigration_LI_r"),
          names_to = "year",
          values_to = "migration_rate"
        ) %>% 
        filter(!(investment == "1" & !!as.symbol(investment_type) == "0"))
    )
  
  stargazer::stargazer(
    outmig_all_reg,
    outmig_li_reg,
    outmig_r_reg,
    outmig_li_r_reg,
    type = quo_name(reg_output),
    title = 
      str_glue(
        "Outmigration Rates by {type} in Area: {area}",
        type = 
          str_to_title(str_replace_all(investment_type, "_", " ")),
        area = str_to_upper(str_extract(df_name, "^\\w+(?=_)"))
      ),
    font.size = font_size,
    column.labels = c("ALL", "LOW INCOME", "RENTER", "LOW INCOME RENTER"),
    column.sep.width = "1pt"
  ) 
}

## Summary Table builder
area_summary <- function(area_psm) {
  
  area_summary_df <-
    area_psm %>% 
    # Select out unneeded variables
    select(
      -c(GEOID,
         distance, 
         location, 
         investment_1:investment_7,
         n_invest, 
         contains("change"),
         contains("outmigration")
      )
    ) %>% 
    # change format of the data
    pivot_longer(
      cols = c(investment:transit), 
      names_to = "investments", 
      values_to = "flag"
    ) %>% 
    # reorder columns
    select(investments, flag, everything()) %>%
    # mutate acs data to be of type double
    mutate(across(3:length(.), as.double)) %>% 
    # group by investment and flag
    group_by(investments, flag) %>% 
    # summarize NOAH and outmigration data
    summarise_all(~ sum(., na.rm = TRUE)) %>% 
    # filter out specific investment types with 0 flag
    filter(flag == "0" & investments == "investment" | flag == "1") %>% 
    # compute change metrics
    mutate(
      noah_tot_change = noah_tot_clean_16 - noah_tot_clean_09,
      noah_nolihtc_change = noah_nolihtc_clean_16 - noah_nolihtc_clean_09,
      perc_noah_tot_change = noah_tot_change / noah_tot_clean_09,
      perc_noah_nolihtc_change = noah_nolihtc_change / noah_nolihtc_clean_09,
      # all outmigration
      outmigration_all_2007 = num_out_2007 / num_fams_2006,
      outmigration_all_2008 = num_out_2008 / num_fams_2007,
      outmigration_all_2009 = num_out_2009 / num_fams_2008,
      outmigration_all_2010 = num_out_2010 / num_fams_2009,
      outmigration_all_2011 = num_out_2011 / num_fams_2010,
      outmigration_all_2012 = num_out_2012 / num_fams_2011,
      outmigration_all_2013 = num_out_2013 / num_fams_2012,
      outmigration_all_2014 = num_out_2014 / num_fams_2013,
      outmigration_all_2015 = num_out_2015 / num_fams_2014,
      outmigration_all_2016 = num_out_2016 / num_fams_2015,
      outmigration_all_2017 = num_out_2017 / num_fams_2016,
      outmigration_all_2018 = num_out_2018 / num_fams_2017,
      # LI outmigration
      outmigration_LI_2007 = num_out_LI_2007 / num_fams_LI_2006,
      outmigration_LI_2008 = num_out_LI_2008 / num_fams_LI_2007,
      outmigration_LI_2009 = num_out_LI_2009 / num_fams_LI_2008,
      outmigration_LI_2010 = num_out_LI_2010 / num_fams_LI_2009,
      outmigration_LI_2011 = num_out_LI_2011 / num_fams_LI_2010,
      outmigration_LI_2012 = num_out_LI_2012 / num_fams_LI_2011,
      outmigration_LI_2013 = num_out_LI_2013 / num_fams_LI_2012,
      outmigration_LI_2014 = num_out_LI_2014 / num_fams_LI_2013,
      outmigration_LI_2015 = num_out_LI_2015 / num_fams_LI_2014,
      outmigration_LI_2016 = num_out_LI_2016 / num_fams_LI_2015,
      outmigration_LI_2017 = num_out_LI_2017 / num_fams_LI_2016,
      outmigration_LI_2018 = num_out_LI_2018 / num_fams_LI_2017,
      # renters outmigration
      outmigration_renters_2007 = num_out_r_2007 / num_fams_r_2006,
      outmigration_renters_2008 = num_out_r_2008 / num_fams_r_2007,
      outmigration_renters_2009 = num_out_r_2009 / num_fams_r_2008,
      outmigration_renters_2010 = num_out_r_2010 / num_fams_r_2009,
      outmigration_renters_2011 = num_out_r_2011 / num_fams_r_2010,
      outmigration_renters_2012 = num_out_r_2012 / num_fams_r_2011,
      outmigration_renters_2013 = num_out_r_2013 / num_fams_r_2012,
      outmigration_renters_2014 = num_out_r_2014 / num_fams_r_2013,
      outmigration_renters_2015 = num_out_r_2015 / num_fams_r_2014,
      outmigration_renters_2016 = num_out_r_2016 / num_fams_r_2015,
      outmigration_renters_2017 = num_out_r_2017 / num_fams_r_2016,
      outmigration_renters_2018 = num_out_r_2018 / num_fams_r_2017,
      # LI renters outmigration
      outmigration_LI_renters_2007 = num_out_LI_r_2007 / num_fams_LI_r_2006,
      outmigration_LI_renters_2008 = num_out_LI_r_2008 / num_fams_LI_r_2007,
      outmigration_LI_renters_2009 = num_out_LI_r_2009 / num_fams_LI_r_2008,
      outmigration_LI_renters_2010 = num_out_LI_r_2010 / num_fams_LI_r_2009,
      outmigration_LI_renters_2011 = num_out_LI_r_2011 / num_fams_LI_r_2010,
      outmigration_LI_renters_2012 = num_out_LI_r_2012 / num_fams_LI_r_2011,
      outmigration_LI_renters_2013 = num_out_LI_r_2013 / num_fams_LI_r_2012,
      outmigration_LI_renters_2014 = num_out_LI_r_2014 / num_fams_LI_r_2013,
      outmigration_LI_renters_2015 = num_out_LI_r_2015 / num_fams_LI_r_2014,
      outmigration_LI_renters_2016 = num_out_LI_r_2016 / num_fams_LI_r_2015,
      outmigration_LI_renters_2017 = num_out_LI_r_2017 / num_fams_LI_r_2016,
      outmigration_LI_renters_2018 = num_out_LI_r_2018 / num_fams_LI_r_2017,
      # Change investment names
      investments = 
        if_else(flag == "0", "no_investment", investments)
    ) %>%  
    # mutate and keep important variables (round to 2nd decimal, show percentage)
    transmute(
      investments = investments,
      perc_noah_tot_change = 
        scales::percent(perc_noah_tot_change, accuracy = 0.01),
      perc_noah_nolihtc_change = 
        scales::percent(perc_noah_nolihtc_change, accuracy = 0.01),
      outmigration_rate_all =
        scales::percent(
          mean(
            c_across(outmigration_all_2007:outmigration_all_2018), 
            na.rm = TRUE
          ),
          accuracy = 0.01
        ),
      outmigration_rate_li =
        scales::percent(
          mean(
            c_across(outmigration_LI_2007:outmigration_LI_2018),
            na.rm = TRUE
          ),
          accuracy = 0.01
        ),
      outmigration_rate_renters =
        scales::percent(
          mean(
            c_across(outmigration_renters_2007:outmigration_renters_2018),
            na.rm = TRUE
          ),
          accuracy = 0.01
        ),
      outmigration_rate_li_renters =
        scales::percent(
          mean(
            c_across(outmigration_LI_renters_2007:outmigration_LI_renters_2018),
            na.rm = TRUE
          ),
          accuracy = 0.01
        )
    ) %>% 
    # Update Investment order
    arrange(
      factor(
        investments, 
        levels = 
          c("no_investment",
            "investment", 
            "greening", 
            "transit", 
            "urban_infill", 
            "active_transportation"
          )
      )
    ) %>% 
    # clean up investment names
    mutate(
      investments = 
        if_else(investments == "investment", "any_investment", investments),
      investments = str_to_title(str_replace_all(investments, "_", " "))) %>% 
    ungroup()
  
  return(area_summary_df)
}

## Investment Summary builder
investment_summary <- function(area_psm, area_stats) {
  
  investment_summary_df <-
    area_psm %>% 
    # Select out unneeded variables
    select(
      -c(GEOID,
         distance, 
         location, 
         investment,
         greening:transit,
         n_invest, 
         contains("change"),
         contains("outmigration")
      )
    ) %>% 
    # change format of the data
    pivot_longer(
      cols = c(investment_1:investment_7), 
      names_to = "investment_no", 
      values_to = "investment_name"
    ) %>% 
    # reorder columns
    select(investment_name, everything(), -investment_no) %>%
    # mutate acs data to be of type double
    mutate(across(2:length(.), as.double)) %>%
    # mutate acs data to be of type double
    group_by(investment_name) %>%
    # summarize NOAH and outmigration data
    summarise_all(~ sum(., na.rm = TRUE)) %>% 
    # compute change metrics
    mutate(
      noah_tot_change = noah_tot_clean_16 - noah_tot_clean_09,
      noah_nolihtc_change = noah_nolihtc_clean_16 - noah_nolihtc_clean_09,
      perc_noah_tot_change = noah_tot_change / noah_tot_clean_09,
      perc_noah_nolihtc_change = noah_nolihtc_change / noah_nolihtc_clean_09,
      # all outmigration
      outmigration_all_2007 = num_out_2007 / num_fams_2006,
      outmigration_all_2008 = num_out_2008 / num_fams_2007,
      outmigration_all_2009 = num_out_2009 / num_fams_2008,
      outmigration_all_2010 = num_out_2010 / num_fams_2009,
      outmigration_all_2011 = num_out_2011 / num_fams_2010,
      outmigration_all_2012 = num_out_2012 / num_fams_2011,
      outmigration_all_2013 = num_out_2013 / num_fams_2012,
      outmigration_all_2014 = num_out_2014 / num_fams_2013,
      outmigration_all_2015 = num_out_2015 / num_fams_2014,
      outmigration_all_2016 = num_out_2016 / num_fams_2015,
      outmigration_all_2017 = num_out_2017 / num_fams_2016,
      outmigration_all_2018 = num_out_2018 / num_fams_2017,
      # LI outmigration
      outmigration_LI_2007 = num_out_LI_2007 / num_fams_LI_2006,
      outmigration_LI_2008 = num_out_LI_2008 / num_fams_LI_2007,
      outmigration_LI_2009 = num_out_LI_2009 / num_fams_LI_2008,
      outmigration_LI_2010 = num_out_LI_2010 / num_fams_LI_2009,
      outmigration_LI_2011 = num_out_LI_2011 / num_fams_LI_2010,
      outmigration_LI_2012 = num_out_LI_2012 / num_fams_LI_2011,
      outmigration_LI_2013 = num_out_LI_2013 / num_fams_LI_2012,
      outmigration_LI_2014 = num_out_LI_2014 / num_fams_LI_2013,
      outmigration_LI_2015 = num_out_LI_2015 / num_fams_LI_2014,
      outmigration_LI_2016 = num_out_LI_2016 / num_fams_LI_2015,
      outmigration_LI_2017 = num_out_LI_2017 / num_fams_LI_2016,
      outmigration_LI_2018 = num_out_LI_2018 / num_fams_LI_2017,
      # renters outmigration
      outmigration_renters_2007 = num_out_r_2007 / num_fams_r_2006,
      outmigration_renters_2008 = num_out_r_2008 / num_fams_r_2007,
      outmigration_renters_2009 = num_out_r_2009 / num_fams_r_2008,
      outmigration_renters_2010 = num_out_r_2010 / num_fams_r_2009,
      outmigration_renters_2011 = num_out_r_2011 / num_fams_r_2010,
      outmigration_renters_2012 = num_out_r_2012 / num_fams_r_2011,
      outmigration_renters_2013 = num_out_r_2013 / num_fams_r_2012,
      outmigration_renters_2014 = num_out_r_2014 / num_fams_r_2013,
      outmigration_renters_2015 = num_out_r_2015 / num_fams_r_2014,
      outmigration_renters_2016 = num_out_r_2016 / num_fams_r_2015,
      outmigration_renters_2017 = num_out_r_2017 / num_fams_r_2016,
      outmigration_renters_2018 = num_out_r_2018 / num_fams_r_2017,
      # LI renters outmigration
      outmigration_LI_renters_2007 = num_out_LI_r_2007 / num_fams_LI_r_2006,
      outmigration_LI_renters_2008 = num_out_LI_r_2008 / num_fams_LI_r_2007,
      outmigration_LI_renters_2009 = num_out_LI_r_2009 / num_fams_LI_r_2008,
      outmigration_LI_renters_2010 = num_out_LI_r_2010 / num_fams_LI_r_2009,
      outmigration_LI_renters_2011 = num_out_LI_r_2011 / num_fams_LI_r_2010,
      outmigration_LI_renters_2012 = num_out_LI_r_2012 / num_fams_LI_r_2011,
      outmigration_LI_renters_2013 = num_out_LI_r_2013 / num_fams_LI_r_2012,
      outmigration_LI_renters_2014 = num_out_LI_r_2014 / num_fams_LI_r_2013,
      outmigration_LI_renters_2015 = num_out_LI_r_2015 / num_fams_LI_r_2014,
      outmigration_LI_renters_2016 = num_out_LI_r_2016 / num_fams_LI_r_2015,
      outmigration_LI_renters_2017 = num_out_LI_r_2017 / num_fams_LI_r_2016,
      outmigration_LI_renters_2018 = num_out_LI_r_2018 / num_fams_LI_r_2017
    ) %>%  
    ungroup() %>% 
    # fix the incorrect No Investment numbers
    filter(!is.na(investment_name)) %>% 
    # join with already computed area stats (eliminate double counting issues)
    full_join(area_stats %>% rename(investment_name = investment)) %>% 
    # turn investment flags into names
    mutate(
      investment_name = 
        case_when(
          investment_name == "1" ~ "any_investment",
          investment_name == "0" ~ "no_investment",
          TRUE ~ investment_name
        )
    ) %>% 
    rowwise() %>% 
    # mutate and keep important variables (round to 2nd decimal, show percentage)
    transmute(
      investment_name = investment_name,
      perc_noah_tot_change = 
        scales::percent(perc_noah_tot_change, accuracy = 0.01),
      perc_noah_nolihtc_change = 
        scales::percent(perc_noah_nolihtc_change, accuracy = 0.01),
      outmigration_rate_all =
        scales::percent(
          mean(
            c_across(outmigration_all_2007:outmigration_all_2018), 
            na.rm = TRUE
          ),
          accuracy = 0.01
        ),
      outmigration_rate_li =
        scales::percent(
          mean(
            c_across(outmigration_LI_2007:outmigration_LI_2018),
            na.rm = TRUE
          ),
          accuracy = 0.01
        ),
      outmigration_rate_renters =
        scales::percent(
          mean(
            c_across(outmigration_renters_2007:outmigration_renters_2018),
            na.rm = TRUE
          ),
          accuracy = 0.01
        ),
      outmigration_rate_li_renters =
        scales::percent(
          mean(
            c_across(outmigration_LI_renters_2007:outmigration_LI_renters_2018),
            na.rm = TRUE
          ),
          accuracy = 0.01
        )
    ) %>% 
    # Update Investment order
    arrange(
      factor(
        investment_name, 
        levels = 
          c(
            "no_investment",
            "any_investment",
            . %>% filter(!(investment_name %in% c("no_investment", "any_investment")))
          )
      )
    ) %>% 
    # clean up investment names
    mutate(
      investment_name = str_to_title(str_replace_all(investment_name, "_", " ")),
      investment_name = str_remove_all(investment_name, "Polygon")
    ) %>% 
    ungroup()
  
  return(investment_summary_df)
}

### NOAH summary table
noah_summary_table <- function(area_summary, table_title) {
  # create vector with colspan
  myHeader <- c(" " = 1, table_title = 2)
  
  # set vector names 
  names(myHeader) <- c(" ", table_title)
  
  # Set limits for coloring
  total_noinvest <- area_summary$perc_noah_tot_change[1]
  nolihtc_noinvest <- area_summary$perc_noah_nolihtc_change[1]
  
  # Print NOAH Summary Table
  area_summary %>% 
    mutate(
      perc_noah_tot_change =
        cell_spec(
          perc_noah_tot_change,
          color =
            # need to convert percentage back to number
            case_when(
              as.numeric(sub("%", "", perc_noah_tot_change)) > 
                as.numeric(sub("%", "", total_noinvest)) ~ "#008000", 
              as.numeric(sub("%", "", perc_noah_tot_change)) < 
                as.numeric(sub("%", "", total_noinvest)) ~ "red",
              TRUE ~ "black"
            )
        ),
      perc_noah_nolihtc_change =
        cell_spec(
          perc_noah_nolihtc_change,
          color =
            case_when(
              as.numeric(sub("%", "", perc_noah_nolihtc_change)) > 
                as.numeric(sub("%", "", nolihtc_noinvest)) ~ "#008000", 
              as.numeric(sub("%", "", perc_noah_nolihtc_change)) < 
                as.numeric(sub("%", "", nolihtc_noinvest)) ~ "red",
              TRUE ~ "black"
            )
        )
    ) %>%
    # remove investments header
    column_to_rownames(var = colnames(area_summary[1])) %>% 
    # filter to just NOAH variables
    select(contains("noah")) %>% 
    # Print into nice table with cleaned variable names
    kable(
      col.names =
        c(
          "Total NOAH Units",
          "No LIHTC NOAH Units"
        ),
      escape = F
    )  %>% 
    kable_classic() %>% 
    # format for markdown
    kable_styling(latex_options="scale_down") %>% 
    # italics/black font for first row (no investment)
    row_spec(1, italic = T, bold = T) %>% 
    # no bold and border for first column
    column_spec(1, bold = F) %>%
    # unify 1st column width
    column_spec(1, width = "2in") %>% 
    # push dates to second line 
    column_spec(c(2, 3), width = "2in") %>% 
    # add title to plot
    add_header_above(
      header = myHeader, 
      escape = T
    )
}

### outmigration summary table
outmigration_summary_table <- function(area_summary, table_title) {
  # create vector with colspan
  myHeader <- c(" " = 1, table_title = 4)
  
  # set vector names 
  names(myHeader) <- c(" ", table_title)
  
  # Set limits for coloring
  outmig_all_lim <- area_summary$outmigration_rate_all[1]
  outmig_li_lim <- area_summary$outmigration_rate_li[1]
  outmig_r_lim <- area_summary$outmigration_rate_renters[1]
  outmig_li_r_lim <- area_summary$outmigration_rate_li_renters[1]
  
  area_summary %>%
    mutate(
      # need to convert % back to numeric for conditional coloring
      outmigration_rate_all =
        cell_spec(
          outmigration_rate_all,
          color =
            case_when(
              as.numeric(sub("%", "", outmigration_rate_all)) > 
                as.numeric(sub("%", "", outmig_all_lim)) ~ "#008000",
              as.numeric(sub("%", "", outmigration_rate_all)) < 
                as.numeric(sub("%", "", outmig_all_lim)) ~ "red",
              TRUE ~ "black"
            )
        ),
      outmigration_rate_li =
        cell_spec(
          outmigration_rate_li,
          color =
            case_when(
              as.numeric(sub("%", "", outmigration_rate_li)) > 
                as.numeric(sub("%", "", outmig_li_lim)) ~ "#008000",
              as.numeric(sub("%", "", outmigration_rate_li)) < 
                as.numeric(sub("%", "", outmig_li_lim)) ~ "red",
              TRUE ~ "black"
            )
        ),
      outmigration_rate_renters =
        cell_spec(
          outmigration_rate_renters,
          color =
            case_when(
              as.numeric(sub("%", "", outmigration_rate_renters)) > 
                as.numeric(sub("%", "", outmig_r_lim)) ~ "#008000",
              as.numeric(sub("%", "", outmigration_rate_renters)) < 
                as.numeric(sub("%", "", outmig_r_lim)) ~ "red",
              TRUE ~ "black"
            )
        ),
      outmigration_rate_li_renters =
        cell_spec(
          outmigration_rate_li_renters,
          color =
            case_when(
              as.numeric(sub("%", "", outmigration_rate_li_renters)) > 
                as.numeric(sub("%", "", outmig_li_r_lim)) ~ "#008000",
              as.numeric(sub("%", "", outmigration_rate_li_renters)) < 
                as.numeric(sub("%", "", outmig_li_r_lim)) ~ "red",
              TRUE ~ "black"
            )
        )    
    ) %>%
    # remove investments header
    column_to_rownames(var = colnames(area_summary[1])) %>%
    # filter to just Outmigration variables
    select(contains("outmigration")) %>%
    # Print into nice table with cleaned variable names
    kable(
      col.names =
        c(
          "Total",
          "Low Income",
          "Renters",
          "Low Income Renters"
        ),
      escape = F
    )  %>%
    kable_classic() %>%
    # format for markdown
    kable_styling(latex_options="scale_down") %>%
    # italics/black font for first row (no investment)
    row_spec(1, italic = T, bold = T) %>%
    # unify width
    column_spec(1, width = "2in") %>%
    # push dates to second line 
    column_spec(2:5, width = "1.5in") %>% 
    # add title to plot
    add_header_above(myHeader, escape = T)
}
  