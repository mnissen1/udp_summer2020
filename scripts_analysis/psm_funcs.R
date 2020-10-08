psm_results <- function(neighborhood_df, neighborhood_cov) {
  
  # create an expression that adds up each covariate
  # concatenate each covariate into a string with "+" separator
  cov_addition <-
    neighborhood_cov %>%
    tibble(covariates = .) %>%
    mutate(row_id = row_number()) %>%
    pivot_wider(names_from = row_id, values_from = covariates) %>%
    unite(col = "expression", sep = " + ") %>%
    pull()
  
  # convert to formula
  cov_formula <- as.formula(paste("investment ~ ", cov_addition))
  
  # Propensity Score Estimation
  print("Running linear model on investment and covariates")
  m_ps <-
    glm(
      # put covariates on right hand side
      cov_formula,
      family = binomial(),
      data = neighborhood_df
    ) 
  
  ## Generate propensity score df
  print("Generating prs dataframe")
  prs_df <-
    data.frame(
      pr_score = predict(m_ps, type = "response"),
      investment = m_ps$model$investment
    )
  
  # Matching algorithm
  # MatchIt does not allow missing values, must drop NA
  neighborhood_nomiss <-
    neighborhood_df %>%
    # select for relevant variables
    select(investment, one_of(neighborhood_cov)) %>%
    # drop NAs
    na.omit()
  
  # use "nearest" method
  mod_match <-
    matchit(
      # put covariates on right hand side
      cov_formula,
      method = "nearest",
      data = neighborhood_nomiss
    )
  
  ## Create matched df
  # convert to df
  dta_m <- match.data(mod_match)
  
  # return the mathced df
  return(dta_m)
}

common_support_plot <- function(neighborhood_test, neighborhood_cov) {
  
  # create an expression that adds up each covariate
  # concatenate each covariate into a string with "+" separator
  cov_addition <-
    neighborhood_cov %>%
    tibble(covariates = .) %>%
    mutate(row_id = row_number()) %>%
    pivot_wider(names_from = row_id, values_from = covariates) %>%
    unite(col = "expression", sep = " + ") %>%
    pull()
  
  # convert to formula
  cov_formula <- as.formula(paste("investment ~ ", cov_addition))
  
  # Propensity Score Estimation
  print("Running linear model on investment and covariates")
  m_ps <-
    glm(
      # put covariates on right hand side
      cov_formula,
      family = binomial(),
      data = neighborhood_test
    ) 
  
  ## Generate propensity score df
  print("Generating prs dataframe")
  prs_df <-
    data.frame(
      pr_score = predict(m_ps, type = "response"),
      investment = m_ps$model$investment
    )
  ## Examine region of common support
  print("Plot of the region of common support for prs_df")
  prs_df %>%
    mutate(
      investment =
        if_else(
          investment == 1,
          "Neighborhood investment",
          "No neighborhood investment"
        )
    ) %>%
    ggplot(aes(x = pr_score)) +
    geom_histogram(color = "white") +
    facet_wrap(~investment) +
    xlab("Probability of neighborhood investment") +
    theme_bw()
}

aasd <- function(dta_m, neighborhood_cov) {
  ## Average absolute standardized difference
  # generate table of standardized diff by covariate
  matched_table <-
    CreateTableOne(
      vars = neighborhood_cov,
      strata = "investment",
      data = dta_m,
      test = FALSE
    )
  
  # extract standardized mean diff
  extracted_smd <- ExtractSmd(matched_table)
  
  # create analysis table of SMD and var name
  smd_table <-
    tibble(
      var_name = matched_table$MetaData$vars,
      smd = as.double(extracted_smd)
    )
  
  #calculate average absolute value of SMD across all covariates
  #(closer to 0 is better)
  abs_std_diff <- abs(sum(smd_table$smd) / count(smd_table))
  
  # return the value
  return(abs_std_diff)
}

covariate_balance_plot <- function(dta_m, neighborhood_cov) {
  
  # Examine covariate balance in matching sample
  ## Visual inspection
  # create function to print grid of tables
  fn_bal <- function(dta, variable) {
    dta$variable <- dta[, variable]
    # convert to factor
    dta$investment <- as.factor(dta$investment)
    # create limits based on max/min
    support <- c(min(dta$variable), max(dta$variable))
    # plot propensity score for variable
    dta %>%
      # plot distance by variable, color by presence of investment
      ggplot(aes(x = distance, y = variable, color = investment)) +
      geom_point(alpha = 0.2, size = 1.3) +
      # create smooted line over points
      geom_smooth(method = "loess", se = F) +
      xlab("Propensity score") +
      # label plot by variable
      ylab(variable) +
      theme_bw() +
      # input max/min limits
      ylim(support)
  }
  
  # generate list of functions to carry out
  fun_list <- list()
  for (variable in 1:length(neighborhood_cov)) {
    fun_list[[variable]] <-
      # if even index number remove legend
      if (variable <= (length(neighborhood_cov) / 2)) {
        fn_bal(dta_m, neighborhood_cov[variable]) + theme(legend.position = 'none')
        # if odd index number keep legend
      } else {
        fn_bal(dta_m, neighborhood_cov[variable])
      }
  }
  
  print("Plots showing covariate balance")
  grid.arrange(grobs = fun_list, nrow = 4, as.table = FALSE)
  #do.call(grid.arrange, fun_list)
  #fun_list
}