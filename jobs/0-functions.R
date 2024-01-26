#********************************************************
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: CREATES FUNCTIONS USED THROUGHOUT THE DATA ANALYSIS PIPELINE
# AUTHOR: NINO CRICCO
# LAST UPDATED: 12/06/2023 (dmy)
#********************************************************

# This function does the opposite of %in% - works as an inverse selector
'%!in%' <- function(x,y)!('%in%'(x,y))

# This function converts all values in a vector to NA
na_codes <- function(x, ...) {
  x[x %in% c(...)] <- NA
  x
}

# This function extracts all numeric characters from a string
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

# This function calculates the p-value appropriate for the combined
# regression coefficients across imputations
MIcombineP <- function(MIcombineRes,digits=3) {
  tStat <- MIcombineRes$coefficients/sqrt(diag(MIcombineRes$variance))
  round(2*pt(-abs(tStat),df=MIcombineRes$df),digits)
}

# This functions takes as arguments the data, years, filtering conditions for the data,
# a vector of covariates, a vector naming an outcome, and a vector naming weights 
# and generates a table with weighted means of all covariates and the outcome for each year
# To see an example of the arguments/change them, see 3-analysis-arguments.R in replication file
generate_means_year_table <- function(data, 
                                      years,
                                      sample_conditions,
                                      covariates,
                                      outcome,
                                      weights) {
  
  # Convert the user-supplied weights string to a symbol
  weights_sym <- rlang::sym(weights)
  
  
  # Create a single combined expression for the filtering conditions
  conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")
  
  # Filtering the data according to the conditions in the argument
  data <- data %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1)
  
  # Update the rest of the function with the user-supplied weights column name
  summary_tables <- data %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    dplyr::select(rlang::sym(outcome), all_of(covariates), weights) %>%
    summarise(across(
      everything(),
      list(mean = ~wtd.mean(., w = !!weights_sym), sd = ~sqrt(wtd.var(., w = !!weights_sym))), # Updated here
      .names = "{col}_w{fn}"
    )) %>%
    mutate("(Intercept)" = 1) %>%
    filter(.imp != 0) %>%
    dplyr::select(year, female, "(Intercept)", everything()) %>%
    dplyr::select(-c(.imp, perwt_wmean, perwt_wsd)) %>%
    ungroup() %>%
    group_by(year, female) %>%
    summarise_all(mean) %>%
    rename_with(~gsub("_w", "_", .x))
  
  # Filtering and returning the final table
  means.year_table <- summary_tables %>%
    left_join(., data %>%
                filter(.imp == 1) %>% # Selecting data from a single imputation
                filter(year %in% years) %>%
                group_by(year, female) %>% # Getting the n's by sex and year
                summarise(n = n()))
  
  return(means.year_table)
}

# This functions takes as arguments the data, years, filtering conditions for the data,
# a vector of covariates, a vector naming an outcome, a vector naming weights, and a 
# vector specifying a group id and generates a table with weighted coefficients 
# regressing the outcome on all covariates for each year, separately by group
# To see an example of the arguments/change them, see 3-analysis-arguments.R in replication file
generate_regression_table <- function(data,
                                      years,
                                      sample_conditions,
                                      covariates,
                                      weights,
                                      group,
                                      outcome) {
  
  # Filters the data according to the conditions provided
  conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")
  data <- data %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1)
  
  # Creates survey design objects for each group and each year
  designs <- list()
  for (year_iter in years) {
    for (group_val in unique(data[[group]])) {
      imp_data <- data %>%
        filter(.imp > 0) %>%
        filter(year == year_iter, !!sym(group) == group_val) %>%
        group_split(.imp)
      designs[[paste0("design_", group_val, "_", year_iter)]] <- imp_data
    }
  }
  
  # Specifies a formula that writes the outcome as a function of the 
  # specified covariates for each imputation, year, and group combination
  run_model <- function(imp_data, year_iter, group_val) {
    formula <- reformulate(covariates, response = outcome)
  
    # If the data is imputed (length(imp_data > 1)), runs a weighted 
    # regression using the survey design object and combines the results across imputations
    # If data is not imputed or contains a single imputation, runs weighted regression
    # using the survey design object 
    if (inherits(imp_data, "imputationList") && length(imp_data) > 1) {
      design <- svydesign(ids = ~0, data = imp_data, weights = ~perwt, nest = T)
      model <- with(design, svyglm(formula))
      model <- MIcombine(model)
      p_values <- MIcombineP(model)
      se_values <- vcov::se(model)
    } else {
      design <- svydesign(ids = ~0, data = imp_data[[1]], weights = ~perwt, nest = T)
      model <- svyglm(formula, design = design)
      t_stat <- coef(model) / survey::SE(model)
      p_values <- 2 * pt(-abs(t_stat), df = df.residual(model))
      se_values <- survey::SE(model)
    }
    
    # Combines results in a single table 
    result <- bind_rows(coef(model), se_values, p_values) %>%
      mutate(year = year_iter, !!sym(group) := group_val, estimate = c("coef", "se", "p_value"))
    
    return(result)
  }
  
  regression.coefs <- list()
  for (year_iter in years) {
    for (group_val_iter in unique(data[[group]])) {
      imp_data <- designs[[paste0("design_", group_val_iter, "_", year_iter)]]
      model_result <- run_model(imp_data, year_iter = year_iter, group_val = group_val_iter)
      coef_name <- paste0("coef_", group_val_iter, "_", year_iter)
      regression.coefs[[coef_name]] <- model_result
    }
  }
  regression.coefs <- bind_rows(regression.coefs)
  
  return(regression.coefs)
}

# This functions takes as arguments the data, years, filtering conditions for the data,
# a vector of covariates, a vector naming an outcome, a vector naming weights, a vector 
# specifying a scale, and a vector specifying a group id and generates a table with the characteristics
# component of the regression. Since at different points, we use different sets of means or coefficients
# to generate the characteristics components, this function also takes as arguments pre-specified
# means and coefficient tables, though if those are not provided it will call the functions that 
# create the means table and the regression coefficients table
# To see an example of the arguments/change them, see 3-analysis-arguments.R in replication file
characteristics_component <- function(data, group, years, outcome, covariates, weights, sample_conditions, scale = "percent", means_year_obj = NULL, baseline_coefs_obj = NULL) {
  
  # This section tells the function to use the prespecified means and coefficients
  # tables if provided- if not, it generates them 
  if (is.null(means_year_obj)) {
    means.year <- generate_means_year_table(data, years = years, covariates, outcome = outcome,
                                            weights = weights, sample_conditions = sample_conditions) %>%
      dplyr::select(year, female, "(Intercept)", ends_with("_mean"))
    names(means.year) <- gsub("_mean", "", names(means.year))
  } else {
    means.year <- means_year_obj
  }
  
  if (is.null(baseline_coefs_obj)) {
    baseline.coefs <- generate_regression_table(data, group = group, years,
                                                outcome = outcome, covariates, 
                                                weights = weights, sample_conditions = sample_conditions)
  } else {
    baseline.coefs <- baseline_coefs_obj
  }
  
  # Generates the denominator to be used as the "percent of change in difference in outcome explained" 
  denom <- (means.year %>% ungroup() %>% filter(year %in% years[[2]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome)) - 
              means.year %>% ungroup() %>%filter(year %in% years[[1]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome))) - 
    (means.year %>% ungroup() %>%filter(year %in% years[[2]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)) - 
       means.year %>% ungroup() %>% filter(year %in% years[[1]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)))
  
  # Creates objects needed to compute the characteristics change component in the decomposition
  chargap_0 <- means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[2]) - means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[1])
  chargap_1 <- means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[2]) - means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[1])
  
  coef_t1_0 <- baseline.coefs %>% filter(!!rlang::sym(group) == 0, year %in% years[1], estimate == "coef") %>%
    dplyr::select(-c(estimate, rlang::sym(group), year)) %>%
    t() %>% as.matrix()
  
  coef_t1_1 <- baseline.coefs %>% filter(!!rlang::sym(group) == 1, year %in% years[1], estimate == "coef") %>%
    dplyr::select(-c(estimate, {{group}}, year)) %>%
    t() %>% as.matrix()
  
  chargap_baseline_0 <- chargap_0[names(chargap_0) %in% c("(Intercept)", covariates)] %>% as.matrix()
  chargap_baseline_1 <- chargap_1[names(chargap_1) %in% c("(Intercept)", covariates)] %>% as.matrix()
  
  # Numerator for the detailed characteristics component, Baseline model
  char_num_baseline_group0 <-  coef_t1_0 * t(chargap_baseline_0)
  char_num_baseline_group1 <-  coef_t1_1 * t(chargap_baseline_1)
  char_num_baseline <- char_num_baseline_group0 - char_num_baseline_group1
  
  # Scaled by the denominator (change in the gender wage gap) if that argument is provided (default),
  # or can just show results in terms of change in log points
  if (scale == "percent") {
    char_baseline <- char_num_baseline %>% as.data.frame() %>% transmute(chargap = V1/denom[[1]])
    char_baseline_0 <- char_num_baseline_group0 %>% as.data.frame() %>% transmute(chargap = V1/denom[[1]])
    char_baseline_1 <- char_num_baseline_group1 %>% as.data.frame() %>% transmute(chargap = V1/denom[[1]]) %>%
      mutate_if(is.numeric, funs(.*-1)) 
  } else if (scale == "log_points") {
    char_baseline <- char_num_baseline %>% as.data.frame() %>% transmute(chargap = V1)
    char_baseline_0 <- char_num_baseline_group0 %>% as.data.frame() %>% transmute(chargap = V1)
    char_baseline_1 <- char_num_baseline_group1 %>% as.data.frame() %>% transmute(chargap = V1) %>%
      mutate_if(is.numeric, funs(.*-1)) 
  } else {
    stop("Invalid 'scale' argument. It must be either 'percent' or 'log_points'.")
  }
  
  return(list("crossgroup" = char_baseline %>% mutate(group = "crossgroup"), 
              "group0" = char_baseline_0 %>% mutate(group = "group0"),
              "group1" = char_baseline_1 %>% mutate(group = "group1")))
}


# This functions takes as arguments the data, years, filtering conditions for the data,
# a vector of covariates, a vector naming an outcome, a vector naming weights, a vector 
# specifying a scale, and a vector specifying a group id and generates a table with the characteristics
# component of the regression. Since at different points, we use different sets of means or coefficients
# to generate the returns components, this function also takes as arguments pre-specified
# means and coefficient tables, though if those are not provided it will call the functions that 
# create the means table and the regression coefficients table
# To see an example of the arguments/change them, see 3-analysis-arguments.R in replication file
returns_component <- function(data, group, years, outcome, covariates,
                              weights, sample_conditions, scale = "percent",
                              means_year_obj = NULL, baseline_coefs_obj = NULL) {
  
  if (is.null(means_year_obj)) {
    means.year <- generate_means_year_table(data, years = years, covariates, outcome = outcome,
                                            weights = weights, sample_conditions = sample_conditions) %>%
      dplyr::select(year, female, "(Intercept)", ends_with("_mean"))
    names(means.year) <- gsub("_mean", "", names(means.year))
  } else {
    means.year <- means_year_obj
  }
  
  if (is.null(baseline_coefs_obj)) {
    baseline.coefs <- generate_regression_table(data, group = group, years,
                                                outcome = outcome, covariates, 
                                                weights = weights, sample_conditions = sample_conditions)
  } else {
    baseline.coefs <- baseline_coefs_obj
  }
  
  denom <- (means.year %>% ungroup() %>% filter(year %in% years[[2]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome)) - 
              means.year %>% ungroup() %>%filter(year %in% years[[1]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome))) - 
    (means.year %>% ungroup() %>%filter(year %in% years[[2]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)) - 
       means.year %>% ungroup() %>% filter(year %in% years[[1]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)))
  
  coefgap_0 <- baseline.coefs %>% filter(!!rlang::sym(group) == 0, estimate == "coef", year %in% years[2]) %>% 
    dplyr::select(-c(estimate, !!rlang::sym(group), year)) -
    baseline.coefs %>% filter(!!rlang::sym(group) == 0, estimate == "coef", year %in% years[1]) %>%
    dplyr::select(-c(estimate, !!rlang::sym(group), year))
  
  coefgap_1 <- baseline.coefs %>% filter(!!rlang::sym(group) == 1, estimate == "coef", year %in% years[2]) %>% 
    dplyr::select(-c(estimate, !!rlang::sym(group), year)) -
    baseline.coefs %>% filter(!!rlang::sym(group) == 1, estimate == "coef", year %in% years[1]) %>%
    dplyr::select(-c(estimate, !!rlang::sym(group), year))
  
  char_t1_0 <- means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[1])
  char_t1_1 <- means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[1])
  
  # Men's characteristics levels at t1 * change's in men's regression coefficients from the Baseline model
  returns_t1_0 <- char_t1_0[names(char_t1_0) %in% c("(Intercept)", covariates)] %>% as.matrix() * coefgap_0
  returns_t1_1 <- char_t1_1[names(char_t1_1) %in% c("(Intercept)", covariates)] %>% as.matrix() * coefgap_1
  
  # Numerator for the detailed returns component, Baseline model model
  returns_num_baseline <- (returns_t1_0 - returns_t1_1)
  
  # Scaled by the denominator (change in the gender wage gap):
  if (scale == "percent") {
    returns_baseline <- returns_num_baseline %>% t() %>% as.data.frame() %>% transmute(returns = V1/denom[[1]])
    returns_baseline_0 <- returns_t1_0 %>% t() %>% as.data.frame() %>% transmute(returns = V1/denom[[1]])
    returns_baseline_1 <- returns_t1_1 %>% t() %>% as.data.frame() %>% transmute(returns = V1/denom[[1]]) %>%
      mutate_if(is.numeric, funs(.*-1)) 
  } else if (scale == "log_points") {
    returns_baseline <- returns_num_baseline %>% t() %>% as.data.frame() %>% transmute(returns = V1)
    returns_baseline_0 <- returns_t1_0 %>% t() %>% as.data.frame() %>% transmute(returns = V1)
    returns_baseline_1 <- returns_t1_1 %>% t() %>% as.data.frame() %>% transmute(returns = V1) %>%
      mutate_if(is.numeric, funs(.*-1)) 
  } else {
    stop("Invalid 'scale' argument. It must be either 'percent' or 'log_points'.")
  }
  
  return(list("crossgroup" = returns_baseline %>% mutate(group = "crossgroup"), 
              "group0" = returns_baseline_0 %>% mutate(group = "group0"),
              "group1" = returns_baseline_1 %>% mutate(group = "group1")))
}

# As with the other two components, this functions takes as arguments the data, 
# years, filtering conditions for the data, a vector of covariates, a vector naming an outcome, 
# a vector naming weights, a vector  specifying a scale, and a vector specifying a group id and 
# generates a table with the characteristics
# component of the regression. Since at different points, we use different sets of means or coefficients
# to generate the returns components, this function also takes as arguments pre-specified
# means and coefficient tables, though if those are not provided it will call the functions that 
# create the means table and the regression coefficients table
# To see an example of the arguments/change them, see 3-analysis-arguments.R in replication file

interactions_component <- function(data, group, years, outcome, covariates,
                                   weights, sample_conditions, scale = "percent",
                                   means_year_obj = NULL, baseline_coefs_obj = NULL) {
  
  if (is.null(means_year_obj)) {
    means.year <- generate_means_year_table(data, years = years, covariates, outcome = outcome,
                                            weights = weights, sample_conditions = sample_conditions) %>%
      dplyr::select(year, female, "(Intercept)", ends_with("_mean"))
    names(means.year) <- gsub("_mean", "", names(means.year))
  } else {
    means.year <- means_year_obj
  }
  
  if (is.null(baseline_coefs_obj)) {
    baseline.coefs <- generate_regression_table(data, group = group, years,
                                                outcome = outcome, covariates, 
                                                weights = weights, sample_conditions = sample_conditions)
  } else {
    baseline.coefs <- baseline_coefs_obj
  }
  
  denom <- (means.year %>% ungroup() %>% filter(year %in% years[[2]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome)) - 
              means.year %>% ungroup() %>%filter(year %in% years[[1]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome))) - 
    (means.year %>% ungroup() %>%filter(year %in% years[[2]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)) - 
       means.year %>% ungroup() %>% filter(year %in% years[[1]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)))
  
  coefgap_0 <- baseline.coefs %>% filter(!!rlang::sym(group) == 0, estimate == "coef", year %in% years[2]) %>% 
    dplyr::select(-c(estimate, !!rlang::sym(group), year)) -
    baseline.coefs %>% filter(!!rlang::sym(group) == 0, estimate == "coef", year %in% years[1]) %>%
    dplyr::select(-c(estimate, !!rlang::sym(group), year))
  
  coefgap_1 <- baseline.coefs %>% filter(!!rlang::sym(group) == 1, estimate == "coef", year %in% years[2]) %>% 
    dplyr::select(-c(estimate, !!rlang::sym(group), year)) -
    baseline.coefs %>% filter(!!rlang::sym(group) == 1, estimate == "coef", year %in% years[1]) %>%
    dplyr::select(-c(estimate, !!rlang::sym(group), year))
  
  chargap_0 <- means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[2]) - means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[1])
  chargap_1 <- means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[2]) - means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[1])
  
  chargap_baseline_0 <- chargap_0[names(chargap_0) %in% c("(Intercept)", covariates)] %>% as.matrix()
  chargap_baseline_1 <- chargap_1[names(chargap_1) %in% c("(Intercept)", covariates)] %>% as.matrix()
  
  char_t1_0 <- means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[1])
  char_t1_1 <- means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[1])
  
  # Interactions Component, Baseline model
  interaction_num_baseline_0 <- (coefgap_0 * chargap_baseline_0)
  interaction_num_baseline_1 <- (coefgap_1 * chargap_baseline_1)
  interaction_num_baseline <- interaction_num_baseline_0-interaction_num_baseline_1
  
  # Scaled by the denominator (change in the gender wage gap):
  if (scale == "percent") {
    interaction_baseline <- interaction_num_baseline %>% t() %>% as.data.frame() %>% transmute(returns = V1/denom[[1]])
    interaction_baseline_0 <- interaction_num_baseline_0 %>% t() %>% as.data.frame() %>% transmute(returns = V1/denom[[1]])
    interaction_baseline_1 <- interaction_num_baseline_1 %>% t() %>% as.data.frame() %>% transmute(returns = V1/denom[[1]]) %>%
      mutate_if(is.numeric, funs(.*-1)) 
  } else if (scale == "log_points") {
    interaction_baseline <- interaction_num_baseline %>% t() %>% as.data.frame() %>% transmute(returns = V1)
    interaction_baseline_0 <- interaction_num_baseline_0 %>% t() %>% as.data.frame() %>% transmute(returns = V1)
    interaction_baseline_1 <- interaction_num_baseline_1 %>% t() %>% as.data.frame() %>% transmute(returns = V1) %>%
      mutate_if(is.numeric, funs(.*-1)) 
  } else {
    stop("Invalid 'scale' argument. It must be either 'percent' or 'log_points'.")
  }
  
  return(list("crossgroup" = interaction_baseline %>% mutate(group = "crossgroup"), 
              "group0" = interaction_baseline_0 %>% mutate(group = "group0"),
              "group1" = interaction_baseline_1 %>% mutate(group = "group1")))
}

# This function combines the same code used across all the separate component funtions into 
# one function.
decomposition_analysis <- function(data, group, years, outcome, covariates,
                                   weights, sample_conditions, scale = "percent",
                                   means_year_obj = NULL, baseline_coefs_obj = NULL) {
  
  if (is.null(means_year_obj)) {
    # Call generate_means_year_table to get means.year object
    means.year <- generate_means_year_table(data, years = years, covariates, outcome = outcome,
                                            weights = weights, sample_conditions = sample_conditions) %>%
      dplyr::select(year, female, "(Intercept)", ends_with("_mean"))
    # Setting names to the weighted means table
    names(means.year) <- gsub("_mean", "", names(means.year))
  } else {
    means.year <- means_year_obj
  }
  
  if (is.null(baseline_coefs_obj)) {
    # Call generate_regression_table to get baseline.coefs object
    baseline.coefs <- generate_regression_table(data, group = group, years,
                                                outcome = outcome, covariates, 
                                                weights = weights, sample_conditions = sample_conditions)
  } else {
    baseline.coefs <- baseline_coefs_obj
  }
  
  # Calculating the denom object needed for decomposition
  
  denom <- (means.year %>% ungroup() %>% filter(year %in% years[[2]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome)) - 
              means.year %>% ungroup() %>%filter(year %in% years[[1]], !!rlang::sym(group) == 0) %>%dplyr::select(!!rlang::sym(outcome))) - 
    (means.year %>% ungroup() %>%filter(year %in% years[[2]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)) - 
       means.year %>% ungroup() %>% filter(year %in% years[[1]], !!rlang::sym(group) == 1) %>%dplyr::select(!!rlang::sym(outcome)))
  
  # Creating objects that compute group-specific characteristic changes and group-specific characteristic levels at t1
  chargap_0 <- means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[2]) - means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[1])
  chargap_1 <- means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[2]) - means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[1])
  char_t1_0 <- means.year %>% filter(!!rlang::sym(group) == 0, year %in% years[1])
  char_t1_1 <- means.year %>% filter(!!rlang::sym(group) == 1, year %in% years[1])
  
  # Men's Baseline model coefficients at t1
  coef_t1_0 <- baseline.coefs %>% filter(!!rlang::sym(group) == 0, year %in% years[1], estimate == "coef") %>%
    dplyr::select(-c(estimate, rlang::sym(group), year)) %>%
    t() %>% as.matrix()
  
  # Women's Baseline model coefficients at t1
  coef_t1_1 <- baseline.coefs %>% filter(!!rlang::sym(group) == 1, year %in% years[1], estimate == "coef") %>%
    dplyr::select(-c(estimate, {{group}}, year)) %>%
    t() %>% as.matrix()
  
  # Characteristic change among men, baseline model characteristics
  chargap_baseline_0 <- chargap_0[names(chargap_0) %in% c("(Intercept)", covariates)] %>% as.matrix()
  
  # Characteristic change among women, Baseline model characteristics
  chargap_baseline_1 <- chargap_1[names(chargap_1) %in% c("(Intercept)", covariates)] %>% as.matrix()
  
  # Numerator for the detailed characteristics component, Baseline model model
  char_num_baseline_group0 <-  coef_t1_0 * t(chargap_baseline_0)
  char_num_baseline_group1 <-  coef_t1_1 * t(chargap_baseline_1)
  char_num_baseline <- char_num_baseline_group0 - char_num_baseline_group1
  
  # Scaled by the denominator (change in the gender wage gap):
  if (scale == "percent") {
    char_baseline <- char_num_baseline/denom[[1]]
    char_baseline_0 <- char_num_baseline_group0/denom[[1]]
    char_baseline_1 <- char_num_baseline_group1/denom[[1]]
  } else if (scale == "log_points") {
    char_baseline <- char_num_baseline
    char_baseline_0 <- char_num_baseline_group0 
    char_baseline_1 <- char_num_baseline_group1
  } else {
    stop("Invalid 'scale' argument. It must be either 'percent' or 'log_points'.")
  }
  
  # Returns component
  
  # Change's in men's regression coefficients from the Baseline model
  coefgap_0 <- baseline.coefs %>% filter(!!rlang::sym(group) == 0, estimate == "coef", year %in% years[2]) %>% 
    dplyr::select(-c(estimate, !!rlang::sym(group), year)) -
    baseline.coefs %>% filter(!!rlang::sym(group) == 0, estimate == "coef", year %in% years[1]) %>%
    dplyr::select(-c(estimate, !!rlang::sym(group), year))
  
  # Change's in women's regression coefficients from the Baseline model
  coefgap_1 <- baseline.coefs %>% filter(!!rlang::sym(group) == 1, estimate == "coef", year %in% years[2]) %>% 
    dplyr::select(-c(estimate, !!rlang::sym(group), year)) -
    baseline.coefs %>% filter(!!rlang::sym(group) == 1, estimate == "coef", year %in% years[1]) %>%
    dplyr::select(-c(estimate, !!rlang::sym(group), year))
  
  # Men's characteristics levels at t1 * change's in men's regression coefficients from the Baseline model
  returns_t1_0 <- char_t1_0[names(char_t1_0) %in% c("(Intercept)", covariates)] %>% as.matrix() * coefgap_0
  
  # Women's characteristics levels at t1 * change's in men's regression coefficients from the Baseline model
  returns_t1_1 <- char_t1_1[names(char_t1_1) %in% c("(Intercept)", covariates)] %>% as.matrix() * coefgap_1
  
  # Numerator for the detailed returns component, Baseline model model
  returns_num_baseline <- (returns_t1_0 - returns_t1_1)
  
  # Scaled by the denominator (change in the gender wage gap):
  if (scale == "percent") {
    returns_baseline <- returns_num_baseline/denom[[1]]
    returns_baseline_0 <- returns_t1_0/denom[[1]]
    returns_baseline_1 <- returns_t1_1/denom[[1]]
  } else if (scale == "log_points") {
    returns_baseline <- returns_num_baseline
    returns_baseline_0 <- returns_t1_0
    returns_baseline_1 <- returns_t1_1
  } else {
    stop("Invalid 'scale' argument. It must be either 'percent' or 'log_points'.")
  }
  
  # Interactions Component, Baseline model 
  interaction_num_baseline_0 <- (coefgap_0 * chargap_baseline_0)
  interaction_num_baseline_1 <- (coefgap_1 * chargap_baseline_1)
  interaction_num_baseline <- interaction_num_baseline_0-interaction_num_baseline_1
  
  # Scaled by the denominator (change in the gender wage gap):
  if (scale == "percent") {
    interaction_baseline <- interaction_num_baseline/denom[[1]]
    interaction_baseline_0 <- interaction_num_baseline_0/denom[[1]]
    interaction_baseline_1 <- interaction_num_baseline_1/denom[[1]]
  } else if (scale == "log_points") {
    interaction_baseline <- interaction_num_baseline
    interaction_baseline_0 <- interaction_num_baseline_0
    interaction_baseline_1 <- interaction_num_baseline_1
  } else {
    stop("Invalid 'scale' argument. It must be either 'percent' or 'log_points'.")
  }
  
  # Groups the components from the Baseline model model into a single object
  t3_baseline_both <- bind_cols(char_baseline %>% as.data.frame() %>% rename("Characteristics Gap" = V1),
                                t(returns_baseline) %>% as.data.frame() %>% rename("Returns" = V1),
                                t(interaction_baseline) %>% as.data.frame() %>% rename("Interaction" = V1))
  
  t3_baseline_group0 <- bind_cols(char_baseline_0 %>% as.data.frame() %>% rename("Characteristics Gap" = V1),
                                  t(returns_baseline_0) %>% as.data.frame() %>% rename("Returns" = V1),
                                  t(interaction_baseline_0) %>% as.data.frame() %>% rename("Interaction" = V1)) 
  
  t3_baseline_group1 <- bind_cols(char_baseline_1 %>% as.data.frame() %>% rename("Characteristics Gap" = V1),
                                  t(returns_baseline_1) %>% as.data.frame() %>% rename("Returns" = V1),
                                  t(interaction_baseline_1) %>% as.data.frame() %>% rename("Interaction" = V1)) %>%
    mutate_if(is.numeric, funs(.*-1)) 
  
  return(list("crossgroup" = t3_baseline_both %>% mutate(group = "crossgroup"), 
              "group0" = t3_baseline_group0 %>% mutate(group = "group0"),
              "group1" = t3_baseline_group1 %>% mutate(group = "group1"), 
              "coefs" = baseline.coefs, 
              "means" = means.year))
}

# This function helps simplify doing the same operations for each of our models
generate_regression_table_func <- function(exclude_covariates, model_label, weights){
  generate_regression_table(
    weights = weights, 
    sample_conditions = sample_conditions, 
    data = data, 
    years = years, 
    outcome = outcome, 
    group = group,
    covariates = covariates[covariates %!in% exclude_covariates]
  ) %>%
    mutate(model = model_label)
}


#  Defining a function that repeats the decomposition operations across different models
perform_decomposition_analysis <- function(exclude_covariates, model_label){
  decomposition_analysis(
    weights = weights, 
    sample_conditions = sample_conditions, 
    data = data, 
    years = years, 
    outcome = outcome, 
    group = group,
    covariates = covariates[covariates %!in% exclude_covariates]
  )[c("crossgroup", "group0", "group1")] %>%
    bind_cols() %>%
    mutate(var = rownames(.)) %>%
    dplyr::select(var, everything(), -starts_with("group")) %>%
    adorn_totals("row") %>%
    mutate(model = model_label)
}

# This function computes the characteristics component for each decade and model, 
# when running the results within time periods that add up to the change across the 
# total time period
get_characteristics_component <- function(years, model_name, covariates_to_exclude, year_name, scale) {
  characteristics_component(weights = weights, years = years,
                            sample_conditions = sample_conditions, 
                            data = data, scale = scale,
                            baseline_coefs_obj = generate_regression_table(
                              data, group = group, years = 1981, outcome = outcome,  
                              weights = weights, sample_conditions = sample_conditions, 
                              covariates = covariates[covariates %!in% covariates_to_exclude]) %>%
                              mutate(year = years[1]), 
                            outcome = outcome, group = group,
                            covariates = covariates[covariates %!in% covariates_to_exclude])["crossgroup"] %>%
    as.data.frame() %>% 
    mutate(var = rownames(.), year = year_name, model = model_name) %>% 
    select(var, Characteristics.Gap = crossgroup.chargap, year, model)
}