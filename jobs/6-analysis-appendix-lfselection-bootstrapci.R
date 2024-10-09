#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: ADJUSTING FOR CHANGING (OBSERVED) PATTERNS OF 
# LABOR FORCE SELECTION
# GENERATES BOOTSTRAPPED ESTIMATES FOR CONFIDENCE INTERVALS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

date_run_id <-  "2024-06-07"

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/1-functions.R")
source("jobs/1-load-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv(paste0("clean_data/psid_final_", date_run_id, ".csv"))
source("jobs/4-analysis-arguments.R")

for(i in 1:10000){
  # First creating a subset of the full data restriting only by age and region
  
  benchmark <- psid_imp %>%
    mutate(age.spline1 = ifelse(age < 45, age, 45), 
           age.spline2 = ifelse(age >= 45, age-45, 0)) %>%
    filter(samp.inc.age == 1, samp.exc.region != 1, year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(2216, 20), rep(2517, 20), rep(3569, 20), rep(4258, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  prob.inc <- benchmark %>% 
    filter(year %in% years) %>%
    group_by(female, year) %>% 
    summarise(Prob.Inclusion = wtd.mean(samp.inc.final, weights = perwt_norm))

# Logit model for probability in the labor market in the early period:
# Age, race, region, marital status, education, fertility
benchmark.mod <- benchmark %>% 
  group_by(female, year, .imp) %>%
  nest() %>%
  mutate(lf.model = 
           map(data, ~ glm(
             samp.inc.final ~ age.spline1 + age.spline2 + Northeast + Northcentral + South + 
               Black + Hispanic + Other + HighSchool + SomeCollege + ba.avdeg + married +
               num.kids.cont + age.youngest.cat_zerotofive +
               age.youngest.cat_sixtoseventeen, family = binomial(link="logit"),
             data = ., weights = perwt_norm))) %>%
  dplyr::select(-data) %>% 
  arrange(year, female)

# Using coefficients for this 1980 logit model to predict probability of
# full-time worker in the later period, using later period covariates: sample
# includes military, part-time workers, etc. 
psid.rw <- benchmark %>% 
  group_by(female, year, .imp) %>%
  nest() %>%
  arrange(year, female) %>%
  left_join(., benchmark.mod %>% filter(year == 1981) %>%
              rename(lf.model_1981 = lf.model) %>% ungroup() %>%
              dplyr::select(-year),
            by = c("female", ".imp")) %>%
  mutate(lf.pred_1981 = map2(lf.model_1981, data, 
                             ~predict(.x, newdata = .y, type = "response"))) %>%
  unnest(data, lf.pred_1981) %>% 
  dplyr::select(-lf.model_1981) %>%
  group_by(female, year, .imp) %>%
  nest() %>%
  arrange(year, female) %>%
  left_join(., benchmark.mod %>% filter(year == 2019) %>%
              rename(lf.model_2019 = lf.model) %>% ungroup() %>%
              dplyr::select(-year),
            by = c("female", ".imp")) %>%
  mutate(lf.pred_2019 = map2(lf.model_2019, data, 
                             ~predict(.x, newdata = .y, type = "response"))) %>%
  unnest(data, lf.pred_2019) %>% 
  dplyr::select(-lf.model_2019) %>%
  # First set of weights multiplies sampling weights in 2017 by predicted probability
  # in the labor force in earlier period for all sample members. Second set of weights
  # does the same, but only for women, keeping men's sampling weights in 2017 the same
  mutate(lf.pred.w = ifelse(year == 1981, perwt_norm, 
                            perwt_norm * (lf.pred_1981/lf.pred_2019))) %>%
  filter(samp.inc.final == 1) %>%
  filter(year %in% years)

# Creating an object to use in the decompositions using the sample means
means.year_sampweights <- psid.rw %>%
  group_by(year, female, .imp) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(all_of(outcome), all_of(covariates), expf, White, West, LessthanHS, 
                weights, lf.pred.w) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = perwt_norm))) %>% 
  mutate("(Intercept)" = 1) %>% # Creating an intercept column
  dplyr::select(year, female, "(Intercept)", everything()) %>% # Ordering the columns
  dplyr::select(-c(.imp, lf.pred.w_wmean)) %>% # Removing weight and imputation column
  ungroup() %>%
  group_by(year, female) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) %>%
  mutate(year = as.character(year))

# Creating an object to use in the decompositions using the sample means
means.year_rw <- psid.rw %>%
  group_by(year, female, .imp) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(all_of(outcome), all_of(covariates), expf, White, West, LessthanHS, 
                perwt_norm, lf.pred.w) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = lf.pred.w))) %>% 
  mutate("(Intercept)" = 1) %>% # Creating an intercept column
  dplyr::select(year, female, "(Intercept)", everything()) %>% # Ordering the columns
  dplyr::select(-c(.imp, lf.pred.w_wmean)) %>% # Removing weight and imputation column
  ungroup() %>%
  group_by(year, female) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) %>%
  mutate(year = paste(year, "RW", sep = "_"))

# Setting names to the weighted means table
names(means.year_sampweights) <- gsub("_wmean", "", names(means.year_sampweights))
names(means.year_rw) <- gsub("_wmean", "", names(means.year_rw))

# Generating the decomposition
# First get the weighted coefficients with sample weights and with 
# labor force adjusted weights
coefs.sw <- mapply(generate_regression_table_func, 
                   covariates_to_exclude, model_labels, weights = weights, 
                   SIMPLIFY = FALSE) %>% 
  bind_rows()

coefs.lfw <- mapply(generate_regression_table_func, 
                    covariates_to_exclude, model_labels, weights = "lf.pred.w", 
                    SIMPLIFY = FALSE) %>% 
  bind_rows()

# Generate the denominator (observed change in difference in hourly wages over the period)
denom <- (means.year_sampweights$lnhrlywage[means.year_sampweights$year == 2019 & means.year_sampweights$female == 0] -
            means.year_sampweights$lnhrlywage[means.year_sampweights$year == 1981 & means.year_sampweights$female == 0]) -
  (means.year_sampweights$lnhrlywage[means.year_sampweights$year == 2019 & means.year_sampweights$female == 1] -
     means.year_sampweights$lnhrlywage[means.year_sampweights$year == 1981 & means.year_sampweights$female == 1])

# Performing the sample weights --> reweighted 2019 weights decomposition,
# using custom means object and coefficient objects
perform_decomposition_analysis_swtorw <- function(exclude_covariates, model_label){
  decomposition_analysis(
    weights = weights, 
    sample_conditions = sample_conditions, 
    data = data, 
    means_year_obj = means.year_rw %>% mutate(year = as.numeric(gsub("_RW", "", year))), 
    baseline_coefs_obj = generate_regression_table(data, group = group, years,
                                                   outcome = outcome, 
                                                   covariates = covariates[covariates %!in% exclude_covariates], 
                                                   weights = weights,
                                                   sample_conditions = sample_conditions),
    scale = "log_points",
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

decomp_swtorw <- mapply(perform_decomposition_analysis_swtorw,
                        covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
  bind_rows() %>%
  dplyr::select(model, var, everything()) %>% 
  rename(Model = model, Variable = var, 
         Total_CharGap = "Characteristics Gap...2", 
         Total_Returns = "Returns...3",
         Total_Interaction = "Interaction...4",
         Men_CharGap = "Characteristics Gap...5",
         Men_Returns = "Returns...6",
         Men_Interaction = "Interaction...7",
         Women_CharGap = "Characteristics Gap...8",
         Women_Returns = "Returns...9",
         Women_Interaction = "Interaction...10") %>% 
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, ~./denom[[1]]) %>%
  mutate_if(is.numeric, round, digits = 2)

# Performing the reweighted 2019 weights --> 2019 sample weights decomposition,
# using custom means object and coefficient objects 
perform_decomposition_analysis_rwtosw <- function(exclude_covariates, model_label){
  decomposition_analysis(
    weights = weights, 
    sample_conditions = sample_conditions, 
    data = data, 
    means_year_obj = bind_rows(means.year_rw %>% mutate(year = as.numeric(gsub("_RW", "", year))) %>%
                                 filter(year == 2019) %>% mutate(year = 1981), 
                               means.year_sampweights %>% filter(year == 2019) %>%
                                 mutate(year = as.numeric(year))), 
    baseline_coefs_obj = generate_regression_table(data, group = group, years,
                                                   outcome = outcome, 
                                                   covariates = covariates[covariates %!in% exclude_covariates], 
                                                   weights = weights,
                                                   sample_conditions = sample_conditions),
    scale = "log_points",
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

decomp_rwtosw <- mapply(perform_decomposition_analysis_rwtosw,
                        covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
  bind_rows() %>%
  dplyr::select(model, var, everything()) %>% 
  rename(Model = model, Variable = var, 
         Total_CharGap = "Characteristics Gap...2", 
         Total_Returns = "Returns...3",
         Total_Interaction = "Interaction...4",
         Men_CharGap = "Characteristics Gap...5",
         Men_Returns = "Returns...6",
         Men_Interaction = "Interaction...7",
         Women_CharGap = "Characteristics Gap...8",
         Women_Returns = "Returns...9",
         Women_Interaction = "Interaction...10") %>% 
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, ~./denom[[1]]) %>%
  mutate_if(is.numeric, round, digits = 2)

# Creating table A13
tlfadj <- left_join(decomp_rwtosw %>% #filter(Variable %in% c("num.kids.cont", "Total")) %>%
                    dplyr::select(Variable, Model, "1980 to 2018, Reweighted" = "Total_CharGap"), 
                  decomp_swtorw %>% #filter(Variable %in% c("num.kids.cont", "Total")) %>%
                    dplyr::select(Variable, Model, "2018 Reweighted to 2018" = "Total_CharGap"), 
                  by = c("Variable", "Model"))

saveRDS(tlfadj, file = paste("bootstrap_lfadj/tlfadj_", i, ".RDS", sep = ""))

toc()

}

# List all .RDS files
files <- list.files(path = "bootstrap_lfadj", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_lfadj", x, sep = "/"))
}

# Load all .RDS files into a list of data frames
df_list <- lapply(files, load_rds)

# Combine all data frames into one data frame
df_combined <- do.call(rbind, df_list) %>%
  gather(quantity, value, -c(Variable, Model)) %>%
  pivot_wider(values_from = value, names_from = c(Variable, quantity, Model)) %>%
  unnest()

# Calculate 5th and 95th percentiles for each column
df_percentiles <- sapply(df_combined,
                         function(x) quantile(x, probs=c(0.05, 0.95)))

# Print out the percentile data frame
bootstrap <- as.data.frame(df_percentiles %>% t()) %>%
  mutate(var = rownames(.)) %>%
  separate(var, into = c("Group", "Measure", "Model"), sep = "_")

rownames(bootstrap) <- NULL

bootstrap_table <- bootstrap %>% 
  select(Measure, Group, Model, everything()) %>% 
  mutate(Model = factor(Model, 
                        levels = c("Model 1: Baseline", 
                                   "Model 2: + Background", 
                                   "Model 3: + Education", 
                                   "Model 4: + Work Experience and Job Tenure",
                                   "Model 5: Full"))) %>%
  rename(fifth = "5%", ninetyfifth = "95%") 

write_csv(bootstrap_table, "tables/bootstrap_table_lfadj.csv")

# Checking that mdipoint between 5th and 95th percentiles approximates the point estimates
t3 <- read_csv("tables/table_lfadj_pointestimates.csv") %>%
  gather(Group, pointestimate, -c(Variable, Model)) %>%
  left_join(., read_csv("tables/bootstrap_table_lfadj.csv")  %>%
              rename(Variable = Group, Group = Measure),
            by = c("Variable", "Group", "Model")) %>%
  mutate(midpoint = (fifth + ninetyfifth) / 2)


t_lfadj_bs <- read_csv("tables/table_lfadj_pointestimates.csv") %>% 
  mutate_if(is.numeric, ~ sprintf("%.2f", .x)) %>%
  bind_rows(read_csv("tables/bootstrap_table_lfadj.csv") %>%
              mutate_if(is.numeric, ~ sprintf("%.2f", .x)) %>%
              mutate(confint = paste0("(", fifth, ", ", ninetyfifth, ")")) %>% 
              transmute(Variable = Group, Group = Measure, Model, confint) %>% 
              pivot_wider(names_from = c(Group), values_from = confint)) %>%
  arrange(Model, Variable)

kable(t_lfadj_bs %>% filter(Variable %in% c("num.kids.cont", "Total")) %>%
        select(-Model), format = "latex", booktabs = T,
      caption = "Table A7: Percent of Gender Pay Convergence 1980-2018 Explained by Changing Characteristics, Decomposing Population Change and Changing Labor Force Selection") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Note: See text for details of model specifications. 95 % bootstrapped confidence intervals are shown in parentheses.",
           threeparttable = TRUE) %>%
  pack_rows("Model 1: Baseline", 1, 4, bold = TRUE) %>%
  pack_rows("Model 2: + Background", 5, 8, bold = TRUE) %>%
  pack_rows("Model 3: + Education", 9, 12, bold = TRUE) %>%
  pack_rows("Model 4: + Work Experience and Job Tenure", 13, 16, bold = TRUE) %>%
  pack_rows("Model 5: Full", 17, 20, bold = TRUE)


