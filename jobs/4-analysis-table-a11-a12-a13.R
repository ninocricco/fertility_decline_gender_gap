#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: AUXILIARY ANALYSES, LABOR FORCE SELECTION ADJUSTMENTS
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/02/2023 (mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

source("jobs/3-analysis-arguments.R")

# First creating a subset of the full data restriting only by age and region

benchmark <- psid_imp %>%
  mutate(age.spline1 = ifelse(age < 45, age, 45), 
         age.spline2 = ifelse(age >= 45, age-45, 0)) %>%
  filter(samp.inc.age == 1, samp.exc.region != 1, year %in% years)

# Split the data by imputation for women in 1981, 2019
rw.samp.wt0  <- imputationList(benchmark %>%
                                 filter(year == 1981, female == 1) %>%
                                 group_split(.imp))

rw.samp.wt1  <- imputationList(benchmark %>%
                                 filter(year == 2019, female == 1) %>%
                                 group_split(.imp))

# Creates a survey design object for this split data
rw.design.wt0 <-svydesign(ids = ~0, data = rw.samp.wt0, weights = ~perwt, nest = T)
rw.design.wt1 <-svydesign(ids = ~0, data = rw.samp.wt1, weights = ~perwt, nest = T)

# Estimates the labor force selection model separately for each imputation and year for women
lfselec.wt0 <- with(rw.design.wt0, svyglm(
  samp.inc.final ~ age.spline1 + age.spline2 + Northeast + Northcentral + South + 
    Black + Hispanic + Other + HighSchool + SomeCollege + ba.avdeg + married +
    num.kids.cont + age.youngest.cat_zerotofive +
    age.youngest.cat_sixtoseventeen, family = binomial(link="logit")))

lfselec.wt1 <- with(rw.design.wt1, svyglm(
  samp.inc.final ~ age.spline1 + age.spline2 + Northeast + Northcentral + South + 
    Black + Hispanic + Other + HighSchool + SomeCollege + ba.avdeg + married +
    num.kids.cont + age.youngest.cat_zerotofive +
    age.youngest.cat_sixtoseventeen, family = binomial(link="logit")))

# Combining results across imputations 
lfselec.wt0.comb <- MIcombine((lfselec.wt0))
lfselec.wt1.comb <- MIcombine((lfselec.wt1))

# Using results combined from imputations to generate a dataframe with 
# model coefficients and standard errors
lfselec.w.df <- bind_rows(coef(lfselec.wt0.comb), vcov::se(lfselec.wt0.comb)) %>%
  mutate(year = 1981, female = 1, estimate = c("coef", "se")) %>%
  bind_rows(bind_rows(coef(lfselec.wt1.comb), vcov::se(lfselec.wt1.comb)) %>%
              mutate(year = 2019, female = 1, estimate = c("coef", "se")))

# Following the same procedure as above, for men
rw.samp.mt0  <- imputationList(benchmark %>%
                                 filter(year == 1981, female == 0) %>%
                                 group_split(.imp))

rw.samp.mt1  <- imputationList(benchmark %>%
                                 filter(year == 2019, female == 0) %>%
                                 group_split(.imp))

rw.design.mt0 <-svydesign(ids = ~0, data = rw.samp.mt0, weights = ~perwt, nest = T)

rw.design.mt1 <-svydesign(ids = ~0, data = rw.samp.mt1, weights = ~perwt, nest = T)

lfselec.mt0 <- with(rw.design.mt0, svyglm(
  samp.inc.final ~ age.spline1 + age.spline2 + Northeast + Northcentral + South + 
    Black + Hispanic + Other + HighSchool + SomeCollege + ba.avdeg + married +
    num.kids.cont + age.youngest.cat_zerotofive +
    age.youngest.cat_sixtoseventeen, family = binomial(link="logit")))

lfselec.mt1 <- with(rw.design.mt1, svyglm(
  samp.inc.final ~ age.spline1 + age.spline2 + Northeast + Northcentral + South + 
    Black + Hispanic + Other + HighSchool + SomeCollege + ba.avdeg + married +
    num.kids.cont + age.youngest.cat_zerotofive +
    age.youngest.cat_sixtoseventeen, family = binomial(link="logit")))

lfselec.mt0.comb <- MIcombine((lfselec.mt0))
lfselec.mt1.comb <- MIcombine((lfselec.mt1))

lfselec.m.df <- bind_rows(coef(lfselec.mt0.comb), vcov::se(lfselec.mt0.comb)) %>%
  mutate(year = 1981, female = 0, estimate = c("coef", "se")) %>%
  bind_rows(bind_rows(coef(lfselec.mt1.comb), vcov::se(lfselec.mt1.comb)) %>%
              mutate(year = 2019, female = 0, estimate = c("coef", "se")))

# Creating a dataframe of the rewieghting model coefficients
rw.modelcoefs <- bind_rows(lfselec.m.df, lfselec.w.df) %>% 
  gather(key, value, -c(female, year, estimate)) %>% 
  mutate(key = as_factor(key)) %>%
  pivot_wider(id_cols = c(key),
              names_from = c(estimate, female, year), values_from = c(value)) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  gather(names, values, -key) %>%
  separate(names, into = c("estimate", "group", "year")) %>%
  mutate(group = ifelse(group == 0, "Men", "Women")) %>%
  pivot_wider(names_from = c(group, year), values_from = values) %>%
  arrange(key)

write_csv(rw.modelcoefs, "tables/tablea11.csv")

knitr::kable(rw.modelcoefs, booktabs = T, format = "latex", 
             caption = "Table A10: Coefficients from the Reweighting Model") %>%
  footnote("Coefficients from gender-specific logistic regressions predicting sample inclusion. Standard errors in parentheses", 
           threeparttable = T) %>%
  add_header_above(c(" " = 2, "Male" = 2, "Female" = 2)) %>%
  pack_rows("Age, Linear Spline", 3, 6, bold = F)  %>%
  pack_rows("Region", 7, 12, bold = F) %>% pack_rows("Race", 13, 18, bold = F) %>%
  pack_rows("Years of Education", 19, 24, bold = F) %>%
  pack_rows("Age, Youngest Child in Household", 29, 32, bold = F)

# Measuring the probability of sample inclusion

prob.inc <- benchmark %>% 
  filter(year %in% years) %>%
  group_by(female, year) %>% 
  summarise(Prob.Inclusion = wtd.mean(samp.inc.final, weights = perwt))

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
             data = ., weights = perwt))) %>%
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
  mutate(lf.pred.w = ifelse(year == 1981, perwt, 
                            perwt * (lf.pred_1981/lf.pred_2019))) %>%
  filter(samp.inc.final == 1) %>%
  filter(year %in% years)

# Creating an object to use in the decompositions using the sample means
means.year_sampweights <- psid.rw %>%
  group_by(year, female, .imp) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(all_of(outcome), all_of(covariates), expf, White, West, LessthanHS, 
                perwt, lf.pred.w) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = perwt))) %>% 
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
                perwt, lf.pred.w) %>%
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

means.table <- 
  bind_rows(means.year_sampweights, means.year_rw #%>% filter(year == "2019_RW")
  ) %>%
  mutate_at(vars(all_of(region), all_of(race), "married", all_of(ed), all_of(wrkhrs),
                 "union", "govt.job", "occ.managers", "manuf", "LessthanHS", 
                 "West", "White"), function (x) x*100) %>% 
  mutate_if(is.numeric, round, digits = 1) %>%
  gather(vars, means, -c(year, female)) %>%
  mutate(female = ifelse(female == 1, "Women", "Men")) %>%
  pivot_wider(id_cols = c(vars),
              names_from = c(female, year), values_from = means)

write_csv(means.table, "tables/tablea12.csv")

# Generating the table
knitr::kable(means.table %>% dplyr::select(vars, Men_2019, Men_2019_RW, Women_2019, Women_2019_RW) %>%
               filter(vars %!in% c("lnhrlywage", "(Intercept)", "agesq", "log.expf", "perwt")) %>%
               dplyr::select(vars, Men_2019, Men_2019_RW, Women_2019, Women_2019_RW), 
             booktabs = T, format = "latex", 
             caption = "Table A11: 2018 Sample Descriptive Statistics Adjusting for Labor Force Selection") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Descriptive statistics show weighted averages for the analytic sample by year and gender, using sample means and adjusting for changing patterns of labor force selection. The proportion of all PSID heads and wives aged 30-55 that meets our sample inclusion criteria are 76.3 and 62.8 for men and women in 1980, respectively, and 77.9 and 71.3 for men and women in 2018. The sample consists of PSID heads and wives reporting non-zero wages, excludes individuals who report being self-employed, and individuals employed in agriculture or the military", 
           threeparttable = T) %>%
  pack_rows("+ Background", 1, 2, bold = F) %>%
  pack_rows("Race", 3, 5, bold = F) %>% pack_rows("Region", 6, 8, bold = F) %>%
  pack_rows("Years of Education", 9, 11, bold = F)

# Generating the decomposition
# First get the weighted coefficients with sample weights and with 
# labor force adjusted weights
coefs.sw <- mapply(generate_regression_table_func, 
                   covariates_to_exclude, model_labels, weights = "perwt", 
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
ta13 <- left_join(decomp_rwtosw %>% filter(Variable %in% c("num.kids.cont", "Total")) %>%
                    dplyr::select(Variable, Model, "1980 to 2018, Reweighted" = "Total_CharGap"), 
                  decomp_swtorw %>% filter(Variable %in% c("num.kids.cont", "Total")) %>%
                    dplyr::select(Variable, Model, "2018 Reweighted to 2018" = "Total_CharGap"), 
                  by = c("Variable", "Model"))

write_csv(ta13, "tables/tablea13.csv")

kable(ta13 %>% dplyr::select(-Model), booktabs = T, format = "latex", 
      caption = "Table A12: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, Decomposing Population Change and Changing Labor Force Selection") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T)
