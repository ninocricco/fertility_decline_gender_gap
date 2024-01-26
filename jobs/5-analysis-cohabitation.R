#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, SUPPLEMENTARY (COHABITATION)
# AUTHOR: NINO CRICCO
# LAST UPDATED: 14/06/2023 (mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final_06.13.23.csv") %>%
  dummy_cols(., select_columns = c("marstat.cohab")) %>%
  rename_at(vars(starts_with('marstat.cohab_')), funs(sub('_', ".", .)))
  
source("jobs/3-analysis-arguments.R")

covariates = c("num.kids.cont", "age", "agesq",
               "Black", "Hispanic", "Other",
               "Northeast", "Northcentral", "South", 
               "HighSchool", "SomeCollege", "ba.avdeg", 
               "marstat.cohab.married", "marstat.cohab.cohabiting", "marstat.cohab.prev.married",
               "log.expf", "emp.tenure", 
               "ftormore", "overwork", 
               "union", "govt.job", "occ.pct.female", 
               "occ.managers", "manuf")

marstat = c("marstat.cohab.married", "marstat.cohab.cohabiting", "marstat.cohab.prev.married")

# Defines covariates and labels for each model we want to estimate
covariates_to_exclude <- list(
  c(age, race, region, ed, marstat, wrkhrs, laborsupply, jobchar),
  c(ed, laborsupply, jobchar),
  c(laborsupply, jobchar),
  c(wrkhrs, jobchar),
  c()
)

model_labels <- c("Model 1: Baseline",
                  "Model 2: + Background", 
                  "Model 3: + Education", 
                  "Model 4: + Work Experience and Job Tenure",
                  "Model 5: Full")

# We then create a main decomposition object that maps our decomposition analysis function across each covariate list
cohab_decomp <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  mutate_if(is.numeric, round, digits = 2)

# Generates LaTex code to produce Cohabitation Decomp Table: 
# Decomposition % of Change in Pay Gap Explained by Declining Fertility and Marital Status, by Gender
t3_cohab <- cohab_decomp %>%
  filter(Variable %in% c("marstat.cohab.married",
                         "marstat.cohab.cohabiting",
                         "marstat.cohab.prev.married", 
                         "num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

write_csv(t3_cohab, "tables/cohabiting_decomp_1981refyear.csv")

years = c(2019, 1981)

cohab_decomp_2019 <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  mutate_if(is.numeric, round, digits = 2)

t3_cohab_2019 <- cohab_decomp_2019 %>%
  filter(Variable %in% c("marstat.cohab.married",
                         "marstat.cohab.cohabiting",
                         "marstat.cohab.prev.married", 
                         "num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

write_csv(t3_cohab_2019, "tables/cohabiting_decomp_2018refyear.csv")

means.marstat <- generate_means_year_table(data = data, years = years, sample_conditions = sample_conditions,
                          outcome = "lnhrlywage", weights = "perwt",
                          covariates = c("marstat.cohab.married", "marstat.cohab.cohabiting",
                                         "marstat.cohab.prev.married", "marstat.cohab.unmarried")) %>%
  dplyr::select(year, female, starts_with("marstat")) %>%
  mutate(year = year-1) %>%
  gather(key, value, - c("year", "female")) %>%
  separate(key, c("var", "estimate"), sep = "_") %>%
  pivot_wider(id_cols = c(var, estimate),
              names_from = c(female, year), values_from = value) %>%
  dplyr::select(var, estimate, starts_with("0"), everything()) %>%
  mutate(var = gsub("marstat.cohab.", "", var)) %>%
  mutate_if(is.numeric, ~.*100)

write_csv(means.marstat, "tables/cohabiting_means.csv")

coeftable.marstat <- mapply(generate_regression_table_func, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
  bind_rows() %>%
  dplyr::select(year, female, model, estimate, starts_with("marstat.cohab"))

write_csv(coeftable.marstat, "tables/cohabiting_regressioncoefs.csv")

# Generating the table
knitr::kable(means.marstat %>% dplyr::select(-estimate) %>%
               mutate_if(is.numeric, round, digits = 2),
             booktabs = T, format = "latex", 
             caption = "Means, Alternative Measures of Marital Status") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2))

kable(coeftable.marstat %>% gather(key, value, -c(year, female, model, estimate)) %>%
        mutate(key = gsub("marstat.cohab.", "", key), 
               value = round(value, digits = 2),
               female = ifelse(female == 1, "Women", "Men")) %>%
        pivot_wider(names_from = c(female, year), values_from = value) %>%
        arrange(model, key, estimate) %>%
        dplyr::select(estimate, key, Men_1981, Men_2019, Women_1981, Women_2019, -model)
      , booktabs = T, format = "latex",
      caption = "Coefficients, Alternative Measures of Marital Status on Wages") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Table shows coefficients for number of children from gender-and-year specific regressions on hourly wages. Standard errors are shown in parentheses. See text for details of the model specifications.",
           threeparttable = T) %>%
  pack_rows("Model 3: + Marital Status", 1, 9, bold = T) %>% 
  pack_rows("Model 4: + Labor Supply", 10, 18, bold = T) %>% 
  pack_rows("Model 5: + Job Characteristics", 19, 27, bold = T)

kable(t3_cohab_2019 %>%
  mutate(group = ifelse(Variable %in% c("marstat.cohab.married", 
                                        "marstat.cohab.prev.married", 
                                        "marstat.cohab.cohabiting"), 
                        "Marital Status", Variable)) %>%
  group_by(Model, group) %>%
  summarise_if(is.numeric, sum) %>% ungroup() %>% select(-Model), 
  booktabs = T, format = "latex", 
  caption = "Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, Alternative Marital Status definitions") %>%
  pack_rows("Model 3: + Marital Status", 1, 3, bold = T) %>% 
  pack_rows("Model 4: + Labor Supply", 4, 6, bold = T) %>% 
  pack_rows("Model 5: + Job Characteristics", 7, 9, bold = T)

