#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, APPENDIX TABLE 6: 
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/2023 (mdy)
#**********************************************************

# This conducts the same analyses as the main decompositions,
# but for each inter-decade period from 1980 to 2018. Whereas
# table 5 shows the percent explained by declining fertility 
# across decades in each decade, Table A6 shows the percent 
# explained by declining fertility within each decade
opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

source("jobs/3-analysis-arguments.R")

decomp_decades_within <-bind_rows(characteristics_component(weights = weights, years = c(1981, 1991),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  ed, marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1981), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                ed, marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1980-1990", model = "Model 1: Demographic Controls") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1981, 1991),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1981), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1980-1990", model = "Model 2: + Education") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1981, 1991),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1981), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1980-1990", model = "Model 3: + Marital Status") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1981, 1991),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1981), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1980-1990", model = "Model 4: + Labor Supply") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1981, 1991),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates) %>%
                                                              filter(year == 1981) %>% mutate(year = 1981), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates)["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1980-1990", model = "Model 5: + Job Characteristics") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1991, 2001),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  ed, marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1991), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                ed, marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1990-2001", model = "Model 1: Demographic Controls") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1991, 2001),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1991), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1990-2001", model = "Model 2: + Education") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1991, 2001),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1991), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1990-2001", model = "Model 3: + Marital Status") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1991, 2001),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 1991), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1990-2001", model = "Model 4: + Labor Supply") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(1991, 2001),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates) %>%
                                                              filter(year == 1981) %>% mutate(year = 1991), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates)["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "1990-2001", model = "Model 5: + Job Characteristics") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2001, 2011),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  ed, marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2001), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                ed, marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2000-2010", model = "Model 1: Demographic Controls") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2001, 2011),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2001), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2000-2010", model = "Model 2: + Education") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2001, 2011),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2001), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2000-2010", model = "Model 3: + Marital Status") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2001, 2011),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2001), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2000-2010", model = "Model 4: + Labor Supply") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2001, 2011),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates) %>%
                                                              filter(year == 1981) %>% mutate(year = 2001), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates)["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2000-2010", model = "Model 5: + Job Characteristics") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2011, 2019),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  ed, marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2011), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                ed, marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2010-2018", model = "Model 1: Demographic Controls") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2011, 2019),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  marstat, laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2011), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                marstat, laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2010-2018", model = "Model 2: + Education") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2011, 2019),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  laborsupply, jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2011), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                laborsupply, jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2010-2018", model = "Model 3: + Marital Status") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2011, 2019),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates[
                                                                covariates %!in% c(
                                                                  jobchar)]) %>%
                                                              filter(year == 1981) %>% mutate(year = 2011), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates[
                                                              covariates %!in% c(
                                                                jobchar)])["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2010-2018", model = "Model 4: + Labor Supply") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model),
                                  characteristics_component(weights = weights, years = c(2011, 2019),
                                                            sample_conditions = sample_conditions, 
                                                            data = data, 
                                                            baseline_coefs_obj = generate_regression_table(
                                                              data, group = group, years = c(1981, 2019), outcome = outcome,  
                                                              weights = weights, sample_conditions = sample_conditions, 
                                                              covariates = covariates) %>%
                                                              filter(year == 1981) %>% mutate(year = 2011), 
                                                            outcome = outcome, group = group,
                                                            covariates = covariates)["crossgroup"] %>%
                                    as.data.frame() %>% 
                                    mutate(var = rownames(.), year = "2010-2018", model = "Model 5: + Job Characteristics") %>% 
                                    select(var, Characteristics.Gap = crossgroup.chargap, year,
                                           model)) %>%
  mutate_if(is.numeric, ~.*100)

ta6 <- decomp_decades_within %>% filter(var == "num.kids.cont") %>%
  pivot_wider(names_from = year, values_from = "Characteristics.Gap") %>%
  mutate_if(is.numeric, round, digits = 2)

write_csv(ta6, "tables/tablea6.csv")

kable(ta6 %>% dplyr::select(-model), format = "latex", booktabs = T, 
      caption = "Table A6: Percent of the Changing Gender Pay Gap Within Decade Explained by Fertility Decline") %>%
  pack_rows("Model 1: Demographic Controls", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Education", 2, 2, bold = T) %>%
  pack_rows("Model 3: + Marital Status", 3, 3, bold = T) %>%
  pack_rows("Model 4: + Labor Supply", 4, 4, bold = T) %>%
  pack_rows("Model 5: + Job Characteristics", 5, 5, bold = T)
