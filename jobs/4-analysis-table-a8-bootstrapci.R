#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: BOOTSTRAPPED SE'S, TABLE A8: ALTERNATIVE FERTILITY MEASURES
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/02/2023 (mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")
source("jobs/3-analysis-arguments.R")

covariates_cat = c("numkids.1", "numkids.2", "numkids.3plus", "age", "agesq",
                   "Black", "Hispanic", "Other",
                   "Northeast", "Northcentral", "South", 
                   "HighSchool", "SomeCollege", "ba.avdeg", 
                   "married", "log.expf", "samp.inc.ft",
                   "emp.tenure", "overwork", "union",
                   "govt.job", "occ.pct.female", 
                   "occ.managers", "manuf")

covariates_timing = c("afb.cat_21minus",  "afb.cat_23to27",  "afb.cat_28plus",
                      "age", "agesq",
                      "Black", "Hispanic", "Other",
                      "Northeast", "Northcentral", "South", 
                      "HighSchool", "SomeCollege", "ba.avdeg", 
                      "married", "log.expf", "samp.inc.ft",
                      "emp.tenure", "overwork", "union",
                      "govt.job", "occ.pct.female", 
                      "occ.managers", "manuf")

covariates_cattiming = c("num.kids.cont",
                         "afb.cat_21minus",  "afb.cat_23to27",  "afb.cat_28plus",
                         "age", "agesq",
                         "Black", "Hispanic", "Other",
                         "Northeast", "Northcentral", "South", 
                         "HighSchool", "SomeCollege", "ba.avdeg", 
                         "married", "log.expf", "samp.inc.ft",
                         "emp.tenure", "overwork", "union",
                         "govt.job", "occ.pct.female", 
                         "occ.managers", "manuf")

for(i in 1:10000){
  
  set.seed(i)
  
  psid_imp2 <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(1235, 20), rep(1430, 20), rep(2819, 20), rep(3185, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  t3_cat <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat[
                             covariates_cat %!in% c(
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Demographic Controls"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat[
                             covariates_cat %!in% c(
                               "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat[
                             covariates_cat %!in% c(
                               laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Marital Status"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat[
                             covariates_cat %!in% jobchar])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Labor Supply"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat)["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Job Characteristics")) %>%
    filter(var %in% c("numkids.1", "numkids.2", "numkids.3plus", "Total")) %>%
    select(model, var, "Categorical" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  t3_timing <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% c(
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Demographic Controls"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% c(
                               "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% c(
                               laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Marital Status"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% jobchar])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Labor Supply"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing)["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Job Characteristics")) %>%
    filter(var %in% c("afb.cat_21minus", "afb.cat_23to27", "afb.cat_28plus", "Total")) %>%
    select(model, var, "Timing" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  t3_cattiming <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% c(
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Demographic Controls"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% c(
                               "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% c(
                               laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Marital Status"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% jobchar])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Labor Supply"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming)["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Job Characteristics")) %>%
    filter(var %in% c("num.kids.cont",
                      "afb.cat_21minus", "afb.cat_23to27", "afb.cat_28plus",
                      "Total")) %>%
    select(model, var, "Categorical + Timing" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  fert_decomp <- left_join(t3_cat, t3_timing, by = c("model", "group")) %>%
    left_join(., t3_cattiming, by = c("model", "group"))
  
  # Stores results
  
  saveRDS(fert_decomp, file = paste("bootstrap_tablea8/tablea8_bs_14.13.23_", i, ".RDS", sep = ""))
  
  toc()
  
}

# List all .RDS files
files <- list.files(path = "bootstrap_tablea7", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_tablea7", x, sep = "/"))
}

# Load all .RDS files into a list of data frames
df_list <- lapply(files, load_rds)

# Combine all data frames into one data frame
df_combined <- do.call(rbind, df_list) %>%
  gather(quantity, value, - c(group, model)) %>%
  pivot_wider(values_from = value, names_from = c(group, quantity, model)) %>%
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
                        levels = c("Demographic Controls",
                                   "+ Education", 
                                   "+ Marital Status", 
                                   "+ Labor Supply",
                                   "+ Job Characteristics"))) %>%
  rename(fifth = "5%", ninetyfifth = "95%") %>%
  mutate(fifth = round(fifth * 100, digits = 2),
         ninetyfifth = round(ninetyfifth * 100, digits = 2))

write_csv(bootstrap_table, "tables/bootstrap_tablea8.csv")