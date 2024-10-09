#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: ALTERNATIVE FERTILITY MEASURES
# GENERATES BOOTSTRAPPED ESTIMATES FOR CONFIDENCE INTERVALS
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

date_run_id <-  "2024-05-30"

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/1-functions.R")
source("jobs/1-load-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv(paste0("clean_data/psid_final_", date_run_id, ".csv"))
source("jobs/4-analysis-arguments.R")

covariates_cat = c("numkids.1", "numkids.2", "numkids.3plus", "age", "agesq",
                   "Black", "Hispanic", "Other",
                   "Northeast", "Northcentral", "South", 
                   "HighSchool", "SomeCollege", "ba.avdeg", 
                   "married", "log.expf", "ftormore",
                   "emp.tenure", "overwork", "union",
                   "govt.job", "occ.pct.female", 
                   "occ.managers", "manuf")

covariates_timing = c("afb.cat_21minus",  "afb.cat_23to27",  "afb.cat_28plus",
                      "age", "agesq",
                      "Black", "Hispanic", "Other",
                      "Northeast", "Northcentral", "South", 
                      "HighSchool", "SomeCollege", "ba.avdeg", 
                      "married", "log.expf", "ftormore",
                      "emp.tenure", "overwork", "union",
                      "govt.job", "occ.pct.female", 
                      "occ.managers", "manuf")

covariates_cattiming = c("num.kids.cont",
                         "afb.cat_21minus",  "afb.cat_23to27",  "afb.cat_28plus",
                         "age", "agesq",
                         "Black", "Hispanic", "Other",
                         "Northeast", "Northcentral", "South", 
                         "HighSchool", "SomeCollege", "ba.avdeg", 
                         "married", "log.expf", "ftormore",
                         "emp.tenure", "overwork", "union",
                         "govt.job", "occ.pct.female", 
                         "occ.managers", "manuf")

sample_conditions_40plus = list("age >= 40",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0")

covariates = c("num.kids.cont", "age", "agesq",
               "Black", "Hispanic", "Other",
               "Northeast", "Northcentral", "South", 
               "HighSchool", "SomeCollege", "ba.avdeg", 
               "married", "log.expf", "emp.tenure", 
               "ftormore", "overwork", 
               "union", "govt.job", "occ.pct.female", 
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
                             covariates_cat %!in% c(age, race, region,
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Baseline"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat[
                             covariates_cat %!in% c(
                               ed, laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Background"),
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
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cat[
                             covariates_cat %!in% c(wrkhrs, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Work Experience and Job Tenure"),
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
      mutate(model = "Full")) %>%
    filter(var %in% c("numkids.1", "numkids.2", "numkids.3plus", "Total")) %>%
    select(model, var, "Categorical" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  t3_timing <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% c(age, race, region,
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Baseline"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% c(
                               ed, laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Background"),
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
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_timing[
                             covariates_timing %!in% c(wrkhrs, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Work Experience and Job Tenure"),
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
      mutate(model = "Full")) %>%
    filter(var %in% c("afb.cat_21minus", "afb.cat_23to27", "afb.cat_28plus", "Total")) %>%
    select(model, var, "Timing" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  t3_cattiming <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% c(age, race, region,
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Baseline"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% c(
                               ed, laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Background"),
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
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates_cattiming[
                             covariates_cattiming %!in% c(wrkhrs, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Work Experience and Job Tenure"),
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
      mutate(model = "Full")) %>%
    filter(var %in% c("num.kids.cont",
                      "afb.cat_21minus", "afb.cat_23to27", "afb.cat_28plus",
                      "Total")) %>%
    select(model, var, "Categorical + Timing" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  t3_40plus <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions_40plus, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(age, race, region,
                               ed, "married", laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Baseline"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions_40plus, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               ed, laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Background"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions_40plus, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               laborsupply, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions_40plus, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(wrkhrs, jobchar)])["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "+ Work Experience and Job Tenure"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions_40plus, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates)["crossgroup"] %>%
      imap(~ .x %>%
             select("Characteristics Gap") %>%
             setNames(paste0("chargap_", .y))) %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything()) %>%
      adorn_totals("row") %>%
      mutate(model = "Full")) %>%
    filter(var %in% c("num.kids.cont",
                      "afb.cat_21minus", "afb.cat_23to27", "afb.cat_28plus",
                      "Total")) %>%
    select(model, var, "40+" = chargap_crossgroup) %>%
    mutate(group = ifelse(var == "Total", "Total", "Fertility")) %>%
    group_by(group, model) %>%
    summarise_if(is.numeric, funs(sum(., na.rm = TRUE)))
  
  fert_decomp <- left_join(t3_cat, t3_timing, by = c("model", "group")) %>%
    left_join(., t3_cattiming, by = c("model", "group")) %>%
    left_join(., t3_40plus, by = c("model", "group"))
  
  # Stores results
  
  saveRDS(fert_decomp, file = paste("bootstrap_fertility/tablea8_bs_23.12.20_", i, ".RDS", sep = ""))
  
  toc()
  
}

# List all .RDS files
files <- list.files(path = "bootstrap_fertility", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_fertility", x, sep = "/"))
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
                        levels = c("Baseline", 
                                   "+ Background", 
                                   "+ Education", 
                                   "+ Work Experience and Job Tenure",
                                   "Full"))) %>%
  rename(fifth = "5%", ninetyfifth = "95%") %>%
  mutate(fifth = round(fifth * 100, digits = 2),
         ninetyfifth = round(ninetyfifth * 100, digits = 2))

write_csv(bootstrap_table, "tables/bootstrap_table_fertility.csv")

# Checking that mdipoint between 5th and 95th percentiles approximates the point estimates
ta8 <- read_csv("tables/tablea8.csv") %>%
  gather(Measure, pointestimate, -c(Model, group)) %>%
  mutate(Model = gsub("Model [0-9]+: ", "", Model)) %>%
  rename(Group = group) %>%
  left_join(., bootstrap_table %>% mutate(Measure = ifelse(Measure == "Categorical + Timing", "TimingCont", Measure)),
            by = c("Measure", "Group", "Model")) %>%
  mutate(midpoint = (fifth + ninetyfifth) / 2)
