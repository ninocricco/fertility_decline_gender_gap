#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: BOOTSTRAPPED SE'S, TABLE 3, MAIN DECOMPOSITION
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/02/1993(mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")
source("jobs/3-analysis-arguments.R")

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
  
  # Runs the main decomposition analysis
  
  main_decomp <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               ed, marstat, laborsupply, jobchar)])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 1: Demographic Controls"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               marstat, laborsupply, jobchar)])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 2: + Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               laborsupply, jobchar)])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 3: + Marital Status"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               jobchar)])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 4: + Labor Supply"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates)[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 5: + Job Characteristics")) %>%
    dplyr::select(model, var, everything()) %>% 
    rename(Model = model, Variable = var, 
           Total_CharGap = "Characteristics Gap...2", 
           Total_Returns = "Returns...3",
           Total_Interaction = "Interaction...4",
           Men_CharGap = "Characteristics Gap...5",
           Men_Returns = "Returns...6",
           Men_Interaction = "Interaction...7",
           Women_Chargap = "Characteristics Gap...8",
           Women_Returns = "Returns...9",
           Women_Interaction = "Interaction...10")
  
  # Stores results
  
  saveRDS(main_decomp, file = paste("bootstrap_maindecomp/maindecomp_bs_14.13.23_", i, ".RDS", sep = ""))
  
  toc()
  
}

#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, CONFIDENCE INTERVALS 
# AUTHOR: NINO CRICCO
# LAST UPDATED: 14/06/2023 (mdy)
#**********************************************************
#
# List all .RDS files
files <- list.files(path = "bootstrap_maindecomp", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_maindecomp", x, sep = "/"))
}

# Load all .RDS files into a list of data frames
df_list <- lapply(files, load_rds)

# Combine all data frames into one data frame
df_combined <- do.call(rbind, df_list) %>%
  gather(quantity, value, - c(Variable, Model)) %>%
  pivot_wider(values_from = value, names_from = c(Variable, quantity, Model)) %>%
  unnest()

# Calculate 5th and 95th percentiles for each column
df_percentiles <- sapply(df_combined, function(x) quantile(x, probs=c(0.05, 0.95)))

# Print out the percentile data frame
bootstrap <- as.data.frame(df_percentiles %>% t()) %>%
  mutate(var = rownames(.)) %>%
  separate(var, into = c("Quantity", "Group", "Component", "Model"), sep = "_")

rownames(bootstrap) <- NULL

bootstrap_table <- bootstrap %>% 
  select(Quantity, Group, Component, Model, everything()) %>% 
  mutate(Model = factor(Model, 
                        levels = c("Model 1: Demographic Controls", 
                                   "Model 2: + Education", 
                                   "Model 3: + Marital Status", 
                                   "Model 4: + Labor Supply",
                                   "Model 5: + Job Characteristics"))) %>%
  arrange(Model, Component, Group, Quantity) %>%
  rename(fifth = "5%", ninetyfifth = "95%") %>%
  mutate(fifth = round(fifth * 100, digits = 2),
         ninetyfifth = round(ninetyfifth * 100, digits = 2))

write_csv(bootstrap_table, "tables/bootstrap_maindecomp.csv")

bootstrap_tablea3 <- bootstrap_table %>% filter(Quantity %in% c("num.kids.cont", "Total"))

write_csv(bootstrap_tablea3, "tables/bootstrap_tablea3.csv")

# Checking that mdipoint between 5th and 95th percentiles approximates the point estimates
ta3 <- read_csv("tables/tablea3.csv") %>%
  gather(key, pointestimate, -c(Variable, Model)) %>%
  separate(key, into = c("Group", "Component")) %>%
  left_join(., bootstrap_table %>%
              rename(Variable = Quantity),
            by = c("Variable", "Group", "Component", "Model")) %>%
  mutate(midpoint = (fifth + ninetyfifth) / 2)