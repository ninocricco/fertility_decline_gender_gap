#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: ALTERNATIVE REFERENCE YEAR WAGE STRUCTURE
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

years = c(2019, 1981)

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
  
  covariates_to_exclude <- list(
    c(wrkhrs, jobchar),
    c()
  )
  
  
  main_decomp <- bind_rows(
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(age, race, region, ed, marstat, wrkhrs, laborsupply, jobchar)
                             ])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 1: Baseline"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(ed, laborsupply, jobchar)
                               ])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 2: + Background"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               laborsupply, jobchar)
                             ])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 3: + Education"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates[
                             covariates %!in% c(
                               wrkhrs, jobchar)
                               ])[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 4: + Work Experience and Job Tenure"),
    decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                           years = years, outcome = outcome, group = group,
                           covariates = covariates)[c("crossgroup", "group0", "group1")] %>%
      bind_cols() %>%
      mutate(var = rownames(.)) %>%
      dplyr::select(var, everything(), -starts_with("group")) %>%
      adorn_totals("row") %>%
      mutate(model = "Model 5: Full")) %>%
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
  
  saveRDS(main_decomp, file = paste0(
    "bootstrap_wagestruc/bootstrap_wagestruc", "_", i, ".RDS"))
  
  toc()
  
}

# List all .RDS files
files <- list.files(path = "bootstrap_wagestruc", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_wagestruc", x, sep = "/"))
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
                        levels = c("Model 1: Baseline", 
                                   "Model 2: + Background", 
                                   "Model 3: + Education", 
                                   "Model 4: + Work Experience and Job Tenure",
                                   "Model 5: Full"))) %>%
  arrange(Model, Component, Group, Quantity) %>%
  rename(fifth = "5%", ninetyfifth = "95%") %>%
  mutate(fifth = round(fifth * 100, digits = 2),
         ninetyfifth = round(ninetyfifth * 100, digits = 2))

write_csv(bootstrap_table, "tables/bootstrap_wagestruc.csv")

# Checking that mdipoint between 5th and 95th percentiles approximates the point estimates
t_altref_check <- read_csv("tables/table_altrefyear.csv") %>%
  gather(key, pointestimate, -c(Variable, Model)) %>%
  #mutate(Model = gsub("Model [0-9]+: ", "", Model)) %>%
  separate(key, into = c("Group", "Component")) %>%
  left_join(., read_csv("tables/bootstrap_wagestruc.csv")  %>%
              rename(Variable = Quantity) %>%
              mutate(Component = ifelse(Component == "Chargap", "CharGap", Component)),
            by = c("Variable", "Group", "Component", "Model")) %>%
  mutate(midpoint = (fifth + ninetyfifth) / 2)

t_altref <- read_csv("tables/table_altrefyear.csv") %>% 
  transmute(Model, Variable, 
            Total_CharGap, Men_CharGap, Women_CharGap) %>%
  mutate_if(is.numeric, as.character) %>%
  bind_rows(read_csv("tables/bootstrap_wagestruc.csv") %>%
              mutate(confint = paste0("(", fifth, ", ", ninetyfifth, ")")) %>% 
              select(Quantity, Model, Group, Component, confint) %>% 
              pivot_wider(names_from = c(Group, Component), values_from = confint) %>%
              rename(Variable = Quantity) %>%
              transmute(Model, Variable, 
                        Total_CharGap, Men_CharGap, Women_CharGap = Women_Chargap)) %>%
  arrange(Model, Variable)

kable(t_altref %>% select(Model, Variable, ends_with("Gap")) %>% 
        filter(Variable %in% c("num.kids.cont", "Total")), 
      format = "latex")
