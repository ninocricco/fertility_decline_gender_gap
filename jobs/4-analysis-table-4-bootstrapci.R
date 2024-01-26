#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: BOOTSTRAPPED SE'S, TABLE 4, LABOR SUPPLY DECOMPOSITION
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
  
  expf_decomp <- decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                                        years = years, outcome = "expf", group = group,
                                        covariates = covariates[covariates %!in% c(laborsupply, jobchar)])
  
  tenure_decomp <- decomposition_analysis(weights = weights, sample_conditions = sample_conditions, data = psid_imp2, 
                                          years = years, outcome = "emp.tenure", group = group,
                                          covariates = covariates[covariates %!in% c(laborsupply, jobchar)])
  
  # Creating LaTex output for Table 4: Decomposing the % Explained in Changing Labor Supply
  t4 <- bind_rows(expf_decomp[c("crossgroup", "group0", "group1")] %>%
                    bind_cols() %>%
                    mutate(var = rownames(.)) %>%
                    dplyr::select(var, everything(), -starts_with("group")) %>%
                    adorn_totals("row") %>%
                    select(var, starts_with("Characteristics")) %>%
                    rename(Variable = var, 
                           Total_CharGap = "Characteristics Gap...1", 
                           Men_CharGap = "Characteristics Gap...5",
                           Women_CharGap = "Characteristics Gap...9") %>% 
                    mutate_if(is.numeric, ~.*100) %>%
                    mutate_if(is.numeric, round, digits = 2) %>%
                    filter(Variable %in% c("num.kids.cont")),
                  tenure_decomp[c("crossgroup", "group0", "group1")] %>%
                    bind_cols() %>%
                    mutate(var = rownames(.)) %>%
                    dplyr::select(var, everything(), -starts_with("group")) %>%
                    adorn_totals("row") %>%
                    select(var, starts_with("Characteristics")) %>%
                    rename(Variable = var, 
                           Total_CharGap = "Characteristics Gap...1", 
                           Men_CharGap = "Characteristics Gap...5",
                           Women_CharGap = "Characteristics Gap...9") %>% 
                    mutate_if(is.numeric, ~.*100) %>%
                    mutate_if(is.numeric, round, digits = 2) %>%
                    filter(Variable %in% c("num.kids.cont"))) %>%
    mutate(outcome = c("FullTimeExperience", "EmployerTenure"))
  
  saveRDS(t4, file = paste("bootstrap_table4/table4_bs_14.13.23_", i, ".RDS", sep = ""))
  
  toc()
  
}

# List all .RDS files
files <- list.files(path = "bootstrap_table4", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_table4", x, sep = "/"))
}

# Load all .RDS files into a list of data frames
df_list <- lapply(files, load_rds)

# Combine all data frames into one data frame
df_combined <- do.call(rbind, df_list) %>%
  gather(quantity, value, - c(Variable, outcome)) %>%
  pivot_wider(values_from = value, names_from = c(Variable, quantity, outcome)) %>%
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
                        levels = c("FullTimeExperience",
                                   "EmployerTenure")), 
         Group = factor(Group, levels = c("Total", "Men", "Women"))) %>%
  arrange(Model, Component, Group, Quantity) %>%
  rename(fifth = "5%", ninetyfifth = "95%")

write_csv(bootstrap_table, "tables/bootstrap_table4.csv")