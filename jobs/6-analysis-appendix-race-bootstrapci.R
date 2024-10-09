#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: SUBGROUP ESTIMATES BY RACE
# GENERATES BOOTSTRAPPED ESTIMATES FOR CONFIDENCE INTERVALS FOR FIGURE A4
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

date_run_id <-  "2024-06-07"

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/1-functions.R")
source("jobs/1-load-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv(paste0("clean_data/psid_final", ".csv"))
source("jobs/4-analysis-arguments.R")

covariates_to_exclude <- list(
  c(age, race, region, ed, marstat, laborsupply, jobchar),
  c(ed, race, laborsupply, jobchar),
  c(laborsupply, race, jobchar),
  c(wrkhrs, race, jobchar),
  c(race)
)

for(i in 1:10000){
  
  set.seed(i)
  
  sample_conditions = list("samp.inc.age == 1",
                           "samp.exc.mil.ag != 1", 
                           "samp.exc.selfemp != 1", 
                           "samp.exc.region != 1",
                           "samp.exc.zerowage != 1", 
                           "ann.wrk.hrs > 0",
                           "White == 1")
  
  conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")
  
  psid_impwhite <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(1085, 20), rep(975, 20), rep(1378, 20), rep(1455, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  data <-psid_impwhite
  
  t3_white <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  
  sample_conditions = list("samp.inc.age == 1",
                           "samp.exc.mil.ag != 1", 
                           "samp.exc.selfemp != 1", 
                           "samp.exc.region != 1",
                           "samp.exc.zerowage != 1", 
                           "ann.wrk.hrs > 0",
                           "White == 0")
  
  conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")
  
  psid_impnonwhite <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(653, 20), rep(699, 20), rep(1441, 20), rep(1730, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  data <-psid_impnonwhite
  
  t3_nonwhite <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

  sample_conditions = list("samp.inc.age == 1",
                           "samp.exc.mil.ag != 1", 
                           "samp.exc.selfemp != 1", 
                           "samp.exc.region != 1",
                           "samp.exc.zerowage != 1", 
                           "ann.wrk.hrs > 0",
                           "Black == 1")
  
  conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")
  
  psid_impblack <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(506, 20), rep(586, 20), rep(864, 20), rep(1185, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  data <-psid_impblack
  
  t3_black <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

  t3_race <- left_join(t3_white %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                     rename(White = "Total_CharGap"), 
                   t3_nonwhite %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                     rename(NonWhite = "Total_CharGap"), by = c("Model", "Variable")) %>%
    left_join(., t3_black %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                rename(Black = "Total_CharGap"), by = c("Model", "Variable")) %>%
    #filter(Variable %in% c("num.kids.cont", "Total")) %>%
    dplyr::select(Model, Variable, White, NonWhite, Black)
  # Stores results
  
  saveRDS(t3_race, file = paste("bootstrap_race/decomp_race_", i, ".RDS", sep = ""))
  
  toc()
  
}

# List all .RDS files
files <- list.files(path = "bootstrap_race", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_race", x, sep = "/"))
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
  separate(var, into = c("Quantity", "Group", "Model"), sep = "_")

rownames(bootstrap) <- NULL

bootstrap_table <- bootstrap %>% 
  select(Quantity, Group, everything()) %>% 
  mutate(Model = factor(Model, 
                        levels = c("Model 1: Baseline", 
                                   "Model 2: + Background", 
                                   "Model 3: + Education", 
                                   "Model 4: + Work Experience and Job Tenure",
                                   "Model 5: Full"))) %>%
  arrange(Model, Group, Quantity) %>%
  rename(fifth = "5%", ninetyfifth = "95%")

write_csv(bootstrap_table, "tables/bootstrap_race.csv")