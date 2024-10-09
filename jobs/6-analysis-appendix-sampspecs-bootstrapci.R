#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: ALTERNATIVE SAMPLE SPECIFICATIONS
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
  
  set.seed(i)
  
  sample_conditions_ft = list("samp.inc.age == 1",
                              "samp.exc.mil.ag != 1", 
                              "samp.exc.selfemp != 1", 
                              "samp.exc.region != 1",
                              "samp.exc.zerowage != 1", 
                              "ann.wrk.hrs > 0",
                              "samp.inc.ft == 1")
  
  conditions_expr <- glue::glue("({paste(sample_conditions_ft, collapse = ') & (')})")
  
  psid_imp_ft <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(1549, 20), rep(1005, 20), rep(2606, 20), rep(2564, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  covariates_to_exclude <- list(
    c(age, race, region, ed, marstat, laborsupply, jobchar),
    c(ed, laborsupply, jobchar),
    c(laborsupply, jobchar),
    c(wrkhrs, jobchar),
    c("ftormore")
  )
  
  data <- psid_imp_ft
  
  ta7_ft <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  
  sample_conditions_noimm = list("samp.inc.age == 1",
                                 "samp.exc.mil.ag != 1", 
                                 "samp.exc.selfemp != 1", 
                                 "samp.exc.region != 1",
                                 "samp.exc.zerowage != 1", 
                                 "ann.wrk.hrs > 0",
                                 "imm.sample.17 == 0", 
                                 "imm.sample.97 == 0")
  
  conditions_expr <- glue::glue("({paste(sample_conditions_noimm, collapse = ') & (')})")
  
  psid_imp_noimm <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(1738, 20), rep(1674, 20), rep(2362, 20), rep(2791, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  data <- psid_imp_noimm
  
  covariates_to_exclude <- list(
    c(age, race, region, ed, marstat, laborsupply, jobchar),
    c(ed, laborsupply, jobchar),
    c(laborsupply, jobchar),
    c(wrkhrs, jobchar),
    c()
  )
  
  ta7_noimm <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  
  sample_conditions_agerest = list("samp.inc.age == 1",
                                   "samp.exc.mil.ag != 1", 
                                   "samp.exc.selfemp != 1", 
                                   "samp.exc.region != 1",
                                   "samp.exc.zerowage != 1", 
                                   "ann.wrk.hrs > 0",
                                   "age %in% c(35:55)")
  
  conditions_expr <- glue::glue("({paste(sample_conditions_agerest, collapse = ') & (')})")
  
  psid_imp_agerest <- psid_imp %>%
    mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
    filter(samp.inc == 1) %>%
    filter(year %in% years) %>%
    group_by(year, female, .imp) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(year, female) %>%
    mutate(n = c(rep(1080, 20), rep(1106, 20), rep(2114, 20), rep(2339, 20))) %>%
    mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    dplyr::select(-data) %>%
    unnest(samp) %>%
    dplyr::select(-n)
  
  data <- psid_imp_agerest
  
  ta7_35to55 <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  
  ta7 <- left_join(ta7_ft %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                     rename(Fulltime = "Total_CharGap"), 
                   ta7_noimm %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                     rename(NoImm = "Total_CharGap"), by = c("Model", "Variable")) %>%
    left_join(., ta7_35to55 %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                rename(Age35to55 = "Total_CharGap"), by = c("Model", "Variable")) %>%
    filter(Variable %in% c("num.kids.cont", "Total")) %>%
    dplyr::select(Model, Variable, Fulltime, NoImm, Age35to55)
  # Stores results
  
  saveRDS(ta7, file = paste("bootstrap_altsamp/table3_altsamp_", i, ".RDS", sep = ""))
  
  toc()
  
}

# List all .RDS files
files <- list.files(path = "bootstrap_altsamp", pattern="*.RDS")

# Function to load .RDS files
load_rds <- function(x){
  readRDS(paste("bootstrap_altsamp", x, sep = "/"))
}

# Load all .RDS files into a list of data frames
df_list <- lapply(files, load_rds)

# Combine all data frames into one data frame
df_combined <- do.call(rbind, df_list) %>%
  gather(quantity, value, - c(Variable, Model)) %>%
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

write_csv(bootstrap_table, "tables/bootstrap_table_altsamp.csv")

# Checking that mdipoint between 5th and 95th percentiles approximates the point estimates
t3 <- read_csv("tables/table_sampspecs.csv") %>%
  gather(Group, pointestimate, -c(Variable, Model)) %>%
  left_join(., read_csv("tables/bootstrap_table_altsamp.csv")  %>%
              rename(Variable = Group, Group = Measure),
            by = c("Variable", "Group", "Model")) %>%
  mutate(midpoint = (fifth + ninetyfifth) / 2)

t_altsamp <- read_csv("tables/table_sampspecs.csv") %>% 
  mutate_if(is.numeric, ~ sprintf("%.2f", .x)) %>%
  bind_rows(read_csv("tables/bootstrap_table_altsamp.csv") %>%
              mutate_if(is.numeric, ~ sprintf("%.2f", .x)) %>%
              mutate(confint = paste0("(", fifth, ", ", ninetyfifth, ")")) %>% 
              transmute(Variable = Group, Group = Measure, Model, confint) %>% 
              pivot_wider(names_from = c(Group), values_from = confint)) %>%
  arrange(Model, Variable) %>% 
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  mutate(Variable = ifelse(Variable == "num.kids.cont", "Number of Children", Variable),
         Variable = factor(Variable, levels = c("Number of Children", "Total"))) %>%
  arrange(Model, Variable)

t_fert_boot <- read_csv("tables/bootstrap_fertility_full.csv") %>%
  select(FortyPlus)

kable(t_altsamp %>% bind_cols(t_fert_boot) %>% 
        select(-Model), format = "latex", booktabs = T,
      caption = "Table A3: Percent of Gender Pay Convergence 1980-2018 Explained by Fertility Decline, Alternative Sample Specifications") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Note: See text for details of model specifications. 95 % bootstrapped confidence intervals are shown in parentheses.",
           threeparttable = TRUE) %>%
  pack_rows("Model 1: Baseline", 1, 4, bold = TRUE) %>%
  pack_rows("Model 2: + Background", 5, 8, bold = TRUE) %>%
  pack_rows("Model 3: + Education", 9, 12, bold = TRUE) %>%
  pack_rows("Model 4: + Work Experience and Job Tenure", 13, 16, bold = TRUE) %>%
  pack_rows("Model 5: Full", 17, 20, bold = TRUE)
