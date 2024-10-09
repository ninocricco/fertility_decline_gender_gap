#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: ALTERNATIVE REFERENCE YEAR
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

date_run_id <-  "2024-06-07"

opts <- options(knitr.kable.NA = "")

# Loading helper functions and libraries
source("jobs/1-load-libraries.R")
source("jobs/1-functions.R")

# Loading the imputed dataset
psid_imp <- read_csv(paste0("clean_data/psid_final", ".csv"))

# Loading in objects that specify arguments passed to the 
# analysis functions multiple times
source("jobs/4-analysis-arguments.R")

years <- c(2019, 1981)

main_decomp_altrefyear <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

t_altref <- main_decomp_altrefyear %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

write_csv(t_altref, "tables/table_altrefyear.csv")

# Generates LaTex code to produce Table A5: Decomposition % of Change in Pay Gap Explained by Declining Fertility, by Gender
kable(t_altref %>%
        dplyr::select(-c(Variable, Model)), booktabs = T, format = "latex", 
      caption = "Table A4:  Percent of Gender Pay Convergence 1980-2018 Explained by Fertility Decline, Using 2018 Wage Returns, by Gender") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T)
