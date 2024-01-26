#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, APPENDIX TABLE 9: Alternative reference year
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/23 (mdy)
#**********************************************************

# This conducts the same analyses as the main decompositions,
# but using 2019 instead of 1980 as the reference year

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

source("jobs/3-analysis-arguments.R")

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

ta9 <- main_decomp_altrefyear %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

write_csv(ta9, "tables/tablea9.csv")

# Generates LaTex code to produce Table A9: Decomposition % of Change in Pay Gap Explained by Declining Fertility, by Gender
kable(ta9 %>%
        dplyr::select(-c(Variable, Model)), booktabs = T, format = "latex", 
      caption = "Table A9: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Fertility, Using 2018 Wage Structure") %>%
  pack_rows("Model 1: Demographic Controls", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Education", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Marital Status", 5, 6, bold = T) %>% 
  pack_rows("Model 4:+ Labor Supply", 7, 8, bold = T) %>% 
  pack_rows("Model 5: + Job Characteristics", 9, 10, bold = T)
