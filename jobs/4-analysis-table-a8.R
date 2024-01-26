#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, APPENDIX TABLE 8: ALTERNATIVE FERTILITY MEASURES
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/23 (mdy)
#**********************************************************

# This conducts the same analyses as the main decompositions,
# but with different fertility measures to assess the robustness
# of our results to measures typically used in the literature
# (continuous fertility measure, categorical measure distinguishing
# 1, 2, and 3+ kids, and adding a categorical measure of fertility 
# timing in addition to the measure of parity

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

source("jobs/3-analysis-arguments.R")

covariates = c("numkids.1", "numkids.2", "numkids.3plus", "age", "agesq",
                   "Black", "Hispanic", "Other",
                   "Northeast", "Northcentral", "South", 
                   "HighSchool", "SomeCollege", "ba.avdeg", 
                   "married", "log.expf", "samp.inc.ft",
                   "emp.tenure", "overwork", "union",
                   "govt.job", "occ.pct.female", 
                   "occ.managers", "manuf")

ta8_cat <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

covariates = c("afb.cat_21minus",  "afb.cat_23to27",  "afb.cat_28plus",
                      "age", "agesq",
                      "Black", "Hispanic", "Other",
                      "Northeast", "Northcentral", "South", 
                      "HighSchool", "SomeCollege", "ba.avdeg", 
                      "married", "log.expf", "samp.inc.ft",
                      "emp.tenure", "overwork", "union",
                      "govt.job", "occ.pct.female", 
                      "occ.managers", "manuf")

ta8_timing <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

covariates = c("num.kids.cont",
                         "afb.cat_21minus",  "afb.cat_23to27",  "afb.cat_28plus",
                         "age", "agesq",
                         "Black", "Hispanic", "Other",
                         "Northeast", "Northcentral", "South", 
                         "HighSchool", "SomeCollege", "ba.avdeg", 
                         "married", "log.expf", "samp.inc.ft",
                         "emp.tenure", "overwork", "union",
                         "govt.job", "occ.pct.female", 
                         "occ.managers", "manuf")

ta8_timingcont <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

ta8 <- left_join(ta8_cat %>%  dplyr::select(Model, Variable, Total_CharGap) %>%
                   rename(Categorical= "Total_CharGap") %>% filter(
                     Variable %in% c("numkids.1", "numkids.2", "numkids.3plus", "Total")) %>%
                   mutate(group = ifelse(Variable == "Total", "Total", "Fertility")) %>%
                   group_by(group, Model) %>% 
                   summarise_if(is.numeric, funs(sum(., na.rm = TRUE))), 
                 ta8_timing %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                   rename(Timing = "Total_CharGap") %>% filter(
                     Variable %in% c("afb.cat_21minus", "afb.cat_23to27",
                                "afb.cat_28plus", "Total")) %>%
                   mutate(group = ifelse(Variable == "Total", "Total", "Fertility")) %>%
                   group_by(group, Model) %>%
                   summarise_if(is.numeric, funs(sum(., na.rm = TRUE))), by = c("Model", "group")) %>%
  left_join(., ta8_timingcont %>% dplyr::select(Model, Variable, Total_CharGap) %>%
              rename(TimingCont = "Total_CharGap") %>% filter(
                Variable %in% c("num.kids.cont", "afb.cat_21minus", "afb.cat_23to27",
                                "afb.cat_28plus", "Total")) %>%
              mutate(group = ifelse(Variable == "Total", "Total", "Fertility")) %>%
              group_by(group, Model) %>%
              summarise_if(is.numeric, funs(sum(., na.rm = TRUE))),
            by = c("Model", "group")) %>%
  arrange(Model)

write_csv(ta8, "tables/tablea8.csv")


kable(ta8 %>% dplyr::select(-Model), booktabs = T, format = "latex", 
      caption = "Table A8: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Fertility, Alternative Fertility Specifications") %>%
  pack_rows("Model 1: Demographic Controls", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Education", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Marital Status", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Labor Supply", 7, 8, bold = T) %>% 
  pack_rows("Model 5: + Job Characteristics", 9, 10, bold = T)
