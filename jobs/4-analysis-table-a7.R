#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, APPENDIX TABLE 7: ALTERNATIVE SAMPLE SPECS
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/23 (mdy)
#**********************************************************

# This conducts the same analyses as the main decompositions,
# but with different sample restrictions to check the robustness
# of our results to different sample specifications (such as 
# working full0-time, excluding the immigrant sample, and with
# more limited age criteria)

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

source("jobs/3-analysis-arguments.R")

sample_conditions = list("samp.inc.age == 1",
                            "samp.exc.mil.ag != 1", 
                            "samp.exc.selfemp != 1", 
                            "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0",
                            "samp.inc.ft == 1")

covariates_to_exclude <- list(
  c(ed, marstat, laborsupply, jobchar),
  c(marstat, laborsupply, jobchar),
  c(laborsupply, jobchar),
  c("ftormore", jobchar),
  c("ftormore")
)

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

sample_conditions = list("samp.inc.age == 1",
                               "samp.exc.mil.ag != 1", 
                               "samp.exc.selfemp != 1", 
                               "samp.exc.region != 1",
                               "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0",
                         "age %in% c(35:55)")

covariates_to_exclude <- list(
  c(ed, marstat, laborsupply, jobchar),
  c(marstat, laborsupply, jobchar),
  c(laborsupply, jobchar),
  c(jobchar),
  c()
)

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

sample.conditions = list("samp.inc.age == 1",
                               "samp.exc.mil.ag != 1", 
                               "samp.exc.selfemp != 1", 
                               "samp.exc.region != 1",
                               "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0",
                         "imm.sample == 0")

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

ta7 <- left_join(ta7_ft %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                 rename(Fulltime = "Total_CharGap"), 
                 ta7_noimm %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                 rename(NoImm = "Total_CharGap"), by = c("Model", "Variable")) %>%
  left_join(., ta7_35to55 %>% dplyr::select(Model, Variable, Total_CharGap) %>%
              rename(Age35to55 = "Total_CharGap"), by = c("Model", "Variable")) %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, Fulltime, NoImm, Age35to55)

write_csv(ta7, "tables/tablea7.csv")

kable(ta7 %>% dplyr::select(-Model), booktabs = T, format = "latex", 
      caption = "Table A7: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Fertility, Alternative Sample Specifications") %>%
  pack_rows("Model 1: Demographic Controls", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Education", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Marital Status", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Labor Supply", 7, 8, bold = T) %>% 
  pack_rows("Model 5: + Job Characteristics", 9, 10, bold = T)
