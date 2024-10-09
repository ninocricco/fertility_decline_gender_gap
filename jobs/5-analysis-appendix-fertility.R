#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX: ALTERNATIVE MEASURES OF FERTILITY
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


covariates = c("numkids.1", "numkids.2", "numkids.3plus", "age", "agesq",
               "Black", "Hispanic", "Other",
               "Northeast", "Northcentral", "South", "married",
               "HighSchool", "SomeCollege", "ba.avdeg", 
               "emp.tenure", "log.expf", "ftormore",
               "overwork", "union",
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
               "Black", "Hispanic", "Other",
               "Northeast", "Northcentral", "South", "married",
               "HighSchool", "SomeCollege", "ba.avdeg", 
               "emp.tenure", "log.expf", "ftormore",
               "overwork", "union",
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
               "Black", "Hispanic", "Other",
               "Northeast", "Northcentral", "South", "married",
               "HighSchool", "SomeCollege", "ba.avdeg", 
               "emp.tenure", "log.expf", "ftormore",
               "overwork", "union",
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

conditions_expr <- glue::glue("({paste(sample_conditions_40plus, collapse = ') & (')})")

psid_imp <- psid_imp %>%
  mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
  filter(samp.inc == 1) 

ta8_40plus <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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
  left_join(., ta8_40plus %>% dplyr::select(Model, Variable, Total_CharGap) %>%
              rename("40+" = "Total_CharGap") %>% filter(
                Variable %in% c("num.kids.cont", "Total")) %>%
              mutate(group = ifelse(Variable == "Total", "Total", "Fertility")) %>%
              group_by(group, Model) %>%
              summarise_if(is.numeric, funs(sum(., na.rm = TRUE))),
            by = c("Model", "group")) %>%
  arrange(Model)

write_csv(ta8, "tables/table_altfertspecs.csv")

kable(ta8 %>% dplyr::select(-Model), booktabs = T, format = "latex", 
      caption = "Table A5: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Fertility Decline, Alternative Fertility Specifications") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T)
