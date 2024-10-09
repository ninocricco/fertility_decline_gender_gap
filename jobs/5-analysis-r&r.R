#********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: SOCIAL FORCES R&R
# AUTHOR: NINO CRICCO
#********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv") 

# Loading in objects that specify arguments passed to the 
# analysis functions multiple times
source("jobs/3-analysis-arguments.R")


# RACE/ETHNICITY

sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0",
                         "perwt > 0",
                         "White == 1")

covariates_to_exclude <- list(
  c(age, race, region, ed, marstat, laborsupply, jobchar),
  c(race, ed, laborsupply, jobchar),
  c(race, laborsupply, jobchar),
  c(race, wrkhrs, jobchar),
  c(race, "ftormore")
)

t3_white <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels,
                   MoreArgs = list(weights = "perwt", sample_conditions = list("samp.inc.age == 1",
                                                               "samp.exc.mil.ag != 1", 
                                                               "samp.exc.selfemp != 1", 
                                                               "samp.exc.region != 1",
                                                               "samp.exc.zerowage != 1", 
                                                               "ann.wrk.hrs > 0",
                                                               "perwt > 0",
                                                               "White == 1"),
                   data = psid_imp %>% mutate(perwt = perwt_norm)),
                   SIMPLIFY = FALSE) %>% 
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

t3_nonwhite <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels,
                      MoreArgs = list(weights = "perwt", sample_conditions = list("samp.inc.age == 1",
                                                                                  "samp.exc.mil.ag != 1", 
                                                                                  "samp.exc.selfemp != 1", 
                                                                                  "samp.exc.region != 1",
                                                                                  "samp.exc.zerowage != 1", 
                                                                                  "ann.wrk.hrs > 0",
                                                                                  "perwt > 0",
                                                                                  "White == 0"),
                                      data = psid_imp %>% mutate(perwt = perwt_norm)),
                      SIMPLIFY = FALSE) %>% 
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

t3_black <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, 
                   MoreArgs = list(weights = "perwt", sample_conditions = list("samp.inc.age == 1",
                                                                               "samp.exc.mil.ag != 1", 
                                                                               "samp.exc.selfemp != 1", 
                                                                               "samp.exc.region != 1",
                                                                               "samp.exc.zerowage != 1", 
                                                                               "ann.wrk.hrs > 0",
                                                                               "perwt > 0",
                                                                               "Black == 1"),
                                   data = psid_imp %>% mutate(perwt = perwt_norm)),
                   SIMPLIFY = FALSE) %>% 
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

t_race <- left_join(t3_white %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                   rename(White = "Total_CharGap"), 
                 t3_nonwhite %>% dplyr::select(Model, Variable, Total_CharGap) %>%
                   rename(NonWhite = "Total_CharGap"), by = c("Model", "Variable")) %>%
  left_join(., t3_black %>% dplyr::select(Model, Variable, Total_CharGap) %>%
              rename(Black = "Total_CharGap"), by = c("Model", "Variable")) %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, White, NonWhite, Black)

write_csv(t_race, "tables/t_race.csv")

kable(t_race %>% dplyr::select(-Model), booktabs = T, format = "latex", 
      caption = "Table 3: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Fertility, by Race") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T)

race_sampsize <- psid_imp %>% filter(.imp == 1, samp.inc.age == 1,
                    samp.exc.mil.ag != 1, 
                    samp.exc.selfemp != 1, 
                    samp.exc.region != 1,
                    samp.exc.zerowage != 1, 
                    ann.wrk.hrs > 0, 
                    year %in% c(1981, 2019)) %>%
  group_by(race, female, year) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = c(race), values_from = n)

kable(race_sampsize %>% ungroup() %>% select(-female),
      booktabs = T, format = "latex", 
      caption = "Sample Sizes by Race") %>%
  pack_rows("Men", 1, 2, bold = T) %>%
  pack_rows("Women", 3, 4, bold = T)
  
sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "perwt > 0",
                         "ann.wrk.hrs > 0")

means_race <- bind_rows(generate_means_year_table(
  weights = "perwt_norm",
  sample_conditions = append(sample_conditions, "White == 1"),
  data = data, years = c(1981, 1991, 2001, 2011, 2019),
  outcome = outcome, covariates = c("wages.hrly")) %>%
    mutate(race = "White"), 
  generate_means_year_table(
    weights = weights,
    sample_conditions = append(sample_conditions, "White == 0"),
    data = data, years = c(1981, 1991, 2001, 2011, 2019),
    outcome = outcome, covariates = c("wages.hrly")) %>%
    mutate(race = "NonWhite"), 
  generate_means_year_table(
    weights = weights,
    sample_conditions = append(sample_conditions, "Black == 1"),
    data = data, years = c(1981, 1991, 2001, 2011, 2019),
    outcome = outcome, covariates = c("wages.hrly")) %>%
    mutate(race = "Black")) %>%
  dplyr::select(year, female, wages.hrly_mean, race) %>% 
  pivot_wider(names_from = c(race, female), values_from = wages.hrly_mean) %>%
  transmute(year, White = (White_1/White_0) * 100,
         NonWhite = (NonWhite_1/NonWhite_0) * 100,
         Black = (Black_1/Black_0) * 100) %>%
  gather(race, value, -year) %>%
  ggplot(aes(x = year, y = value, linetype = race, shape = race)) +
  geom_point() +
  theme_bw() + 
  labs(title = "Changes in the Gender Pay Gap, 1980-2018, by Race", 
       y = "Mean Hourly Wage Women / Men", 
       x = "") +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5, size = 15.5), legend.position = "bottom", 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  geom_hline(yintercept = 100, linetype = "dashed")

# DESC STATS WITH/WITHOUT IMMIGRANT SAMPLE
sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "perwt > 0",
                         "ann.wrk.hrs > 0")

means.year_table <- generate_means_year_table(weights = weights, sample_conditions = sample_conditions, data = data, 
                                              years = c(2019), outcome = outcome, 
                                              covariates = c("wages.hrly", "age", "White", "Black", "Hispanic", "Other",
                                                             "Northeast", "Northcentral", "South", "West",
                                                             "married", "num.kids.cont",
                                                             "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg", 
                                                             "expf", "ftormore","overwork", "emp.tenure",
                                                             "union", "govt.job", "occ.pct.female", 
                                                             "occ.managers", "manuf")) %>%
  mutate(year = year-1, 
         sample = "Full Sample") %>% 
  bind_rows(generate_means_year_table(weights = weights,
                                      sample_conditions = append(sample_conditions, "imm.sample == 0"), data = data, 
                                      years = c(2019), outcome = outcome, 
                                      covariates = c("wages.hrly", "age", "White", "Black", "Hispanic", "Other",
                                                     "Northeast", "Northcentral", "South", "West",
                                                     "married", "num.kids.cont",
                                                     "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg", 
                                                     "expf", "ftormore","overwork", "emp.tenure",
                                                     "union", "govt.job", "occ.pct.female", 
                                                     "occ.managers", "manuf")) %>%
              mutate(year = year-1, 
                     sample = "Exc. Immigrant Sample"))

order_vars <- c("(Intercept)", "wages.hrly", "num.kids.cont", "age", "agesq",
                "White", "Black", "Hispanic", "Other", 
                "Northeast", "Northcentral", "South", "West", "married",
                "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg",
                "expf", "log.expf", "emp.tenure", 
                "ftormore","overwork",
                "union", "govt.job", "manuf", "occ.pct.female", "occ.managers", 
                "n")

t1_exc_imm <- means.year_table %>%
  dplyr::select(-starts_with(c("lnhrlywage", "(")), -year) %>%
  gather(key, value, - c("sample", "female")) %>%
  separate(key, c("var", "estimate"), sep = "_") %>%
  pivot_wider(id_cols = c(var, estimate),
              names_from = c(female, sample), values_from = value) %>%
  dplyr::select(var, estimate, starts_with("0"), everything()) %>%
  mutate(var = factor(var, levels = order_vars)) %>%
  arrange(var) %>%
  mutate(across(where(is.numeric),
                ~ ifelse(var %in% c("Northeast", "Northcentral", "South", "West", "Black", "Hispanic", "Other", "White",
                                    "married", "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg", 
                                    "ftormore","overwork",
                                    "union", "govt.job", "occ.pct.female", "occ.managers", "manuf"),
                         . * 100, .))) %>%
  mutate_if(is.numeric, round, digits = 1)

knitr::kable(t1_exc_imm %>% select(-estimate), booktabs = T, format = "latex", 
             caption = "Sample Descriptive Statistics in 2019, Including vs. Excluding Immigrant Refresher Sample") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  #footnote("Descriptive statistics show weighted averages for the analytic sample by year and gender. The sample consists of PSID heads and wives aged 30 to 55 who report non-zero wages. It excludes individuals who report being self-employed and individuals employed in agriculture or the military.", 
   #        threeparttable = T) %>%
  pack_rows("Race", 7, 14, bold = F) %>%
  pack_rows("Region", 15, 22, bold = F) %>% 
  pack_rows("Years of Education", 24, 32, bold = F) %>%
  pack_rows("Work Hours", 37, 40, bold = F)

# Fertility for people who aren't employed
sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "perwt > 0")

bind_rows(generate_means_year_table(weights = weights, sample_conditions = sample_conditions, data = data, 
                          years = c(1981, 1991, 2001, 2011, 2019), outcome = outcome, 
                          covariates = c("num.kids.cont")) %>%
            select(year, female, num.kids.cont_mean) %>%
            mutate(sample = "Incl. Not Employed"), 
          generate_means_year_table(weights = weights, 
                                    sample_conditions = append(sample_conditions, c("samp.exc.zerowage != 1", 
                                                               "ann.wrk.hrs > 0")), data = data, 
                                    years = c(1981, 1991, 2001, 2011, 2019), outcome = outcome, 
                                    covariates = c("num.kids.cont")) %>%
            select(year, female, num.kids.cont_mean) %>%
            mutate(sample = "Employed Sample")) %>%
  mutate(Gender = ifelse(female == 0, "Men", "Women")) %>%
  ggplot(aes(x = year, y = num.kids.cont_mean, linetype = sample, shape = sample)) +
  geom_point() +
  theme_bw() + 
  facet_wrap(~Gender) +
  labs(title = "Changes in Fertility", 
       y = "Mean Number of Children", 
       x = "") +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5, size = 15.5), legend.position = "bottom", 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018))

#Correlation matrices
sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0",
                         "perwt > 0")

# Create a single combined expression for the filtering conditions
conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")

average_cor_matrices <- function(mat_list) {
  avg_matrix <- Reduce(`+`, mat_list) / length(mat_list)
  colnames(avg_matrix) <- colnames(mat_list[[1]])
  rownames(avg_matrix) <- rownames(mat_list[[1]])
  return(avg_matrix)
}

covariates = c("lnhrlywage", "num.kids.cont",  
               "yrs.ed.fam", "expf", "emp.tenure", 
               "ann.wrk.hrs",
               "union", "govt.job", "occ.pct.female", 
               "occ.managers", "manuf")

cov_names <- c("Log Hourly Wage", "Number of Kids",  
               "Years of Education", "FT Experience", "Tenure", 
               "Annual Work Hours",
               "Union Member", "Government Job", "Occ. % Female", 
               "Occ. Managers", "Manuf. Job")

name_map <- setNames(cov_names, covariates)

cormatrix <- psid_imp %>%
  mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0),
         Gender = ifelse(female == 1, "Women", "Men")) %>%
  filter(samp.inc == 1, year %in% c(1981, 2019)) %>%
  mutate(year = ifelse(year == 1981, 1980, 2018)) %>%
  group_by(Gender, year, .imp) %>%
  do(correlation_matrix = cor(select(., all_of(covariates)))) %>%
  ungroup() %>%
  group_by(Gender, year) %>%
  summarise(list(average_cor_matrices(correlation_matrix)) %>%
              melt(.)) %>%
  ungroup() %>%
  arrange() %>%
  mutate(Var1 = as.character(Var1), 
         Var1 = recode(Var1, !!!name_map),
         Var2 = as.character(Var2), 
         Var2 = recode(Var2, !!!name_map),
         Var1 = factor(Var1, levels = cov_names),
         Var2 = factor(Var2, levels = cov_names))

variable_order <- setNames(seq_along(cov_names), cov_names)
reversed_variable_order <- rev(variable_order) 

# Create an index for Var1 and Var2 based on 'variable_order'
cormatrix <- cormatrix %>%
  mutate(Var2_index = variable_order[as.character(Var2)],
         Var1_index = variable_order[as.character(Var1)],
         mask = Var1_index < Var2_index)  # Mask upper triangle + diagonal


cormatrixplot <- cormatrix %>% filter(mask) %>%
  ggplot(aes(Var2, factor(Var1, levels = names(reversed_variable_order)), fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "beige",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  labs(title = "Figure A2: Variable Correlation Matrix by Year and Gender",
       x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = .5),
        legend.position = "bottom") +
  facet_grid(cols = vars(Gender), rows = vars(year))

ggsave("figures/fig_a2.pdf", units = "in", height = 8, width = 8)

sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0", 
                         "age %in% c(30:39)",
                         "perwt > 0")

fert_30_39 <- generate_means_year_table(weights = weights, 
                                        sample_conditions = list("samp.inc.age == 1",
                                                                 "samp.exc.mil.ag != 1", 
                                                                 "samp.exc.selfemp != 1", 
                                                                 "samp.exc.region != 1",
                                                                 "samp.exc.zerowage != 1", 
                                                                 "ann.wrk.hrs > 0", 
                                                                 "age %in% c(30:39)",
                                                                 "perwt > 0"),
                                        data = data, 
                                              years = c(1981, 1991, 2001, 2011, 2019), outcome = outcome, 
                                              covariates = c("num.kids.cont")) %>%
  mutate(year = year-1)


fert_40_55 <- generate_means_year_table(weights = weights, 
                                        sample_conditions = list("samp.inc.age == 1",
                                                                 "samp.exc.mil.ag != 1", 
                                                                 "samp.exc.selfemp != 1", 
                                                                 "samp.exc.region != 1",
                                                                 "samp.exc.zerowage != 1", 
                                                                 "ann.wrk.hrs > 0", 
                                                                 "age %in% c(40:55)",
                                                                 "perwt > 0"),
                                        data = data, 
                                        years = c(1981, 1991, 2001, 2011, 2019), outcome = outcome, 
                                        covariates = c("num.kids.cont")) %>%
  mutate(year = year-1)

fig2_values_age <- bind_rows(fert_30_39 %>% mutate(Age = "30-39"),
                         fert_40_55 %>% mutate(Age = "40-55")) %>%
  dplyr::select(year, female, Age, num.kids.cont_mean) %>% 
  pivot_wider(names_from = female, values_from = num.kids.cont_mean) %>%
  rename(Women = "1", Men = "0") %>%
  gather(Gender, value, -c(year, Age))

# Creates figure 2
fig2_age <- fig2_values_age %>%
  ggplot(aes(y = value, x = year, linetype = Gender)) +
  geom_point() +
  geom_line(size = 1.3) +
  theme_bw() + 
  labs(y = "Average Number of Children",
       title = "Figure A1: Fertility Decline by Age Group, 1980-2018") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.key.width=unit(3,"line"),
        legend.title = element_text(size = 14)) +
  facet_wrap(~Age) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  #scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2"), name="Gender")+
  scale_linetype_manual(values = c(2, 1), name = "Gender")

ggsave(plot = fig2_age, "figures/fig_a1.pdf", 
       width = 9, height = 7, units = "in", dpi=700)

main_decomp <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels,
                             MoreArgs = list(data = psid_imp %>% mutate(perwt = perwt_norm),
                                             weights = "perwt", sample_conditions = list("samp.inc.age == 1",
                                                                                         "samp.exc.mil.ag != 1", 
                                                                                         "samp.exc.selfemp != 1", 
                                                                                         "samp.exc.region != 1",
                                                                                         "samp.exc.zerowage != 1", 
                                                                                         "ann.wrk.hrs > 0", 
                                                                                         "perwt > 0")),
                             SIMPLIFY = FALSE) %>% 
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

t3_norm <- main_decomp %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

# T3 without normalizing weights
main_decomp_nonorm <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels,
                      MoreArgs = list(data = psid_imp,
                                      weights = "perwt", sample_conditions = list("samp.inc.age == 1",
                                                                                  "samp.exc.mil.ag != 1", 
                                                                                  "samp.exc.selfemp != 1", 
                                                                                  "samp.exc.region != 1",
                                                                                  "samp.exc.zerowage != 1", 
                                                                                  "ann.wrk.hrs > 0", 
                                                                                  "perwt > 0")),
                      SIMPLIFY = FALSE) %>% 
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

t3_nonorm <- main_decomp_nonorm %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

# Longitudinal weights test
psid_noimp <- read_csv("clean_data/psid_clean_wlongweights.csv") %>%
  select(indiv.id, year, starts_with("perwt"))

psid_imp2 <- psid_imp %>%
  left_join(., psid_noimp, by = c("indiv.id", "year")) %>%
  mutate(perwt = perwt.long)

main_decomp_long <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels,
                              MoreArgs = list(data = psid_imp2,
                                              weights = "perwt", 
                                              sample_conditions = list("samp.inc.age == 1",
                                                                       "samp.exc.mil.ag != 1", 
                                                                       "samp.exc.selfemp != 1", 
                                                                       "samp.exc.region != 1",
                                                                       "samp.exc.zerowage != 1", 
                                                                       "ann.wrk.hrs > 0", 
                                                                       "perwt > 0")),
                              SIMPLIFY = FALSE) %>% 
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

t3_long <- main_decomp_long %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

t3_weights <- bind_cols(t3_norm, t3_long %>% dplyr::select(-c(Model, Variable)))

kable(t3_weights %>% dplyr::select(-c(Model, Variable)), booktabs = T, format = "latex", 
      caption = "Table 3: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Fertility, Weights") %>%
  add_header_above(c(" ", "Cross-Sectional" = 3, "Longitudinal" = 3)) %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T)
