#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, APPENDIX FIGURES
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

#------------------------------------------------------------------------------
# FIGURE A1: PROPORTION HEAD/WIFE BY GENDER, AGE, AND YEAR
#------------------------------------------------------------------------------
fig_a1 <- read_csv("clean_data/intermediate_psid_raw.csv") %>%
  filter(age >= 20 & age <= 65) %>%
  filter(year %in% c(1981, 2019)) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men"),
         year = ifelse(year == 1981, 1980, 2018)) %>%
  group_by(year, age, Gender) %>%
  filter(complete.cases(age)) %>%
  summarise(wtd.pct = wpct(hd.wife, weight = perwt)[2] * 100) %>%
  ggplot(aes(x = age, y = wtd.pct, linetype = Gender)) +
  geom_line() + 
  facet_wrap(~year) +
  theme_bw() +
  labs(
    #title = "Figure A1: Percent of PSID Sample Members Classified as Reference Persons
    #\ and Spouses/Partners, by Year, Gender, and Age",
    y = "Percent of Sample Members Reference Person or Spouse/Partner (weighted)",
    x = "Age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 15), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 15))

ggsave(plot = fig_a1, 
       "figures/sf_final/fig_a1.tiff", units = "in", height = 9, width = 12, 
       dpi = 700, bg = "white")

#------------------------------------------------------------------------------
# FIGURE A2: FERTILITY BY AGE GROUP
#------------------------------------------------------------------------------
fert_30_39 <- generate_means_year_table(
  weights = weights, 
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

fert_40_55 <- generate_means_year_table(
  weights = weights, 
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

fig_fert_age_values <- bind_rows(fert_30_39 %>% mutate(Age = "30-39"),
                                 fert_40_55 %>% mutate(Age = "40-55")) %>%
  dplyr::select(year, female, Age, num.kids.cont_mean) %>% 
  pivot_wider(names_from = female, values_from = num.kids.cont_mean) %>%
  rename(Women = "1", Men = "0") %>%
  gather(Gender, value, -c(year, Age))

# Creates figure A2
fig_fert_age <- fig_fert_age_values %>%
  ggplot(aes(y = value, x = year, linetype = Gender)) +
  geom_point() +
  geom_line() +
  theme_bw() + 
  labs(y = "Average Number of Children",
       # title = "Figure A2: Fertility Decline by Gender and Age Group, 1980-2018",
       x = "",
       # caption = "Note: Figure shows weighted mean number of children born to date for the analytic sample by year, gender and age group. \
       # The sample consists of PSID reference persons and spouses/partners aged 30-39 (left) and 40-55 (right)\
       # who have non-zero individual sample weights and report non-zero wages. \ 
       # It excludes individuals who report being self-employed and individuals employed in agriculture or the military."
  ) +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(hjust = 0.5, size = 15.5),
        axis.text.x = element_text(angle = 12, size = 14, vjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 12, face = "italic"),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 15)) +
  facet_wrap(~Age) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  scale_linetype_manual(values = c(1, 2), name = "Gender")

ggsave(plot = fig_fert_age, "figures/sf_final/fig_a2.tiff", 
       width = 10, height = 8, units = "in", bg = "white", dpi = 700)

#------------------------------------------------------------------------------
# FIGURE A3: VARIABLE CORRELATION MATRIX
#------------------------------------------------------------------------------

#Correlation matrices
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

cov_names <- c("Log Hourly Wage", "Number of Children",  
               "Years of Education", "Full-Time Experience", "Employer Tenure", 
               "Annual Work Hours",
               "Unionized Job", "Government Job", "Percent Female in Occ.", 
               "Professional/Management Occ.", "Manuf. Job")

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
  labs(#title = "Figure A3: Variable Correlation Matrix, by Year and Gender",
    x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5, size = 15.5),
        legend.position = "bottom") +
  facet_grid(cols = vars(Gender), rows = vars(year))

ggsave(plot = cormatrixplot, "figures/sf_final/fig_a3.tiff", 
       units = "in", height = 8, width = 8, bg = "white", dpi = 700)

#------------------------------------------------------------------------------
# FIGURE A4: DECOMPOSITION RESULTS BY RACE SUBGROUPS
#------------------------------------------------------------------------------
# Loading the imputed dataset
psid_imp <- read_csv(paste0("clean_data/psid_final_", date_run_id, ".csv"))

# Loading in objects that specify arguments passed to the 
# analysis functions multiple times
source("jobs/4-analysis-arguments.R")

covariates_to_exclude <- list(
  c(age, race, region, ed, marstat, laborsupply, jobchar),
  c(ed, race, laborsupply, jobchar),
  c(laborsupply, race, jobchar),
  c(wrkhrs, race, jobchar),
  c(race)
)

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
  filter(samp.inc == 1)

data <- psid_impwhite

t3_white <- mapply(perform_decomposition_analysis, covariates_to_exclude,
                   model_labels, SIMPLIFY = FALSE) %>% 
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
  filter(samp.inc == 1)

data <- psid_impnonwhite

t3_nonwhite <- mapply(perform_decomposition_analysis, covariates_to_exclude,
                      model_labels, SIMPLIFY = FALSE) %>% 
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
  filter(samp.inc == 1)

data <- psid_impblack

t3_black <- mapply(perform_decomposition_analysis, covariates_to_exclude,
                   model_labels, SIMPLIFY = FALSE) %>% 
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

write_csv(t_race, file = paste("tables/table_race_pointestimates",".csv", sep = ""))

t_race_bs <- read_csv("tables/table_race_pointestimates.csv") %>%
  gather(key, pointestimate, -c(Variable, Model)) %>%
  separate(key, into = c("Group")) %>%
  left_join(., read_csv("tables/bootstrap_race.csv") %>%
              rename(Variable = Quantity),
            by = c("Variable", "Group", "Model")) %>%
  mutate(midpoint = (fifth + ninetyfifth) / 2) 

fig_a4 <- t_race_bs %>%
  separate(Model, into = c("Model", "Descriptor"), sep = ":") %>%
  filter(Group %in% c("White", "Black"), Variable == "num.kids.cont") %>%
  mutate(Variable = ifelse(Variable == "num.kids.cont", "Number of Children", Variable),
         Race = factor(Group, levels = c("White", "Black"))) %>%
  ggplot(aes(x = Race, y = pointestimate, shape = Race)) +
  geom_point(size = 4) +                             # Plot the point estimates
  geom_errorbar(aes(ymin = fifth, ymax = ninetyfifth),     # Add error bars for CI
                width = 0.2) +        
  facet_grid(rows = vars(Variable), cols = vars(Model), scales = "free") +
  # Adjust the width of the error bars
  labs(x = "", y = "Estimate, Percent Explained",
       #    title = "Figure A4: Percent of Gender Pay Convergence 1980-2018 Explained by Fertility Decline, by Race",
       #   caption = "Note: Results from decomposition models stratified by race. See text for details of model specifications. 
       #  \ 95 % bootstrapped confidence intervals are shown for all point estimates. 
       # \ In 1980, the sample includes 506 Black men, 586 Black women, 1085 white men, and 975 white women. 
       #\ In 2018 the sample includes 864 Black men, 1185 Black Women, 1378 white men, and 1455 white women."
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") + # Label the axes
  theme_minimal() +  
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15.5),
        plot.caption = element_text(size = 12, face = "italic", hjust = 0.5)) 

ggsave(plot = fig_a4, "figures/sf_final/fig_a4.tiff",  width = 10, height = 8,
       units = "in", bg = "white", dpi=700)