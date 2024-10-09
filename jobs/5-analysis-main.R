#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, MAIN TABLES
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
# SAMPLE MEANS: TABLE 1, FIGURE 1 AND FIGURE 2
#------------------------------------------------------------------------------

# Creating object with means of all covariates and the outcome in each year
# we use. Many subsequent analyses and figures draw on this object. We call 
# a function we generated that takes arguments specifying sample conditions,
# grouping indicators (here years, )

means.year_table <- generate_means_year_table(
  weights = weights, sample_conditions = sample_conditions, data = data, 
  years = c(1981, 1991, 2001, 2011, 2019), outcome = outcome, 
  covariates = c("wages.hrly", "age", "White", "Black", "Hispanic", "Other",
                 "Northeast", "Northcentral", "South", "West",
                 "married", "num.kids.cont",
                 "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg", 
                 "expf", "ftormore","overwork", "emp.tenure",
                 "union", "govt.job", "occ.pct.female", 
                 "occ.managers", "manuf")) %>%
  mutate(year = year-1)

# Creates the values for figure 1 based on the means object
fig1_values <- means.year_table %>%
  dplyr::select(year, female, wages.hrly_mean) %>% 
  pivot_wider(names_from = female, values_from = wages.hrly_mean) %>%
  rename(Women = "1", Men = "0") %>%
  mutate(ratio = (Women/Men) * 100)

write_csv(fig1_values, "tables/fig1values.csv")

# Creates figure 1
fig1 <- fig1_values %>%
  ggplot(aes(x = year, y = ratio)) +
  geom_point() +
  theme_bw() + 
  labs(title = "Figure 1: Changes in Women's Mean Pay Relative to Men's, 1980-2018", 
       y = "Mean Hourly Wage Women / Men", 
       x = "", 
       caption = "Note: Figure shows ratio of women's to men's weighted mean wages for the analytic sample by year. \
       The sample consists of PSID reference persons and spouses/partners aged 30 to 55 who have non-zero \
       individual sample weights and report non-zero wages. It excludes individuals who report being \
       self-employed and individuals employed in agriculture or the military.") +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5, size = 15.5),
        legend.position = "bottom", 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 12, face = "italic", hjust = 0.5)) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  ylim(50, 100)

# Saves figure 1 to the directory
ggsave(plot = fig1, "figures/fig1.jpg", 
       width = 9, height = 8, units = "in", device='jpeg', dpi=700)

# Creates the values for figure 2 from the means object
fig2_values <- means.year_table %>%
  dplyr::select(year, female, num.kids.cont_mean) %>% 
  pivot_wider(names_from = female, values_from = num.kids.cont_mean) %>%
  rename(Women = "1", Men = "0") %>%
  gather(Gender, value, - year)

write_csv(fig2_values, "tables/fig2values.csv")

# Creates figure 2
fig2 <- fig2_values %>%
  ggplot(aes(y = value, x = year, linetype = Gender)) +
  geom_point() +
  geom_line() +
  theme_bw() + 
  labs(y = "Average Number of Children",
       title = "Figure 2: Fertility Decline 1980-2018, by Gender", 
       caption = "Note: Figure shows weighted mean number of children born to date for the analytic sample by year and gender. \
              The sample consists of PSID reference persons and spouses/partners aged 30 to 55 who have non-zero \
       individual sample weights and report non-zero wages. It excludes individuals who report being \
       self-employed and individuals employed in agriculture or the military.") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.key.width=unit(3,"line"),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 12, face = "italic", hjust = 0.5)) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  scale_linetype_manual(values = c(2, 1), name = "Gender")

# Saves figure 2 to the directory
ggsave(plot = fig2, "figures/fig2.jpg", 
       width = 9, height = 8, units = "in", device='jpeg', dpi=700)

# Creating vector that sets the order of variables for tables
order_vars <- c("(Intercept)", "wages.hrly", "num.kids.cont", "age", "agesq",
                "White", "Black", "Hispanic", "Other", 
                "Northeast", "Northcentral", "South", "West", "married",
                "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg",
                "expf", "log.expf", "emp.tenure", 
                "ftormore","overwork",
                "union", "govt.job", "manuf", "occ.pct.female", "occ.managers",
                "n")

var_names <- c(
  "(Intercept)" = "(Intercept)",
  "wages.hrly" = "Hourly Wages",
  "num.kids.cont" = "Number of Children",
  "age" = "Age",
  "agesq" = "Age Squared",
  "White" = "White",
  "Black" = "Black",
  "Hispanic" = "Hispanic",
  "Other" = "Other",
  "Northeast" = "Northeast",
  "Northcentral" = "North Central",
  "South" = "South",
  "West" = "West",
  "married" = "Married",
  "LessthanHS" = "$<$ 12.0",
  "HighSchool" = "12",
  "SomeCollege" = "13-15",
  "ba.avdeg" = "16+",
  "expf" = "Full-Time Experience, Years",
  "log.expf" = "Log of Full-Time Experience, Years",
  "emp.tenure" = "Employment Tenure, Years",
  "ftormore" = "Full Time",
  "overwork" = "Overwork",
  "union" = "Union",
  "govt.job" = "Government Job",
  "manuf" = "Manufacturing",
  "occ.pct.female" = "Percent Female in Occupation",
  "occ.managers" = "Professional/Management Occ.",
  "n" = "Sample Size"
)

# Generating R object with values for T1, Descriptive Stats for Main Sample
t1 <- means.year_table %>% filter(year %in% c(1980, 2018)) %>%
  dplyr::select(-starts_with(c("lnhrlywage", "("))) %>%
  gather(key, value, - c("year", "female")) %>%
  separate(key, c("var", "estimate"), sep = "_") %>%
  pivot_wider(id_cols = c(var, estimate),
              names_from = c(female, year), values_from = value) %>%
  dplyr::select(var, estimate, starts_with("0"), everything()) %>%
  mutate(var = factor(var, levels = order_vars)) %>%
  arrange(var) %>%
  mutate(across(where(is.numeric),
                ~ ifelse(var %in% c(
                  "Northeast", "Northcentral", "South", "West", 
                  "Black", "Hispanic", "Other", "White", "married", 
                  "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg", 
                  "ftormore","overwork","union", "govt.job", 
                  "occ.pct.female", "occ.managers", "manuf"),
                         . * 100, .))) %>%
  mutate_if(is.numeric, ~ sprintf("%.1f", .x)) %>%
  mutate(var = recode(var, !!!var_names))

write_csv(t1, "tables/table1.csv")

# Generating Latex output for Table 1
knitr::kable(t1 %>%
               mutate(
                 across(
                   `0_1980`:`1_2018`, ~ 
                     if_else(estimate == "sd", 
                             paste0("(", ., ")"), as.character(.)))) %>%
               select(-estimate), booktabs = T, format = "latex", 
             caption = "Table 1: Sample Descriptive Statistics, by Year and Gender") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Descriptive statistics show weighted averages for the analytic sample by year and gender (standard errors for continuous variables in parentheses). The sample consists of PSID reference persons and spouses/partners aged 30 to 55 who report non-zero wages. It excludes individuals who report being self-employed and individuals employed in agriculture or the military.", 
           threeparttable = T) %>%
  pack_rows("Background", 5, 5, bold = T) %>%
  pack_rows("Race", 7, 14, bold = F) %>%
  pack_rows("Region", 15, 22, bold = F) %>% 
  pack_rows("Years of Education", 25, 33, bold = T) %>%
  pack_rows("Work Experience and Job Tenure", 33, 33, bold = T) %>%
  pack_rows("Job Characteristics", 37, 37, bold = T) %>%
  pack_rows("Work Hours", 37, 40, bold = F)

#------------------------------------------------------------------------------
# ESTIMATED REGRESSION COEFFICIENTS: TABLE 2
#------------------------------------------------------------------------------

# We then generate an object that contains the regression coefficients for all
# models specified in the main text.
# Table 2 in the main text draws on this object
# We also save

# We then create a table of all coefficients for all models, mapping
# our function generating the regression table across each covariate list
coeftable_all <- mapply(
  generate_regression_table_func, covariates_to_exclude,
  model_labels, SIMPLIFY = FALSE) %>% 
  bind_rows()

t2 <- coeftable_all %>%
  select(year, female, estimate, num.kids.cont, model) %>%
  gather(estimate, value, -c(model, female, year, num.kids.cont)) %>% 
  pivot_wider(names_from = c(female, year), values_from = num.kids.cont) %>%
  select(model, value, starts_with("0"), starts_with("1")) %>% 
  rename("Men, 1980" = "0_1981", "Men, 2018" = "0_2019",
         "Women, 1980" = "1_1981", "Women, 2018" = "1_2019", 
         estimate = value) %>% dplyr::select(-model) %>%
  mutate_if(is.numeric, ~ sprintf("%.3f", .x))
  
write_csv(t2, "tables/table2.csv")
write_csv(coeftable_all, "tables/table_all_coefs.csv")

# Generates LaTex code to produce Table 2: Coefficients for # Kids, All Models
kable(t2 %>% 
        mutate(across(everything(), ~ str_replace(., "NA", ""))),
      booktabs = TRUE, format = "latex",
      caption = "Table 2: Wage Regressions by Year and Gender, Number of Children Coefficients") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Table shows coefficients for number of children from gender-and-year-specific regressions on hourly wages. The difference between men's and women's coefficients is statistically significant at the 5 percent level in 1980 and 2018 across all models. The changes across years in the between-gender gap in coefficients are not statistically significant at the 5 percent  level. The changes across years in gender-specific coefficients are not statistically significant except for the change in men's coefficients in Models 1 and 2.",
           threeparttable = TRUE) %>%
  pack_rows("Model 1: Baseline", 1, 3, bold = TRUE) %>%
  pack_rows("Model 2: + Background", 4, 6, bold = TRUE) %>%
  pack_rows("Model 3: + Education", 7, 9, bold = TRUE) %>%
  pack_rows("Model 4: + Work Experience and Job Tenure", 10, 12, bold = TRUE) %>%
  pack_rows("Model 5: Full", 13, 15, bold = TRUE)

#------------------------------------------------------------------------------
# DECOMPOSITION: TABLE 3, TABLE A1, TABLE A2
#------------------------------------------------------------------------------

# We then create a main decomposition object that maps our decomposition 
# analysis function across each covariate list
main_decomp <- mapply(perform_decomposition_analysis, covariates_to_exclude,
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
  mutate_if(is.numeric, ~.*100)

# Generates LaTex code to produce Table 3: Decomposition % of Change in Pay Gap
# Explained by Declining Fertility, by Gender
t3 <- main_decomp %>%
  mutate_if(is.numeric, ~ sprintf("%.2f", .x)) %>%
  mutate(Variable = recode(Variable, !!!var_names)) %>%
  filter(Variable %in% c("Number of Children", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

rownames(t3) <- NULL

write_csv(t3, "tables/table3.csv")

kable(t3 %>% dplyr::select(Model, Variable, everything()) %>% 
        rename(Total = Total_CharGap, 
               Men = Men_CharGap, 
               Women = Women_CharGap) %>%
        dplyr::select(-c(Model)), booktabs = T, format = "latex", 
      caption = "Table 3: Percent of Gender Pay Convergence 1980-2018 Explained by Fertility Decline, by Gender") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T) %>%
  footnote("Note: See text for details of model specifications. 95 % bootstrapped confidence intervals are shown in parentheses.",
           threeparttable = T)

rownames(main_decomp) <- NULL

# Generating LaTex code to produce Table A1: Decomposition of % Change in Pay 
# Gap Explained by Changing Characteristics, Returns, and Interactions
knitr::kable(main_decomp %>%
               mutate_if(is.numeric, ~ sprintf("%.2f", .x)) %>%
               mutate(Variable = recode(Variable, !!!var_names)) %>%
               filter(Variable %in% c("Number of Children", "Total")) %>%
               dplyr::select(Variable, Total_CharGap, Men_CharGap, Women_CharGap,
                             Total_Returns, Men_Returns, Women_Returns,
                             Total_Interaction, Men_Interaction, 
                             Women_Interaction, -Model),
             booktabs = T, format = "latex", 
             caption = "Table A1: Percent of Gender Pay Convergence 1980-2018 Explained by Changing Characteristics, Returns, and Interactions, by Gender") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: + Full", 9, 10, bold = T) %>% 
  add_header_above(c(
    " ", "Characteristics" = 3, "Returns" = 3, "Interactions" = 3)) %>%
  footnote("Note: See text for details of model specifications. 95 % bootstrapped confidence intervals are shown in parentheses.", 
           threeparttable = T)

write_csv(main_decomp, "tables/table_decomp_full.csv")

# Generating Table A2 from main decomposition results
ta2 <- main_decomp %>%
  mutate(group = case_when(Variable == "num.kids.cont" ~ "Number of Children",
                           Variable %in% age ~ "Age",
                           Variable %in% race ~ "Race", 
                           Variable %in% region ~ "Region", 
                           Variable %in% ed ~ "Education",
                           Variable %in% marstat ~ "Married",
                           Variable %in% wrkhrs ~ "Work Hours", 
                           Variable == "union" ~ "Unionized Job",
                           Variable == "govt.job" ~ "Government Job",
                           Variable == "log.expf" ~ "Full-Time Experience, Years",
                           Variable == "emp.tenure" ~ "Employer Tenure, Years",
                           Variable == "occ.pct.female" ~ "Percent Female in Occupation",
                           Variable == "manuf" ~ "Manufacturing Job",
                           Variable == "occ.managers" ~ "Professional/Management Occ.",
                           Variable == "Total" ~ "Total",
                           Variable == "(Intercept)" ~ "Intercept")) %>%
  group_by(group, Model) %>%
  summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>%
  rename("Total" = "Total_CharGap", "Men" = "Men_CharGap", 
         "Women" = "Women_CharGap") %>%
  gather(key, value, -c(group, Model)) %>%
  pivot_wider(names_from = c(Model, key), values_from = value) %>% 
  mutate(group = factor(group, levels = c("Number of Children", "Age", "Race", "Region", 
                                          "Education", "Married", 
                                          "Full-Time Experience, Years",  
                                          "Employer Tenure, Years", "Work Hours",
                                          "Unionized Job", "Government Job",
                                          "Manufacturing Job",
                                          "Percent Female in Occupation", "Professional/Management Occ.",
                                          "Intercept", "Total"))) %>%
  arrange(group) %>%
  mutate_if(is.numeric, ~ sprintf("%.2f", .x))

write_csv(ta2, "tables/tablea2.csv")

kable(ta2 %>% dplyr::select(group, "Model 1: Baseline_Total", 
                            "Model 2: + Background_Total", "Model 3: + Education_Total", 
                            "Model 4: + Work Experience and Job Tenure_Total", "Model 5: Full_Total"), booktabs = T, format = "latex", 
      caption = "Table A2: Percent of Gender Pay Convergence 1980-2018 Explained by Fertililty Decline, by Gender, Detailed")

kable(ta2 %>% dplyr::select(group, "Model 1: Baseline_Men", 
                            "Model 2: + Background_Men", "Model 3: + Education_Men", 
                            "Model 4: + Work Experience and Job Tenure_Men", "Model 5: Full_Men"), booktabs = T, format = "latex", 
      caption = "Table A2: Percent of Gender Pay Convergence 1980-2018 Explained by Fertililty Decline, by Gender, Detailed")

kable(ta2 %>% dplyr::select(group, "Model 1: Baseline_Women", 
                            "Model 2: + Background_Women", "Model 3: + Education_Women", 
                            "Model 4: + Work Experience and Job Tenure_Women", "Model 5: Full_Women"), booktabs = T, format = "latex", 
      caption = "Table A2: Percent of Gender Pay Convergence 1980-2018 Explained by Fertililty Decline, by Gender, Detailed")

#------------------------------------------------------------------------------
# DECOMPOSITION ACROSS DECADES: FIGURE 3
#------------------------------------------------------------------------------

# Generating the across-decade decomposition object

# First, we calculate the change in the pay gap across the entire period to scale the change
# in log points within each decade by the overall change across the whole period
denom <- (means.year_table$lnhrlywage_mean[means.year_table$year == 2018 & means.year_table$female == 0] -
            means.year_table$lnhrlywage_mean[means.year_table$year == 1980 & means.year_table$female == 0]) -
  (means.year_table$lnhrlywage_mean[means.year_table$year == 2018 & means.year_table$female == 1] -
     means.year_table$lnhrlywage_mean[means.year_table$year == 1980 & means.year_table$female == 1])

# We then define the years for analysis
years <- list(c(1981, 1991), c(1991, 2001), c(2001, 2011), c(2011, 2019))

# We then define the year names for each time frame
year_names <- c("1980-1990", "1990-2000", "2000-2010", "2010-2018")

# Estimating the decomposition for each model and year range,
# using the change in the gap across the entire period as a denominator
decomp_decades <- lapply(seq_along(years), function(i) {
  bind_rows(lapply(seq_along(model_labels), function(j) {
    get_characteristics_component(years[[i]], model_labels[j], covariates_to_exclude[[j]], year_names[i], 
                                  scale = "log_points")
  }))
}) %>% bind_rows() %>% 
  mutate(Characteristics.Gap = Characteristics.Gap/denom) %>%
  mutate_if(is.numeric, ~.*100)

# Creating Figure 3 looking at the across-decade decomposition, using the 
# change in the pay gap across the entire period as a denominator

fig3_values <- decomp_decades %>%
  filter(var == "num.kids.cont")

write_csv(fig3_values, "tables/fig3values.csv")

fig3 <- fig3_values %>% 
  rename(decade = year) %>%
  mutate(model = factor(model, 
                        levels = c("Model 1: Baseline", "Model 2: + Background",
                                   "Model 3: + Education", "Model 4: + Work Experience and Job Tenure", 
                                   "Model 5: Full"))) %>%
  ggplot(aes(x = model, y = Characteristics.Gap, fill = decade)) +
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity") +
  labs(x = "", y = "% Change in Pay Gap Explained", 
       title = "Figure 3: Percent of Gender Pay Convergence 1980-2018 Explained by Fertility Decline in each Decade",
       fill = "",
       caption = "Note: See text for details of model specifications.") +
  theme_bw() +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(hjust = 0.5, size = 15.5),
        axis.text.x = element_text(angle = 12, size = 14, vjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 12, face = "italic", hjust = 0.5),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 15)) +
  scale_fill_manual(values=c("grey70", "grey50", "grey37", "grey15"))

ggsave(plot = fig3, "figures/fig3.jpg",
       width = 10.5, height = 8, units = "in", device='jpeg', dpi=700)