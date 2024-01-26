#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, MAIN TABLES
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/2023 (mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

# Loading in objects that specify arguments passed to the 
# analysis functions multiple times
source("jobs/3-analysis-arguments.R")

# Creating object with means of all covariates and the outcome in each year we use. Many subsequent analyses 
# and figures draw on this object. We call a function we generated that takes arguments specifying sample conditions,
# grouping indicators (here years, )

means.year_table <- generate_means_year_table(weights = weights, sample_conditions = sample_conditions, data = data, 
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
  labs(title = "Figure 1: Changes in the Gender Pay Gap, 1980-2018", 
       y = "Mean Hourly Wage Women / Men", 
       x = "") +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5, size = 15.5), legend.position = "bottom", 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  geom_hline(yintercept = 100, linetype = "dashed") +
  ylim(50, 100)

# Saves figure 1 to the directory
ggsave(plot = fig1, "figures/fig1.jpg", 
       width = 8, height = 7, units = "in", device='jpeg', dpi=700)

# Creates the values for figure 2 from the means object
fig2_values <- means.year_table %>%
  dplyr::select(year, female, num.kids.cont_mean) %>% 
  pivot_wider(names_from = female, values_from = num.kids.cont_mean) %>%
  rename(Women = "1", Men = "0") %>%
  gather(Gender, value, - year)

write_csv(fig2_values, "tables/fig2values.csv")

# Creates figure 2
fig2 <- fig2_values %>%
  ggplot(aes(y = value, x = year, color = Gender, linetype = Gender)) +
  geom_point() +
  geom_line(size = 1.3) +
  theme_bw() + 
  labs(y = "Average Number of Children",
       title = "Figure 2: Fertility Decline, 1980-2018") +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        legend.position = "bottom",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.key.width=unit(3,"line"),
        legend.title = element_text(size = 14)) +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2"), name="Gender")+
  scale_linetype_manual(values = c(3, 1), name = "Gender")

# Saves figure 2 to the directory
ggsave(plot = fig2, "figures/fig2.jpg", 
       width = 8, height = 7, units = "in", device='jpeg', dpi=700)

# Creating vector that sets the order of variables for tables
order_vars <- c("(Intercept)", "wages.hrly", "num.kids.cont", "age", "agesq",
                "White", "Black", "Hispanic", "Other", 
                "Northeast", "Northcentral", "South", "West", "married",
                "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg",
                "expf", "log.expf", "emp.tenure", 
                "ftormore","overwork",
                "union", "govt.job", "manuf", "occ.pct.female", "occ.managers", 
                "n")

# Generating R object with values for Table 1: Descriptive Stats for Main Analysis sample
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
                ~ ifelse(var %in% c("Northeast", "Northcentral", "South", "West", "Black", "Hispanic", "Other", "White",
                                    "married", "LessthanHS", "HighSchool", "SomeCollege", "ba.avdeg", 
                                    "ftormore","overwork",
                                    "union", "govt.job", "occ.pct.female", "occ.managers", "manuf"),
                         . * 100, .))) %>%
  mutate_if(is.numeric, round, digits = 1)

write_csv(t1, "tables/table1.csv")

# Generating Latex output for Table 1
knitr::kable(t1 %>% select(-estimate), booktabs = T, format = "latex", 
             caption = "Table 1: Sample Descriptive Statistics") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Descriptive statistics show weighted averages for the analytic sample by year and gender. The sample consists of PSID heads and wives aged 30 to 55 who report non-zero wages. It excludes individuals who report being self-employed and individuals employed in agriculture or the military.", 
           threeparttable = T) %>%
  pack_rows("Race", 7, 14, bold = F) %>%
  pack_rows("Region", 15, 22, bold = F) %>% 
  pack_rows("Years of Education", 24, 32, bold = F) %>%
  pack_rows("Work Hours", 37, 40, bold = F)

# We then generate an object that contains the regression coefficients for all models specified
# in the main text. Several tables draw on this object, including the tables that create Table 2
# in the main text and Tables A1 and A2 in the appendix

# We then create a table of all coefficients for all models, using mapply to map
# our function generating the regression table across each covariate list
coeftable_all <- mapply(generate_regression_table_func, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
  bind_rows()

# Generates LaTex code to produce Table A1:  All regression coefficients for all models, Men
kable(coeftable_all %>% select(female, year, estimate, model, everything()) %>%
        filter(female == 0) %>% select(-female) %>%
        mutate_if(is.numeric, round, digits = 3) %>%
        gather(key, value, - c("year", "estimate", "model")) %>%
        pivot_wider(id_cols = c(estimate, key),
                    names_from = c(year, model), values_from = value)  %>%
        dplyr::select(estimate, key, starts_with("1981"), everything()) %>%
        mutate(key = factor(key, levels = order_vars)) %>%
        arrange(key),
      booktabs = T, format = "latex", 
      caption = "Table A1: Men's Wage Regresions by Year, All Coefficients") %>%
  add_header_above(c(" ", "1980" = 5, "2018" = 5)) %>%
  footnote("Table shows coefficients for year-specific regressions on men's hourly wages. Standard errors are shown in parentheses. See text for details of the model specifications.",
           threeparttable = T)

# Generates LaTex code to produce Table A2: All regression coefficients for all models, Women
kable(coeftable_all %>% select(female, year, estimate, model, everything()) %>%
        filter(female == 1) %>% select(-female) %>%
        mutate_if(is.numeric, round, digits = 3) %>%
        gather(key, value, - c("year", "estimate", "model")) %>%
        pivot_wider(id_cols = c(estimate, key),
                    names_from = c(year, model), values_from = value)  %>%
        dplyr::select(estimate, key, starts_with("1981"), everything()) %>%
        mutate(key = factor(key, levels = order_vars)) %>%
        arrange(key),
      booktabs = T, format = "latex", 
      caption = "Table A2: Women's Wage Regresions by Year, All Coefficients") %>%
  add_header_above(c(" ", "1980" = 5, "2018" = 5)) %>%
  footnote("Table shows coefficients for year-specific regressions on women's hourly wages. Standard errors are shown in parentheses. See text for details of the model specifications.",
           threeparttable = T)

t2 <- coeftable_all %>%
  select(year, female, estimate, num.kids.cont, model) %>%
  gather(estimate, value, -c(model, female, year, num.kids.cont)) %>% 
  pivot_wider(names_from = c(female, year), values_from = num.kids.cont) %>%
  select(model, value, starts_with("0"), starts_with("1")) %>% 
  rename("Men, 1980" = "0_1981", "Men, 2018" = "0_2019",
         "Women, 1980" = "1_1981", "Women, 2018" = "1_2019", 
         estimate = value) %>% dplyr::select(-model) %>%
  mutate_if(is.numeric, round, digits = 3)

write_csv(t2, "tables/table2.csv")
write_csv(coeftable_all, "tables/tablea1a2.csv")

# Generates LaTex code to produce Table 2: Regression coefficients for # Kids, All Models
kable(t2, booktabs = T, format = "latex",
      caption = "Table 2: Wage Regressions by Year and Gender, Number of Children Coefficients") %>%
  add_header_above(c(" ", "Men" = 2, "Women" = 2)) %>%
  footnote("Table shows coefficients for number of children from gender-and-year specific regressions on hourly wages. Standard errors are shown in parentheses. See text for details of the model specifications.",
           threeparttable = T) %>%
  pack_rows("Model 1: Baseline", 1, 3, bold = T) %>% 
  pack_rows("Model 2: + Background", 4, 6, bold = T) %>% 
  pack_rows("Model 3: + Education", 7, 9, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 10, 12, bold = T) %>% 
  pack_rows("Model 5: Full", 13, 15, bold = T)

# We then create a main decomposition object that maps our decomposition analysis function across each covariate list
main_decomp <- mapply(perform_decomposition_analysis, covariates_to_exclude, model_labels, SIMPLIFY = FALSE) %>% 
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

# Generates LaTex code to produce Table 3: Decomposition % of Change in Pay Gap Explained by Declining Fertility, by Gender
t3 <- main_decomp %>%
  filter(Variable %in% c("num.kids.cont", "Total")) %>%
  dplyr::select(Model, Variable, ends_with("CharGap"))

write_csv(t3, "tables/table3.csv")

kable(t3 %>% dplyr::select(Model, Variable, everything()) %>% 
        rename(Total = Total_CharGap, 
               Men = Men_CharGap, 
               Women = Women_CharGap) %>%
        dplyr::select(-c(Variable, Model)), booktabs = T, format = "latex", 
      caption = "Table 3: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, by Gender") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: Full", 9, 10, bold = T) %>%
  footnote("Note: see text for details of model specifications", threeparttable = T)

# Generating LaTex code to produce Table A3: Decomposition of % Change in Pay Gap Explained by Changing Characteristics, Returns, and Interactions
knitr::kable(main_decomp %>%
               filter(Variable %in% c("num.kids.cont", "Total")) %>%
               dplyr::select(Total_CharGap, Men_CharGap, Women_CharGap,
                             Total_Returns, Men_Returns, Women_Returns,
                             Total_Interaction, Men_Interaction, 
                             Women_Interaction, -Model),
             booktabs = T, format = "latex", 
             caption = "Table A3: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, Returns, and Interactions, by Gender") %>%
  pack_rows("Model 1: Baseline", 1, 2, bold = T) %>% 
  pack_rows("Model 2: + Background", 3, 4, bold = T) %>% 
  pack_rows("Model 3: + Education", 5, 6, bold = T) %>% 
  pack_rows("Model 4: + Work Experience and Job Tenure", 7, 8, bold = T) %>% 
  pack_rows("Model 5: + Full", 9, 10, bold = T) %>% 
  add_header_above(c(" ", "Characteristics" = 3, "Returns" = 3, "Interactions" = 3)) %>%
  footnote("Note: see text for details of model specifications", threeparttable = T)

write_csv(main_decomp, "tables/tablea3.csv")

# Generating Table A4 from main decomposition results
ta4 <- main_decomp %>%
  mutate(group = case_when(Variable == "num.kids.cont" ~ "Number of Children",
                           Variable %in% age ~ "Age",
                           Variable %in% race ~ "Race", 
                           Variable %in% region ~ "Region", 
                           Variable %in% ed ~ "Education",
                           Variable %in% marstat ~ "Marital Status",
                           Variable %in% wrkhrs ~ "Work Hours (Categorical)", 
                           Variable == "union" ~ "Unionized Job",
                           Variable == "govt.job" ~ "Government Job",
                           Variable == "log.expf" ~ "Years Full-Time Experience",
                           Variable == "emp.tenure" ~ "Employer Tenure",
                           Variable == "occ.pct.female" ~ "Percent Female in Occupation",
                           Variable == "manuf" ~ "Manufacturing",
                           Variable == "occ.managers" ~ "Management/Professional Occ.",
                           Variable == "Total" ~ "Total",
                           Variable == "(Intercept)" ~ "Intercept",)) %>%
  group_by(group, Model) %>%
  summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>%
  rename("Total" = "Total_CharGap", "Men" = "Men_CharGap", 
         "Women" = "Women_CharGap") %>%
  gather(key, value, -c(group, Model)) %>%
  pivot_wider(names_from = c(Model, key), values_from = value) %>% 
  mutate(group = factor(group, levels = c("Number of Children", "Age", "Race", "Region", 
                                          "Education", "Marital Status", 
                                          "Years Full-Time Experience",  
                                          "Employer Tenure", "Work Hours (Categorical)",
                                          "Unionized Job", "Government Job",
                                          "Manufacturing",
                                          "Percent Female in Occupation", "Management/Professional Occ.",
                                          "Intercept", "Total"))) %>%
  arrange(group)

write_csv(ta4, "tables/tablea4.csv")

kable(ta4 %>% dplyr::select(group, "Model 1: Baseline_Total", 
                            "Model 2: + Background_Total", "Model 3: + Education_Total", 
                            "Model 4: + Work Experience and Job Tenure_Total", "Model 5: Full_Total"), booktabs = T, format = "latex", 
      caption = "Table A4: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, by Gender and Model, Detailed")

kable(ta4 %>% dplyr::select(group, "Model 1: Baseline_Men", 
                            "Model 2: + Background_Men", "Model 3: + Education_Men", 
                            "Model 4: + Work Experience and Job Tenure_Men", "Model 5: Full_Men"), booktabs = T, format = "latex", 
      caption = "Table A4: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, by Gender and Model, Detailed")

kable(ta4 %>% dplyr::select(group, "Model 1: Baseline_Women", 
                            "Model 2: + Background_Women", "Model 3: + Education_Women", 
                            "Model 4: + Work Experience and Job Tenure_Women", "Model 5: Full_Women"), booktabs = T, format = "latex", 
      caption = "Table A4: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, by Gender and Model, Detailed")

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

# Outputting the tables
ta5 <- decomp_decades %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  filter(var != "(Intercept)") %>%
  mutate(group = case_when(var == "num.kids.cont" ~ "Number of Children", 
                           var %in% age ~ "Age",
                           var %in% race ~ "Race",
                           var %in% region ~ "Region",
                           var %in% ed ~ "Education", 
                           var %in% marstat ~ "Marital Status",
                           var %in% c("log.expf", "emp.tenure") ~ "Work Experience",
                           var %in% c("ftormore", "overwork") ~ "Work Hours (Categorical)",
                           var %in% c("union", "govt.job", "occ.pct.female", 
                                      "occ.managers", "manuf") ~ "Job Characteristics")) %>%
  group_by(group, model, year) %>% 
  summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>%
  pivot_wider(names_from = c(year), values_from = "Characteristics.Gap") %>%
  mutate(group = factor(group, levels = c("Number of Children", "Age", "Race", "Region", 
                                          "Marital Status", "Education", "Work Experience",
                                          "Work Hours (Categorical)",
                                          "Job Characteristics"))) %>%
  arrange(model, group) %>% ungroup() %>% #group_by(model) %>%
  #summarise_if(is.numeric, funs(sum(., na.rm = TRUE))) %>%
  select(-model)

write_csv(ta5, "tables/tablea5.csv")

kable(ta5, format = "latex", booktabs = T, 
      caption = "Table A5: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Characteristics, By Decade") %>%
  pack_rows("Model 1: Baseline", 1, 1, bold = T) %>% 
  pack_rows("Model 2: + Background", 2, 6, bold = T) %>%
  pack_rows("Model 3: + Education", 7, 12, bold = T) %>%
  pack_rows("Model 4: + Work Experience and Job Tenure", 13, 19, bold = T) %>%
  pack_rows("Model 5: Full", 20, 28, bold = T) %>%
  footnote("Note: see text for details of model specifications.", threeparttable = T)

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
       title = "Figure 3: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Fertility in each Decade",
       fill = "") +
  theme_bw() +
  theme(legend.position = c(.95, .95), legend.justification = c("right", "top"),
        legend.box.just = "right", legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(hjust = 0.5, size = 15.5),
        axis.text.x = element_text(angle = 12, size = 14, vjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 12, face = "italic"),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 15)) +
  scale_fill_manual(values=c("skyblue2", "dodgerblue1", "dodgerblue4", "blue4"))

ggsave(plot = fig3, "figures/fig3.jpg",
       width = 15, height = 8, units = "in", device='jpeg', dpi=700)

# Creating Figure A2, showing fertility decline by subgroup
p1 <- psid_imp %>%
  filter(year %in% c(1981, 1991, 2001, 2011, 2019)) %>%
  group_by(year, female, .imp, married) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(num.kids.cont, perwt) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = perwt))) %>% 
  ungroup() %>%
  dplyr::select(year, female, everything(), -c(.imp, perwt_wmean)) %>% # Ordering the columns
  group_by(year, female, married) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men"),
         group = ifelse(married == 1, "Married", "Unmarried")
  ) %>%
  ggplot(aes(x = year, y = num.kids.cont_wmean, color = group)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Gender,  nrow = 2) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
  xlab("") +
  ylab("") 

p2 <-psid_imp %>%
  filter(year %in% c(1981, 1991, 2001, 2011, 2019)) %>%
  filter(race %in% c("White", "Black")) %>%
  group_by(year, female, .imp, race) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(num.kids.cont, perwt) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = perwt))) %>% 
  ungroup() %>%
  dplyr::select(year, female, everything(), -c(.imp, perwt_wmean)) %>% # Ordering the columns
  group_by(year, female, race) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men"),
         group = factor(race, levels = c("White", "Black")
         )) %>%
  ggplot(aes(x = year, y = num.kids.cont_wmean, color = group)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Gender,  nrow = 2) +
  theme_bw() +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
  theme(legend.position = "bottom",
        panel.spacing = unit(2, "lines")) +
  xlab("") +
  ylab("Average Number of Children") 

p3 <- psid_imp %>%
  filter(year %in% c(1981, 1991, 2001, 2011, 2019)) %>%
  mutate(ed.dummy = ifelse(yrs.ed.fam > 12, "> High School", "High School & Less")) %>%
  group_by(year, female, .imp, ed.dummy) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(num.kids.cont, perwt) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = perwt))) %>% 
  ungroup() %>%
  dplyr::select(year, female, everything(), -c(.imp, perwt_wmean)) %>% # Ordering the columns
  group_by(year, female, ed.dummy) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men"),
         group = ed.dummy
  ) %>%
  ggplot(aes(x = year, y = num.kids.cont_wmean, color = group)) +
  geom_point() +
  geom_line() +
  facet_wrap(~Gender,  nrow = 2) +
  theme_bw() +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
  theme(legend.position = "bottom",
        panel.spacing = unit(2, "lines")) +
  xlab("Year") +
  ylab("") 

figa2 <- grid.arrange(p1, p2, p3, nrow = 1, 
                      top = "Average Number of Children")

ggsave(plot = figa2, "figures/figa2.jpg", 
       width = 10.5, height = 6, units = "in", device='jpeg', dpi=700)

figa1_new <- grid.arrange(figa2, p4, nrow = 2,
                          top = "Figure A1: Subgroup Results, 1980-2018")

ggsave(plot = figa1_new, "figures/figa1_new.jpg", 
       width = 10.5, height = 12, units = "in", device='jpeg', dpi=700)

# New figure: Pay Gap trends by parity
# Creating Figure A2, showing fertility decline by subgroup
parityplot <- psid_imp %>%
  filter(year %in% c(1981, 1991, 2001, 2011, 2019)) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men"), 
         year = year-1) %>%
  group_by(year, Gender, .imp, num.kids.trunc) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select( wages.hrly, perwt) %>%
  summarize_all(list(wmean = ~weighted.mean(., w = perwt))) %>% 
  ungroup() %>%
  dplyr::select(year, Gender, everything(), -c(.imp, perwt_wmean)) %>% # Ordering the columns
  group_by(year, Gender, num.kids.trunc) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) %>%
  pivot_wider(names_from = Gender, values_from = wages.hrly_wmean) %>%
  mutate(ratio = Women/Men) %>%
  ggplot(aes(x = year, y = ratio, color = num.kids.trunc)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 15.5),       
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(values=c("skyblue2", "dodgerblue1", "dodgerblue4", "blue4")) +
  labs(title = "Gender Wage Gap by Parity") +
  xlab("") +
  ylab("")