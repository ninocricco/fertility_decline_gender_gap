#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# ANALYSIS FILE: MAIN DECOMPOSITION ANALYSES
# AUTHOR: NINO CRICCO
# LAST UPDATED: 09/06/2020 (dmy)
# RUNTIME: 1131.871 sec (~19 min)
#**********************************************************

library(tidyverse)
library(survey)
library(mitools)
library(fastDummies)
library(Hmisc)
library(knitr)
library(kableExtra)
library(weights)
library(gridExtra)
library(mice)
library(janitor)
library(ggrepel)
library(tictoc)
library(foreign)
library(systemfit)

source("jobs/helperfunctions.R")
options(survey.lonely.psu = "certainty")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final_2019_12.08.22.csv")

# Creating the analytic sample
psid_analytic <- psid_imp %>%
  filter(.imp > 0) %>% # Removing non-imputed data %>%
  filter(samp.inc.final == 1) # Selecting sample members 

# Creating an object to use in the decompositions using the sample means
means.year <- psid_analytic %>%
  group_by(year, female, .imp) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(lnhrlywage, hrlywage, Northeast, Northcentral, South, West, Black, Hispanic, Other, White,
                married, 
                #housework, 
                num.kids.cont, 
                LessthanHS, HighSchool, SomeCollege, ba.avdeg, 
                union, govt.job, expf, log.expf, overwork, emp.tenure, 
                occ.pct.female, occ.managers.professional, manuf, perwt) %>%
  summarize_all(list(wmean = ~wtd.mean(., w = perwt))) %>% 
  mutate("(Intercept)" = 1) %>% # Creating an intercept column
  filter(.imp != 0) %>% # Filtering out the non-imputed data
  dplyr::select(year, female, "(Intercept)", everything()) %>% # Ordering the columns
  dplyr::select(-c(.imp, perwt_wmean)) %>% # Removing weight and imputation column
  ungroup() %>%
  group_by(year, female) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) 

se.year <- psid_analytic %>%
  filter(year %in% c(1981, 2019)) %>%
  group_by(year, female, .imp) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(hrlywage,  
                num.kids.cont, 
                expf, emp.tenure, 
                occ.pct.female, perwt) %>%
  summarize_all(list(wsd = ~sqrt(wtd.var(., w = perwt)))) %>% 
  mutate("(Intercept)" = 1) %>% # Creating an intercept column
  filter(.imp != 0) %>% # Filtering out the non-imputed data
  dplyr::select(year, female, "(Intercept)", everything()) %>% # Ordering the columns
  dplyr::select(-c(.imp, perwt_wsd)) %>% # Removing weight and imputation column
  ungroup() %>%
  group_by(year, female) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) 

# Setting names to the weighted means table
names(means.year) <- gsub("_wmean", "", names(means.year))

# We use this table of year-and-sex specific means for the decomposition analyses
# Each row is a year X sex combination, columns are year-sex specific means of selected variables
means.year_table <- means.year %>% 
  filter(year %in% c(1981, 1991, 2001, 2011, 2019))

# Next, we run the regressions

cov.years <- c(1981, 2019) # sets the reference years for the decomposition

psid_t1 <- psid_analytic %>%
  filter(year %in% cov.years) # Selecting focal years

# For each yer by sex group, we create list objects where each element contains an imputed dataset
# We then create a survey design object that takes into account the uncertainty in the PSID's sampling design
# For Men in t1
mice.out.imp.mt1 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == cov.years[1], female == 0) %>%
                                     group_split(.imp))

design.mt1 <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                       data = mice.out.imp.mt1, weights = ~perwt, nest = T)

# For Men in t2
mice.out.imp.mt2 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == cov.years[2], female == 0) %>%
                                     group_split(.imp))

design.mt2 <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                       data = mice.out.imp.mt2, weights = ~perwt, nest = T)

# For Women in t1
mice.out.imp.ft1 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == cov.years[1], female == 1) %>%
                                     group_split(.imp))

design.ft1 <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                       data = mice.out.imp.ft1, weights = ~perwt, nest = T)

# For Women in t2
mice.out.imp.ft2 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == cov.years[2], female == 1) %>%
                                     group_split(.imp))

design.ft2 <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                       data = mice.out.imp.ft2, weights = ~perwt, nest = T)

# For Stat Sig
mice.out.imp.t1 <- imputationList(psid_t1 %>% 
                                    filter(.imp > 0) %>%
                                    filter(year == cov.years[1]) %>%
                                    group_split(.imp))

design.t1 <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                      data = mice.out.imp.t1, weights = ~perwt, nest = T)

mice.out.imp.t2 <- imputationList(psid_t1 %>% 
                                    filter(.imp > 0) %>%
                                    filter(year == cov.years[2]) %>%
                                    group_split(.imp))

design.t2 <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                      data = mice.out.imp.t2, weights = ~perwt, nest = T)

fam.t1 <- with(design.t1, svyglm(
  lnhrlywage ~ married*female + num.kids.cont*female
))

fam.t1 <- MIcombine((fam.t1))

full.t1 <- with(design.t1, svyglm(
  lnhrlywage ~ Northeast*female + Northcentral*female +
    South*female + Black*female  + Hispanic*female  + Other*female  +
    married*female + 
    #housework + 
    num.kids.cont*female +
    HighSchool*female  + SomeCollege*female  + ba.avdeg*female  + 
    union*female  + govt.job*female  +
    log.expf*female  + overwork*female  + 
    emp.tenure*female  + 
    occ.pct.female*female  + occ.managers.professional*female  +
    manuf*female))
full.t1 <- MIcombine((full.t1))

fam.t2 <- with(design.t2, svyglm(
  lnhrlywage ~ married*female + num.kids.cont*female
))

fam.t2 <- MIcombine((fam.t2))

full.t2 <- with(design.t2, svyglm(
  lnhrlywage ~ Northeast*female + Northcentral*female +
    South*female  + Black*female  + Hispanic*female  + Other*female  +
    married*female + 
    #housework + 
    num.kids.cont*female +
    HighSchool*female  + SomeCollege*female  + ba.avdeg*female  + 
    union*female  + govt.job*female  +
    log.expf*female  + overwork*female  + 
    emp.tenure*female  + 
    occ.pct.female*female  + occ.managers.professional*female  + manuf*female ))
full.t2 <- MIcombine((full.t2))

MIcombineP <- function(MIcombineRes,digits=3) {
  tStat <- MIcombineRes$coefficients/sqrt(diag(MIcombineRes$variance))
  round(2*pt(-abs(tStat),df=MIcombineRes$df),digits)
}

MIcombineP(fam.t1)

mice.out.imp.fem <- imputationList(psid_t1 %>% 
                                     mutate(year = ifelse(year == 1981, 0, 1)) %>%
                                     filter(.imp > 0) %>%
                                     filter(female ==1) %>%
                                     group_split(.imp))

design.fem <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                       data = mice.out.imp.fem, weights = ~perwt, nest = T)

mice.out.imp.male <- imputationList(psid_t1 %>% 
                                      mutate(year = ifelse(year == 1981, 0, 1)) %>%
                                      filter(.imp > 0) %>%
                                      filter(female == 0) %>%
                                      group_split(.imp))

design.male <-svydesign(ids = ~samp_error_cluster, strata = ~samp_error_stratum, 
                        data = mice.out.imp.male, weights = ~perwt, nest = T)

fam.fem <- with(design.fem, svyglm(
  lnhrlywage ~ married*year + num.kids.cont*year
))

fam.fem <- MIcombine((fam.fem))

full.fem <- with(design.fem, svyglm(
  lnhrlywage ~ Northeast*year + Northcentral*year +
    South*year + Black*year  + Hispanic*year  + Other*year  +
    married*year + 
    num.kids.cont*year +
    HighSchool*year  + SomeCollege*year  + ba.avdeg*year  + 
    union*year  + govt.job*year  +
    log.expf*year  + overwork*year  + 
    emp.tenure*year  + 
    occ.pct.female*year  + occ.managers.professional*year  +
    manuf*year))

full.fem <- MIcombine((full.fem))

fam.male <- with(design.male, svyglm(
  lnhrlywage ~ married*year + num.kids.cont*year
))

fam.male <- MIcombine((fam.male))

full.male <- with(design.male, svyglm(
  lnhrlywage ~ Northeast*year + Northcentral*year +
    South*year  + Black*year  + Hispanic*year  + Other*year  +
    married*year + 
    #housework + 
    num.kids.cont*year +
    HighSchool*year  + SomeCollege*year  + ba.avdeg*year  + 
    union*year  + govt.job*year  +
    log.expf*year  + overwork*year  + 
    emp.tenure*year  + 
    occ.pct.female*year  + occ.managers.professional*year  + manuf*year ))
full.male <- MIcombine((full.male))

MIcombineP(fam.fem)
MIcombineP(full.fem)
MIcombineP(fam.male)
MIcombineP(full.male)
# We then run the regressions corresponding to each of our models on each year by sex group
# Using the survey design object on each imputed dataset, then combine the regression results 
# across imputations 

# Family model for Men in t1
fam.mt1 <- with(design.mt1, svyglm(
  lnhrlywage ~ married + num.kids.cont
))

# Combining the regressioin results across imputations
fam.mt1 <- MIcombine((fam.mt1))
MIcombineP(fam.mt1)

# From the combined results object, create a dataframe that contains 
# the coefficients and standard errors
fam.mt1 <- bind_rows(coef(fam.mt1), vcov::se(fam.mt1)) %>%
  # We add a row to the dataframe that labels the year and sex groups
  mutate(year = cov.years[1], female = 0, estimate = c("coef", "se"))

# Repeating the same procedure for each year by sex group
# Family model for Men in t2
fam.mt2 <- with(design.mt2, svyglm(
  lnhrlywage ~ married+ num.kids.cont
))
fam.mt2 <- MIcombine((fam.mt2))
MIcombineP(fam.mt2)
fam.mt2 <- bind_rows(coef(fam.mt2), vcov::se(fam.mt2)) %>%
  mutate(year = cov.years[2], female = 0, estimate = c("coef", "se"))

# Family model for women in t1
fam.ft1 <- with(design.ft1, svyglm(
  lnhrlywage ~ married+ num.kids.cont
))
fam.ft1 <- MIcombine((fam.ft1))
MIcombineP(fam.ft1)
fam.ft1 <- bind_rows(coef(fam.ft1), vcov::se(fam.ft1)) %>%
  mutate(year = cov.years[1], female = 1, estimate = c("coef", "se"))

# Family model for women in t2
fam.ft2 <- with(design.ft2, svyglm(
  lnhrlywage ~ married + num.kids.cont
))
fam.ft2 <- MIcombine((fam.ft2))
MIcombineP(fam.ft2)
fam.ft2 <- bind_rows(coef(fam.ft2), vcov::se(fam.ft2)) %>%
  mutate(year = cov.years[2], female = 1, estimate = c("coef", "se"))

# We then create a dataframe that combines the year by sex 
# coefficients and standard errors for the family model
fam.coefs <- bind_rows(fam.mt1, fam.ft1, fam.mt2, fam.ft2)

# We repeat the same procedure with our full model
# Full model for men in t1
full.mt1 <- with(design.mt1, svyglm(
  lnhrlywage ~ Northeast + Northcentral + South + Black + Hispanic + Other +
    married+ 
    #housework + 
    num.kids.cont+
    HighSchool + SomeCollege + ba.avdeg + union + govt.job +
    log.expf + overwork + 
    emp.tenure + 
    occ.pct.female + occ.managers.professional + manuf))
full.mt1 <- MIcombine((full.mt1))
MIcombineP(full.mt1)
full.mt1 <- bind_rows(coef(full.mt1), vcov::se(full.mt1)) %>%
  mutate(year = cov.years[1], female = 0, estimate = c("coef", "se"))

# Full model for men in t2
full.mt2 <- with(design.mt2, svyglm(
  lnhrlywage ~ Northeast + Northcentral + South + Black + Hispanic + Other +
    married+
    #housework + 
    num.kids.cont+
    HighSchool + SomeCollege + ba.avdeg + union + govt.job +
    log.expf + overwork +
    emp.tenure + 
    occ.pct.female + occ.managers.professional + manuf))
full.mt2 <- MIcombine((full.mt2))
MIcombineP(full.mt2)
full.mt2 <- bind_rows(coef(full.mt2), vcov::se(full.mt2)) %>%
  mutate(year = cov.years[2], female = 0, estimate = c("coef", "se"))

# Full model for women in t1
full.ft1 <- with(design.ft1, svyglm(
  lnhrlywage ~ Northeast + Northcentral + South + Black + Hispanic + Other +
    married+
    #housework + 
    num.kids.cont+
    HighSchool + SomeCollege + ba.avdeg + union + govt.job +
    log.expf + overwork +
    emp.tenure +
    occ.pct.female + occ.managers.professional + manuf))
full.ft1 <- MIcombine((full.ft1))
MIcombineP(full.ft1)
full.ft1 <- bind_rows(coef(full.ft1), vcov::se(full.ft1)) %>%
  mutate(year = cov.years[1], female = 1, estimate = c("coef", "se"))

# Full model for women in t2
full.ft2 <- with(design.ft2, svyglm(
  lnhrlywage ~ Northeast + Northcentral + South + Black + Hispanic + Other +
    married+
    #housework + 
    num.kids.cont+
    HighSchool + SomeCollege + ba.avdeg + union + govt.job +
    log.expf + overwork +
    emp.tenure + 
    occ.pct.female + occ.managers.professional + manuf))
full.ft2 <- MIcombine((full.ft2))
MIcombineP(full.ft2)
full.ft2 <- bind_rows(coef(full.ft2), vcov::se(full.ft2)) %>%
  mutate(year = cov.years[2], female = 1, estimate = c("coef", "se"))

# Creatinig a dataframe that combines the year by sex coefs and se's for the full model
full.coefs <- bind_rows(full.mt1, full.ft1, full.mt2, full.ft2)

#******************************************************************
# THIS SECTION USES THE MEANS AND REGRESSION COEFFICIENTS GENERATED
# ABOVE FOR OUR DECOMPOSITION ANALYSES OF CHANGES IN THE GAP
#******************************************************************

# We create these vectors that group coefficients, used later for analyses
int <- "(Intercept)"
ed <- c("HighSchool", "SomeCollege", "ba.avdeg")
exp.ft <- "log.expf"
region <- c("Northeast", "Northcentral", "South")
race <- c("Black", "Hispanic", "Other") 
union <- "union"
wrk.hrs <- "overwork"
manuf <- "manuf"
govt.job <- "govt.job"
marital.status <- "married"
fertility <- "num.kids.cont"
tenure <- "emp.tenure"
occ.char <-  "occ.pct.female"

# First we set the reference years. For our main specification:
cov.years <- c(1981, 2019)
# For alternative specifications, comment out line above and instead run one of the following lines
# cov.years <- c(2019, 1981) # When swapping order of reference years
# cov.years <- c(1980, 2017) # When using prior survey wave covariates

# Our first analyses decompose the changes in the gender wage gap across years
# into changes in characteristics, changes in returns, and changes in the interaction
# between these characteristics and returns

# We scale all three components by changes in the gender wage gap:
denom <- (means.year$lnhrlywage[means.year$female == 0 & means.year$year == cov.years[2]] -
            means.year$lnhrlywage[means.year$female == 0 & means.year$year == cov.years[1]]) - 
  (means.year$lnhrlywage[means.year$female == 1 & means.year$year == cov.years[2]] -
     means.year$lnhrlywage[means.year$female == 1 & means.year$year == cov.years[1]])

# For the characteristics component, the numerator is:
# Men's coef, t1 * (Men's char, t2 - Men's char, t1) -  
# Women's coef, t2 * (Women's char, t2 - Women's char, t1) 

# For the returns component, the numerator is:
# Men's char, t1 * (Men's coef, t2 - Men's coef, t1) - 
# Women's char, t1 * (Women's coef, t2 - Women's coef, t1)

# For the interactions component, the numerator is:
# (Men's coef t2 - Men's coef t1) * (Men's char t2 - Men's char t1) - 
# (Women's coef t2 - Women's coef t1) * (Women's char t2 - Women's char t1)

# First, we create objects that compute sex-specific characteristic changes
# and sex-specific characteristic levels at t1
mchargap <- means.year %>% filter(female == 0, year == cov.years[2]) -
  means.year %>% filter(female == 0, year == cov.years[1])

wchargap <- means.year %>% filter(female == 1, year == cov.years[2]) -
  means.year %>% filter(female == 1, year == cov.years[1])

m_chart1 <- means.year %>% filter(female == 0, year == cov.years[1])
w_chart1 <- means.year %>% filter(female == 1, year == cov.years[1])

# Using these characteristics gaps and the regression coefficients for each
# model, we compute the components of the decomposition. For expositional clarity, 
# we create objects that separate out each sub-component that go into the decomposition

#******************************************************************
# FAMILY MODEL
# CHARACTERISTICS CHANGE COMPONENT, FAMILY MODEL
# Men's family coefficients at t1
m_famcoeft1 <- fam.coefs %>% filter(female == 0, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Women's family coefficients at t1
w_famcoeft1 <- fam.coefs %>% filter(female == 1, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Characteristic change among men, family characteristics
mchargap_fam <- mchargap[names(mchargap) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Characteristic change among women, family characteristics
wchargap_fam <- wchargap[names(wchargap) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Numerator for the detailed characteristics component, Family model
char_num_fam <-  m_famcoeft1 * t(mchargap_fam) - (w_famcoeft1 * t(wchargap_fam))
# Scaled by the denominator (change in the gender wage gap):
char_fam <- char_num_fam/denom

# RETURNS COMPONENT, FAMILY MODEL
# Change's in men's regression coefficients from the family model
mcoefgap_fam <- fam.coefs %>% filter(female == 0, estimate == "coef", year == cov.years[2]) %>% 
  dplyr::select(-c(estimate, female, year)) -
  fam.coefs %>% filter(female == 0, estimate == "coef", year == cov.years[1]) %>%
  dplyr::select(-c(estimate, female, year))

# Change's in women's regression coefficients from the family model
wcoefgap_fam <- fam.coefs %>% filter(female == 1, estimate == "coef", year == cov.years[2]) %>% 
  dplyr::select(-c(estimate, female, year)) -
  fam.coefs %>% filter(female == 1,  estimate == "coef", year == cov.years[1]) %>% 
  dplyr::select(-c(estimate, female, year))

# Men's characteristics levels at t1 * change's in men's regression coefficients from the family model
returns_men_fam_num <- m_chart1[names(m_chart1) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix() * mcoefgap_fam

# Women's characteristics levels at t1 * change's in men's regression coefficients from the family model
returns_women_fam_num <- w_chart1[names(w_chart1) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix() * wcoefgap_fam

# Numerator for the detailed returns component, Family model
returns_num_fam <- (returns_men_fam_num-returns_women_fam_num)
# Scaled by the denominator (change in the gender wage gap):
returns_fam <- returns_num_fam/denom

# INTERACTIONS COMPONENT, FAMILY MODEL
interaction_num_fam <- (mcoefgap_fam * mchargap_fam) - (wcoefgap_fam * wchargap_fam)
# Scaled by the denominator (change in the gender wage gap):
interaction_fam <- interaction_num_fam/denom

# Groups the component from the family model into a single object
t3_fam_detailed <- bind_cols(char_fam %>% as.data.frame() %>% rename("Characteristics Gap" = V1),
                             t(returns_fam) %>% as.data.frame() %>% rename("Returns" = V1),
                             t(interaction_fam) %>% as.data.frame() %>% rename("Interaction" = V1))

# Summarizes the detailed results for each variable group by 
# summing across the relevant category
t3_fam_sum <- t3_fam_detailed %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marriage", 
                           rownames(.) %in% fertility ~ "Fertility",
                           TRUE ~ "Intercept")) %>%
  group_by(group) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) %>%
  adorn_totals("row") %>%
  filter(group != "Intercept") %>%
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename("Variables" = group) %>%
  mutate(model = "Family")

#******************************************************************
# FULL MODEL
# CHARACTERISTICS CHANGE COMPONENT, FULL MODEL
# Men's full coefficients at t1
m_fullcoeft1 <- full.coefs %>% filter(female == 0, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Women's full coefficients at t1
w_fullcoeft1 <- full.coefs %>% filter(female == 1, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Characteristic change among men, full characteristics
mchargap_full <- mchargap[names(mchargap) %in% 
                            c(int, region, race, marital.status, fertility,
                              ed, union, govt.job, exp.ft,  wrk.hrs, 
                              tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Characteristic change among women, full characteristics
wchargap_full <- wchargap[names(wchargap) %in% 
                            c(int, region, race, marital.status, fertility,
                              ed, union, govt.job, exp.ft,  wrk.hrs, 
                              tenure,occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Numerator for the detailed characteristics component, Full model
char_num_full <-  m_fullcoeft1 * t(mchargap_full) - (w_fullcoeft1 * t(wchargap_full))
# Scaled by the denominator (change in the gender wage gap):
char_full <- char_num_full/denom

# RETURNS COMPONENT, FULL MODEL
# Change's in men's regression coefficients from the full model
mcoefgap_full <- full.coefs %>% filter(female == 0, estimate == "coef", year == cov.years[2]) %>% 
  dplyr::select(-c(estimate, female, year)) -
  full.coefs %>% filter(female == 0, estimate == "coef", year == cov.years[1]) %>%
  dplyr::select(-c(estimate, female, year))

# Change's in women's regression coefficients from the full model
wcoefgap_full <- full.coefs %>% filter(female == 1, estimate == "coef", year == cov.years[2]) %>% 
  dplyr::select(-c(estimate, female, year)) -
  full.coefs %>% filter(female == 1,  estimate == "coef", year == cov.years[1]) %>% 
  dplyr::select(-c(estimate, female, year))

# Men's characteristics levels at t1 * change's in men's regression coefficients from the full model
returns_men_full_num <- m_chart1[names(m_chart1) %in% 
                                   c(int, region, race, marital.status,  fertility,
                                     ed, union, govt.job, exp.ft,  wrk.hrs, 
                                     tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix() * mcoefgap_full

# Women's characteristics levels at t1 * change's in women's regression coefficients from the full model
returns_women_full_num <- w_chart1[names(w_chart1) %in% 
                                     c(int, region, race, marital.status, fertility,
                                       ed, union, govt.job, exp.ft,  wrk.hrs, 
                                       tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix() * wcoefgap_full

# Numerator for the detailed returns component, Full model
returns_num_full <- (returns_men_full_num-returns_women_full_num)
# Scaled by the denominator (change in the gender wage gap):
returns_full <- returns_num_full/denom

# INTERACTIONS COMPONENT, FULL MODEL
interaction_num_full <- (mcoefgap_full * mchargap_full) - (wcoefgap_full * wchargap_full)
# Scaled by the denominator (change in the gender wage gap):
interaction_full <- interaction_num_full/denom

# Groups the component from the family model into a single object
t3_full_detailed <- bind_cols(char_full %>% as.data.frame() %>% rename("Characteristics Gap" = V1),
                              t(returns_full) %>% as.data.frame() %>% rename("Returns" = V1),
                              t(interaction_full) %>% as.data.frame() %>% rename("Interaction" = V1))

# Summarizes the detailed results for each variable group by 
# summing across the relevant category
t3_full <- t3_full_detailed %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marital Status", 
                           rownames(.) %in% fertility ~ "Fertility",
                           rownames(.) %in% c(race, region) ~ "Demographic",
                           #rownames(.) %in% c(race) ~ "Race", 
                           #rownames(.) %in% c(region) ~ "Region", 
                           rownames(.) %in% ed ~ "Education",
                           rownames(.) %in% int ~ "Intercept",
                           TRUE ~ "Job Traits")) %>%
  group_by(group) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) %>%
  adorn_totals("row") %>%
  filter(group != "Intercept") %>%
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename("Variables" = group)

# Summarizes the detailed results distinguishing role of family variables from other variables
t3_full_sum <- t3_full %>%
  mutate(vargroup = case_when(Variables %in% c("Marital Status", "Fertility", "Housework") ~ "Sum, Family",
                              TRUE ~ "Other")) %>%
  group_by(vargroup) %>% 
  summarise_if(is.numeric, sum) %>%
  filter(vargroup != "Other") %>%
  rename("Variables" = vargroup) %>%
  bind_rows(t3_full, .) %>%
  mutate(model = "Full")

# Combining the summarised results from all components across both models 
t3_allcomponents <- bind_rows(t3_fam_sum,t3_full_sum)

#******************************************************************
# THIS SECTION DECOMPOSES CHANGES OVER TIME AMONG MEN AND WOMEN 
#******************************************************************
# We compute each decomposition component (changing characteristics,
# returns, and interactions) for each group, for each model

#******************************************************************
# FAMILY MODEL
# CHANGING CHARACTERISTICS BY SEX
# Changing coefficients for men, family model
m_famcoeft1 <- fam.coefs %>% filter(female == 0, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Changing family characteristics for men
mchargap_fam <- mchargap[names(mchargap) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Numerator for the detailed characteristics change component among men, Family model
mchar_num_fam <-  m_famcoeft1 * t(mchargap_fam) 
# Scaled by changes in the gender wage gap
char_men_fam <- mchar_num_fam/denom

# Changing coefficientss for women, family model
w_famcoeft1 <- fam.coefs %>% filter(female == 1, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Changing family characteristics for women
wchargap_fam <- wchargap[names(wchargap) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Numerator for the detailed characteristics change component among women, Family model
wchar_num_fam <- w_famcoeft1 * t(wchargap_fam)
# Scaled by changes in the gender wage gap
char_women_fam <- wchar_num_fam/denom

# CHANGING RETURNS BY SEX
# (we already computed these when decomposing the contribution of changes in the different returns in lines above) 
returns_men_fam <- returns_men_fam_num/denom
returns_women_fam <- returns_women_fam_num/denom

# CHANGING INTERACTIONS BY SEX
interaction_men_num_fam <- (mcoefgap_fam * mchargap_fam)
interaction_men_fam <- (mcoefgap_fam * mchargap_fam)/denom

interaction_women_num_fam <- (wcoefgap_fam * wchargap_fam)
interaction_women_fam <- (wcoefgap_fam * wchargap_fam)/denom

# Groups the component from the family model for each sex into a single object
t3_fam_sex_detailed <- bind_cols(char_men_fam %>% as.data.frame() %>% rename("Characteristics Gap Men" = V1),
                                 t(returns_men_fam) %>% as.data.frame() %>% rename("Returns Men" = V1),
                                 t(interaction_men_fam) %>% as.data.frame() %>% rename("Interaction Men" = V1),
                                 char_women_fam %>% as.data.frame() %>% rename("Characteristics Gap Women" = V1),
                                 t(returns_women_fam) %>% as.data.frame() %>% rename("Returns Women" = V1),
                                 t(interaction_women_fam) %>% as.data.frame() %>% rename("Interaction Women" = V1))

# Summarizes the detailed results for each variable group by 
# summing across the relevant category
t3_fam_sex_sum <- t3_fam_sex_detailed %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marriage", 
                           rownames(.) %in% fertility ~ "Fertility",
                           TRUE ~ "Intercept")) %>%
  group_by(group) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) %>%
  adorn_totals("row") %>%
  filter(group != "Intercept") %>%
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename("Variables" = group) %>%
  mutate(model = "Family")

#******************************************************************
# FULL MODEL
# CHANGING CHARACTERISTICS BY SEX
# Coefficients at t1 for men, full model
m_fullcoeft1 <- full.coefs %>% filter(female == 0, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Changing characteristics for men, full characteristics
mchargap_full <- mchargap[names(mchargap) %in% 
                            c(int,region, race, marital.status, fertility,
                              ed, union, govt.job, exp.ft,  wrk.hrs, 
                              tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Numerator for the detailed characteristics change component among men, Full model
mchar_num_full <-  m_fullcoeft1 * t(mchargap_full)
# Scaled by changes in the gender wage gap
char_men_full <- mchar_num_full/denom

# Coefficients at t1 for women, full model
w_fullcoeft1 <- full.coefs %>% filter(female == 1, year == cov.years[1], estimate == "coef") %>%
  dplyr::select(-c(estimate, female, year)) %>%
  t() %>% as.matrix()

# Changing characteristics for women, full characteristics
wchargap_full <- wchargap[names(wchargap) %in% 
                            c(int, region, race, marital.status, fertility,
                              ed, union, govt.job, exp.ft,  wrk.hrs, 
                              tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

# Numerator for the detailed characteristics change component among women, Full model
wchar_num_full <- w_fullcoeft1 * t(wchargap_full)
# Scaled by changes in the gender wage gap
char_women_full <- wchar_num_full/denom

# CHANGING RETURNS BY SEX
# (we already computed these when decomposing the contribution of changes in the different returns in lines above) 
returns_men_full <- returns_men_full_num/denom
returns_women_full <- returns_women_full_num/denom

# CHANGING INTERACTIONS BY SEX
interaction_men_num_full <- (mcoefgap_full * mchargap_full)
interaction_men_full <- (mcoefgap_full * mchargap_full)/denom

interaction_women_num_full <- (wcoefgap_full * wchargap_full)
interaction_women_full <- (wcoefgap_full * wchargap_full)/denom

# Groups the component from the full model for each sex into a single object
t3_full_sex_detailed <- bind_cols(char_men_full %>% as.data.frame() %>% rename("Characteristics Gap Men" = V1),
                                  t(returns_men_full) %>% as.data.frame() %>% rename("Returns Men" = V1),
                                  t(interaction_men_full) %>% as.data.frame() %>% rename("Interaction Men" = V1),
                                  char_women_full %>% as.data.frame() %>% rename("Characteristics Gap Women" = V1),
                                  t(returns_women_full) %>% as.data.frame() %>% rename("Returns Women" = V1),
                                  t(interaction_women_full) %>% as.data.frame() %>% rename("Interaction Women" = V1))

# Summarizes the detailed results for each variable group by 
# summing across the relevant category
t3_full_sex <- t3_full_sex_detailed %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marital Status", 
                           rownames(.) %in% fertility ~ "Fertility",
                           rownames(.) %in% c(race, region) ~ "Demographic", 
                           #rownames(.) %in% c(race) ~ "Race", 
                           #rownames(.) %in% c(region) ~ "Region",
                           rownames(.) %in% ed ~ "Education",
                           rownames(.) %in% int ~ "Intercept",
                           TRUE ~ "Job Traits")) %>%
  group_by(group) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) %>%
  adorn_totals("row") %>%
  filter(group != "Intercept") %>%
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename("Variables" = group)

# Summarizes the detailed results distinguishing role of family variables from other variables
t3_full_sex_sum <- t3_full_sex %>%
  mutate(vargroup = case_when(Variables %in% c("Marital Status", "Fertility") ~ "Sum, Family",
                              TRUE ~ "Other")) %>%
  group_by(vargroup) %>% 
  summarise_if(is.numeric, sum) %>%
  filter(vargroup != "Other") %>%
  rename("Variables" = vargroup) %>%
  bind_rows(t3_full_sex, .) %>%
  mutate(model = "Full")

# Combining the summarised results from all components across both models 
t3_combined_sex <- bind_rows(t3_fam_sex_sum, t3_full_sex_sum)

t3_final <- bind_cols(t3_allcomponents %>% filter(model != "famed") %>%
                        dplyr::select(Variables, "Characteristics Gap") %>%
                        rename("Total" = "Characteristics Gap"), 
                      t3_combined_sex %>% dplyr::select("Characteristics Gap Men", "Characteristics Gap Women") %>%
                        rename("Men" = "Characteristics Gap Men",
                               "Women" = "Characteristics Gap Women")) %>%
  rbind(c("Change in Pay", denom, denom, denom)) %>%
  mutate(Women = round(as.numeric(Women) * -1, digits = 2))

# Figure 3: census talk
test <- t3_final %>% 
  rename("paygap" = "Total") %>%
  mutate(Women = as.numeric(Women), 
         Men = as.numeric(Men),
         paygap = as.numeric(paygap)) %>%
  filter(Variables %in% c("Total", "Fertility", "Marital Status", "Marriage", 
                          "Sum, Family")) %>%
  mutate(model = c(rep("Family", 3), rep("Full", 4)))

plot_family <- test %>%
  filter(model == "Family") %>%
  dplyr::select(-model) %>%
  gather(key, value, -Variables) %>%
  mutate(type = c(rep("by Variable", 2), "% Explained, Family Characteristics", rep("by Gender and Variable", 2),
                  "by Gender", rep("by Gender and Variable", 2), "by Gender"), 
         type = factor(type, levels = c("by Gender and Variable",
                                        "by Variable", 
                                        "by Gender", "% Explained, Family Characteristics")), 
         key = ifelse(key == "paygap", "", key),
         Variables = ifelse(Variables == "Total", "Total, Family Traits", Variables)) %>%
  mutate(value = ifelse(type %in% c("% Explained, Family Characteristics"#,
                                    #"by Gender"#, "by Variable", "by Gender and Variable"
  ), 
  value, NA), 
  key = ifelse(type %in% c("% Explained, Family Characteristics"#,
                           #"by Gender"#, "by Variable"#, "by Gender and Variable"
  ), 
  key, NA)) %>%  
  mutate(label_y = c(20, 4, 15, 25, 4, 20, 16, 0, 7)) %>%
  #mutate(label_y = c(rep(NA, 5), 25, NA, NA, 10)) %>%
  ggplot(aes(x = type, y = value, fill = Variables,  color = key
  )) +
  geom_bar(stat="identity") +
  theme_bw() +
  geom_text(aes(y = label_y, label = key), color = "black", size = 4) +
  coord_flip() +
  scale_fill_manual(values=c("#56B4E9", "#999999",
                             "#D55E00")) +
  scale_color_manual(values=c("grey1", "grey2", "grey3")) +
  labs(x = "", y = "% Change in Pay Gap Explained by Family Characteristics, Family Model", 
       title = "Contribution of Changes in Family Characteristics to the Changing Gender Pay Gap, 1980-2018") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 15, vjust = 0.5)) +
  guides(color = FALSE) 

plot_family

ggsave(plot = plot_family, "output/socday_p1.jpg", 
       width = 13, height = 9, units = "in", device='jpeg', dpi=700)

plot_full <- test %>%
  filter(model == "Full") %>%
  dplyr::select(-model) %>%
  gather(key, value, -Variables) %>%
  mutate(exclude = ifelse(key != "paygap" & Variables == "Total", 1, 0)) %>%
  filter(exclude == 0) %>%
  filter(Variables != "Total") %>%
  mutate(type = c(rep("by Variable", 2), 
                  #"% Explained, All Characteristics",
                  "% Explained, Family Characteristics", rep("by Sex and Variable", 2),
                  rep("by Sex", 1), rep("by Sex and Variable", 2), rep("by Sex", 1)), 
         type = factor(type, levels = c("by Sex and Variable",
                                        "by Variable", 
                                        "by Sex", "% Explained, Family Characteristics",
                                        "% Explained, All Characteristics")), 
         key = ifelse(key == "paygap", "", key),
         Variables = ifelse(Variables == "Sum, Family", "Sum, Family Traits",
                            ifelse(Variables == "Total", "Sum, All Traits", Variables))) %>%
  mutate(label_y = c(0, 0, 0, 12.5, 2.5, 9.5, 7.5, 0, 2.5)) %>%
  mutate(value = ifelse(type %in% c("% Explained, Family Characteristics"#,
                                    #"by Sex", "by Variable", "by Sex and Variable"
                                    ), value, NA), 
         key = ifelse(type %in% c("% Explained, Family Characteristics"#,
                                    #"by Sex", "by Variable", "by Sex and Variable"
         ), key, NA)) %>%
  ggplot(aes(x = type, y = value, fill = Variables,  color = key
  )) +
  geom_bar(stat="identity") +
  theme_bw() +
  geom_text(aes(y = label_y, label = key), color = "black", size = 5) +
  coord_flip() +
  scale_fill_manual(values=c("#56B4E9", "#999999",
                             "#D55E00")) +
  scale_color_manual(values=c("grey1", "grey2", "grey3")) +
  labs(x = "", y = "% Change in Pay Gap Explained by Family Characteristics, Full Model", 
       title = "Contribution of Changes in Family Characteristics to the Changing Gender Pay Gap, 1980-2018") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_text(size = 15, vjust = 0.5)) +
  guides(color = FALSE)

plot_full

ggsave(plot = plot_full, "output/fulldecomp_p1.jpg", 
       width = 13, height = 9, units = "in", device='jpeg', dpi=700)




#********************************************************************
# IN THE FOLLOWING SECTION, WE REPEAT THE SAME ANALYSIS AS WE DID FOR
# THE TWO-SEXES DECOMPOSITION FOR EACH INTERDECADE PERIOD, FOCUSING 
# ONLY ON THE CHANGING CHARACTERISTICS COMPONENT. WE SCALE THE 
# CONTRIBUTION OF EACH COMPONENT BY THE CHANGE IN THE GENDER PAY GAP 
# OVER THE WHOLE PERIOD
#********************************************************************

cov.years <- c(1981, 1991)

mchargap_1991 <- means.year %>% filter(female == 0, year == cov.years[2]) -
  means.year %>% filter(female == 0, year == cov.years[1])

wchargap_1991 <- means.year %>% filter(female == 1, year == cov.years[2]) -
  means.year %>% filter(female == 1, year == cov.years[1])

mchargap_fam_1991 <- mchargap_1991[names(mchargap_1991) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_fam_1991 <- wchargap_1991[names(wchargap_1991) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

char_num_fam_1991 <-  m_famcoeft1 * t(mchargap_fam_1991) - (w_famcoeft1 * t(wchargap_fam_1991))
char_fam_1991 <- char_num_fam_1991/denom

mchargap_full_1991 <- mchargap_1991[names(mchargap_1991) %in% 
                                      c(int, region, race, marital.status,  fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_full_1991 <- wchargap_1991[names(wchargap_1991) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure,occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

char_num_full_1991 <-  m_fullcoeft1 * t(mchargap_full_1991) - (w_fullcoeft1 * t(wchargap_full_1991))
char_full_1991 <- char_num_full_1991/denom

cov.years <- c(1991, 2001)

mchargap_2001 <- means.year %>% filter(female == 0, year == cov.years[2]) -
  means.year %>% filter(female == 0, year == cov.years[1])

wchargap_2001 <- means.year %>% filter(female == 1, year == cov.years[2]) -
  means.year %>% filter(female == 1, year == cov.years[1])

mchargap_fam_2001 <- mchargap_2001[names(mchargap_2001) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_fam_2001 <- wchargap_2001[names(wchargap_2001) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

char_num_fam_2001 <-  m_famcoeft1 * t(mchargap_fam_2001) - (w_famcoeft1 * t(wchargap_fam_2001))
char_fam_2001 <- char_num_fam_2001/denom

mchargap_full_2001 <- mchargap_2001[names(mchargap_2001) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_full_2001 <- wchargap_2001[names(wchargap_2001) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()


char_num_full_2001 <-  m_fullcoeft1 * t(mchargap_full_2001) - (w_fullcoeft1 * t(wchargap_full_2001))
char_full_2001 <- char_num_full_2001/denom

cov.years <- c(2001, 2011)

mchargap_2011 <- means.year %>% filter(female == 0, year == cov.years[2]) -
  means.year %>% filter(female == 0, year == cov.years[1])

wchargap_2011 <- means.year %>% filter(female == 1, year == cov.years[2]) -
  means.year %>% filter(female == 1, year == cov.years[1])

mchargap_fam_2011 <- mchargap_2011[names(mchargap_2011) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_fam_2011 <- wchargap_2011[names(wchargap_2011) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

char_num_fam_2011 <-  m_famcoeft1 * t(mchargap_fam_2011) - (w_famcoeft1 * t(wchargap_fam_2011))
char_fam_2011 <- char_num_fam_2011/denom

mchargap_full_2011 <- mchargap_2011[names(mchargap_2011) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_full_2011 <- wchargap_2011[names(wchargap_2011) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()


char_num_full_2011 <-  m_fullcoeft1 * t(mchargap_full_2011) - (w_fullcoeft1 * t(wchargap_full_2011))
char_full_2011 <- char_num_full_2011/denom

cov.years <- c(2011, 2019)

mchargap_2019 <- means.year %>% filter(female == 0, year == cov.years[2]) -
  means.year %>% filter(female == 0, year == cov.years[1])

wchargap_2019 <- means.year %>% filter(female == 1, year == cov.years[2]) -
  means.year %>% filter(female == 1, year == cov.years[1])

mchargap_fam_2019 <- mchargap_2019[names(mchargap_2019) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_fam_2019 <- wchargap_2019[names(wchargap_2019) %in% c(int, marital.status, fertility)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

char_num_fam_2019 <-  m_famcoeft1 * t(mchargap_fam_2019) - (w_famcoeft1 * t(wchargap_fam_2019))
char_fam_2019 <- char_num_fam_2019/denom

mchargap_full_2019 <- mchargap_2019[names(mchargap_2019) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

wchargap_full_2019 <- wchargap_2019[names(wchargap_2019) %in% 
                                      c(int, region, race, marital.status, fertility,
                                        ed, union, govt.job, exp.ft,  wrk.hrs, 
                                        tenure, occ.char, "occ.managers.professional", manuf)] %>% 
  dplyr::select("(Intercept)", everything()) %>%
  as.matrix()

char_num_full_2019 <-  m_fullcoeft1 * t(mchargap_full_2019) - (w_fullcoeft1 * t(wchargap_full_2019))
char_full_2019 <- char_num_full_2019/denom


# Binds the characteristics change contributions by decade for the family model to one object
char_fam_decades <- bind_cols(as.data.frame(char_fam_1991) %>% rename("1980-1990" = V1), 
                              as.data.frame(char_fam_2001) %>% rename("1990-2000" = V1), 
                              as.data.frame(char_fam_2011) %>% rename("2000-2010" = V1), 
                              as.data.frame(char_fam_2019) %>% rename("2010-2018" = V1))

# Summarises the contributions of each variable group by decade for the family model
char_fam_decades_summary <- char_fam_decades %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marital Status", 
                           rownames(.) %in% fertility ~ "Fertility")) %>%
  group_by(group) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) %>%
  filter(group != "Intercept") %>%
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename("Variables" = group) %>%
  mutate(model = "Family Model")

# Binds the characteristics change contributions by decade for the full model to one object
char_full_decades <- bind_cols(as.data.frame(char_full_1991) %>% rename("1980-1990" = V1), 
                               as.data.frame(char_full_2001) %>% rename("1990-2000" = V1), 
                               as.data.frame(char_full_2011) %>% rename("2000-2010" = V1), 
                               as.data.frame(char_full_2019) %>% rename("2010-2018" = V1))

# Summarises the contributions of each variable group by decade for the full model
char_full_decades_summary <- char_full_decades %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marital Status", 
                           rownames(.) %in% fertility ~ "Fertility",
                           rownames(.) %in% c(race, region) ~ "Demographic", 
                           rownames(.) %in% ed ~ "Education",
                           rownames(.) %in% int ~ "Intercept",
                           TRUE ~ "Job Traits")) %>%
  group_by(group) %>%
  summarise_each(funs(sum(., na.rm = TRUE))) %>%
  filter(group != "Intercept") %>%
  mutate_if(is.numeric, ~.*100) %>%
  mutate_if(is.numeric, round, digits = 2) %>% 
  rename("Variables" = group) %>%
  mutate(model = "Full Model")

#********************************************************************
# FIGURE 3 SHOWS THE CONTRIBUTION OF CHANGING FAMILY CHARACTERISTICS
# BY DECADE FOR EACH MODEL
#********************************************************************

fig3 <- bind_rows(char_fam_decades_summary, 
                  char_full_decades_summary) %>%
  filter(Variables %in% c('Marital Status', "Fertility", "Housework")) %>%
  mutate(Variables = factor(Variables, levels = c('Marital Status', "Fertility", "Housework"))) %>%
  mutate(Variables = ifelse(Variables == "Marital Status", "Marriage",
                            ifelse(Variables == "Fertility","Fertility",
                                   Variables))) %>%
  gather(decade, value, - c("Variables", "model")) %>%
  filter(model == "Family Model") %>%
  ggplot(aes(x = decade, y = value, fill = Variables)) +
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  facet_wrap(~ model) +
  labs(x = "", y = "% Change in Pay Gap Explained", 
       title = "Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Family Demography, By Decade",
       fill = "") +
  theme_bw() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.x = element_text(angle = 12, size = 14, vjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 12, face = "italic"),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 15)) +
  scale_fill_manual(values=c("#56B4E9", 
                             "#999999"))

ggsave(plot = fig3, "output/fig3_socday.jpg", 
       width = 12.5, height = 8, units = "in", device='jpeg', dpi=700)


