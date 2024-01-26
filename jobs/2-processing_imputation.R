#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: PSID WIDE IMPUTATION
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/2022 (dmy)
#**********************************************************

# Loading helper functions
# Loading helper functions
source("jobs/functions.R")
source("jobs/libraries.R")

psid <- read_csv("clean_data/psid_clean.csv") %>%
  # We set wages for both the main timing and the alternate timing
  # to equal wages as measured in 1981, 2017
  mutate(wages = ifelse(year %in% c(1980, 1990, 
                                    1999, 2009, 2017), lead.wage, wages),
         ann.wrk.hrs = ifelse(year %in% c(1980, 1990, 
                                          1999, 2009, 2017), lead.ann.wrk.hrs, ann.wrk.hrs)) %>%
  # Removing "lead" variables used to generate the sample restriction +
  # annual work hours and wages for the alternate variable timings
  dplyr::select(-c(starts_with("lead"), family.id68, person.number68, family.id,
                   military, agriculture))

#**********************************************************
# IMPUTATION
#**********************************************************

# Creating a vector of variables that will later be converted to factors
factor.vars <- c("indiv.id", "samp_error_stratum", "samp_error_cluster", 
                 "rel.head_1980", "rel.head_1981", "rel.head_1990", "rel.head_1991", 
                 "rel.head_1999", "rel.head_2001", "rel.head_2009", "rel.head_2011",
                 "rel.head_2015", "rel.head_2017", "rel.head_2019",
                 "marstat.hd_1980", "marstat.hd_1981", "marstat.hd_1990", "marstat.hd_1991",
                 "marstat.hd_1999", "marstat.hd_2001", "marstat.hd_2009", "marstat.hd_2011",
                 "marstat.hd_2015", "marstat.hd_2017", "marstat.hd_2019", 
                 "race_1980", "race_1981", "race_1990", "race_1991", "race_1999", "race_2001", 
                 "race_2009", "race_2011", "race_2015", "race_2017", "race_2019",
                 "region_1980", "region_1981", "region_1990", "region_1991", "region_1999", 
                 "region_2001", "region_2009", "region_2011","region_2015", "region_2017", "region_2019",
                 "num.kids.trunc_1980", "num.kids.trunc_1981", "num.kids.trunc_1990", "num.kids.trunc_1991", "num.kids.trunc_1999",
                 "num.kids.trunc_2001", "num.kids.trunc_2009", "num.kids.trunc_2011", "num.kids.trunc_2015", "num.kids.trunc_2017", "num.kids.trunc_2019",
                 "afb.cat_1980", "afb.cat_1981", "afb.cat_1990", "afb.cat_1991", "afb.cat_1999",
                 "afb.cat_2001", "afb.cat_2009", "afb.cat_2011", "afb.cat_2015", "afb.cat_2017", "afb.cat_2019", 
                 "age.youngest_1980", "age.youngest_1981", "age.youngest_1990", "age.youngest_1991", "age.youngest_1999",
                 "age.youngest_2001", "age.youngest_2009", "age.youngest_2011", "age.youngest_2015", "age.youngest_2017", "age.youngest_2019", 
                 "marstat_1980", "marstat_1981", "marstat_1990", "marstat_1991", "marstat_1999",
                 "marstat_2001", "marstat_2009", "marstat_2011", "marstat_2015", "marstat_2017", "marstat_2019", 
                 "married_1980", "married_1981", "married_1990", "married_1991", "married_1999",
                 "marstat.cohab_2001", "marstat.cohab_2009", "marstat.cohab_2011", "marstat.cohab_2015", "marstat.cohab_2017", "marstat.cohab_2019",
                 "marstat.cohab_1980", "marstat.cohab_1981", "marstat.cohab_1990", "marstat.cohab_1991", "marstat.cohab_1999",
                 "marstat.cohab_2001", "marstat.cohab_2009", "marstat.cohab_2011", "marstat.cohab_2015", "marstat.cohab_2017", "marstat.cohab_2019",
                 "mar.hhroster_1980", "mar.hhroster_1981", "mar.hhroster_1990", "mar.hhroster_1991", "mar.hhroster_1999",
                 "mar.hhroster_2001", "mar.hhroster_2009", "mar.hhroster_2011", "mar.hhroster_2015", "mar.hhroster_2017", "mar.hhroster_2019",
                 "cohab.hhroster_1980", "cohab.hhroster_1981", "cohab.hhroster_1990", "cohab.hhroster_1991", "cohab.hhroster_1999",
                 "cohab.hhroster_2001", "cohab.hhroster_2009", "cohab.hhroster_2011", "cohab.hhroster_2015", "cohab.hhroster_2017", "cohab.hhroster_2019")

# We impute the data by year pairs, using data from the target survey year and the 
# prior survey year (in most cases, the year for which wage information is collected)

# Transforming the data to wide format by period for imputation
# First doing it for the first period
period1 <- psid %>%
  filter(year %in% c(1980, 1981)) %>% # Selecting two early years
  # Transforming data to long format, except for id, year, and sample restriction vars
  gather(variable, value, - c(indiv.id, year, female)) %>%
  # Turning data to wide format
  pivot_wider(id_cols = c(indiv.id, female),
              names_from = c(variable, year), values_from = value) %>%  
  # Transforming factors to factors, all other vars to numeric
  mutate_at(vars(one_of(factor.vars)), as_factor) %>%
  mutate_at(vars(-one_of(factor.vars)), as.numeric)

# Initializing the imputation
init <- mice(period1, maxit = 0, method = "cart", seed = 500) # Set classification/regression tree as dflt method
meth <- init$method # Sets the method of imputation for each column
predM <- init$predictorMatrix # Establishes the predictor matrix
# Setting id and weight variables in the predictor matrix to zero so that they don't affect the imputation
predM[, c("indiv.id", "perwt_1980", "perwt_1981")]= 0 
predM[, c("occ2010_1980", "occ2010_1981",
          "occ.orig_1980", "occ.orig_1981",
          "ind.orig_1980", "ind.orig_1981",
          "ind1990_1980", "ind1990_1981", 
          "num.kids.trunc_1980", "num.kids.trunc_1981", 
          "dummy.afb.synth_1980", "dummy.afb.synth_1981",
          "dummy.nkids.synth_1980", "dummy.nkids.synth_1981")]= 0 

# Running imputation with the specs outlined above
full.imp.p1 <- mice(period1, method = meth, predictorMatrix=predM, m = 20, seed = 500)

# Creating a long version of the imputed data
implong.p1 <- mice::complete(full.imp.p1, action = "long", include = TRUE) %>%
  # Turning data back to long format
  gather(var, value, -c(.imp, .id, indiv.id, female)) %>%
  separate(var, c("var", "year"), "_19") %>%
  mutate(year = case_when(year == 80 ~ 1980, 
                          year == 81 ~ 1981)) %>%
  spread(var, value)

saveRDS(implong.p1, "clean_data/implong.p1.RDS")

# Transforming the data to wide format by period for imputation
# First doing it for the first period
period2 <- psid %>%
  filter(year %in% c(1990, 1991)) %>% # Selecting two early years
  # Transforming data to long format, except for id, year, and sample restriction vars
  gather(variable, value, - c(indiv.id, year, female)) %>%
  # Turning data to wide format
  pivot_wider(id_cols = c(indiv.id, female),
              names_from = c(variable, year), values_from = value) %>%  
  # Transforming factors to factors, all other vars to numeric
  mutate_at(vars(one_of(factor.vars)), as_factor) %>%
  mutate_at(vars(-one_of(factor.vars)), as.numeric)

# Initializing the imputation
init <- mice(period2, maxit = 0, method = "cart", seed = 500) # Set classiffication/regression tree as dflt method
meth <- init$method # Sets the method of imputation for each column
predM <- init$predictorMatrix # Establishes the predictor matrix
# Setting id and weight variables in the predictor matrix to zero so that they don't affect the imputation
predM[, c("indiv.id", "perwt_1990", "perwt_1991")]= 0 
predM[, c("occ2010_1990", "occ2010_1991",
          "occ.orig_1990", "occ.orig_1991", "ind.orig_1990", "ind.orig_1991", "ind1990_1990", "ind1990_1991", 
          "num.kids.trunc_1990", "num.kids.trunc_1991", "dummy.afb.synth_1990", "dummy.afb.synth_1991",
          "dummy.nkids.synth_1990", "dummy.nkids.synth_1991")]= 0 

# Running imputation with the specs outlined above
full.imp.p2 <- mice(period2, method = meth, predictorMatrix=predM, m = 20, seed = 500)

# Creating a long version of the imputed data
implong.p2 <- mice::complete(full.imp.p2, action = "long", include = TRUE) %>%
  # Turning data back to long format
  gather(var, value, -c(.imp, .id, indiv.id, female)) %>%
  separate(var, c("var", "year"), "_19") %>%
  mutate(year = case_when(year == 90 ~ 1990, 
                          year == 91 ~ 1991)) %>%
  spread(var, value)

saveRDS(implong.p2, "clean_data/implong.p2.RDS")

# Transforming the data to wide format by period for imputation
# First doing it for the first period
period3 <- psid %>%
  filter(year %in% c(1999, 2001)) %>% # Selecting two early years
  # Transforming data to long format, except for id, year, and sample restriction vars
  gather(variable, value, - c(indiv.id, year, female)) %>%
  # Turning data to wide format
  pivot_wider(id_cols = c(indiv.id, female),
              names_from = c(variable, year), values_from = value) %>%  
  # Transforming factors to factors, all other vars to numeric
  mutate_at(vars(one_of(factor.vars)), as_factor) %>%
  mutate_at(vars(-one_of(factor.vars)), as.numeric)

# Initializing the imputation
init <- mice(period3, maxit = 0, method = "cart", seed = 500) # Set classiffication/regression tree as dflt method
meth <- init$method # Sets the method of imputation for each column
predM <- init$predictorMatrix # Establishes the predictor matrix
# Setting id and weight variables in the predictor matrix to zero so that they don't affect the imputation
predM[, c("indiv.id", "perwt_1999", "perwt_2001")]= 0 
predM[, c("occ2010_1999", "occ2010_2001",
          "occ.orig_1999", "occ.orig_2001", "ind.orig_1999", "ind.orig_2001", "ind1990_1999", "ind1990_2001", 
          "num.kids.trunc_1999", "num.kids.trunc_2001", "dummy.afb.synth_1999", "dummy.afb.synth_2001",
          "dummy.nkids.synth_1999", "dummy.nkids.synth_2001")]= 0 


# Running imputation with the specs outlined above
full.imp.p3 <- mice(period3, method = meth, predictorMatrix=predM, m = 20, seed = 500)

# Creating a long version of the imputed data
implong.p3 <- mice::complete(full.imp.p3, action = "long", include = TRUE) %>%
  # Turning data back to long format
  gather(var, value, -c(.imp, .id, indiv.id, female)) %>%
  separate(var, c("var", "year"), "_\\d\\d") %>%
  mutate(year = case_when(year == "99" ~ 1999, 
                          year == "01" ~ 2001)) %>%
  spread(var, value)

saveRDS(implong.p3, "clean_data/implong.p3.RDS")

# Transforming the data to wide format by period for imputation
# First doing it for the first period
period4 <- psid %>%
  filter(year %in% c(2009, 2011)) %>% # Selecting two early years
  # Transforming data to long format, except for id, year, and sample restriction vars
  gather(variable, value, - c(indiv.id, year, female)) %>%
  # Turning data to wide format
  pivot_wider(id_cols = c(indiv.id, female),
              names_from = c(variable, year), values_from = value) %>%  
  # Transforming factors to factors, all other vars to numeric
  mutate_at(vars(one_of(factor.vars)), as_factor) %>%
  mutate_at(vars(-one_of(factor.vars)), as.numeric)

# Initializing the imputation
init <- mice(period4, maxit = 0, method = "cart", seed = 500) # Set classiffication/regression tree as dflt method
meth <- init$method # Sets the method of imputation for each column
predM <- init$predictorMatrix # Establishes the predictor matrix
# Setting id and weight variables in the predictor matrix to zero so that they don't affect the imputation
predM[, c("indiv.id", "perwt_2009", "perwt_2011")]= 0 
predM[, c("occ2010_2009", "occ2010_2011",
          "occ.orig_2009", "occ.orig_2011", "ind.orig_2009", "ind.orig_2011", "ind1990_2009", "ind1990_2011", 
          "num.kids.trunc_2009", "num.kids.trunc_2011", "dummy.afb.synth_2009", "dummy.afb.synth_2011",
          "dummy.nkids.synth_2009", "dummy.nkids.synth_2011")]= 0 

# Running imputation with the specs outlined above
full.imp.p4 <- mice(period4, method = meth, predictorMatrix=predM, m = 20, seed = 500)

# Creating a long version of the imputed data
implong.p4 <- mice::complete(full.imp.p4, action = "long", include = TRUE) %>%
  # Turning data back to long format
  gather(var, value, -c(.imp, .id, indiv.id, female)) %>%
  separate(var, c("var", "year"), "_20") %>%
  mutate(year = case_when(year == "09" ~ 2009, 
                          year == "11" ~ 2011)) %>%
  spread(var, value)

saveRDS(implong.p4, "clean_data/implong.p4.RDS")

# Repeating the same procedure for the second period
period5 <- psid %>%
  filter(year %in% c(2017, 2019)) %>% # Selecting two early years
  # Transforming data to long format, except for id, year, and sample restriction vars
  gather(variable, value, - c(indiv.id, year, female)) %>%
  # Turning data to wide format
  pivot_wider(id_cols = c(indiv.id, female),
              names_from = c(variable, year), values_from = value) %>%  
  # Transforming factors to factors, all other vars to numeric
  mutate_at(vars(one_of(factor.vars)), as_factor) %>%
  mutate_at(vars(-one_of(factor.vars)), as.numeric)

# Initializing the imputation
init <- mice(period5, maxit = 0, method = "cart", seed = 500) # Set classiffication/regression tree as dflt method
meth <- init$method # Sets the method of imputation for each column
predM <- init$predictorMatrix # Estbalishes the predictor matrix
# Setting id and weight variables in the predictor matrix to zero so that they don't affect the imputation
predM[, c("indiv.id", "perwt_2017", "perwt_2019")]= 0 
predM[, c("occ2010_2017", "occ2010_2019",
          "occ.orig_2017", "occ.orig_2019", "ind.orig_2017", "ind.orig_2019", "ind1990_2017", "ind1990_2019", 
          "num.kids.trunc_2017", "num.kids.trunc_2019", "dummy.afb.synth_2017", "dummy.afb.synth_2019",
          "dummy.nkids.synth_2017", "dummy.nkids.synth_2019")]= 0 

# Running imputation with the specs outlined above
full.imp.p5 <- mice(period5, method = meth, predictorMatrix=predM, m = 20, seed = 500)

# Turning to long
implong.p5 <- mice::complete(full.imp.p5, action = "long", include = TRUE) %>%
  # Turning data back to long format
  gather(var, value, -c(.imp, .id, indiv.id, female)) %>%
  separate(var, c("var", "year"), "_20") %>%
  mutate(year = case_when(year == 17 ~ 2017, 
                          year == 19 ~ 2019)) %>%
  spread(var, value)

saveRDS(implong.p5, "clean_data/implong.p5.RDS")

# Merging imputed dataframes
implong.p1 <- readRDS("clean_data/implong.p1.RDS")
implong.p2 <- readRDS("clean_data/implong.p2.RDS")
implong.p3 <- readRDS("clean_data/implong.p3.RDS")
implong.p4 <- readRDS("clean_data/implong.p4.RDS")
implong.p5 <- readRDS("clean_data/implong.p5.RDS")

df_imp.cart <- rbind(implong.p1, implong.p2, implong.p3, implong.p4, implong.p5)

# Saving the imputation to an RDS file
write_csv(df_imp.cart, "clean_data/psid_imputed.csv")
rm(df_imp.cart)

# Reading imputed data back in
df_imp.cart <- read_csv("clean_data/psid_imputed.csv")

# Creating final dataset with auxiliary variables based on imputed variables 
# (factors into dummies, log transformations, etc.) 
psid_imp <- df_imp.cart %>%
  arrange(indiv.id, .imp, year) %>%
  mutate(
    # For race: in the imputation model, race is only ever imputed for one of two years in the same 
    # "period" (so either 2017 or 1980) as the "wide" values for each year are the same. This means that
    # there are some missings in the imputed race data for some respondents in a given year: for one
    # respondent in 1980 and for 61 respondents in 2017. For these individuals, we use their assinged
    # race in the imputed data in the alternate year in the period (so for the 61 missing cases in 2017, 
    # we asssign the imputed race in 2015 in that imputed dataset: for the 1 missing case in 1980, we 
    # assign the imputed race in 1981 in that imputed dataset
    race = case_when(is.na(race) & year == 1980 ~ lead(race), 
                     is.na(race) & year == 1991 ~ lag(race),
                     is.na(race) & year == 2001 ~ lag(race),
                     is.na(race) & year == 2011 ~ lag(race),
                     is.na(race) & year == 2019 ~ lag(race),
                     TRUE ~ race),
    expf = ifelse(expf > expt, expt, expf), 
    expparttime = expt-expf,
    wrk.hrs.cat = case_when(ann.wrk.hrs >= 2500 ~ "overwork",
                            ann.wrk.hrs >= 1500 ~ "ftormore",
                            ann.wrk.hrs > 0 ~ "parttime", 
                            ann.wrk.hrs == 0 ~ "zero"),
    wages.hrly = ifelse(wages == 0 | ann.wrk.hrs == 0,  0,  wages/ann.wrk.hrs), 
    lnhrlywage = ifelse(wages.hrly == 0 | ann.wrk.hrs == 0, NA, log(wages.hrly)),
    agesq = age^2, 
    agelog = log(age), 
    expfsq = expf^2,
    expparttimesq = expparttime^2, 
    log.expf = ifelse(expf == 0, 0, log(expf)), 
    log.expparttime = ifelse(expparttime == 0, 0, log(expparttime)),
    ed.factor = ifelse(yrs.ed.fam < 12, "LessthanHS", ifelse(
      yrs.ed.fam == 12, "HighSchool", ifelse(
        yrs.ed.fam > 12 & yrs.ed.fam < 16, "SomeCollege", "ba.avdeg"))),
    num.kids.trunc = ifelse(num.children.synth == 0, "0", ifelse(
      num.children.synth == 1, "1", ifelse(
        num.children.synth == 2, "2", ifelse(
          num.children.synth >=3, "3plus", NA)))),
    num.kids.cont = ifelse(num.children.synth > 6, 6, num.children.synth),
    # Creating factor variable for age of youngest child 
    age.youngest.cat = ifelse(age.youngest == 0, "nokids", ifelse(
      age.youngest <= 5 , "zerotofive", "sixtoseventeen"))) %>%
  dummy_cols(., select_columns = 
               c("ed.factor", "num.kids.trunc", "afb.cat","region", 
                 "race", "married", "marstat.cohab", "age.youngest.cat", 
                 "wrk.hrs.cat")) %>%
  dplyr::select(-"age.youngest.cat_NA") %>%
  rename_at(vars(starts_with('ed.factor_')), funs(sub('ed.factor_', "", .))) %>%
  rename_at(vars(starts_with('wrk.hrs.cat_')), funs(sub('wrk.hrs.cat_', "", .))) %>%
  dplyr::select(-"NA") %>%
  rename_at(vars(starts_with('region_')), funs(sub('region_', "", .))) %>%
  dplyr::select(-"NA") %>%
  rename_at(vars(starts_with('race_')), funs(sub('race_', "", .))) %>%
  dplyr::select(-"NA") %>%
  rename_at(vars(starts_with('num.kids.trunc_')), funs(sub('num.kids.trunc_', "numkids.", .))) %>%
  dplyr::select(-c("numkids.NA", "afb.cat_NA")) %>%
  # Normalizing weights within year
  mutate(perwt_nonzero = ifelse(perwt == 0, NA, perwt)) %>%
  group_by(year) %>%
  mutate(perwt = ifelse(perwt != 0, perwt/mean(perwt_nonzero, na.rm = T),
                        perwt)) %>%
  filter(perwt > 0)

# Creates income top-code by year and joins back to main data, 
# creates top-coded income measure
psid_noimp <- psid_imp %>%
  filter(samp.inc.final == 1) %>% 
  group_by(year) %>%
  summarise(topcode = wtd.quantile(lnhrlywage, weight = perwt, probs = .99), 
            bottomcode = wtd.quantile(lnhrlywage, weight = perwt, probs = .01)) %>%
  left_join(., psid_imp) %>%
  mutate(lnhrlywage = ifelse(lnhrlywage > topcode, topcode, ifelse(
    lnhrlywage < bottomcode, bottomcode, lnhrlywage))) %>%
  filter(.imp > 0)

# Saving the final data object as a .csv file
write_csv(psid_noimp, "clean_data/psid_final.csv")

missing_rates <- psid_imp %>% 
  filter(.imp == 0, samp.inc.final == 1, year %in% c(1981, 2019)) %>%
  dplyr::select(year, age, region, race, num.kids.cont, married, ed.factor,
                expf, govt.job, union, manuf, occ.managers, occ.pct.female, ann.wrk.hrs, 
                emp.tenure) %>%
  group_by(year) %>%
  # Summing the number of missing values by variable by year
  summarise_all(funs(sum(is.na(.)))) %>% 
  left_join(., psid_imp %>% filter(.imp == 0) %>% group_by(year) %>% summarise(n = n()), 
            by = "year") %>%
  mutate_all(funs((. / n) * 100)) %>%
  gather(var, value, -year) %>%
  pivot_wider(names_from = year, values_from = value) %>%
  filter(var != "n") %>%
  rename(Variable = var, "1981" = "45.6662056247118",
         "2019" = "22.2332342253056")

# Saving table containing the missing data rates by variable
write_csv(missing_rates, "tables/missingrates.csv")
