#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: PSID WIDE IMPUTATION
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

# Loading libraries
# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")
psid <- read_csv(paste0("clean_data/psid_clean_", Sys.Date(), ".csv"))

#------------------------------------------------------------------------------
# IMPUTATION
#------------------------------------------------------------------------------

# Creating a vector of variables that will later be converted to factors
factor_vars <- c("rel.head", "race", "region", "afb.cat", 
                 "age.youngest")

# We impute the data by year pairs, using data from the target survey year
# and the  prior survey year (in most cases, the year for which wage 
# information is collected)

# Writing function to process imputation for each interdecade period
process_period <- function(data, years, factor_vars, m) {
  
  # Takes factor vars vector and adds year subscripts for class conversion
  factor.vars <- sapply(factor_vars, function(var) {
    sapply(years, function(year) paste0(var, "_", year))
  }) %>% as.vector()

  # Prepares the period data by selecting appropriate years, 
  # reformatting to wide, then setting variables as factor or numeric based on 
  # factor.vars vector
  period_data <- psid %>%
    filter(year %in% years) %>%
    gather(variable, value, -c(indiv.id, year, female)) %>%
    pivot_wider(id_cols = c(indiv.id, female),
                names_from = c(variable, year), values_from = value) %>%
    mutate(across(c(one_of(factor.vars), indiv.id), as_factor)) %>%
    mutate(across(-one_of(factor.vars), as.numeric))
  
  # Initialize the imputation
  init <- mice(period_data, maxit = 0, method = "cart", seed = 500)
  meth <- init$method
  predM <- init$predictorMatrix
  predM[, c("indiv.id", grep("perwt", names(predM), value = TRUE))] <- 0
  
  # Run imputation
  full_imp <- mice(period_data, 
                   method = meth, 
                   predictorMatrix = predM, 
                   m = m, seed = 500)
  
  # Create long version of the imputed data
  implong <- mice::complete(full_imp, action = "long", include = TRUE) %>%
    gather(var, value, -c(.imp, .id, indiv.id, female)) %>%
    separate(var, c("var", "year"), "_") %>%
    mutate(year = as.numeric(year)) %>%
    spread(var, value)
  
  implong
}

# Create list of periods to iterate through
# This facilitates parallelization- can rull with slurm arrays on FASRC cluster
periods <- list(
  c(1980, 1981),
  c(1990, 1991),
  c(1999, 2001),
  c(2009, 2011),
  c(2017, 2019)
)

# Map imputation function onto all periods and save results
results <- map(periods, ~process_period(psid, .x, factor_vars, m = 20))

# Save each period to an RDS file
walk2(results, 1:length(results), ~saveRDS(
  .x, paste0("clean_data/implong.p", .y, ".RDS")))

# Merge all imputed dataframes
df_imp.cart <- bind_rows(results)

# Save the merged imputation to a CSV file
write_csv(df_imp.cart, "clean_data/intermediate_psid_imp.csv")
rm(df_imp.cart)

#------------------------------------------------------------------------------
# CREATING FINAL CLEAN DATA FILE FROM IMPUTATIONS
#------------------------------------------------------------------------------

# Reading imputed data back in
df_imp.cart <- read_csv("clean_data/intermediate_psid_imp.csv")

# Creating final dataset with auxiliary variables based on imputed variables 
# (factors into dummies, log transformations, etc.) 
psid_imp <- df_imp.cart %>%
  arrange(indiv.id, .imp, year) %>%
  mutate(
    # For race: in the imputation model, race is only ever imputed for one of two years in the same 
    # "period" (so either 2017 or 1980) as the "wide" values for each year are the same. This means that
    # there are some missings in the imputed race data for some respondents in a given year: for one
    # respondent in 1980 and for 61 respondents in 2017. For these individuals, we use their assigned
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
  rename_at(vars(starts_with('ed.factor_')), 
            funs(sub('ed.factor_', "", .))) %>%
  rename_at(vars(starts_with('wrk.hrs.cat_')), 
            funs(sub('wrk.hrs.cat_', "", .))) %>%
  dplyr::select(-"NA") %>%
  rename_at(vars(starts_with('region_')), funs(sub('region_', "", .))) %>%
  dplyr::select(-"NA") %>%
  rename_at(vars(starts_with('race_')), funs(sub('race_', "", .))) %>%
  dplyr::select(-"NA") %>%
  rename_at(vars(starts_with('num.kids.trunc_')),
            funs(sub('num.kids.trunc_', "numkids.", .))) %>%
  dplyr::select(-c("numkids.NA", "afb.cat_NA")) %>%
  # Normalizing weights within year
  mutate(perwt_nonzero = ifelse(perwt == 0, NA, perwt)) %>%
  group_by(year) %>%
  mutate(perwt_norm = ifelse(perwt != 0, perwt/mean(perwt_nonzero, na.rm = T),
                             perwt))

# Creates income top-code by year and joins back to main data
psid_imputed_final <- psid_imp %>%
  filter(samp.inc.final == 1) %>% 
  group_by(year) %>%
  summarise(topcode = wtd.quantile(lnhrlywage, weight = perwt, probs = .99), 
            bottomcode = wtd.quantile(lnhrlywage, weight = perwt, probs = .01)) %>%
  left_join(., psid_imp) %>%
  mutate(lnhrlywage = ifelse(lnhrlywage > topcode, topcode, ifelse(
    lnhrlywage < bottomcode, bottomcode, lnhrlywage))) %>%
  filter(.imp > 0)

# Saving the final data object as a .csv file
write_csv(psid_imputed_final, paste0("clean_data/psid_final_", Sys.Date(), ".csv"))

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
  filter(var != "n")

colnames(missing_rates) <- c("var", unique(psid_imp$year)[2], unique(psid_imp$year)[4])

# Saving table containing the missing data rates by variable
write_csv(missing_rates, "tables/missingrates.csv")
