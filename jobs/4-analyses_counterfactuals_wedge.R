#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# ANALYSIS FILE: COUNTERFACTUAL DECOMPOSITION ANALYSES
# AUTHOR: NINO CRICCO
# LAST UPDATED: 09/06/2020 (dmy)
# RUNTIME: 1131.871 sec (~19 min)
#**********************************************************

# NOTE: THIS CODE FILE DRAWS ON IMPUTS PRODUCED IN THE ANALYSIS_MAIN.R FILE AND SHOULD BE RUN AFTER

#**********************************************************************
# FIGURE 4: COUNTERFACTUAL SCENARIOS FOR CHANGES IN THE GENDER WAGE GAP
#**********************************************************************
# We first "ungroup" the means dataframe (grouped by sex and year)
means.year <- ungroup(means.year)

# These vectors are later used for variable selection
fam_vars <- c("(Intercept)", "married", "num.kids.cont")

full_vars <- c("(Intercept)", "Northeast", "Northcentral", "South", "Black", "Hispanic", "Other",
               "married", "num.kids.cont", "HighSchool", 
               "SomeCollege", "ba.avdeg", "union", "govt.job", "log.expf", "overwork",
               "emp.tenure", "occ.pct.female", "occ.managers.professional", "manuf")

# First, we compute the observed gender wage gap by year
observed_gap1980 <- exp(means.year$lnhrlywage[means.year$female == 1 & means.year$year == 1981])/
  exp(means.year$lnhrlywage[means.year$female == 0 & means.year$year == 1981])
observed_gap1991 <- exp(means.year$lnhrlywage[means.year$female == 1 & means.year$year == 1991]) /
  exp(means.year$lnhrlywage[means.year$female == 0 & means.year$year == 1991])
observed_gap2001 <- exp(means.year$lnhrlywage[means.year$female == 1 & means.year$year == 2001]) /
  exp(means.year$lnhrlywage[means.year$female == 0 & means.year$year == 2001])
observed_gap2011 <- exp(means.year$lnhrlywage[means.year$female == 1 & means.year$year == 2011]) /
  exp(means.year$lnhrlywage[means.year$female == 0 & means.year$year == 2011])
observed_gap2019 <- exp(means.year$lnhrlywage[means.year$female == 1 & means.year$year == 2019]) /
  exp(means.year$lnhrlywage[means.year$female == 0 & means.year$year == 2019])

# Across counterfactual scenarios, we use the sex-specific coefficients for each model for the start of the period
wcoef_fam1980 <- fam.coefs %>% filter(female == 1,  estimate == "coef", year == 1981) %>% 
  dplyr::select(-c(estimate, female, year))

mcoef_fam1980 <- fam.coefs %>% filter(female == 0,  estimate == "coef", year == 1981) %>% 
  dplyr::select(-c(estimate, female, year))

wcoef_full1980 <- full.coefs %>% filter(female == 1,  estimate == "coef", year == 1981) %>% 
  dplyr::select(-c(estimate, female, year))

mcoef_full1980 <- full.coefs %>% filter(female == 0,  estimate == "coef", year == 1981) %>% 
  dplyr::select(-c(estimate, female, year))

# We then compute the predicted gender wage gap in each model by multiplying the
# year-sex-specific means to the appropriate regression coefficients 
# for each model and year-sex combination

# We use these intermediate objects later in the counterfactuaal scenarios
woutcome_fam <- as.matrix(means.year %>% filter(female == 1, year == 1981) %>%
                            dplyr::select(fam_vars)) %*% t(wcoef_fam1980)

moutcome_fam <- as.matrix(means.year %>% filter(female == 0, year == 1981) %>%
                            dplyr::select(fam_vars)) %*% t(mcoef_fam1980)

fam1980 <- exp(woutcome_fam)/exp(moutcome_fam)

full1980 <- exp(as.matrix(means.year %>% filter(female == 1, year == 1981) %>%
                            dplyr::select(full_vars)) %*% t(wcoef_full1980)) /
  exp(as.matrix(means.year %>% filter(female == 0, year == 1981) %>%
                  dplyr::select(full_vars)) %*% t(mcoef_full1980))

fam1991 <- exp((as.matrix(means.year %>% filter(female == 1, year == 1991) %>%
                            dplyr::select(fam_vars)) %*% t(wcoef_fam1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 1991) %>%
                   dplyr::select(fam_vars)) %*% t(mcoef_fam1980)))

full1991 <- exp((as.matrix(means.year %>% filter(female == 1, year == 1991) %>%
                             dplyr::select(full_vars)) %*% t(wcoef_full1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 1991) %>%
                   dplyr::select(full_vars)) %*% t(mcoef_full1980)))

fam2001 <- exp((as.matrix(means.year %>% filter(female == 1, year == 2001) %>%
                            dplyr::select(fam_vars)) %*% t(wcoef_fam1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 2001) %>%
                   dplyr::select(fam_vars)) %*% t(mcoef_fam1980)))

full2001 <- exp((as.matrix(means.year %>% filter(female == 1, year == 2001) %>%
                             dplyr::select(full_vars)) %*% t(wcoef_full1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 2001) %>%
                   dplyr::select(full_vars)) %*% t(mcoef_full1980)))

fam2011 <- exp((as.matrix(means.year %>% filter(female == 1, year == 2011) %>%
                            dplyr::select(fam_vars)) %*% t(wcoef_fam1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 2011) %>%
                   dplyr::select(fam_vars)) %*% t(mcoef_fam1980)))

full2011 <- exp((as.matrix(means.year %>% filter(female == 1, year == 2011) %>%
                             dplyr::select(full_vars)) %*% t(wcoef_full1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 2011) %>%
                   dplyr::select(full_vars)) %*% t(mcoef_full1980)))

fam2019 <- exp((as.matrix(means.year %>% filter(female == 1, year == 2019) %>%
                            dplyr::select(fam_vars)) %*% t(wcoef_fam1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 2019) %>%
                   dplyr::select(fam_vars)) %*% t(mcoef_fam1980)))

full2019 <- exp((as.matrix(means.year %>% filter(female == 1, year == 2019) %>%
                             dplyr::select(full_vars)) %*% t(wcoef_full1980))) /
  exp((as.matrix(means.year %>% filter(female == 0, year == 2019) %>%
                   dplyr::select(full_vars)) %*% t(mcoef_full1980)))

# The following section computes the counterfactual wage gaps for each year
# Across all counterfactual scenarios, we hold the "returns" to men and women's characteristics
# at the level observed in 1980, so we multiply the hypothetical changes in men and women's
# characteristics by the sex-specific regression coefficients in 1980 to get predicted wages
# under different counterfactual scenarios of characteristics change

# First we compute the scenarios highlighting the composition process
# In this scenario, the "levels" of men and women's family characteristics change over time,
# but sex gap in characteristics stays constant at 1980 levels

# When "women" are the reference group, women's family characteristics are anchored to
# changes in men's family characteristics: that is, men's family characteristics change as observed,
# while women's family characteristics are manipulated to change in ways that maintain the
# characteristics gap with men observed in 1980

# Creating the counterfactual means for women under the composition scenario
q_comp_1991_women <-   means.year %>% filter(female == 0, year == 1991) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars))

# Computing the numerator (women's average pay under the composition scenario *
# women's coefficients for the family model in 1980
num_comp_1991_refwomen <- as.matrix(wcoef_fam1980) %*% t(q_comp_1991_women)

# Computing the denominator (men's average pay changes as observed *
# men's coefficients for the family model in 1980
denom_comp_1991_refwomen <- as.matrix(mcoef_fam1980) %*%
  t(means.year %>% filter(female == 0, year == 1991) %>% dplyr::select(fam_vars))

# Computing the counterfactual pay gap
comp_1991_refwomen  <- exp(num_comp_1991_refwomen)/exp(denom_comp_1991_refwomen)

# When "men" are the reference group, men's family characteristics are anchored to
# changes in women's family characteristics: that is, women's family characteristics change as observed,
# while men's family characteristics are manipulated to change in ways that maintain the
# characteristics gap with women observed in 1980

# Creating the counterfactual means for men under the composition scenario
q_comp_1991_men <-   means.year %>% filter(female == 1, year == 1991) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars))

# Computing the numerator (women's average pay changes as observed *
# women's coefficients for the family model in 1980)
num_comp_1991_refmen <- as.matrix(wcoef_fam1980) %*% 
  t(means.year %>% filter(female == 1, year == 1991) %>% dplyr::select(fam_vars))

denom_comp_1991_refmen  <- as.matrix(mcoef_fam1980) %*% t(q_comp_1991_men)

comp_1991_refmen  <- exp(num_comp_1991_refmen)/exp(denom_comp_1991_refmen)

# Composition: 2001
q_comp_2001_women <-   means.year %>% filter(female == 0, year == 2001) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars))

q_comp_2001_men <-   means.year %>% filter(female == 1, year == 2001) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars))

num_comp_2001_refwomen <- as.matrix(wcoef_fam1980) %*% t(q_comp_2001_women)

denom_comp_2001_refwomen  <- as.matrix(mcoef_fam1980) %*%
  t(means.year %>% filter(female == 0, year == 2001) %>% dplyr::select(fam_vars))

comp_2001_refwomen  <- exp(num_comp_2001_refwomen)/exp(denom_comp_2001_refwomen)

num_comp_2001_refmen <- as.matrix(wcoef_fam1980) %*% 
  t(means.year %>% filter(female == 1, year == 2001) %>% dplyr::select(fam_vars))

denom_comp_2001_refmen  <- as.matrix(mcoef_fam1980) %*% t(q_comp_2001_men)

comp_2001_refmen  <- exp(num_comp_2001_refmen)/exp(denom_comp_2001_refmen)

# Composition: 2011
q_comp_2011_women <-   means.year %>% filter(female == 0, year == 2011) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars))

q_comp_2011_men <-   means.year %>% filter(female == 1, year == 2011) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars))

num_comp_2011_refwomen <- as.matrix(wcoef_fam1980) %*% t(q_comp_2011_women)

denom_comp_2011_refwomen  <- as.matrix(mcoef_fam1980) %*%
  t(means.year %>% filter(female == 0, year == 2011) %>% dplyr::select(fam_vars))

comp_2011_refwomen  <- exp(num_comp_2011_refwomen)/exp(denom_comp_2011_refwomen)

num_comp_2011_refmen <- as.matrix(wcoef_fam1980) %*% 
  t(means.year %>% filter(female == 1, year == 2011) %>% dplyr::select(fam_vars))

denom_comp_2011_refmen  <- as.matrix(mcoef_fam1980) %*% t(q_comp_2011_men)

comp_2011_refmen  <- exp(num_comp_2011_refmen)/exp(denom_comp_2011_refmen)

# Composition: 2019
q_comp_2019_women <-   means.year %>% filter(female == 0, year == 2019) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars))

q_comp_2019_men <-   means.year %>% filter(female == 1, year == 2019) %>%
  dplyr::select(fam_vars) +
  (means.year %>% filter(female == 0, year == 1981) %>%
     dplyr::select(fam_vars) -
     means.year %>% filter(female == 1, year == 1981) %>%
     dplyr::select(fam_vars))

num_comp_2019_refwomen <- as.matrix(wcoef_fam1980) %*% t(q_comp_2019_women)

denom_comp_2019_refwomen  <- as.matrix(mcoef_fam1980) %*%
  t(means.year %>% filter(female == 0, year == 2019) %>% dplyr::select(fam_vars))

comp_2019_refwomen  <- exp(num_comp_2019_refwomen)/exp(denom_comp_2019_refwomen)

num_comp_2019_refmen <-as.matrix(wcoef_fam1980) %*%
  t(means.year %>% filter(female == 1, year == 2019) %>% dplyr::select(fam_vars))

denom_comp_2019_refmen  <- as.matrix(mcoef_fam1980) %*% t(q_comp_2019_men)

comp_2019_refmen  <- exp(num_comp_2019_refmen)/exp(denom_comp_2019_refmen)


# We then compute the scenarios highlighting the convergence process
# In this scenario, the sex gap in family characteristics changes over time, but
# remain anchored to the "levels" of men and women's family characteristics at their 1980 levels.
# For these scenarios, the gap between men and women's family characteristics changes as observed,
# but one group remains anchored at their 1980 levels. When women are the reference group, men’s traits
# are anchored at their 1980 levels and women’s traits converge to / diverge from that level as observed.

conv_1991_women <- exp(as.matrix(wcoef_fam1980) %*%  # We multiply women's 1980 family coefficients by 
                         # coounterfactual traits for women, where their traits change as they did relative
                         # to men's over time but remain anchored to men's 1980 levels
                         t((means.year %>% filter(female == 0, year == 1981) %>%
                              dplyr::select(fam_vars) + 
                              (means.year %>% filter(female == 1, year == 1991) %>%
                                 dplyr::select(fam_vars) - 
                                 means.year %>% filter(female == 0, year == 1991) %>%  
                                 dplyr::select(fam_vars)))))/
  # Here, we already computed the predicted outcome for men with traits at 1980 levels and
  # with coefficients from the 1980 family model for men- men's traits are anchored at their 1980 levels
  exp(moutcome_fam) 

# When men are the reference groups, women's traits are anchored at their 1980 levels
# We use the already computed predicted outcome for women with traits at their 1980 levels and
# with coefficients from the 1980 family model for women
conv_1991_men <- exp(woutcome_fam) /
  # We then multiply men's 1980 family coefficients by counterfactual traits for men, 
  # where men's traits change as they did over time relative to women's, but remain anchored at women's 1980 levels
  exp(as.matrix(mcoef_fam1980) %*% t(means.year %>%
                                       filter(female == 1, year == 1981) %>%
                                       dplyr::select(fam_vars) + 
                                       (means.year %>%
                                          filter(female == 0, year == 1991) %>%
                                          dplyr::select(fam_vars) -
                                          means.year %>%
                                          filter(female == 1, year == 1991) %>%
                                          dplyr::select(fam_vars))))

# Repeating the same procedure as above for subsequent years
conv_2001_women <- exp(as.matrix(wcoef_fam1980) %*% t((means.year %>%
                                                         filter(female == 0, year == 1981) %>%
                                                         dplyr::select(fam_vars)+ 
                                                         (means.year %>%
                                                            filter(female == 1, year == 2001) %>%  dplyr::select(fam_vars) - 
                                                            means.year %>%
                                                            filter(female == 0, year == 2001) %>%  dplyr::select(fam_vars)))))/
  exp(moutcome_fam)

conv_2001_men <- exp(woutcome_fam) /
  exp(as.matrix(mcoef_fam1980) %*% t(means.year %>%
                                       filter(female == 1, year == 1981) %>%
                                       dplyr::select(fam_vars) + 
                                       (means.year %>%
                                          filter(female == 0, year == 2001) %>%
                                          dplyr::select(fam_vars) -
                                          means.year %>%
                                          filter(female == 1, year == 2001) %>%
                                          dplyr::select(fam_vars))))

conv_2011_women <- exp(as.matrix(wcoef_fam1980) %*% t((means.year %>%
                                                         filter(female == 0, year == 1981) %>%
                                                         dplyr::select(fam_vars)+ 
                                                         (means.year %>%
                                                            filter(female == 1, year == 2011) %>%  dplyr::select(fam_vars) - 
                                                            means.year %>%
                                                            filter(female == 0, year == 2011) %>%  dplyr::select(fam_vars)))))/
  exp(moutcome_fam)

conv_2011_men <- exp(woutcome_fam) /
  exp(as.matrix(mcoef_fam1980) %*% t(means.year %>%
                                       filter(female == 1, year == 1981) %>%
                                       dplyr::select(fam_vars) + 
                                       (means.year %>%
                                          filter(female == 0, year == 2011) %>%
                                          dplyr::select(fam_vars) -
                                          means.year %>%
                                          filter(female == 1, year == 2011) %>%
                                          dplyr::select(fam_vars))))

conv_2019_women <- exp(as.matrix(wcoef_fam1980) %*% t((means.year %>%
                                                         filter(female == 0, year == 1981) %>%
                                                         dplyr::select(fam_vars)+ 
                                                         (means.year %>%
                                                            filter(female == 1, year == 2019) %>%  dplyr::select(fam_vars) - 
                                                            means.year %>%
                                                            filter(female == 0, year == 2019) %>%  dplyr::select(fam_vars)))))/
  exp(moutcome_fam)

conv_2019_men <- exp(woutcome_fam) /
  exp(as.matrix(mcoef_fam1980) %*% t(means.year %>%
                                       filter(female == 1, year == 1981) %>%
                                       dplyr::select(fam_vars) + 
                                       (means.year %>%
                                          filter(female == 0, year == 2019) %>%
                                          dplyr::select(fam_vars) -
                                          means.year %>%
                                          filter(female == 1, year == 2019) %>%
                                          dplyr::select(fam_vars))))

# For the full models, we use the same procedure for both counterfactual scenarios
# but the level of men and women's characteristics change as observed for 
# non-family characteristics

full_vars_exc <- c("Northeast", "Northcentral", "South", "Black", "Hispanic", "Other",
                   "HighSchool",
                   "SomeCollege", "ba.avdeg", "union", "govt.job", "log.expf", "overwork",
                   "emp.tenure", "occ.pct.female", "occ.managers.professional", "manuf")

# In the composion scenario, we use the already computed counterfactual family traits for women by year
q_comp_1991_women_full <- bind_cols(q_comp_1991_women, 
                                    # and combine them with the observed non-family characteristics
                                    means.year %>% filter(female == 1, year == 1991) %>%
                                      dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars) # this line re-orders the variables in the appropriate order

# Multiplying coefficients from women's full 1980 regression to women's counterfactual 
# characteristics in 1991 (non-family variables change as observed)
num_comp_1991_refwomen_full <- as.matrix(wcoef_full1980) %*%  t(q_comp_1991_women_full)

# Multiplying coefficients from men's full 1980 regression to men's observed 1991 characteristics
denom_comp_1991_refwomen_full  <- as.matrix(mcoef_full1980) %*%
  t(means.year %>% filter(female == 0, year == 1991) %>% dplyr::select(full_vars))

# Computing wage gap under the composition counterfactual scenario with the full model and women as ref group
comp_1991_refwomen_full  <- exp(num_comp_1991_refwomen_full)/exp(denom_comp_1991_refwomen_full)


# Repeating the same procedure as above, but using the already computed counterfactual family traits for men by year
q_comp_1991_men_full <- bind_cols(q_comp_1991_men, 
                                  means.year %>% filter(female == 0, year == 1991) %>%
                                    dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

# When men are the reference group, women's characteristics change as observed
num_comp_1991_refmen_full <- as.matrix(wcoef_full1980) %*% 
  t(means.year %>% 
      filter(female == 1, year == 1991) %>%
      dplyr::select(full_vars))

# Men's famly characteristics change as per the composition counterfactual scenario, 
# non-family characteristics change as observed. Multiplying by men's full coefficients in 1980
denom_comp_1991_refmen_full  <- as.matrix(mcoef_full1980) %*% t(q_comp_1991_men_full)

# Computing wage gap under the composition counterfactual scenario with the full model and men as ref group
comp_1991_refmen_full  <- exp(num_comp_1991_refmen_full)/exp(denom_comp_1991_refmen_full)

# Composition, full, 2001, women as reference group
q_comp_2001_women_full <- bind_cols(q_comp_2001_women, 
                                    means.year %>% filter(female == 1, year == 2001) %>%
                                      dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

num_comp_2001_refwomen_full <- as.matrix(wcoef_full1980) %*% t(q_comp_2001_women_full)

denom_comp_2001_refwomen_full  <- as.matrix(mcoef_full1980) %*%
  t(means.year %>% filter(female == 0, year == 2001) %>% dplyr::select(full_vars))

comp_2001_refwomen_full  <- exp(num_comp_2001_refwomen_full)/exp(denom_comp_2001_refwomen_full)

# Composition, full, 2001, men as reference group
q_comp_2001_men_full <- bind_cols(q_comp_2001_men, 
                                  means.year %>% filter(female == 0, year == 2001) %>%
                                    dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

num_comp_2001_refmen_full <- as.matrix(wcoef_full1980) %*% 
  t(means.year %>% filter(female == 1, year == 2001) %>% dplyr::select(full_vars))

denom_comp_2001_refmen_full  <- as.matrix(mcoef_full1980) %*% t(q_comp_2001_men_full)

comp_2001_refmen_full  <- exp(num_comp_2001_refmen_full)/exp(denom_comp_2001_refmen_full)

# Composition, full, 2011, women as reference group
q_comp_2011_women_full <- bind_cols(q_comp_2011_women, 
                                    means.year %>% filter(female == 1, year == 2011) %>%
                                      dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)


num_comp_2011_refwomen_full <- as.matrix(wcoef_full1980) %*% t(q_comp_2011_women_full)

denom_comp_2011_refwomen_full  <- as.matrix(mcoef_full1980) %*%
  t(means.year %>% filter(female == 0, year == 2011) %>% dplyr::select(full_vars))

comp_2011_refwomen_full  <- exp(num_comp_2011_refwomen_full)/exp(denom_comp_2011_refwomen_full)

# Composition, full, 2011, men as reference group
q_comp_2011_men_full <- bind_cols(q_comp_2011_men, 
                                  means.year %>% filter(female == 0, year == 2011) %>%
                                    dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

num_comp_2011_refmen_full <- as.matrix(wcoef_full1980) %*% 
  t(means.year %>% filter(female == 1, year == 2011) %>% dplyr::select(full_vars))

denom_comp_2011_refmen_full  <- as.matrix(mcoef_full1980) %*% t(q_comp_2011_men_full)

comp_2011_refmen_full  <- exp(num_comp_2011_refmen_full)/exp(denom_comp_2011_refmen_full)

# Composition, full, 2019, women as reference group
q_comp_2019_women_full <- bind_cols(q_comp_2019_women, 
                                    means.year %>% filter(female == 1, year == 2019) %>%
                                      dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

num_comp_2019_refwomen_full <- as.matrix(wcoef_full1980) %*% t(q_comp_2019_women_full)

denom_comp_2019_refwomen_full  <- as.matrix(mcoef_full1980) %*%
  t(means.year %>% filter(female == 0, year == 2019) %>% dplyr::select(full_vars))

comp_2019_refwomen_full  <- exp(num_comp_2019_refwomen_full)/exp(denom_comp_2019_refwomen_full)

# Composition, full, 2019, men as reference group
q_comp_2019_men_full <- bind_cols(q_comp_2019_men, 
                                  means.year %>% filter(female == 0, year == 2019) %>%
                                    dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

num_comp_2019_refmen_full <- as.matrix(wcoef_full1980) %*% 
  t(means.year %>% filter(female == 1, year == 2019) %>% dplyr::select(full_vars))

denom_comp_2019_refmen_full  <- as.matrix(mcoef_full1980) %*% t(q_comp_2019_men_full)

comp_2019_refmen_full  <- exp(num_comp_2019_refmen_full)/exp(denom_comp_2019_refmen_full)

# For the convergence scenario, we once again allow non-family characteristics to change 
# as observed. When women are the reference group, women's characteristics converge with
# men's characteristics as they did, but changes are anchored at men's 1980 levels: 
# thus, when women are the reference group, men's family characteristics also stay at 1981 levels

# We first create the counterfactual characteristics for women
q_conv_1991_women_full <- (means.year %>%
                             filter(female == 0, year == 1981) %>%
                             dplyr::select(fam_vars)+ 
                             (means.year %>%
                                filter(female == 1, year == 1991) %>%  dplyr::select(fam_vars) - 
                                means.year %>%
                                filter(female == 0, year == 1991) %>%  dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 1, year == 1991) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

# Then, we compute the counterfactual wage gap under this scenario
conv_1991_women_full <- exp(as.matrix(wcoef_full1980) %*% t(q_conv_1991_women_full))/
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 1991) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

# Repeating the same process as above, first creating counterfactual characteristics for men 
# (family traits converge with women's traits as observed, but are anchored at women's 1980 levels)
q_conv_1991_men_full <- (means.year %>%
                           filter(female == 1, year == 1981) %>%
                           dplyr::select(fam_vars) + 
                           (means.year %>%
                              filter(female == 0, year == 1991) %>%
                              dplyr::select(fam_vars) -
                              means.year %>%
                              filter(female == 1, year == 1991) %>%
                              dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 0, year == 1991) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

# Then, we compute the counterfactual wage gap under this scenario
conv_1991_men_full <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                      filter(female == 1, year == 1981) %>%
                                                                      dplyr::select(fam_vars), 
                                                                    means.year %>%filter(female == 1, year == 1991) %>%
                                                                      dplyr::select(full_vars_exc)) %>%
                                                            dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(q_conv_1991_men_full))

# Repeating the process for subsequent years
q_conv_2001_women_full <- (means.year %>%
                             filter(female == 0, year == 1981) %>%
                             dplyr::select(fam_vars)+ 
                             (means.year %>%
                                filter(female == 1, year == 2001) %>%  dplyr::select(fam_vars) - 
                                means.year %>%
                                filter(female == 0, year == 2001) %>%  dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 1, year == 2001) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

conv_2001_women_full <- exp(as.matrix(wcoef_full1980) %*% t(q_conv_2001_women_full))/
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 2001) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

q_conv_2001_men_full <- (means.year %>%
                           filter(female == 1, year == 1981) %>%
                           dplyr::select(fam_vars) + 
                           (means.year %>%
                              filter(female == 0, year == 2001) %>%
                              dplyr::select(fam_vars) -
                              means.year %>%
                              filter(female == 1, year == 2001) %>%
                              dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 0, year == 2001) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

conv_2001_men_full <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                      filter(female == 1, year == 1981) %>%
                                                                      dplyr::select(fam_vars), 
                                                                    means.year %>%filter(female == 1, year == 2001) %>%
                                                                      dplyr::select(full_vars_exc)) %>%
                                                            dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(q_conv_2001_men_full))

q_conv_2011_women_full <- (means.year %>%
                             filter(female == 0, year == 1981) %>%
                             dplyr::select(fam_vars)+ 
                             (means.year %>%
                                filter(female == 1, year == 2011) %>%  dplyr::select(fam_vars) - 
                                means.year %>%
                                filter(female == 0, year == 2011) %>%  dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 1, year == 2011) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

conv_2011_women_full <- exp(as.matrix(wcoef_full1980) %*% t(q_conv_2011_women_full))/
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 2011) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

q_conv_2011_men_full <- (means.year %>%
                           filter(female == 1, year == 1981) %>%
                           dplyr::select(fam_vars) + 
                           (means.year %>%
                              filter(female == 0, year == 2011) %>%
                              dplyr::select(fam_vars) -
                              means.year %>%
                              filter(female == 1, year == 2011) %>%
                              dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 0, year == 2011) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

conv_2011_men_full <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                      filter(female == 1, year == 1981) %>%
                                                                      dplyr::select(fam_vars), 
                                                                    means.year %>%filter(female == 1, year == 2011) %>%
                                                                      dplyr::select(full_vars_exc)) %>%
                                                            dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(q_conv_2011_men_full))

q_conv_2019_women_full <- (means.year %>%
                             filter(female == 0, year == 1981) %>%
                             dplyr::select(fam_vars)+ 
                             (means.year %>%
                                filter(female == 1, year == 2019) %>%  dplyr::select(fam_vars) - 
                                means.year %>%
                                filter(female == 0, year == 2019) %>%  dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 1, year == 2019) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

conv_2019_women_full <- exp(as.matrix(wcoef_full1980) %*% t(q_conv_2019_women_full))/
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 2019) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

q_conv_2019_men_full <- (means.year %>%
                           filter(female == 1, year == 1981) %>%
                           dplyr::select(fam_vars) + 
                           (means.year %>%
                              filter(female == 0, year == 2019) %>%
                              dplyr::select(fam_vars) -
                              means.year %>%
                              filter(female == 1, year == 2019) %>%
                              dplyr::select(fam_vars))) %>%
  bind_cols(., means.year %>% filter(female == 0, year == 2019) %>%
              dplyr::select(full_vars_exc)) %>%
  dplyr::select(full_vars)

conv_2019_men_full <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                      filter(female == 1, year == 1981) %>%
                                                                      dplyr::select(fam_vars), 
                                                                    means.year %>%filter(female == 1, year == 2019) %>%
                                                                      dplyr::select(full_vars_exc)) %>%
                                                            dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(q_conv_2019_men_full))

# We create an additional counterfactual scenario for comparison to the full model, where
# family traits stay at their 1980 levels for both groups but non-family traits change as observed
nofamchange_1991 <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                    filter(female == 1, year == 1981) %>%
                                                                    dplyr::select(fam_vars), 
                                                                  means.year %>%filter(female == 1, year == 1991) %>%
                                                                    dplyr::select(full_vars_exc)) %>%
                                                          dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 1991) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

nofamchange_2001 <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                    filter(female == 1, year == 1981) %>%
                                                                    dplyr::select(fam_vars), 
                                                                  means.year %>%filter(female == 1, year == 2001) %>%
                                                                    dplyr::select(full_vars_exc)) %>%
                                                          dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 2001) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

nofamchange_2011 <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                    filter(female == 1, year == 1981) %>%
                                                                    dplyr::select(fam_vars), 
                                                                  means.year %>%filter(female == 1, year == 2011) %>%
                                                                    dplyr::select(full_vars_exc)) %>%
                                                          dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 2011) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

nofamchange_2019 <- exp(as.matrix(wcoef_full1980) %*% t(bind_cols(means.year %>%
                                                                    filter(female == 1, year == 1981) %>%
                                                                    dplyr::select(fam_vars), 
                                                                  means.year %>%filter(female == 1, year == 2019) %>%
                                                                    dplyr::select(full_vars_exc)) %>%
                                                          dplyr::select(full_vars))) /
  exp(as.matrix(mcoef_full1980) %*% t(bind_cols(means.year %>%
                                                  filter(female == 0, year == 1981) %>%
                                                  dplyr::select(fam_vars), 
                                                means.year %>%filter(female == 0, year == 2019) %>%
                                                  dplyr::select(full_vars_exc)) %>%
                                        dplyr::select(full_vars)))

# Combining the observed wage gap into one vector
observed <- data.frame(year = c(1980, 1990, 2000, 2010, 2019),
                       Observed = c(observed_gap1980, observed_gap1991, observed_gap2001,
                                    observed_gap2011, observed_gap2019))

# Combining the wage gap predicted under each model into one vector
predicted <- data.frame(year = c(1980, 1990, 2000, 2010, 2019),
                        Family = c(fam1980[1], fam1991[1], fam2001[1], fam2011[1], fam2019[1]),  
                        Full = c(full1980[1], full1991[1], full2001[1], full2011[1], full2019[1]))

# Combining the wage gap predicted under the scenario of no family change into one vector
nofamchange <- data.frame(year = c(1980, 1990, 2000, 2010, 2019), 
                          Family = rep(NA, 5),
                          Full = c(full1980[1], nofamchange_1991[1], nofamchange_2001[1], nofamchange_2011[1], nofamchange_2019[1]))

# We then combine these elements into a single dataframe 
fig4_data <- data.frame(year = c(1980, 1990, 2000, 2010, 2019),
                        "Convergence_Women.Family" = c(
                          fam1980[1], conv_1991_women[1], conv_2001_women[1], 
                          conv_2011_women[1], conv_2019_women[1]), 
                        "Convergence_Men.Family" = c(
                          fam1980[1], conv_1991_men[1], conv_2001_men[1], 
                          conv_2011_men[1], conv_2019_men[1]),
                        "Convergence_Women.Full" = c(
                          full1980[1], conv_1991_women_full[1], conv_2001_women_full[1], 
                          conv_2011_women_full[1], conv_2019_women_full[1]),
                        "Convergence_Men.Full" = c(
                          full1980[1], conv_1991_men_full[1], conv_2001_men_full[1], 
                          conv_2011_men_full[1], conv_2019_men_full[1]), 
                        "Composition_Women.Family" = c(
                          fam1980[1], comp_1991_refwomen[1], comp_2001_refwomen[1], 
                          comp_2011_refwomen[1], comp_2019_refwomen[1]), 
                        "Composition_Men.Family" = c(
                          fam1980[1], comp_1991_refmen[1], comp_2001_refmen[1], 
                          comp_2011_refmen[1], comp_2019_refmen[1]), 
                        "Composition_Women.Full" = c(
                          fam1980[1], comp_1991_refwomen_full[1], comp_2001_refwomen_full[1], 
                          comp_2011_refwomen_full[1], comp_2019_refwomen_full[1]), 
                        "Composition_Men.Full" = c(
                          fam1980[1], comp_1991_refmen_full[1], comp_2001_refmen_full[1], 
                          comp_2011_refmen_full[1], comp_2019_refmen_full[1])) %>%
  gather(model, value, -year) %>%
  mutate(pathway = ifelse(grepl("Convergence", model), "Convergence", "Composition"), 
         reference = ifelse(grepl("Women", model), "Women", "Men"), 
         model = ifelse(grepl("Family", model), "Family", "Full")) %>%
  left_join(., predicted %>% gather(model, predicted, - c(year))) %>%
  left_join(., nofamchange %>% gather(model, nofamchange, - c(year))) %>%  
  left_join(., observed) %>%
  gather(linetype, value, -c(year, model, pathway, reference)) %>%
  mutate(linetype = case_when(linetype == "Observed" ~ "Observed", 
                              linetype == "predicted" ~ "Covariate Change", 
                              linetype == "value" ~ "Hypothetical",
                              linetype == "nofamchange" ~ "No Family Change"), 
         linetype = ifelse(linetype == "Hypothetical", paste(linetype, pathway, sep = ", "), linetype))

fig4_comp <- fig4_data %>%
  filter(pathway != "Convergence") %>%
  mutate(linetype = ifelse(reference == "Women" & 
                             linetype == "Hypothetical, Composition",
                           "Composition, Women's Traits Change as Observed", ifelse(
                             reference == "Men" &
                               linetype == "Hypothetical, Composition",
                             "Composition, Men's Traits Change as Observed", linetype))) %>%
  dplyr::select(-c(reference, pathway)) %>%
  distinct() %>%
  ggplot(aes(y = value, x = year, linetype = linetype, color = linetype, shape = linetype)) +
  geom_line() +
  facet_wrap(~model) +
  theme_bw() + 
  geom_point() + 
  scale_linetype_manual(values=c(5, 4, 3, 2, 1)) +
  scale_color_manual(values = c("grey31", "snow4", "grey55", "grey69", "grey1")) +
  scale_shape_manual(values=c(15, 17, 3, 8, 19)) +
  labs(title = "Counterfactual Trajectories for the Gender Pay Gap", 
       y = "Gender Pay Gap", 
       x = "") +
  geom_point() +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  ylim(.5, 1)

ggsave(plot = fig4_comp, "output/fig4.jpg", 
       width = 12, height = 10, units = "in", device='jpeg', dpi=700)

# Outputting figure values
View(fig4_data %>%
       pivot_wider(names_from = c(linetype, pathway), values_from = value) %>%
       dplyr::select(-c("Observed_Composition", 
                        "Hypothetical, Convergence_Convergence",
                        "Covariate Change_Composition", 
                        "No Family Change_Composition")) %>%
       rename("Observed" = "Observed_Convergence", 
              "Covariate Change" = "Covariate Change_Convergence",
              "No Family Change" = "No Family Change_Convergence"))

# We also create an appendix Table to show the counterfactual means in each scenario
app_table_q <- bind_rows(
  q_conv_1991_men_full %>% mutate(year = 1991, ref = "men", q = "convergence"),
  q_conv_2001_men_full %>% mutate(year = 2001, ref = "men", q = "convergence"),
  q_conv_2011_men_full %>% mutate(year = 2011, ref = "men", q = "convergence"),
  q_conv_2019_men_full %>% mutate(year = 2019, ref = "men", q = "convergence"), 
  q_conv_1991_women_full %>% mutate(year = 1991, ref = "women", q = "convergence"),
  q_conv_2001_women_full %>% mutate(year = 2001, ref = "women", q = "convergence"),
  q_conv_2011_women_full %>% mutate(year = 2011, ref = "women", q = "convergence"),
  q_conv_2019_women_full %>% mutate(year = 2019, ref = "women", q = "convergence"), 
  q_comp_1991_men_full %>% mutate(year = 1991, ref = "men", q = "composition"),
  q_comp_2001_men_full %>% mutate(year = 2001, ref = "men", q = "composition"),
  q_comp_2011_men_full %>% mutate(year = 2011, ref = "men", q = "composition"),
  q_comp_2019_men_full %>% mutate(year = 2019, ref = "men", q = "composition"),
  q_comp_1991_women_full %>% mutate(year = 1991, ref = "women", q = "composition"),
  q_comp_2001_women_full %>% mutate(year = 2001, ref = "women", q = "composition"),
  q_comp_2011_women_full %>% mutate(year = 2011, ref = "women", q = "composition"),
  q_comp_2019_women_full %>% mutate(year = 2019, ref = "women", q = "composition"))

app_table_q_long <- app_table_q %>%
  filter(q == "composition") %>%
  dplyr::select(year, ref, married, num.kids.cont) %>%
  gather(key, value, -c(year, ref)) %>%
  filter(key != "(Intercept)") %>%
  rename("Counterfactual Means, Composition" = "value") %>%
  left_join(., means.year %>%
              mutate(ref = ifelse(female == 0, "men", "women")) %>%
              dplyr::select(year, ref, married, num.kids.cont) %>%
              gather(key, "Means", - c(year,ref)), 
            by = c("key", "year", "ref"))


knitr::kable(app_table_q_long, 
             booktabs = T, format = "latex", digits = 3,
             caption = "Counterfactual Means, Composition")

# In our convergence/composition pathways, the contribution from changes to the
# non-family traits has to be identical to what they are in table 3, since we’re
# allowing those traits to change as they really did. So in the new table 4 I 
# think both the Family and Full model would just have Fertility, Marital Status,
# Sum, Total.

# One thing I’m on the fence about is whether we also want a column that is the
# “full characteristics change”, which would just replicate the first column 
# from table 3. I think yes? It’s a little confusing, because the convergence
# + composition contributions don’t add up to the total, but I think it will
# help the reader see that the composition contribution comes close to the total.

cov.years <- c(1981, 2019)

# Then do an analogous table, but switching which gender is reference group

#######
# First, we create objects that compute sex-specific characteristic changes
# and sex-specific characteristic levels at t1
mchargap <- means.year %>% filter(female == 0, year == cov.years[2]) %>% 
  dplyr::select(full_vars) -
  means.year %>% filter(female == 0, year == cov.years[1]) %>% 
  dplyr::select(full_vars)

wchargap <- app_table_q %>% filter(ref == "women", q == "composition", year == 2019) %>%
  dplyr::select(-c(year, ref, q)) -
  means.year %>% filter(female == 1, year == cov.years[1])  %>% 
  dplyr::select(full_vars)

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

# Groups the component from the family model into a single object
quack <- bind_cols(char_fam %>% as.data.frame() %>% rename("Composition" = V1))

quack1 <- quack %>%
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


quack2 <- bind_cols(char_full %>% as.data.frame() %>% rename("Composition" = V1))

# Summarizes the detailed results for each variable group by 
# summing across the relevant category
quack3 <- quack2 %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marital Status", 
                           rownames(.) %in% fertility ~ "Fertility",
                           rownames(.) %in% c(race, region) ~ "Demographic", 
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
quack4 <- quack3 %>%
  mutate(vargroup = case_when(Variables %in% c("Marital Status", "Fertility", "Housework") ~ "Sum, Family",
                              TRUE ~ "Other")) %>%
  group_by(vargroup) %>% 
  summarise_if(is.numeric, sum) %>%
  filter(vargroup != "Other") %>%
  rename("Variables" = vargroup) %>%
  bind_rows(quack3, .) %>%
  mutate(model = "Full")

# Combining the summarised results from all components across both models 
menref <- bind_rows(quack1, quack4)

#######
# First, we create objects that compute sex-specific characteristic changes
# and sex-specific characteristic levels at t1
mchargap <- app_table_q %>% filter(ref == "men", q == "composition", year == 2019) %>%
  dplyr::select(-c(year, ref, q)) -
  means.year %>% filter(female == 0, year == cov.years[1]) %>% 
  dplyr::select(full_vars)

wchargap <- means.year %>% filter(female == 1, year == cov.years[2])-
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

# Groups the component from the family model into a single object
woof <- bind_cols(char_fam %>% as.data.frame() %>% rename("Composition" = V1))

woof1 <- woof %>%
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


woof2 <- bind_cols(char_full %>% as.data.frame() %>% rename("Composition" = V1))

# Summarizes the detailed results for each variable group by 
# summing across the relevant category
woof3 <- woof2 %>%
  mutate(group = case_when(rownames(.) %in% marital.status ~ "Marital Status", 
                           rownames(.) %in% fertility ~ "Fertility",
                           rownames(.) %in% c(race, region) ~ "Demographic", 
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
woof4 <- woof3 %>%
  mutate(vargroup = case_when(Variables %in% c("Marital Status", "Fertility", "Housework") ~ "Sum, Family",
                              TRUE ~ "Other")) %>%
  group_by(vargroup) %>% 
  summarise_if(is.numeric, sum) %>%
  filter(vargroup != "Other") %>%
  rename("Variables" = vargroup) %>%
  bind_rows(woof3, .) %>%
  mutate(model = "Full")

# Combining the summarised results from all components across both models 
womenref <- bind_rows(woof1, woof4)

table4 <- left_join(menref, womenref, by = c("Variables", "model")) %>%
  rename("Women" = "Composition.x",
         "Men" =  "Composition.y") %>%
  bind_cols(., t3_allcomponents %>% filter(model != "famed") %>%
              dplyr::select("Characteristics Gap") %>%
              rename("Observed" = "Characteristics Gap"))

# Creating the table output
knitr::kable(table4 %>% dplyr::select(-model), 
             booktabs = T, format = "latex", digits = 3,
             caption = "Table 4: Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Family Demography, Observed and Counterfactual Scenarios") %>%
  pack_rows("Family Model", 1, 3, bold = F) %>% pack_rows("Full Model", 4, 9, bold = F) %>%
  add_header_above(c(" ", "Counterfactual Family-Demographic Change" = 2))

barplot.counterfactual <- table4 %>%
  mutate(Variables = ifelse(Variables == "Marital Status", "Marriage", Variables)) %>%
  gather(key, value, -c(model, Variables)) %>%
  filter(Variables %in% c("Fertility", "Marriage")) %>%
  mutate(key = factor(key, levels = c("Men", "Women", "Observed"))) %>%
  ggplot(aes(y = value, fill = Variables, x = key)) +
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  facet_wrap(~model) +
  labs(x = "", y = "% Change in Pay Gap Explained", 
       title = "Percent of the Changing Gender Pay Gap 1980-2018 Explained by Changing Family Demography under Counterfactual Scenarios",
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

ggsave(plot = barplot.counterfactual, "output/barplot_counterfactual.jpg", 
       width = 13.5, height = 9, units = "in", device='jpeg', dpi=700)

