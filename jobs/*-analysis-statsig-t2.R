#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: ANALYSES, TESTING FOR DIFFERENCES ACROSS YEARS AND GENDER
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

date_run_id <-  "2024-06-07"

opts <- options(knitr.kable.NA = "")

# Loading helper functions and libraries
source("jobs/1-load-libraries.R")
source("jobs/1-functions.R")

# Loading the imputed dataset
psid_imp <- read_csv(paste0("clean_data/psid_final_", date_run_id, ".csv"))

# Loading in objects that specify arguments passed to the 
# analysis functions multiple times
source("jobs/4-analysis-arguments.R")

psid_analytical <- psid_imp %>% 
  mutate(samp.inc = ifelse(eval(parse(text = conditions_expr)), 1, 0)) %>%
  filter(samp.inc == 1, year %in% years)

# Checking statistical significance of coefficient differences 
# across periods and genders
mice.out.imp.1981 <- imputationList(psid_analytical %>% 
                                      filter(.imp > 0) %>%
                                      filter(year == years[1]) %>%
                                      group_split(.imp))

design.1981 <-svydesign(ids = ~0, data = mice.out.imp.1981, weights = ~perwt_norm, nest = T)

mice.out.imp.2019 <- imputationList(psid_analytical %>% 
                                      filter(.imp > 0) %>%
                                      filter(year == years[2]) %>%
                                      group_split(.imp))

design.2019 <-svydesign(ids = ~0, data = mice.out.imp.2019, weights = ~perwt_norm, nest = T)

# Difference in coefs across genders, t1, model 1

model1.1981 <- with(design.1981, svyglm(
  lnhrlywage ~ num.kids.cont*female))

model1.1981 <- MIcombine((model1.1981))

MIcombineP(model1.1981)

model1.2019 <- with(design.2019, svyglm(
  lnhrlywage ~ num.kids.cont*female))

model1.2019 <- MIcombine((model1.2019))

MIcombineP(model1.2019)

model2.1981 <- with(design.1981, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female + married*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female))

model2.1981 <- MIcombine((model2.1981))

MIcombineP(model2.1981)

model2.2019 <- with(design.2019, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female + married*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female))

model2.2019 <- MIcombine((model2.2019))

MIcombineP(model2.2019)

model3.1981 <- with(design.1981, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female +
    HighSchool*female + SomeCollege*female + ba.avdeg*female +
    married*female))

model3.1981 <- MIcombine((model3.1981))

MIcombineP(model3.1981)

model3.2019 <- with(design.2019, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female +
    HighSchool*female + SomeCollege*female + ba.avdeg*female+
    married*female))

model3.2019 <- MIcombine((model3.2019))

MIcombineP(model3.2019)

model4.1981 <- with(design.1981, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female +
    HighSchool*female + SomeCollege*female + ba.avdeg*female +
    married*female + log.expf*female +
    emp.tenure*female))

model4.1981 <- MIcombine((model4.1981))

MIcombineP(model4.1981)

model4.2019 <- with(design.2019, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female +
    HighSchool*female + SomeCollege*female + ba.avdeg*female+
    married*female + log.expf*female + 
    emp.tenure*female))

model4.2019 <- MIcombine((model4.2019))

MIcombineP(model4.2019)

model5.1981 <- with(design.1981, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female +
    HighSchool*female + SomeCollege*female + ba.avdeg*female +
    married*female + log.expf*female + ftormore*female + 
    emp.tenure*female + overwork*female + union*female +
    govt.job*female + occ.pct.female*female + occ.managers*female +
    manuf*female))

model5.1981 <- MIcombine((model5.1981))

MIcombineP(model5.1981)

model5.2019 <- with(design.2019, svyglm(
  lnhrlywage ~ age*female + agesq*female + Black*female + 
    Hispanic*female + Other*female +
    num.kids.cont*female + Northeast*female + 
    Northcentral*female + South*female +
    HighSchool*female + SomeCollege*female + ba.avdeg*female+
    married*female + log.expf*female + ftormore*female + 
    emp.tenure*female + overwork*female + union*female +
    govt.job*female + occ.pct.female*female + occ.managers*female +
    manuf*female))

model5.2019 <- MIcombine((model5.2019))

MIcombineP(model5.2019)


# Testing for statsig across years
mice.out.imp.female <- imputationList(psid_analytical %>% 
                                        mutate(year = ifelse(year == 1981, 0, 1)) %>%
                                        filter(.imp > 0) %>%
                                        filter(female ==1) %>%
                                        group_split(.imp))

design.female <-svydesign(ids = ~0, data = mice.out.imp.female, weights = ~perwt_norm, nest = T)

mice.out.imp.male <- imputationList(psid_analytical %>% 
                                      mutate(year = ifelse(year == 1981, 0, 1)) %>%
                                      filter(.imp > 0) %>%
                                      filter(female == 0) %>%
                                      group_split(.imp))

design.male <-svydesign(ids = ~0, data = mice.out.imp.male, weights = ~perwt_norm, nest = T)

model1.male <- with(design.male, svyglm(
  lnhrlywage ~num.kids.cont*year))

model1.male <- MIcombine((model1.male))

MIcombineP(model1.male)

model1.female <- with(design.female, svyglm(
  lnhrlywage ~ num.kids.cont*year))

model1.female <- MIcombine((model1.female))

MIcombineP(model1.female)

model2.male <- with(design.male, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    married*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year))

model2.male <- MIcombine((model2.male))

MIcombineP(model2.male)

model2.female <- with(design.female, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    married*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year))

model2.female <- MIcombine((model2.female))

MIcombineP(model2.female)

model3.male <- with(design.male, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year +
    HighSchool*year + SomeCollege*year + ba.avdeg*year +
    married*year))

model3.male <- MIcombine((model3.male))

MIcombineP(model3.male)

model3.female <- with(design.female, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year +
    HighSchool*year + SomeCollege*year + ba.avdeg*year+
    married*year))

model3.female <- MIcombine((model3.female))

MIcombineP(model3.female)

model4.male <- with(design.male, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year +
    HighSchool*year + SomeCollege*year + ba.avdeg*year +
    married*year + log.expf*year + 
    emp.tenure*year))

model4.male <- MIcombine((model4.male))

MIcombineP(model4.male)

model4.female <- with(design.female, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year +
    HighSchool*year + SomeCollege*year + ba.avdeg*year+
    married*year + log.expf*year + 
    emp.tenure*year))

model4.female <- MIcombine((model4.female))

MIcombineP(model4.female)

model5.male <- with(design.male, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year +
    HighSchool*year + SomeCollege*year + ba.avdeg*year +
    married*year + log.expf*year + ftormore*year + 
    emp.tenure*year + overwork*year + union*year +
    govt.job*year + occ.pct.female*year + occ.managers*year +
    manuf*year))

model5.male <- MIcombine((model5.male))

MIcombineP(model5.male)

model5.female <- with(design.female, svyglm(
  lnhrlywage ~ age*year + agesq*year + Black*year + 
    Hispanic*year + Other*year +
    num.kids.cont*year + Northeast*year + 
    Northcentral*year + South*year +
    HighSchool*year + SomeCollege*year + ba.avdeg*year+
    married*year + log.expf*year + ftormore*year + 
    emp.tenure*year + overwork*year + union*year +
    govt.job*year + occ.pct.female*year + occ.managers*year +
    manuf*year))

model5.female <- MIcombine((model5.female))

MIcombineP(model5.female)

# Testing for statsig in gender gap change across years
# Checking statistical significance of coefficient differences 
# across periods and genders
mice.out.imp <- imputationList(psid_analytical %>% 
                                 filter(.imp > 0) %>%
                                 mutate(year = ifelse(year == 1981, 0, 1)) %>%
                                 group_split(.imp))

design <-svydesign(ids = ~0, data = mice.out.imp, weights = ~perwt_norm, nest = T)

model1 <- with(design, svyglm(
  lnhrlywage ~ num.kids.cont*female*year))

model1 <- MIcombine((model1))

MIcombineP(model1)

model2 <- with(design, svyglm(
  lnhrlywage ~ age*female*year + agesq*female*year  + Black*female*year + 
    Hispanic*female*year  + Other*female*year  + married*female*year +
    num.kids.cont*female*year  + Northeast*female*year  + 
    Northcentral*female*year  + South*female*year ))

model2 <- MIcombine((model2))

MIcombineP(model2)

model3 <- with(design, svyglm(
  lnhrlywage ~ age*female*year + agesq*female*year  + Black*female*year + 
    Hispanic*female*year  + Other*female*year  +
    num.kids.cont*female*year  + Northeast*female*year  + 
    Northcentral*female*year  + South*female*year +
    HighSchool*female*year + SomeCollege*female*year + ba.avdeg*female*year +
    married*female*year))

model3 <- MIcombine((model3))

MIcombineP(model3)

model4 <- with(design, svyglm(
  lnhrlywage ~ age*female*year + agesq*female*year  + Black*female*year + 
    Hispanic*female*year  + Other*female*year  +
    num.kids.cont*female*year  + Northeast*female*year  + 
    Northcentral*female*year  + South*female*year +
    HighSchool*female*year + SomeCollege*female*year + ba.avdeg*female*year +
    married*female*year + log.expf*female*year +
    emp.tenure*female*year))

model4 <- MIcombine((model4))

MIcombineP(model4)

model5 <- with(design, svyglm(
  lnhrlywage ~ age*female*year + agesq*female*year  + Black*female*year + 
    Hispanic*female*year  + Other*female*year  +
    num.kids.cont*female*year  + Northeast*female*year  + 
    Northcentral*female*year  + South*female*year +
    HighSchool*female*year + SomeCollege*female*year + ba.avdeg*female*year +
    married*female*year + log.expf*female*year + ftormore*female*year + 
    emp.tenure*female*year + overwork*female*year + union*female*year +
    govt.job*female*year + occ.pct.female*female*year + occ.managers*female*year +
    manuf*female*year))

model5 <- MIcombine((model5))

MIcombineP(model5)

