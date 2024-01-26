#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, FIGURE A3
# AUTHOR: NINO CRICCO
# LAST UPDATED: 12/29/2023 (mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv") %>%
  dummy_cols(., select_columns = c("marstat.cohab")) %>%
  rename_at(vars(starts_with('marstat.cohab_')), funs(sub('_', ".", .)))

source("jobs/3-analysis-arguments.R")

# Creating categorical variables for subgroup analyses
psid_t1 <- data %>%
  mutate(numkids.mar = ifelse(married == 1, num.kids.cont, 0), 
         numkids.unmar = ifelse(married == 0, num.kids.cont, 0),
         numkids.hsless = ifelse(yrs.ed.fam <= 12, num.kids.cont, 0),
         numkids.morethanhs = ifelse(yrs.ed.fam > 12, num.kids.cont, 0),
         hsandless = ifelse(yrs.ed.fam <= 12, 1, 0), 
         numkids.white = ifelse(White == 1, num.kids.cont, 0),
         numkids.black = ifelse(Black == 1, num.kids.cont, 0),
         numkids.latino = ifelse(Hispanic == 1, num.kids.cont, 0),
         numkids.other = ifelse(Other == 1, num.kids.cont, 0))

# For each yer by sex group, we create list objects where each element contains an imputed dataset
# For Men in t1
mice.out.imp.mt1 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == years[1], female == 0) %>%
                                     group_split(.imp))

design.mt1 <-svydesign(ids = ~0, data = mice.out.imp.mt1, weights = ~perwt, nest = T)

# For Men in t2
mice.out.imp.mt2 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == years[2], female == 0) %>%
                                     group_split(.imp))

design.mt2 <-svydesign(ids = ~0, data = mice.out.imp.mt2, weights = ~perwt, nest = T)

# For Women in t1
mice.out.imp.ft1 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == years[1], female == 1) %>%
                                     group_split(.imp))

design.ft1 <-svydesign(ids = ~0, data = mice.out.imp.ft1, weights = ~perwt, nest = T)

# For Women in t2
mice.out.imp.ft2 <- imputationList(psid_t1 %>% 
                                     filter(.imp > 0) %>%
                                     filter(year == years[2], female == 1) %>%
                                     group_split(.imp))

design.ft2 <-svydesign(ids = ~0, data = mice.out.imp.ft2, weights = ~perwt, nest = T)


# ESTIMATING THE MODEL- by race subgroups
# Model for Men in t1
race.mt1 <- with(design.mt1, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other +
    numkids.white + numkids.black + numkids.latino + numkids.other +
    married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

# Combining the regression results across imputations
race.mt1 <- MIcombine((race.mt1))
MIcombineP(race.mt1)

# From the combined results object, create a dataframe that contains 
# the coefficients and standard errors for each subgroup
race.mt1 <- bind_rows(coef(race.mt1), vcov::se(race.mt1),
                      confint(race.mt1)[,1], confint(race.mt1)[,2]) %>%
  # We add a row to the dataframe that labels the year and sex groups
  mutate(year = years[1], female = 0, estimate = c("coef", "se", 
                                                       "lower", "higher"))
# Repeat procedure for men in t2
race.mt2 <- with(design.mt2, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other +
    numkids.white + numkids.black + numkids.latino + numkids.other +
    married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

race.mt2 <- MIcombine((race.mt2))
MIcombineP(race.mt2)

race.mt2 <- bind_rows(coef(race.mt2), vcov::se(race.mt2),
                      confint(race.mt2)[,1], confint(race.mt2)[,2]) %>%
  mutate(year = years[2], female = 0, estimate = c("coef", "se", 
                                                       "lower", "higher"))
# Repeat procedure for women in t1
race.ft1 <- with(design.ft1, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other +
    numkids.white + numkids.black + numkids.latino + numkids.other +
    married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

race.ft1 <- MIcombine((race.ft1))
MIcombineP(race.ft1)

race.ft1 <- bind_rows(coef(race.ft1), vcov::se(race.ft1),
                      confint(race.ft1)[,1], confint(race.ft1)[,2]) %>%
  mutate(year = years[1], female = 1, estimate = c("coef", "se", 
                                                       "lower", "higher"))

# Repeat the procedure for women in t2
race.ft2 <- with(design.ft2, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other +
    numkids.white + numkids.black + numkids.latino + numkids.other +
    married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

race.ft2 <- MIcombine((race.ft2))
MIcombineP(race.ft2)

race.ft2 <- bind_rows(coef(race.ft2), vcov::se(race.ft2),
                      confint(race.ft2)[,1], confint(race.ft2)[,2]) %>%
  mutate(year = years[2], female = 1, estimate = c("coef", "se", 
                                                       "lower", "higher"))

# Creating dataframe binding all gender-year race coefficients
race.coefs <- bind_rows(race.mt1, race.ft1, 
                        race.mt2, race.ft2)

# Repeating the same procedure as above, but using marital status as the subgroup
marstat.mt1 <- with(design.mt1, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other  +
    numkids.mar + numkids.unmar + married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

marstat.mt1 <- MIcombine((marstat.mt1))
MIcombineP(marstat.mt1)

marstat.mt1 <- bind_rows(coef(marstat.mt1), vcov::se(marstat.mt1),
                         confint(marstat.mt1)[,1], confint(marstat.mt1)[,2]) %>%
  mutate(year = years[1], female = 0, estimate = c("coef", "se", 
                                                       "lower", "higher"))
marstat.mt2 <- with(design.mt2, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other  +
    numkids.mar + numkids.unmar + married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

marstat.mt2 <- MIcombine((marstat.mt2))
MIcombineP(marstat.mt2)

marstat.mt2 <- bind_rows(coef(marstat.mt2), vcov::se(marstat.mt2),
                         confint(marstat.mt2)[,1], confint(marstat.mt2)[,2]) %>%
  mutate(year = years[2], female = 0, estimate = c("coef", "se", 
                                                       "lower", "higher"))

marstat.ft1 <- with(design.ft1, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other  +
    numkids.mar + numkids.unmar + married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

marstat.ft1 <- MIcombine((marstat.ft1))
MIcombineP(marstat.ft1)

marstat.ft1 <- bind_rows(coef(marstat.ft1), vcov::se(marstat.ft1),
                         confint(marstat.ft1)[,1], confint(marstat.ft1)[,2]) %>%
  mutate(year = years[1], female = 1, estimate = c("coef", "se", 
                                                       "lower", "higher"))

marstat.ft2 <- with(design.ft2, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other  +
    numkids.mar + numkids.unmar + married +
    HighSchool  + SomeCollege  + ba.avdeg +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

marstat.ft2 <- MIcombine((marstat.ft2))
MIcombineP(marstat.ft2)

marstat.ft2 <- bind_rows(coef(marstat.ft2), vcov::se(marstat.ft2),
                         confint(marstat.ft2)[,1], confint(marstat.ft2)[,2]) %>%
  mutate(year = years[2], female = 1, estimate = c("coef", "se", 
                                                       "lower", "higher"))

# Creating dataframe binding all gender-year marstat coefficients
marstat.coefs <- bind_rows(marstat.mt1, marstat.ft1, 
                           marstat.mt2, marstat.ft2)

# Again, repeating the same subgroup procedure as above, but with education as subgroup
ed.mt1 <- with(design.mt1, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral+ South +
    Black + Hispanic + Other  +
    married +
    numkids.hsless + numkids.morethanhs + hsandless +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

ed.mt1 <- MIcombine((ed.mt1))
MIcombineP(ed.mt1)

ed.mt1 <- bind_rows(coef(ed.mt1), vcov::se(ed.mt1),
                    confint(ed.mt1)[,1], confint(ed.mt1)[,2]) %>%
  mutate(year = years[1], female = 0, estimate = c("coef", "se", 
                                                       "lower", "higher"))

ed.mt2 <- with(design.mt2, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral + South +
    Black + Hispanic + Other  +
    married +
    numkids.hsless + numkids.morethanhs + hsandless +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

ed.mt2 <- MIcombine((ed.mt2))
MIcombineP(ed.mt2)

ed.mt2 <- bind_rows(coef(ed.mt2), vcov::se(ed.mt2),
                    confint(ed.mt2)[,1], confint(ed.mt2)[,2]) %>%
  mutate(year = years[2], female = 0, estimate = c("coef", "se", 
                                                       "lower", "higher"))

ed.ft1 <- with(design.ft1, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral + South +
    Black + Hispanic + Other  +
    married +
    numkids.hsless + numkids.morethanhs + hsandless +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
))

ed.ft1 <- MIcombine((ed.ft1))
MIcombineP(ed.ft1)

ed.ft1 <- bind_rows(coef(ed.ft1), vcov::se(ed.ft1),
                    confint(ed.ft1)[,1], confint(ed.ft1)[,2]) %>%
  mutate(year = years[1], female = 1, estimate = c("coef", "se", 
                                                       "lower", "higher"))

ed.ft2 <- with(design.ft2, svyglm(
  lnhrlywage ~ age + agesq +
    Northeast + Northcentral + South +
    Black + Hispanic + Other  +
    married +
    numkids.hsless + numkids.morethanhs + hsandless +
    log.expf + emp.tenure + 
    ftormore + overwork + union + govt.job + occ.pct.female + occ.managers + 
    manuf
  ))

ed.ft2 <- MIcombine((ed.ft2))
MIcombineP(ed.ft2)

ed.ft2 <- bind_rows(coef(ed.ft2), vcov::se(ed.ft2),
                    confint(ed.ft2)[,1], confint(ed.ft2)[,2]) %>%
  mutate(year = years[2], female = 1, estimate = c("coef", "se", 
                                                       "lower", "higher"))

# Creating dataframe binding all gender-year ed coefficients
ed.coefs <- bind_rows(ed.mt1, ed.ft1, 
                      ed.mt2, ed.ft2)


# Creating the coefficient plots by subgroup
pd <- position_dodge(0.5)

race.coefplot <- race.coefs %>%
  select(year, female, estimate, starts_with("numkids")) %>%
  gather(key, value, -c(year, female, estimate)) %>% 
  pivot_wider(names_from = c(estimate), values_from = value) %>%
  mutate(female = ifelse(female == 1, "Women", "Men")) %>%
  filter(key %in% c("numkids.white", "numkids.black")) %>%
  mutate(key = ifelse(key == "numkids.white", "White", "Black")) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(y = coef, color = key, x = year)) +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.1, position=pd) +
  geom_point(position=pd) + 
  facet_wrap(~female) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "", color=NULL) +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
  theme(legend.position = "bottom") +
  ylab("") +
  xlab("") +
  ylim(-.15, .12)

ed.coefplot <- ed.coefs %>%
  select(year, female, estimate, starts_with("numkids")) %>%
  gather(key, value, -c(year, female, estimate)) %>% 
  pivot_wider(names_from = c(estimate), values_from = value) %>%
  mutate(female = ifelse(female == 1, "Women", "Men")) %>%
  mutate(key = ifelse(key == "numkids.hsless", "<= High School", "> High School")) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(y = coef, color = key, x = year)) +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.1, position=pd) +
  geom_point(position=pd) + 
  facet_wrap(~female) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "", color=NULL) +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
  theme(legend.position = "bottom") +
  ylab("") +
  xlab("")  +
  ylim(-.15, .12)

marstat.coefplot <- marstat.coefs %>%
  select(year, female, estimate, starts_with("numkids")) %>%
  gather(key, value, -c(year, female, estimate)) %>% 
  pivot_wider(names_from = c(estimate), values_from = value) %>%
  mutate(female = ifelse(female == 1, "Women", "Men")) %>%
  mutate(key = ifelse(key == "numkids.mar", "Married", "Unmarried")) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(y = coef, color = key, x = year)) +
  geom_errorbar(aes(ymin=lower, ymax=higher), width=.1, position=pd) +
  geom_point(position=pd) + 
  facet_wrap(~female) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "", color=NULL) +
  scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
  theme(legend.position = "bottom") +
  ylab("") +
  xlab("")  +
  ylim(-.15, .12)

# Combining all subgroup plots into one plot
p4 <- grid.arrange(
  marstat.coefplot, race.coefplot, ed.coefplot,  
  nrow = 1, 
  top = "Coefficients on Number of Children"#,
  #bottom = caption_grob
)


# Creating first panel of Figure A1, showing fertility decline by subgroup
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
  scale_color_manual(values=c("darkgoldenrod2", "dodgerblue4")) +
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
  scale_color_manual(values=c("darkgoldenrod2", "dodgerblue4")) +
  theme(legend.position = "bottom",
        panel.spacing = unit(2, "lines")) +
  xlab("Year") +
  ylab("") 

figa2 <- grid.arrange(p1, p2, p3, nrow = 1, 
                      top = "Average Number of Children")

figa1 <- grid.arrange(figa2, p4, nrow = 2,
                          top = "Figure A1: Subgroup Results, 1980-2018")

ggsave(plot = figa1, "figures/figa1.jpg", 
       width = 10.5, height = 12, units = "in", device='jpeg', dpi=700)
