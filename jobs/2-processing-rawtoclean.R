#------------------------------------------------------------------------------
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: CREATES PRE-IMPUTATION ANALYTIC DATA FROM THE RAW PSID AND IPUMS DATA
# AUTHOR: NINO CRICCO
#------------------------------------------------------------------------------

# Loading libraries and helper functions
source("jobs/1-load-libraries.R")
source("jobs/1-functions.R")

#------------------------------------------------------------------------------
# LOADING AND CLEANING DATA
#------------------------------------------------------------------------------

# First loading both crosswalks for occupation and industry codes
# Integrated industry and occupations crosswalk gathered from IPUMS
# at the following link at time of writing:
#   https://usa.ipums.org/usa/resources/volii/documents/integrated_ind_occ_crosswalks.xlsx
# Crosswalks were manually edited to include only relevant codes

# For occupation crosswalk: occ2010 tab in the IPUMS integrated crosswalk
# Resource for occ codes in PSID codebook that are not included in crosswalk: 
#   https://www.bls.gov/cps/cenocc2010.htm

crosswalk.occ <- read_xlsx(
  "raw_data/crosswalks/ipums_occ_crosswalk.xlsx") %>%
  rename(occ2010 = "OCC2010", 
         occlab = 'Occupation category description',
         occ1970 = '1970',
         occ2000 = '2000',
         acs2010 = "ACS2010")

# For industry crosswalk: ind1990 tab in the IPUMS integrated crosswalk

crosswalk.ind <- read_xlsx(
  "raw_data/crosswalks/ipums_industry_crosswalk.xlsx") %>%
  rename(ind1990 = IND1990, 
         indlab = 'Industry category description',
         ind1970 = '1970',
         ind2000 = '2000')

# Integrated IPUMS crosswalk does not include 2012 industry codes, used in some PSID years
# Crosswalk for these codes obtained from census bureau at the following link
# at the time of writing: 
#   https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/industry-crosswalk-90-00-02-07-12.xls

crosswalk.ind.2012 <- read_xlsx(
  "raw_data/crosswalks/census_industry_crosswalk_2012.1990.xlsx") %>%
  rename(ind1990 = "1990 Census", 
         indlab = "Census 2012 Category Title",
         ind2012 = "2012 Census Code")

#------------------------------------------------------------------------------
# MAIN PSID DATA
#------------------------------------------------------------------------------

# Both gathers values from the PSID raw file and transforms/recodes values in 
# the process
# Note: Once downloading the data cart from the PSID's online portal, the data ]
# file must be generated in stata.The stata do-file automatically generated
# from the PSID must be edited to reflect the file path where data is stored
# After running the do-file on stata, save file as "psid.dta" in stata 11/12
# format in the "raw_data/psid" directory

data <- read.dta("raw_data/psid/psid.dta") 

psid_raw <- data %>% 
  transmute(
    family.id68 = ER30001, # 1968 interview number
    person.number68 = ER30002, # 1968 person number
    indiv.id = paste(family.id68, person.number68, sep ="_"), # unique individual identifier
    female = ifelse(ER32000 == 1, 0, ifelse(ER32000 == 2, 1, NA)), # identifying sex
    year.firstbornchild = ifelse(ER32024 == 9999, NA, ER32024), # birth year for respondents' first or only child
    yr.born = ifelse(ER34506 %in% c(0, 9999), NA, ER34506), 
    family.id_1980 = ER30313, family.id_1981 = ER30343,
    family.id_1990 = ER30642, family.id_1991 = ER30689,
    family.id_1999 = ER33501, family.id_2001 = ER33601, 
    family.id_2009 = ER34001, family.id_2011 = ER34101,
    family.id_2017 = ER34501, family.id_2019 = ER34701,
    int.num_1980 = ER30313, int.num_1981 = ER30343, int.num_1990 = ER30642, int.num_1991 = ER30689, 
    int.num_1999 = ER33501, int.num_2001 = ER33601, int.num_2009 = ER34001, int.num_2011 = ER34101, 
    int.num_2015 = ER34301, int.num_2017 = ER34501, int.num_2019 = ER34701, # Wave-specific interview number
    seq.num_1980 = ER30314, seq.num_1981 = ER30344, seq.num_1990 = ER30643, seq.num_1991 = ER30690,
    seq.num_1999 = ER33502, seq.num_2001 = ER33602, seq.num_2009 = ER34002, seq.num_2011 = ER34102, 
    seq.num_2015 = ER34302, seq.num_2017 = ER34502, seq.num_2019 = ER34702, # Wave-specific sequence number
    # SAMPLE WEIGHTS
    # We use the core-immigrant cross-sectional weights when available (starting from 1999 of our observation years)
    # For prior years, we use the combined individual core-latino weight for 1990-91
    # and the individual weights in 1980-81. 
    perwt_1980 = ER30342, perwt_1981 = ER30372, perwt_1990 = ER30688, perwt_1991 = ER30732, 
    perwt_1999 = ER33547, perwt_2001 = ER33639, perwt_2009 = ER34046, perwt_2011 = ER34155, 
    perwt_2015 = ER34414, perwt_2017 = ER34651, perwt_2019 = ER34864,
    # Weighting variable for longitudinal weights 
    perwt.long_1980 = ER30342, perwt.long_1981 = ER30372, perwt.long_1990 = ER30688, perwt.long_1991 = ER30732, 
    perwt.long_1999 = ER33546, perwt.long_2001 = ER33637, perwt.long_2009 = ER34045, perwt.long_2011 = ER52436, 
    perwt.long_2015 = ER65492, perwt.long_2017 = ER71570, perwt.long_2019 = ER34863,
    # Relationship to head- coding as head/RP, wife/SP
    # if any other value for rel head (incl. zero values), coded as "other" 
    rel.head_1980 = case_when(ER30315 == 1 ~ "head", ER30315 == 2 ~ "wife", TRUE ~ "other"), 
    rel.head_1981 = case_when(ER30345 == 1 ~ "head", ER30345 == 2 ~ "wife", TRUE ~ "other"), 
    rel.head_1990 = case_when(ER30644 == 10 ~ "head", ER30644 %in% c(20, 22) ~ "wife", TRUE ~ "other"),
    rel.head_1991 = case_when(ER30691 == 10 ~ "head", ER30691 %in% c(20, 22) ~ "wife", TRUE ~ "other"),
    rel.head_1999 = case_when(ER33503 == 10 ~ "head", ER33503 %in% c(20, 22) ~ "wife", TRUE ~ "other"), 
    rel.head_2001 = case_when(ER33603 == 10 ~ "head", ER33603 %in% c(20, 22) ~ "wife", TRUE ~ "other"), 
    rel.head_2009 = case_when(ER34003 == 10 ~ "head", ER34003 %in% c(20, 22) ~ "wife", TRUE ~ "other"), 
    rel.head_2011 = case_when(ER34103 == 10 ~ "head", ER34103 %in% c(20, 22) ~ "wife", TRUE ~ "other"), 
    # For these years, the PSID changes the coding from head/wife to head/spouse. We only use the 
    # head/wife/spouse labels to assign the individual the correct variables- that is, heads are assigned
    # information collected about heads, wives/spouses information about wives/spouses, but do not 
    # otherwise rely on these indicators for any analyses. 
    rel.head_2015 = case_when(ER34303 == 10 ~ "head", ER34303 %in% c(20, 22) ~ "wife", TRUE ~ "other"),
    rel.head_2017 = case_when(ER34503 == 10 ~ "head", ER34503 %in% c(20, 22) ~ "wife", TRUE ~ "other"), 
    rel.head_2019 = case_when(ER34703 == 10 ~ "head", ER34703 %in% c(20, 22) ~ "wife", TRUE ~ "other"),
    # Age- setting codes 0, 999 to NA
    age_1980 = na_codes(ER30316, c(0, 999)), age_1981 = na_codes(ER30346, c(0, 999)),
    age_1990 = na_codes(ER30645, c(0, 999)), age_1991 = na_codes(ER30692, c(0, 999)), 
    age_1999 = na_codes(ER33504, c(0, 999)), age_2001 = na_codes(ER33604, c(0, 999)), 
    age_2009 = na_codes(ER34004, c(0, 999)), age_2011 = na_codes(ER34104, c(0, 999)),
    age_2015 = na_codes(ER34305, c(0, 999)), age_2017 = na_codes(ER34504, c(0,999)), 
    age_2019 = na_codes(ER34704, c(0,999)),
    # Race
    # For the early years, we recode a single measure of race, which 
    # is carried over from the 1972 interview and there is a single value assigned to the family. 
    # For sample members who split after 1972, members are assigned the race in the PSID family in 1972
    racehd_1980 = case_when(V7447 == 1 ~ "White", V7447 == 2 ~ "Black", V7447 == 3 ~ "Hispanic", V7447 == 7 ~ "Other"),
    racehd_1981 = case_when(V8099 == 1 ~ "White", V8099 == 2 ~ "Black", V8099 == 3 ~ "Hispanic", V8099 == 7 ~ "Other"),
    # Marital status of head
    marstat.hd_1980 = V7261, marstat.hd_1981 = V7952, 
    marstat.hd_1990 = na_codes(V18055, c(8,9)), marstat.hd_1991 = na_codes(V19355, c(8,9)), 
    marstat.hd_1999 = na_codes(ER13021, c(8,9)), marstat.hd_2001 = ER17024, 
    marstat.hd_2009 = na_codes(ER42023, c(8,9)), marstat.hd_2011 = na_codes(ER47323, c(8,9)),
    marstat.hd_2015 = na_codes(ER60024, c (8,9)), marstat.hd_2017 = na_codes(ER66024, c(8,9)),
    marstat.hd_2019 = na_codes(ER72024, c(8,9)),
    # Number of kids in the family unit
    numkids.fu_1980 = V7070, numkids.fu_1981 = V7661, numkids.fu_1990 = V18052, numkids.fu_1991 = V19352, 
    numkids.fu_1999 = ER13013, numkids.fu_2001 = ER17016, numkids_2009 = ER42020, numkids.fu_2011 = ER47320,
    numkids.fu_2015 = ER60021, numkids.fu_2017 = ER66021, numkids.fu_2019 = ER72021,
    # Region
    region_1980 = na_if(V7419, 9), region_1981 = na_if(V8071, 9), 
    region_1990 = na_if(V18889, 9), region_1991 = na_if(V20189, 9), 
    region_1999 = na_codes(ER16430, c(0, 9)), region_2001 = na_if(ER20376, 9),
    region_2009 = na_if(ER46974, 9), region_2011 = na_if(ER52398, 9), 
    region_2015 = na_if(ER65451, 9), region_2017 = na_if(ER71530, 9),
    region_2019 = na_if(ER77591, 9),
    # Years of education
    yrs.ed.fam_hd_1980 = na_if(V7387, 99), # Here zero is no educ
    yrs.ed.fam_wf_1980 = na_if(V7346, 99), # Here zero is no educ or no wife
    yrs.ed.fam_hd_1981 = na_if(V8039, 99), yrs.ed.fam_wf_1981 = na_if(V7998, 99), 
    yrs.ed.fam_hd_1990 = na_if(V18898, 9), yrs.ed.fam_wf_1990 = na_if(V18899, 9), # 1990: educ codes are different
    yrs.ed.fam_hd_1991 = na_if(V20198, 99), yrs.ed.fam_wf_1991 = na_if(V20199, 99), 
    yrs.ed.fam_hd_1999 = na_if(ER16516, 99), yrs.ed.fam_wf_1999 = na_if(ER16517, 99), 
    yrs.ed.fam_hd_2001 = na_if(ER20457, 99), yrs.ed.fam_wf_2001 = na_if(ER20458, 99), 
    yrs.ed.fam_hd_2009 = na_if(ER46981, 99), yrs.ed.fam_wf_2009 = na_if(ER46982, 99), 
    yrs.ed.fam_hd_2011 = na_if(ER52405, 99), yrs.ed.fam_wf_2011 = na_if(ER52406, 99), 
    yrs.ed.fam_hd_2015 = na_if(ER65459, 99), yrs.ed.fam_wf_2015 = na_if(ER65460, 99),
    yrs.ed.fam_hd_2017 = na_if(ER71538, 99), yrs.ed.fam_wf_2017 = na_if(ER71539, 99), 
    yrs.ed.fam_hd_2019 = na_if(ER77599, 99), yrs.ed.fam_wf_2019 = na_if(ER77600, 99), 
    # Total annual work hours- recorded in a given year about prior year, 
    # so 1981 values reflect hours worked in 1980, 2017 values reflect
    # hours worked in 2016. 0 = did not work for money. Missing values assigned by PSID
    ann.wrk.hrs_hd_1980 = V6934, ann.wrk.hrs_wf_1980 = V6946,
    ann.wrk.hrs_hd_1981 = V7530, ann.wrk.hrs_wf_1981 = V7540,
    ann.wrk.hrs_hd_1990 = V17744, ann.wrk.hrs_wf_1990 = V17774, 
    ann.wrk.hrs_hd_1991 = V19044, ann.wrk.hrs_wf_1991 = V19074, 
    ann.wrk.hrs_hd_1999 = ER16471, ann.wrk.hrs_wf_1999 = ER16482, 
    ann.wrk.hrs_hd_2001 = ER20399, ann.wrk.hrs_wf_2001 = ER20410, 
    ann.wrk.hrs_hd_2009 = ER46767, ann.wrk.hrs_wf_2009 = ER46788, 
    ann.wrk.hrs_hd_2011 = ER52175, ann.wrk.hrs_wf_2011 = ER52196, 
    ann.wrk.hrs_hd_2015 = ER65156, ann.wrk.hrs_wf_2015 = ER65177,
    ann.wrk.hrs_hd_2017 = ER71233, ann.wrk.hrs_wf_2017 = ER71254,
    ann.wrk.hrs_hd_2019 = ER77255, ann.wrk.hrs_wf_2019 = ER77276,
    # Occupation
    # Note that in 1980, the PSID has two sets of occupation and industry values- one set is using a PSID-generated 
    # scheme for those employed, unemployed, and retired, and a different set is recoded to the census occupation scheme. 
    # However, not all individuals in 1980 were eligible for this retrospective recoding to the census occupation scheme.
    # We use the recoded occ values, for those recoded, to merge to IPUMS occupation characteristics. 
    # Those who were not eligible for retrospective recoding(code 0) are transformed to a special code (9998) in the crosswalk
    # procedure, and are then coded as missing when merging to occupation characteristics. We later impute the occupation
    # characteristics for these individuals as we would for other missing data
    # Note: Previously we used the non-recoded values (V7100) to generate a set of occupation dummies, 
    # but we no longer use those values as we do not take the dummies approach. 
    occ_hd_1980 = na_codes(V7100_A, 999), 
    occ_wf_1980 = na_if(V7198_A, 999),
    # We use the recoded values and an IPUMS-provided crosswalk to convert the 1970 census occupation codes
    # for heads and wives to IPUMS' OCC2010 codes
    # We recode 600 (current armed forces), which has no match in 2010 occs, to 580 (military, rank not specified)
    # We also recode 329, which seems to be a year-specific wild code in the clerical and kindred occupation range, to 325 (file clerks)
    # For details on PSID occupation codes in the 1980 interview, including the use of the 600 code, see pages 641-649 
    # in the following document: https://psidonline.isr.umich.edu/Data/Documentation/pdf_doc/psid81w14.pdf
    occ_hd_1980 = case_when(occ_hd_1980 == 600 ~ 580, 
                            occ_hd_1980 == 329 ~ 325, 
                            TRUE ~ occ_hd_1980), 
    occ_wf_1980 = case_when(occ_wf_1980 == 600 ~ 580,
                            occ_wf_1980 == 329 ~ 325, 
                            TRUE ~ occ_wf_1980),
    # This variable gets the crosswalk row number for rows that match a 1970 value to a 2010 value
    occ.cw.position_hd_1980 = ifelse(is.na(occ_hd_1980), NA, match(occ_hd_1980, crosswalk.occ$occ1970, nomatch = NA)),
    occ.cw.position_wf_1980 = ifelse(is.na(occ_wf_1980), NA, match(occ_wf_1980, crosswalk.occ$occ1970, nomatch = NA)),
    # This variable codes a 2010 value to the 2010 value in the crosswalk that matches the crosswalk's row number
    # Note: the crosswalk was edited to match "0" with "99998". Zero values indicate that the respondent was not 
    # eligible for the PSID's retroactive recoding. While we could code these values as one of the missing codes, 
    # I keep a distinct code to easily identify individuals who weren't eligible for the retrospective recode in 1980
    occ2010_hd_1980 = ifelse(is.na(occ_hd_1980), NA, crosswalk.occ$occ2010[occ.cw.position_hd_1980]),
    occ2010_wf_1980 = ifelse(is.na(occ_wf_1980), NA, crosswalk.occ$occ2010[occ.cw.position_wf_1980]),
    # For 1981-2001, the PSID reports information on occupation, industry, and other job characteristics
    # separately based on respondent's employment status at the point of the interview. Because our sample restrictions 
    # are based on hours worked and wages earned in the year prior to the interview, some individuals meet our criteria 
    # for sample inclusion (working for wages full-time during the prior year) but might report being unemployed, retired,
    # or otherwise out of the labor force by the time of interview. We include these individuals in our sample using 
    # the information recorded for individuals' current job or most recent job. For subsequent years, the PSID reports
    # information on current or most recent job jointly
    empstat_hd_1981 = V7706,
    occ_hd_e_1981 = ifelse(V7712 == 600, 580, V7712), 
    occ_hd_e_1981 = na_codes(occ_hd_e_1981, c(999, 0)),
    occ_hd_u_1981 = ifelse(V7807 == 600, 580, V7807), 
    occ_hd_u_1981 = na_codes(occ_hd_u_1981, c(999, 0)),
    occ_hd_r_1981 = ifelse(V7866 == 600, 580, V7866), 
    occ_hd_r_1981 = na_codes(occ_hd_r_1981, c(999, 0)),
    # This creates a single occ variable which selects the appropriate occupation code based 
    # on the respondents' reported employment status
    occ_hd_1981 = case_when(empstat_hd_1981 %in% c(1,2) ~ occ_hd_e_1981,
                            empstat_hd_1981 == 3 ~ occ_hd_u_1981,
                            empstat_hd_1981 == 4 ~ occ_hd_r_1981),
    # For 1981-2001, occupation values are coded into 1970 census codes. We repeat the same procedure as above to create a 
    # variable that recodes 1970 census occupations observed in 1981 to 2010 census occupations in 1981 using the IPUMS crosswalk.
    # We use this single occ variable based on the respondents' employment status to create the harmonized occupation
    # code according to the OCC2010 IPUMS scheme. Using an IPUMS crosswalk, we match this occ variable (coded according
    # to the 1970 census scheme) to its position in the 1970 column in the IPUMS crosswalk 
    occ.cw.position_hd_1981 = ifelse(is.na(occ_hd_1981), NA, match(occ_hd_1981, crosswalk.occ$occ1970, nomatch = NA)),
    # We then use this position in the crosswalk to recode the 1970 occ scheme value to the IPUMS OCC2010 scheme value
    occ2010_hd_1981 = ifelse(is.na(occ_hd_1981 ), NA, crosswalk.occ$occ2010[occ.cw.position_hd_1981]),
    # We use the same procedure to code wives' occupations 
    empstat_wf_1981 = V7879,
    occ_wf_e_1981 = ifelse(V7885 == 600, 580, V7885), 
    occ_wf_e_1981 = na_codes(occ_wf_e_1981, c(999, 0)),
    occ_wf_u_1981 = ifelse(V7920 == 600, 580, V7920), 
    occ_wf_u_1981 = na_codes(occ_wf_u_1981, c(999, 0)),
    occ_wf_r_1981 = ifelse(V7943 == 600, 580, V7943), 
    occ_wf_r_1981 = na_codes(occ_wf_r_1981, c(999, 0)),
    occ_wf_1981 = case_when(empstat_wf_1981 %in% c(1,2) ~ occ_wf_e_1981,
                            empstat_wf_1981 == 3 ~ occ_wf_u_1981,
                            empstat_wf_1981 == 4 ~ occ_wf_r_1981),
    occ.cw.position_wf_1981 = ifelse(is.na(occ_wf_1981), NA, match(occ_wf_1981, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_wf_1981 = ifelse(is.na(occ_wf_1981), NA, crosswalk.occ$occ2010[occ.cw.position_wf_1981]),
    empstat_hd_1990 = V18093,
    occ_hd_e_1990 = ifelse(V18101 == 600, 580, V18101), 
    occ_hd_e_1990 = na_codes(occ_hd_e_1990, c(999, 0)),
    occ_hd_u_1990 = ifelse(V18262 == 600, 580, V18262), 
    occ_hd_u_1990 = na_codes(occ_hd_u_1990, c(999, 0)),
    occ_hd_1990 = case_when(empstat_hd_1990 %in% c(1,2) ~ occ_hd_e_1990,
                            empstat_hd_1990 == 3 ~ occ_hd_u_1990,
                            empstat_hd_1990 == 4 ~ occ_hd_u_1990),
    occ.cw.position_hd_1990 = ifelse(is.na(occ_hd_1990), NA, match(occ_hd_1990, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_hd_1990 = ifelse(is.na(occ_hd_1990 ), NA, crosswalk.occ$occ2010[occ.cw.position_hd_1990]),
    empstat_wf_1990 = V18395,
    occ_wf_e_1990 = ifelse(V18403 == 600, 580, V18403), 
    occ_wf_e_1990 = na_codes(occ_wf_e_1990, c(999, 0)),
    occ_wf_u_1990 = ifelse(V18564 == 600, 580, V18564), 
    occ_wf_u_1990 = na_codes(occ_wf_u_1990, c(999, 0)),
    occ_wf_1990 = case_when(empstat_wf_1990 %in% c(1,2) ~ occ_wf_e_1990,
                            empstat_wf_1990 == 3 ~ occ_wf_u_1990,
                            empstat_wf_1990 == 4 ~ occ_wf_u_1990),
    occ.cw.position_wf_1990 = ifelse(is.na(occ_wf_1990), NA, match(occ_wf_1990, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_wf_1990 = ifelse(is.na(occ_wf_1990), NA, crosswalk.occ$occ2010[occ.cw.position_wf_1990]),
    empstat_hd_1991 = V19393,
    occ_hd_e_1991 = ifelse(V19401 == 600, 580, V19401), 
    occ_hd_e_1991 = na_codes(occ_hd_e_1991, c(999, 0)),
    occ_hd_u_1991 = ifelse(V19562 == 600, 580, V19562), 
    occ_hd_u_1991 = na_codes(occ_hd_u_1991, c(999, 0)),
    occ_hd_1991 = case_when(empstat_hd_1991 %in% c(1,2) ~ occ_hd_e_1991,
                            empstat_hd_1991 == 3 ~ occ_hd_u_1991,
                            empstat_hd_1991 == 4 ~ occ_hd_u_1991),
    occ.cw.position_hd_1991 = ifelse(is.na(occ_hd_1991), NA, match(occ_hd_1991, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_hd_1991 = ifelse(is.na(occ_hd_1991 ), NA, crosswalk.occ$occ2010[occ.cw.position_hd_1991]),
    empstat_wf_1991 = V19695,
    occ_wf_e_1991 = ifelse(V19703 == 600, 580, V19703), 
    occ_wf_e_1991 = na_codes(occ_wf_e_1991, c(999, 0)),
    occ_wf_u_1991 = ifelse(V19864 == 600, 580, V19864), 
    occ_wf_u_1991 = na_codes(occ_wf_u_1991, c(999, 0)),
    occ_wf_1991 = case_when(empstat_wf_1991 %in% c(1,2) ~ occ_wf_e_1991,
                            empstat_wf_1991 == 3 ~ occ_wf_u_1991,
                            empstat_wf_1991 == 4 ~ occ_wf_u_1991),
    occ.cw.position_wf_1991 = ifelse(is.na(occ_wf_1991), NA, match(occ_wf_1991, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_wf_1991 = ifelse(is.na(occ_wf_1991), NA, crosswalk.occ$occ2010[occ.cw.position_wf_1991]),
    # For years 1999-2001, the PSID creates an individual-level variable for employment status- we use this individual
    # level employment status in conjunction with questions asked about head-employed/unemployed and wife-employed/unemployed
    # to determine heads and wives' occupation
    empstat_hd_1999 = ER33512, 
    occ_hd_e_1999 = ifelse(ER13215  == 600, 580, ER13215), 
    occ_hd_e_1999 = na_codes(occ_hd_e_1999, c(0, 999, 997)),
    occ_hd_u_1999 = ifelse(ER13493 == 600, 580, ER13493), 
    occ_hd_u_1999 = na_codes(occ_hd_u_1999, c(0, 999)),
    occ_hd_1999 = case_when(empstat_hd_1999 %in% c(1,2) ~ occ_hd_e_1999,
                            empstat_hd_1999 == 3 ~ occ_hd_u_1999,
                            empstat_hd_1999 == 4 ~ occ_hd_u_1999),
    occ.cw.position_hd_1999 = ifelse(is.na(occ_hd_1999), NA, match(occ_hd_1999, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_hd_1999 = ifelse(is.na(occ_hd_1999 ), NA, crosswalk.occ$occ2010[occ.cw.position_hd_1999]),
    empstat_wf_1999 = ER33512, # Note that this is the same as for heads, because this variable is coded at the individual level
    occ_wf_e_1999 = ifelse(ER13727 == 600, 580, ER13727), 
    occ_wf_e_1999 = na_codes(occ_wf_e_1999, c(0, 999)),
    occ_wf_u_1999 = ifelse(ER14005 == 600, 580, ER14005), 
    occ_wf_u_1999 = na_codes(occ_wf_u_1999, c(0, 999)),
    occ_wf_1999 = case_when(empstat_wf_1999 %in% c(1,2) ~ occ_wf_e_1999,
                            empstat_wf_1999 == 3 ~ occ_wf_u_1999,
                            empstat_wf_1999 == 4 ~ occ_wf_u_1999),
    occ.cw.position_wf_1999 = ifelse(is.na(occ_wf_1999), NA, match(occ_wf_1999, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_wf_1999 = ifelse(is.na(occ_wf_1999 ), NA, crosswalk.occ$occ2010[occ.cw.position_wf_1999]),
    empstat_hd_2001 = ER33612,
    occ_hd_e_2001 = ifelse(ER17226 == 600, 580, ER17226), 
    occ_hd_e_2001 = na_codes(occ_hd_e_2001, c(0, 999, 810)),
    occ_hd_u_2001 = ifelse(ER17533 == 600, 580, ER17533), 
    occ_hd_u_2001 = na_codes(occ_hd_u_2001, c(0, 999)),
    occ_hd_2001 = case_when(empstat_hd_2001 %in% c(1,2) ~ occ_hd_e_2001,
                            empstat_hd_2001 == 3 ~ occ_hd_u_2001,
                            empstat_hd_2001 == 4 ~ occ_hd_u_2001),
    occ.cw.position_hd_2001 = ifelse(is.na(occ_hd_2001), NA, match(occ_hd_2001, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_hd_2001 = ifelse(is.na(occ_hd_2001 ), NA, crosswalk.occ$occ2010[occ.cw.position_hd_2001]),
    empstat_wf_2001 = ER33612,
    occ_wf_e_2001 = ifelse(ER17796 == 600, 580, ER17796), 
    occ_wf_e_2001 = na_codes(occ_wf_e_2001, c(0, 999)),
    occ_wf_u_2001 = ifelse(ER18104 == 600, 580, ER18104), 
    occ_wf_u_2001 = na_codes(occ_wf_u_2001, c(0, 999)),
    occ_wf_2001 = case_when(empstat_wf_2001 %in% c(1,2) ~ occ_wf_e_2001,
                            empstat_wf_2001 == 3 ~ occ_wf_u_2001,
                            empstat_wf_2001 == 4 ~ occ_wf_u_2001),
    occ.cw.position_wf_2001 = ifelse(is.na(occ_wf_2001), NA, match(occ_wf_2001, crosswalk.occ$occ1970, nomatch = NA)),
    occ2010_wf_2001 = ifelse(is.na(occ_wf_2001 ), NA, crosswalk.occ$occ2010[occ.cw.position_wf_2001]),
    # Starting in 2003, the PSID switches to asking individuals about their current or most recent job, 
    # consolidating the employed/unemployed distinction for the occupation codes in one variable
    # For 2009-2015, we create variables that recode the 2000 census occupation codes to IPUMS' OCC2010 codes w/ the IPUMS crosswalk
    occ_hd_2009 = na_codes(ER42167, c(0, 999)), 
    occ.cw.position_hd_2009 = ifelse(is.na(occ_hd_2009), NA, match(occ_hd_2009, crosswalk.occ$occ2000, nomatch = NA)),
    occ2010_hd_2009 = ifelse(is.na(occ_hd_2009), NA, crosswalk.occ$occ2010[occ.cw.position_hd_2009]), 
    occ_wf_2009 = na_codes(ER42419, c(0, 999)), 
    occ.cw.position_wf_2009 = ifelse(is.na(occ_wf_2009), NA, match(occ_wf_2009, crosswalk.occ$occ2000, nomatch = NA)),
    occ2010_wf_2009 = ifelse(is.na(occ_wf_2009), NA, crosswalk.occ$occ2010[occ.cw.position_wf_2009]), 
    occ_hd_2011 = na_codes(ER47479, c(0, 999)), 
    occ.cw.position_hd_2011 = ifelse(is.na(occ_hd_2011), NA, match(occ_hd_2011, crosswalk.occ$occ2000, nomatch = NA)),
    occ2010_hd_2011 = ifelse(is.na(occ_hd_2011), NA, crosswalk.occ$occ2010[occ.cw.position_hd_2011]), 
    occ_wf_2011 = na_codes(ER47736, c(0, 999)), 
    occ.cw.position_wf_2011 = ifelse(is.na(occ_wf_2011), NA, match(occ_wf_2011, crosswalk.occ$occ2000, nomatch = NA)),
    occ2010_wf_2011 = ifelse(is.na(occ_wf_2011), NA, crosswalk.occ$occ2010[occ.cw.position_wf_2011]), 
    occ_hd_2015 = na_codes(ER60194, c(0, 999)), 
    occ.cw.position_hd_2015 = ifelse(is.na(occ_hd_2015), NA, match(occ_hd_2015, crosswalk.occ$occ2000, nomatch = NA)),
    occ2010_hd_2015 = ifelse(is.na(occ_hd_2015), NA, crosswalk.occ$occ2010[occ.cw.position_hd_2015]), 
    occ_wf_2015 = na_codes(ER60457, c(0, 999)), 
    occ.cw.position_wf_2015 = ifelse(is.na(occ_wf_2015), NA, match(occ_wf_2015, crosswalk.occ$occ2000, nomatch = NA)),
    occ2010_wf_2015 = ifelse(is.na(occ_wf_2015), NA, crosswalk.occ$occ2010[occ.cw.position_wf_2015]), 
    # For 2017 & 2019, we create variables that recode the 2010 census occupation codes to IPUMS-generated 2010 occ codes 
    # w/ the IPUMS provided crosswalk. The IPUMS 2010 occ codes are slightly different from the 2010 census occ codes
    # in the interest of harmonizing changes to achieve the most consistent occupation codes over time.
    # For more information on IPUMS' OCC2010 codes and their relationship to the census 2010 codes, see
    # the following page: 
    # https://usa.ipums.org/usa-action/variables/OCC2010#description_section
    occ_hd_2017 = na_codes(ER66195, c(0, 9999)), 
    occ.cw.position_hd_2017 = ifelse(is.na(occ_hd_2017), NA, match(occ_hd_2017, crosswalk.occ$acs2010, nomatch = NA)),
    occ2010_hd_2017 = ifelse(is.na(occ_hd_2017), NA, crosswalk.occ$occ2010[occ.cw.position_hd_2017]), 
    occ_wf_2017 = na_codes(ER66470, c(0, 9999)), 
    occ.cw.position_wf_2017 = ifelse(is.na(occ_wf_2017), NA, match(occ_wf_2017, crosswalk.occ$acs2010, nomatch = NA)),
    occ2010_wf_2017 = ifelse(is.na(occ_wf_2017), NA, crosswalk.occ$occ2010[occ.cw.position_wf_2017]), 
    occ_hd_2019 = na_codes(ER72195, c(0, 9999)), 
    occ.cw.position_hd_2019 = ifelse(is.na(occ_hd_2019), NA, match(occ_hd_2019, crosswalk.occ$acs2010, nomatch = NA)),
    occ2010_hd_2019 = ifelse(is.na(occ_hd_2019), NA, crosswalk.occ$occ2010[occ.cw.position_hd_2019]), 
    occ_wf_2019 = na_codes(ER72472, c(0, 9999)), 
    occ.cw.position_wf_2019 = ifelse(is.na(occ_wf_2019), NA, match(occ_wf_2019, crosswalk.occ$acs2010, nomatch = NA)),
    occ2010_wf_2019 = ifelse(is.na(occ_wf_2019), NA, crosswalk.occ$occ2010[occ.cw.position_wf_2019]),
    # Industry variables- as with occupation, we use the recoded industry values (1980 PSID industry codes recoded 
    # to 1970 census industry codes) for those eligible for retroactive recoding by the PSID
    # First removing missing values
    ind_hd_1980 = na_codes(V7101_A, c(0, 999)), 
    ind_wf_1980 = na_codes(V7199_A, c(0, 999)), 
    # Similar to occupation, we use a crosswalk to generate variables that recode the 1970 census industry codes
    # to 1990 census industry codes
    ind.cw.position_hd_1980 = ifelse(is.na(ind_hd_1980), NA, match(ind_hd_1980, crosswalk.ind$ind1970, nomatch = NA)),
    ind.cw.position_wf_1980 = ifelse(is.na(ind_wf_1980), NA, match(ind_wf_1980, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_hd_1980 = ifelse(is.na(ind_hd_1980), NA, crosswalk.ind$ind1990[ind.cw.position_hd_1980]),
    ind1990_wf_1980 = ifelse(is.na(ind_wf_1980), NA, crosswalk.ind$ind1990[ind.cw.position_wf_1980]),
    # For 1981-2001, we use information on industry for those employed or unemployed/retired, assigning according
    # to reported employment status
    # Industry values in 1981-2001 were coded according to the 1970 census scheme. As with occupation codes, 
    # we use census and IPUMS crosswalks to convert these 1970 census values to a harmonized scheme provided 
    # by IPUMS (IND 1990 codes).
    ind_hd_e_1981 =  na_codes(V7713, c(0, 999)), 
    ind_hd_u_1981 =  na_codes(V7808, c(0, 999)),
    ind_hd_r_1981 =  na_codes(V7867, c(0, 999)), 
    ind_hd_1981 = case_when(empstat_hd_1981 %in% c(1,2) ~ ind_hd_e_1981,
                            empstat_hd_1981 == 3 ~ ind_hd_u_1981,
                            empstat_hd_1981 == 4 ~ ind_hd_r_1981),
    ind.cw.position_hd_1981 = ifelse(is.na(ind_hd_1981), NA, match(ind_hd_1981, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_hd_1981 = ifelse(is.na(ind_hd_1981), NA, crosswalk.ind$ind1990[ind.cw.position_hd_1981]),
    ind_wf_e_1981 = na_codes(V7886, c(0, 999)),
    ind_wf_u_1981 =  na_codes(V7921, c(0, 999)),
    ind_wf_r_1981 =  na_codes(V7944, c(0, 999)), 
    ind_wf_1981 = case_when(empstat_wf_1981 %in% c(1,2) ~ ind_wf_e_1981,
                            empstat_wf_1981 == 3 ~ ind_wf_u_1981,
                            empstat_wf_1981 == 4 ~ ind_wf_r_1981),
    ind.cw.position_wf_1981 = ifelse(is.na(ind_wf_1981), NA, match(ind_wf_1981, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_wf_1981 = ifelse(is.na(ind_wf_1981), NA, crosswalk.ind$ind1990[ind.cw.position_wf_1981]),
    ind_hd_e_1990 =  na_codes(V18102, c(0, 999)), 
    ind_hd_u_1990 =  na_codes(V18262, c(0, 999)), 
    ind_hd_1990 = case_when(empstat_hd_1990 %in% c(1,2) ~ ind_hd_e_1990,
                            empstat_hd_1990 %in% c(3, 4) ~ ind_hd_u_1990),
    ind.cw.position_hd_1990 = ifelse(is.na(ind_hd_1990), NA, match(ind_hd_1990, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_hd_1990 = ifelse(is.na(ind_hd_1990), NA, crosswalk.ind$ind1990[ind.cw.position_hd_1990]),
    ind_wf_e_1990  = na_codes(V18404, c(0, 999)),
    ind_wf_u_1990  = na_codes(V18564, c(0, 999)),
    ind_wf_1990 = case_when(empstat_wf_1990 %in% c(1,2) ~ ind_wf_e_1990,
                            empstat_wf_1990 %in% c(3, 4) ~ ind_wf_u_1990),
    ind.cw.position_wf_1990 = ifelse(is.na(ind_wf_1990), NA, match(ind_wf_1990, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_wf_1990 = ifelse(is.na(ind_wf_1990), NA, crosswalk.ind$ind1990[ind.cw.position_wf_1990]),
    ind_hd_e_1991 =  na_codes(V19402, c(0, 999)), 
    ind_hd_u_1991 =  na_codes(V19562, c(0, 999)), 
    ind_hd_1991 = case_when(empstat_hd_1991 %in% c(1,2) ~ ind_hd_e_1991,
                            empstat_hd_1991 %in% c(3, 4) ~ ind_hd_u_1991),
    ind.cw.position_hd_1991 = ifelse(is.na(ind_hd_1991), NA, match(ind_hd_1991, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_hd_1991 = ifelse(is.na(ind_hd_1991), NA, crosswalk.ind$ind1990[ind.cw.position_hd_1991]),
    ind_wf_e_1991 = na_codes(V19704, c(0, 999)),
    ind_wf_u_1991 = na_codes(V19864, c(0, 999)),
    ind_wf_1991 = case_when(empstat_wf_1991 %in% c(1,2) ~ ind_wf_e_1991,
                            empstat_wf_1991 %in% c(3, 4) ~ ind_wf_u_1991),
    ind.cw.position_wf_1991 = ifelse(is.na(ind_wf_1991), NA, match(ind_wf_1991, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_wf_1991 = ifelse(is.na(ind_wf_1991), NA, crosswalk.ind$ind1990[ind.cw.position_wf_1991]),
    ind_hd_e_1999 =  na_codes(ER13216, c(0, 999, 503)), 
    ind_hd_u_1999 =  na_codes(ER13494, c(0, 999)), 
    ind_hd_1999 = case_when(empstat_hd_1999 %in% c(1,2) ~ ind_hd_e_1999,
                            empstat_hd_1999 %in% c(3, 4) ~ ind_hd_u_1999),
    ind.cw.position_hd_1999 = ifelse(is.na(ind_hd_1999), NA, match(ind_hd_1999, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_hd_1999 = ifelse(is.na(ind_hd_1999), NA, crosswalk.ind$ind1990[ind.cw.position_hd_1999]),
    ind_wf_e_1999 = na_codes(ER13728, c(0, 999)),
    ind_wf_u_1999 = na_codes(ER14006, c(0, 999)),
    ind_wf_1999 = case_when(empstat_wf_1999 %in% c(1,2) ~ ind_wf_e_1999,
                            empstat_wf_1999 %in% c(3, 4) ~ ind_wf_u_1999),
    ind.cw.position_wf_1999 = ifelse(is.na(ind_wf_1999), NA, match(ind_wf_1999, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_wf_1999 = ifelse(is.na(ind_wf_1999), NA, crosswalk.ind$ind1990[ind.cw.position_wf_1999]),
    ind_hd_e_2001 =  na_codes(ER17227, c(0, 999)), 
    ind_hd_u_2001 =  na_codes(ER17534, c(0, 999)), 
    ind_hd_2001 = case_when(empstat_hd_2001 %in% c(1,2) ~ ind_hd_e_2001,
                            empstat_hd_2001 %in% c(3, 4) ~ ind_hd_u_2001),
    ind.cw.position_hd_2001 = ifelse(is.na(ind_hd_2001), NA, match(ind_hd_2001, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_hd_2001 = ifelse(is.na(ind_hd_2001), NA, crosswalk.ind$ind1990[ind.cw.position_hd_2001]),
    ind_wf_e_2001 = na_codes(ER17797, c(0, 999)),
    ind_wf_u_2001 = na_codes(ER18104, c(0, 999)),
    ind_wf_2001 = case_when(empstat_wf_2001 %in% c(1,2) ~ ind_wf_e_2001,
                            empstat_wf_2001 %in% c(3, 4) ~ ind_wf_u_2001),
    ind.cw.position_wf_2001 = ifelse(is.na(ind_wf_2001), NA, match(ind_wf_2001, crosswalk.ind$ind1970, nomatch = NA)),
    ind1990_wf_2001 = ifelse(is.na(ind_wf_2001), NA, crosswalk.ind$ind1990[ind.cw.position_wf_2001]),
    # For 2009-2015: We use reported industry for main/ most recent job. 
    # For these years, the PSID uses industry codes from the 2000 Census scheme: again, we use
    # the provided crosswalks to recode these industry codes into the harmonized IND 1990 scheme
    # Two industry values in the PSID have no match in the crosswalk nor any matches in any 
    # codebooks we could find for Industry 2000
    # One value is 120 (2 respondents, both male) and falls within the non-durable manufacturing range. 
    # We recode this value to 127, also in the non-durable manufacturing range. 
    # Another value is 775 (5 respondents, all female)  and falls within the Management, Administrative and Support,
    # and Waste Management Services range: we recode this value to 779, which is coded as sanitary services
    ind_hd_2009 = na_codes(ER42168, c(0, 999)), ind_wf_2009 = na_codes(ER42420, c(0, 999)), 
    ind_hd_2009 = ifelse(ind_hd_2009 == 120, 127, ind_hd_2009), ind_wf_2009 = ifelse(ind_wf_2009 == 775, 779, ind_wf_2009), 
    ind.cw.position_hd_2009 = ifelse(is.na(ind_hd_2009), NA, match(ind_hd_2009, crosswalk.ind$ind2000, nomatch = NA)),
    ind.cw.position_wf_2009 = ifelse(is.na(ind_wf_2009), NA, match(ind_wf_2009, crosswalk.ind$ind2000, nomatch = NA)),
    ind1990_hd_2009 = ifelse(is.na(ind_hd_2009), NA, crosswalk.ind$ind1990[ind.cw.position_hd_2009]), 
    ind1990_wf_2009 = ifelse(is.na(ind_wf_2009), NA, crosswalk.ind$ind1990[ind.cw.position_wf_2009]),
    ind_hd_2011 = na_codes(ER47480, c(0, 999)), ind_wf_2011 = na_codes(ER47737, c(0, 999)), 
    ind_hd_2011 = ifelse(ind_hd_2011 == 120, 127, ind_hd_2011), ind_wf_2011 = ifelse(ind_wf_2011 == 775, 779, ind_wf_2011), 
    ind.cw.position_hd_2011 = ifelse(is.na(ind_hd_2011), NA, match(ind_hd_2011, crosswalk.ind$ind2000, nomatch = NA)),
    ind.cw.position_wf_2011 = ifelse(is.na(ind_wf_2011), NA, match(ind_wf_2011, crosswalk.ind$ind2000, nomatch = NA)),
    ind1990_hd_2011 = ifelse(is.na(ind_hd_2011), NA, crosswalk.ind$ind1990[ind.cw.position_hd_2011]), 
    ind1990_wf_2011 = ifelse(is.na(ind_wf_2011), NA, crosswalk.ind$ind1990[ind.cw.position_wf_2011]),
    ind_hd_2015 = na_codes(ER60195, c(0, 999)), ind_wf_2015 = na_codes(ER60458, c(0, 999)), 
    ind_hd_2015 = ifelse(ind_hd_2015 == 120, 127, ind_hd_2015), ind_wf_2015 = ifelse(ind_wf_2015 == 775, 779, ind_wf_2015), 
    ind.cw.position_hd_2015 = ifelse(is.na(ind_hd_2015), NA, match(ind_hd_2015, crosswalk.ind$ind2000, nomatch = NA)),
    ind.cw.position_wf_2015 = ifelse(is.na(ind_wf_2015), NA, match(ind_wf_2015, crosswalk.ind$ind2000, nomatch = NA)),
    ind1990_hd_2015 = ifelse(is.na(ind_hd_2015), NA, crosswalk.ind$ind1990[ind.cw.position_hd_2015]), 
    ind1990_wf_2015 = ifelse(is.na(ind_wf_2015), NA, crosswalk.ind$ind1990[ind.cw.position_wf_2015]),
    # For 2017-2019, the PSID uses a four-digit 2012 Census Detailed Industry Code.
    # We use the provided crosswalks to recode these industry codes into the harmonized IND 1990 scheme
    ind_hd_2017 = na_codes(ER66196, c(0, 9999)), ind_wf_2017 = na_codes(ER66471, c(0, 9999)), 
    ind.cw.position_hd_2017 = ifelse(is.na(ind_hd_2017), NA, match(ind_hd_2017, crosswalk.ind.2012$ind2012, nomatch = NA)), 
    ind.cw.position_wf_2017 = ifelse(is.na(ind_wf_2017), NA, match(ind_wf_2017, crosswalk.ind.2012$ind2012, nomatch = NA)), 
    ind1990_hd_2017 = ifelse(is.na(ind_hd_2017), NA, crosswalk.ind.2012$ind1990[ind.cw.position_hd_2017]),
    ind1990_wf_2017 = ifelse(is.na(ind_wf_2017), NA, crosswalk.ind.2012$ind1990[ind.cw.position_wf_2017]),
    ind_hd_2019 = na_codes(ER72196, c(0, 9999)), ind_wf_2019 = na_codes(ER72473, c(0, 9999)), 
    ind.cw.position_hd_2019 = ifelse(is.na(ind_hd_2019), NA, match(ind_hd_2019, crosswalk.ind.2012$ind2012, nomatch = NA)), 
    ind.cw.position_wf_2019 = ifelse(is.na(ind_wf_2019), NA, match(ind_wf_2019, crosswalk.ind.2012$ind2012, nomatch = NA)), 
    ind1990_hd_2019 = ifelse(is.na(ind_hd_2019), NA, crosswalk.ind.2012$ind1990[ind.cw.position_hd_2019]),
    ind1990_wf_2019 = ifelse(is.na(ind_wf_2019), NA, crosswalk.ind.2012$ind1990[ind.cw.position_wf_2019]),
    # Unionization variables - 9 and 8 are implicitly coded as missing
    union_hd_1980 = case_when(V7098 == 1 ~ 1, V7098 == 5 ~ 0), union_wf_1980 = case_when(V7196 == 1 ~ 1, V7196 == 5 ~ 0), 
    union_hd_1981 = case_when(V7709 == 1 ~ 1, V7709 == 5 ~ 0), union_wf_1981 = case_when(V7882 == 1 ~ 1, V7882 == 5 ~ 0),
    union_hd_1990 = case_when(V18099 == 1 ~ 1, V18099 == 5 ~ 0), union_wf_1990 = case_when(V18401 == 1 ~ 1, V18401 == 5 ~ 0),
    union_hd_1991 = case_when(V19399 == 1 ~ 1, V19399 == 5 ~ 0), union_wf_1991 = case_when(V19701 == 1 ~ 1, V19701 == 5 ~ 0),
    union_hd_1999 = case_when(ER13213 == 1 ~ 1, ER13213 == 5 ~ 0), union_wf_1999 = case_when(ER13725 == 1 ~ 1, ER13725 == 5 ~ 0),
    union_hd_2001 = case_when(ER17224 == 1 ~ 1, ER17224 == 5 ~ 0), union_wf_2001 = case_when(ER17794 == 1 ~ 1, ER17794 == 5 ~ 0),
    union_hd_2009 = case_when(ER42178 == 1 ~ 1, ER42178 == 5 ~ 0), union_wf_2009 = case_when(ER42430 == 1 ~ 1, ER42430 == 5 ~ 0),
    union_hd_2011 = case_when(ER47491 == 1 ~ 1, ER47491 == 5 ~ 0), union_wf_2011 = case_when(ER47748 == 1 ~ 1, ER47748 == 5 ~ 0),
    union_hd_2015 = case_when(ER60206 == 1 ~ 1, ER60206 == 5 ~ 0), union_wf_2015 = case_when(ER60469 == 1 ~ 1, ER60469 == 5 ~ 0),
    union_hd_2017 = case_when(ER66207 == 1 ~ 1, ER66207 == 5 ~ 0), union_wf_2017 = case_when(ER66482 == 1 ~ 1, ER66482 == 5 ~ 0),
    union_hd_2019 = case_when(ER72207 == 1 ~ 1, ER72207 == 5 ~ 0), union_wf_2019 = case_when(ER72484 == 1 ~ 1, ER72484 == 5 ~ 0),
    # Self-employment variables: R is coded as self-employed if they report being self-employed in the main job
    self.emp_hd_1980 = case_when(V7096 %in% c(2, 3) ~ 1, V7096 == 1 ~ 0), self.emp_wf_1980 = case_when(V7194 %in% c(2, 3) ~ 1, V7194 == 1 ~ 0),
    self.emp_hd_1981 = case_when(V7707 %in% c(2, 3) ~ 1, V7707 == 1 ~ 0), self.emp_wf_1981 = case_when(V7880 %in% c(2, 3) ~ 1, V7880 == 1 ~ 0),
    # For 1990-2001, we use information on self-employment for those employed or unemployed/retired, assigning according
    # to reported employment status
    self.emp_hd_e_1990 = case_when(V18096 %in% c(2, 3) ~ 1, V18096 == 1 ~ 0), 
    self.emp_hd_u_1990 = case_when(V18264 %in% c(2, 3) ~ 1, V18264 == 1 ~ 0),
    self.emp_hd_1990 = case_when(empstat_hd_1990 %in% c(1,2) ~ self.emp_hd_e_1990,
                                 empstat_hd_1990 %in% c(3, 4)  ~ self.emp_hd_u_1990),
    self.emp_wf_e_1990 = case_when(V18398 %in% c(2, 3) ~ 1, V18398 == 1 ~ 0),
    self.emp_wf_u_1990 = case_when(V18566 %in% c(2, 3) ~ 1, V18566 == 1 ~ 0),
    self.emp_wf_1990 = case_when(empstat_wf_1990 %in% c(1,2) ~ self.emp_wf_e_1990,
                                 empstat_wf_1990 %in% c(3, 4)  ~ self.emp_wf_u_1990),
    self.emp_hd_e_1991 = case_when(V19396 %in% c(2, 3) ~ 1, V19396 == 1 ~ 0),
    self.emp_hd_u_1991 = case_when(V19564 %in% c(2, 3) ~ 1, V19564 == 1 ~ 0),
    self.emp_hd_1991 = case_when(empstat_hd_1991 %in% c(1,2) ~ self.emp_hd_e_1991,
                                 empstat_hd_1991 %in% c(3, 4)  ~ self.emp_hd_u_1991),
    self.emp_wf_e_1991 = case_when(V19698 %in% c(2, 3) ~ 1, V19698 == 1 ~ 0),
    self.emp_wf_u_1991 = case_when(V19866 %in% c(2, 3) ~ 1, V19866 == 1 ~ 0),
    self.emp_wf_1991 = case_when(empstat_wf_1991 %in% c(1,2) ~ self.emp_wf_e_1991,
                                 empstat_wf_1991 %in% c(3, 4)  ~ self.emp_wf_u_1991),
    self.emp_hd_e_1999 = case_when(ER13210 %in% c(2, 3) ~ 1, ER13210 == 1 ~ 0), 
    self.emp_hd_u_1999 = case_when(ER13495 %in% c(2, 3) ~ 1, ER13495 == 1 ~ 0), 
    self.emp_hd_1999 = case_when(empstat_hd_1999 %in% c(1,2) ~ self.emp_hd_e_1999,
                                 empstat_hd_1999 %in% c(3, 4)  ~ self.emp_hd_u_1999),
    self.emp_wf_e_1999 = case_when(ER13722 %in% c(2, 3) ~ 1, ER13722 == 1 ~ 0),
    self.emp_wf_u_1999 = case_when(ER14007 %in% c(2, 3) ~ 1, ER14007 == 1 ~ 0),
    self.emp_wf_1999 = case_when(empstat_wf_1999 %in% c(1,2) ~ self.emp_wf_e_1999,
                                 empstat_wf_1999 %in% c(3, 4)  ~ self.emp_wf_u_1999),
    self.emp_hd_e_2001 = case_when(ER17221 %in% c(2, 3) ~ 1, ER17221 == 1 ~ 0), 
    self.emp_hd_u_2001 = case_when(ER17585 %in% c(2, 3) ~ 1, ER17585 == 1 ~ 0), 
    self.emp_hd_2001 = case_when(empstat_hd_2001 %in% c(1,2) ~ self.emp_hd_e_2001,
                                 empstat_hd_2001 %in% c(3, 4)  ~ self.emp_hd_u_2001),
    self.emp_wf_e_2001 = case_when(ER17791 %in% c(2, 3) ~ 1, ER17791 == 1 ~ 0),
    self.emp_wf_u_2001 = case_when(ER18156 %in% c(2, 3) ~ 1, ER18156 == 1 ~ 0),
    self.emp_wf_2001 = case_when(empstat_wf_2001 %in% c(1,2) ~ self.emp_wf_e_2001,
                                 empstat_wf_2001 %in% c(3, 4)  ~ self.emp_wf_u_2001),
    # # Starting in 2003, the PSID switches to asking individuals about their current or most recent job, 
    # consolidating the employed/unemployed distinction for the self-employed questions
    self.emp_hd_2009 = case_when(ER42169 %in% c(2,3) ~ 1, ER42169 == 1 ~ 0), self.emp_wf_2009 = case_when(ER42421 %in% c(2,3) ~ 1, ER42421 == 1 ~ 0), 
    self.emp_hd_2011 = case_when(ER47482 %in% c(2,3) ~ 1, ER47482 == 1 ~ 0), self.emp_wf_2011 = case_when(ER47739 %in% c(2,3) ~ 1, ER47739 == 1 ~ 0), 
    self.emp_hd_2015 = case_when(ER60197 %in% c(2,3) ~ 1, ER60197 == 1 ~ 0), self.emp_wf_2015 = case_when(ER60460 %in% c(2,3) ~ 1, ER60460 == 1 ~ 0),
    self.emp_hd_2017 = case_when(ER66198 %in% c(2,3) ~ 1, ER66198 == 1 ~ 0), self.emp_wf_2017 = case_when(ER66473 %in% c(2,3) ~ 1, ER66473 == 1 ~ 0),
    self.emp_hd_2019 = case_when(ER72198 %in% c(2,3) ~ 1, ER72198 == 1 ~ 0), self.emp_wf_2019 = case_when(ER72475 %in% c(2,3) ~ 1, ER72475 == 1 ~ 0),
    # Government Job: Using main job reports
    govt.job_hd_1980 = case_when(V7097 == 1 ~ 1, V7097 == 5 ~ 0), govt.job_wf_1980 = case_when(V7195 == 1 ~ 1, V7195 == 5 ~ 0),
    govt.job_hd_1981 = case_when(V7708 == 1 ~ 1, V7708 == 5 ~ 0), govt.job_wf_1981 = case_when(V7881 == 1 ~ 1, V7881 == 5 ~ 0),
    # For 1990-2001, we use information on whether the job is a government job for those employed or unemployed/retired,
    # assigning according to reported employment status
    govt.job_hd_e_1990 = case_when(V18098 %in% c(1,2,3) ~ 1, V18098 %in% c(4,7) ~ 0), 
    govt.job_hd_u_1990 = case_when(V18266 %in% c(1,2,3) ~ 1, V18266 %in% c(4,7) ~ 0), 
    govt.job_hd_1990 = case_when(empstat_hd_1990 %in% c(1,2) ~ govt.job_hd_e_1990,
                                 empstat_hd_1990 %in% c(3, 4)  ~ govt.job_hd_u_1990),
    govt.job_wf_e_1990 = case_when(V18400 %in% c(1,2,3) ~ 1, V18400 %in% c(4,7) ~ 0),
    govt.job_wf_u_1990 = case_when(V18568 %in% c(1,2,3) ~ 1, V18568 %in% c(4,7) ~ 0),
    govt.job_wf_1990 = case_when(empstat_wf_1990 %in% c(1,2) ~ govt.job_wf_e_1990,
                                 empstat_wf_1990 %in% c(3, 4)  ~ govt.job_wf_u_1990),
    govt.job_hd_e_1991 = case_when(V19398 %in% c(1,2,3) ~ 1, V19398 %in% c(4,7) ~ 0),
    govt.job_hd_u_1991 = case_when(V19566 %in% c(1,2,3) ~ 1, V19566 %in% c(4,7) ~ 0), 
    govt.job_hd_1991 = case_when(empstat_hd_1991 %in% c(1,2) ~ govt.job_hd_e_1991,
                                 empstat_hd_1991 %in% c(3, 4)  ~ govt.job_hd_u_1991),
    govt.job_wf_e_1991 = case_when(V19700 %in% c(1,2,3) ~ 1, V19700 %in% c(4,7) ~ 0),
    govt.job_wf_u_1991 = case_when(V19868 %in% c(1,2,3) ~ 1, V19868 %in% c(4,7) ~ 0),
    govt.job_wf_1991 = case_when(empstat_wf_1991 %in% c(1,2) ~ govt.job_wf_e_1991,
                                 empstat_wf_1991 %in% c(3, 4)  ~ govt.job_wf_u_1991),
    govt.job_hd_e_1999 = case_when(ER13212 %in% c(1,2,3) ~ 1, ER13212 %in% c(4,7) ~ 0),
    govt.job_hd_u_1999 = case_when(ER13497 %in% c(1,2,3) ~ 1, ER13497 %in% c(4,7) ~ 0),
    govt.job_hd_1999 = case_when(empstat_hd_1999 %in% c(1,2) ~ govt.job_hd_e_1999,
                                 empstat_hd_1999 %in% c(3, 4)  ~ govt.job_hd_u_1999),
    govt.job_wf_e_1999 = case_when(ER13724 %in% c(1,2,3) ~ 1, ER13724 %in% c(4,7) ~ 0),
    govt.job_wf_u_1999 = case_when(ER14009 %in% c(1,2,3) ~ 1, ER14009 %in% c(4,7) ~ 0),
    govt.job_wf_1999 = case_when(empstat_wf_1999 %in% c(1,2) ~ govt.job_wf_e_1999,
                                 empstat_wf_1999 %in% c(3, 4)  ~ govt.job_wf_u_1999),
    govt.job_hd_e_2001 = case_when(ER17223 %in% c(1,2,3) ~ 1, ER17223 %in% c(4,7) ~ 0),
    govt.job_hd_u_2001 = case_when(ER17537 %in% c(1,2,3) ~ 1, ER17537 %in% c(4,7) ~ 0),
    govt.job_hd_2001 = case_when(empstat_hd_2001 %in% c(1,2) ~ govt.job_hd_e_2001,
                                 empstat_hd_2001 %in% c(3, 4)  ~ govt.job_hd_u_2001),
    govt.job_wf_e_2001 = case_when(ER17793 %in% c(1,2,3) ~ 1, ER17793 %in% c(4,7) ~ 0),
    govt.job_wf_u_2001 = case_when(ER18108 %in% c(1,2,3) ~ 1, ER18108 %in% c(4,7) ~ 0),
    govt.job_wf_2001 = case_when(empstat_wf_2001 %in% c(1,2) ~ govt.job_wf_e_2001,
                                 empstat_wf_2001 %in% c(3, 4)  ~ govt.job_wf_u_2001),
    # # Starting in 2003, the PSID switches to asking individuals about their current or most recent job, 
    # consolidating the employed/unemployed distinction for the government job questions
    govt.job_hd_2009 = case_when(ER42171 %in% c(1,2,3) ~ 1, ER42171 %in% c(4,7) ~ 0),
    govt.job_wf_2009 = case_when(ER42423 %in% c(1,2,3) ~ 1, ER42423 %in% c(4,7) ~ 0),
    govt.job_hd_2011 = case_when(ER47484 %in% c(1,2,3) ~ 1, ER47484 %in% c(4,7) ~ 0),
    govt.job_wf_2011 = case_when(ER47741 %in% c(1,2,3) ~ 1, ER47741 %in% c(4,7) ~ 0),
    govt.job_hd_2015 = case_when(ER60199 %in% c(1,2,3) ~ 1, ER60199 %in% c(4,7) ~ 0),
    govt.job_wf_2015 = case_when(ER60462 %in% c(1,2,3) ~ 1, ER60462 %in% c(4,7) ~ 0), 
    govt.job_hd_2017 = case_when(ER66200 %in% c(1,2,3) ~ 1, ER66200 %in% c(4,7) ~ 0), 
    govt.job_wf_2017 = case_when(ER66475 %in% c(1,2,3) ~ 1, ER66475 %in% c(4,7) ~ 0), 
    govt.job_hd_2019 = case_when(ER72200 %in% c(1,2,3) ~ 1, ER72200 %in% c(4,7) ~ 0), 
    govt.job_wf_2019 = case_when(ER72477 %in% c(1,2,3) ~ 1, ER72477 %in% c(4,7) ~ 0),
    # Wage data: collected for wage income earned in prior year. For head and wife, using total labor income for 1981 
    # as PSID doesn't collect separate values for wage/salary income and self employment/farm income for wives in
    # early years. Editing topcode for 1981 following Blau & Kahn. 
    # Adjusting dollars to 2010 dollars using consumer price index
    # by multiplying income year to 1999 dollars, then 1999 dollars to 2010 values (1.309)
    # https://cps.ipums.org/cps/cpi99.shtml
    # Because we use information on covariates measured during the focal wage years (ie, focal wage year is 1980, as
    # reported in 1981) but we don't use wage information collected during the focal wage year about the prior calendar
    # year (ie, wages in 1979 as reported in 1980) we set these to missing
    wages_hd_1980 = NA, wages_wf_1980 = NA, 
    wages_hd_1981 = ifelse(V8066 == 99999, 99999*1.45, V8066) * 2.022 * 1.507, 
    wages_wf_1981 = ifelse(V7580 == 99999, 99999*1.45, V7580) * 2.022 * 1.507,
    wages_hd_1990 = NA, wages_wf_1990 = NA, 
    wages_hd_1991 = V20178 * 1.275 * 1.507, 
    wages_wf_1991 = V19136 * 1.275 * 1.507,
    # Starting in 1999, farm and business income are distinguished, allowing us to exclude the labor part of farm and business income
    wages_hd_1999 = NA, wages_wf_1999 = NA, 
    wages_hd_2001 = ER20443 * 0.967 * 1.507, 
    wages_wf_2001 = ER20447 * 0.967 * 1.507,
    wages_hd_2009 = NA, wages_wf_2009 = NA, 
    wages_hd_2011 = ER52237 * 0.764 * 1.507, 
    wages_wf_2011 = ER52249 * 0.764 * 1.507,
    wages_hd_2015 = NA, wages_wf_2015 = NA,
    wages_hd_2017 = ER71293 * 0.694 * 1.507, wages_wf_2017 = ER71321 * 0.694 * 1.507, 
    wages_hd_2019 = ER77315 * 0.663 * 1.507, wages_wf_2019 = ER77343 * 0.663 * 1.507,
    # Employer Tenure: not available in 1980, but collected in 1981- 1991 as number of months
    emp.tenure_hd_1980 = NA, emp.tenure_wf_1980 = NA,
    emp.tenure_hd_1981 = ifelse(V7711 == 999, NA, V7711), emp.tenure_wf_1981 = ifelse(V7884 == 999, NA, V7884),
    emp.tenure_hd_1990 = ifelse(V18120 == 999, NA, V18120), emp.tenure_wf_1990 = ifelse(V18422 == 999, NA, V18422),
    emp.tenure_hd_1991 = ifelse(V19420 == 999, NA, V19420), emp.tenure_wf_1991 = ifelse(V19722 == 999, NA, V19722),
    # For subsequent years, we use weeks,  months and years with this employer to generate an equivalent measure
    emp.tenure.wk_hd_1999 = ifelse(ER13245 >= 98, NA, ER13245/4), emp.tenure.mon_hd_1999 = ifelse(ER13244 >= 98, NA, ER13244), emp.tenure.yr_hd_1999 = ifelse(ER13243 >= 98, NA, ER13243) * 12,
    emp.tenure.wk_wf_1999 = ifelse(ER13757 >= 98, NA, ER13757/4), emp.tenure.mon_wf_1999 = ifelse(ER13756 >= 98, NA, ER13756), emp.tenure.yr_wf_1999 = ifelse(ER13755 >= 98, NA, ER13755) * 12,
    emp.tenure.wk_hd_2001 = ifelse(ER17256 >= 98, NA, ER17256/4), emp.tenure.mon_hd_2001 = ifelse(ER17255 >= 98, NA, ER17255), emp.tenure.yr_hd_2001 = ifelse(ER17254 >= 98, NA, ER17254) * 12,
    emp.tenure.wk_wf_2001 = ifelse(ER17826 >= 98, NA, ER17826/4), emp.tenure.mon_wf_2001 = ifelse(ER17825 >= 98, NA, ER17825), emp.tenure.yr_wf_2001 = ifelse(ER17824 >= 98, NA, ER17824) * 12,
    emp.tenure.wk_hd_2009 = ifelse(ER42202 >= 98, NA, ER42202/4), emp.tenure.mon_hd_2009 = ifelse(ER42201 >= 98, NA, ER42201), emp.tenure.yr_hd_2009 = ifelse(ER42200 >= 98, NA, ER42200) * 12,
    emp.tenure.wk_wf_2009 = ifelse(ER42454 >= 98, NA, ER42454/4), emp.tenure.mon_wf_2009 = ifelse(ER42453 >= 98, NA, ER42453), emp.tenure.yr_wf_2009 = ifelse(ER42452 >= 98, NA, ER42452) * 12,
    emp.tenure.wk_hd_2011 = ifelse(ER47515 >= 98, NA, ER47515/4), emp.tenure.mon_hd_2011 = ifelse(ER47514 >= 98, NA, ER47514), emp.tenure.yr_hd_2011 = ifelse(ER47513 >= 98, NA, ER47513) * 12,
    emp.tenure.wk_wf_2011 = ifelse(ER47772 >= 98, NA, ER47772/4), emp.tenure.mon_wf_2011 = ifelse(ER47771 >= 98, NA, ER47771), emp.tenure.yr_wf_2011 = ifelse(ER47770 >= 98, NA, ER47770) * 12,
    emp.tenure.wk_hd_2015 = ifelse(ER60230 >= 98, NA, ER60230/4), emp.tenure.mon_hd_2015 = ifelse(ER60229 >= 98, NA, ER60229), emp.tenure.yr_hd_2015 = ifelse(ER60228 >= 98, NA, ER60228) * 12,
    emp.tenure.wk_wf_2015 = ifelse(ER60493 >= 98, NA, ER60493/4), emp.tenure.mon_wf_2015 = ifelse(ER60492 >= 98, NA, ER60492), emp.tenure.yr_wf_2015 = ifelse(ER60491 >= 98, NA, ER60491) * 12,
    emp.tenure.wk_hd_2017 = ifelse(ER66233 >= 98, NA, ER66233/4), emp.tenure.mon_hd_2017 = ifelse(ER66232 >= 98, NA, ER66232), emp.tenure.yr_hd_2017 = ifelse(ER66231 >= 98, NA, ER66231) * 12,
    emp.tenure.wk_wf_2017 = ifelse(ER66508 >= 98, NA, ER66508/4), emp.tenure.mon_wf_2017 = ifelse(ER66507 >= 98, NA, ER66507), emp.tenure.yr_wf_2017 = ifelse(ER66506 >= 98, NA, ER66506) * 12,
    emp.tenure.wk_hd_2019 = ifelse(ER72233 >= 98, NA, ER72233/4), emp.tenure.mon_hd_2019 = ifelse(ER72232 >= 98, NA, ER72232), emp.tenure.yr_hd_2019 = ifelse(ER72231 >= 98, NA, ER72231) * 12,
    emp.tenure.wk_wf_2019 = ifelse(ER72510 >= 98, NA, ER72510/4), emp.tenure.mon_wf_2019 = ifelse(ER72509 >= 98, NA, ER72509), emp.tenure.yr_wf_2019 = ifelse(ER72508 >= 98, NA, ER72508) * 12,
    # Age of youngest child in the household
    age.youngest_1980 = V7071, 
    age.youngest_1981 = V7662, 
    age.youngest_1990 = na_if(V18053, 99),
    age.youngest_1991 = na_if(V19353, 99),
    age.youngest_1999 = ER13014,
    age.youngest_2001 = ER17017,
    age.youngest_2009 = na_if(ER42021, 999), 
    age.youngest_2011 = na_if(ER47321, 999), 
    age.youngest_2015 = na_if(ER60022, 999), 
    age.youngest_2017 = na_if(ER66022, 999),
    age.youngest_2019 = na_if(ER72022, 999),
    # Creating an indicator variable for whether the respondent is interviewed as a head/wife
    # that meets our desired age range in the outcome years
    samp.inc.1981 = case_when(int.num_1981 > 0 & seq.num_1981 <= 20 & seq.num_1981 != 0 
                              & rel.head_1981 %in% c("head", "wife") & age_1981 %in% seq(30, 55, 1) ~ 1, TRUE ~ 0),
    samp.inc.1991 = case_when(int.num_1991 > 0 & seq.num_1991 <= 20 & seq.num_1991 != 0 
                              & rel.head_1991 %in% c("head", "wife") & age_1991 %in% seq(30, 55, 1) ~ 1, TRUE ~ 0),
    samp.inc.2001 = case_when(int.num_2001 > 0 & seq.num_2001 <= 20 & seq.num_2001 != 0 
                              & rel.head_2001 %in% c("head", "wife") & age_2001 %in% seq(30, 55, 1)~ 1, TRUE ~ 0),
    samp.inc.2011 = case_when(int.num_2011 > 0 & seq.num_2011 <= 20 & seq.num_2011 != 0 
                              & rel.head_2011 %in% c("head", "wife") & age_2011 %in% seq(30, 55, 1) ~ 1, TRUE ~ 0),
    samp.inc.2019 = case_when(int.num_2019 > 0 & seq.num_2019 <= 20 & seq.num_2019 != 0 
                              & rel.head_2019 %in% c("head", "wife") & age_2019 %in% seq(30, 55, 1)~ 1, TRUE ~ 0)) %>%
  # Turning to long format where key = varname for all vars except time-constant variables
  # (Sex, 1968 household and person numbers and individual identifier)
  gather(key, value, -c(family.id68, person.number68, indiv.id, female, yr.born,
                        year.firstbornchild, samp.inc.1981, samp.inc.1991, 
                        samp.inc.2001, samp.inc.2011, samp.inc.2019)) %>%
  # Creating year variable based on the covariate label
  mutate(
    year = case_when(grepl("_1980", key) ~ 1980, grepl("_1981", key) ~ 1981, 
                     grepl("_1990", key) ~ 1990, grepl("_1991", key) ~ 1991,
                     grepl("_1999", key) ~ 1999, grepl("_2001", key) ~ 2001,
                     grepl("_2009", key) ~ 2009, grepl("_2011", key) ~ 2011, 
                     grepl("_2015", key) ~ 2015, grepl("_2017", key) ~ 2017,
                     grepl("_2019", key) ~ 2019),
    var = str_remove(key, "_[0-9]+[0-9]+"),
    # Creating indicator variables for whether the respondent belongs in one of the immigrant or latino samples 
    # (families in the Latino sample are not included in our target years)
    imm.sample.97 =  ifelse(family.id68 >= 3001 & family.id68 <= 3511, 1, 0),
    imm.sample.17 = ifelse(family.id68 >= 4001 & family.id68 <= 4851, 1, 0),
    latino.sample = ifelse(family.id68 >= 7001 & family.id68 <= 9308, 1, 0)) %>%
  dplyr::select(-key) %>%
  # Grouping by individual id
  group_by(indiv.id) %>%
  # Turning data back to wide format, each record is a person-year
  spread(var, value, convert = T) %>%
  mutate(
    # Creating indicator variable labeling whether respondent is head or wife in that year
    hd.wife = ifelse(rel.head %in% c("head", "wife"), 1, 0))

write_csv(psid_raw, "clean_data/intermediate_psid_raw.csv")

# Creating a version of the data where we limit observations to individuals who are observed 
# as heads or wives in our outcome years
psid_clean <- psid_raw %>%
  # This keeps observations in both the target year and the prior survey wave
  # for individuals observed as heads/wives in our age range in the target year
  filter(year %in% c(1980,1981) & samp.inc.1981 == 1 |
           year %in% c(1990,1991)  & samp.inc.1991 == 1 |
           year %in% c(1999,2001) & samp.inc.2001 == 1 |
           year %in% c(2009,2011) & samp.inc.2011 == 1 |
           year %in% c(2017,2019) & samp.inc.2019 == 1) %>%
  # Then we generate variables that take the value of the head for R's that are heads, value of spouse for spouses
  mutate(empstat = ifelse(rel.head == "head", empstat_hd, empstat_wf),
         ann.wrk.hrs = ifelse(rel.head == "head", ann.wrk.hrs_hd, ann.wrk.hrs_wf),
         govt.job = ifelse(rel.head == "head", govt.job_hd, govt.job_wf),
         ind.orig = ifelse(rel.head == "head", ind_hd, ind_wf), 
         occ.orig = ifelse(rel.head == "head", occ_hd, occ_wf), 
         ind1990  = ifelse(rel.head == "head", ind1990_hd, ind1990_wf),
         occ2010 = ifelse(rel.head == "head", occ2010_hd, occ2010_wf),
         self.emp = ifelse(rel.head == "head", self.emp_hd, self.emp_wf),
         union = ifelse(rel.head == "head", union_hd, union_wf),
         wages = ifelse(rel.head == "head", wages_hd, wages_wf),
         yrs.ed.fam = ifelse(rel.head == "head", yrs.ed.fam_hd, yrs.ed.fam_wf),
         emp.tenure.wk = ifelse(rel.head == "head", emp.tenure.wk_hd, emp.tenure.wk_wf),
         emp.tenure.mon = ifelse(rel.head == "head", emp.tenure.mon_hd, emp.tenure.mon_wf),
         emp.tenure.yr = ifelse(rel.head == "head", emp.tenure.yr_hd, emp.tenure.yr_wf),
         emp.tenure = ifelse(rel.head == "head", emp.tenure_hd, emp.tenure_wf), 
         # Creating age at first birth measure at the individual level
         # For those who report year when they were born, age first birth = year first child born - year born
         # For those w missig information for year born, year first child born - (survey year - reported age) 
         # This leaves us with missing values for thse who do not become parents and those who don't 
         # report the year when their first child was born- we fill in this information later 
         age.first.birth = ifelse(!is.na(yr.born), year.firstbornchild - yr.born, year.firstbornchild - (year-age)),
         # Recoding marital status of head into categories
         marstat.hd = case_when(marstat.hd == 1 ~ "married",
                                marstat.hd == 2 ~ "unmarried", 
                                marstat.hd %in% 3:5 ~ "prev.married"),
         # Employer tenure is completely missing in 1980. 
         # In the later years some cases have missing year and 0 months/weeks: these are all coded to NA
         emp.tenure = ifelse(year %in% c(1980, 1981, 1990, 1991), emp.tenure, ifelse(
           year > 1991 & is.na(emp.tenure.yr) & is.na(emp.tenure.mon) & is.na(emp.tenure.wk), NA, 
           rowSums(across(c(emp.tenure.wk, emp.tenure.mon, emp.tenure.yr)), na.rm = T))),
         # Adding zeros in front of occupation codes that have less than 3 digits
         occ.orig = str_pad(occ.orig, 3, pad = "0"),
         # "New" Mining Industries recorded in crosswalk for 2012, which don't have a match in the 1990 codes
         # (the crosswalk just matches to "new"). We recode to equivalent 1990 mining industry
         ind1990 = ifelse(ind1990 == "New", "40", ind1990),
         # Adding zeros in front of industry codes that have less than 3 digits
         ind1990 = as.numeric(str_pad(ind1990, 3, pad = "0"))) %>%
  # Selecting variables at the individual-person level (all vars apply to R- if R is head, var takes value for head)
  dplyr::select(family.id68, person.number68, family.id,
                indiv.id, female, year, 
                latino.sample, imm.sample.97, imm.sample.17, perwt, perwt.long, 
                age, rel.head, marstat.hd, numkids.fu, 
                racehd, region, ann.wrk.hrs, govt.job, ind.orig, ind1990,
                occ.orig, occ2010, self.emp, union, wages, yrs.ed.fam, emp.tenure, empstat,
                age.first.birth, age.youngest, hd.wife) %>%
  arrange(indiv.id, year) %>% # Arrange by year and individual id to create lead terms
  group_by(indiv.id) %>% # Group by id to create lead terms
  # Creating lead terms which will be used to create sample selection variables
  # for supplementary analyses using prior years' covariates
  mutate(self.emp.lead = lead(self.emp), # Records self-employed status in the subsequent year
         lead.ind1990 = lead(ind1990), lead.occ2010 = lead(occ2010), # Records industry and occ in the subsequent year
         lead.ann.wrk.hrs = lead(ann.wrk.hrs), # Records annual work hours in the subsequent year
         lead.wage = lead(wages), lead.age = lead(age), # Recording wage, age, and region in the subsequent year
         lead.region = lead(region)) %>% 
  ungroup() %>%
  mutate(# Creating a measure for whether individuals work in a manufacturing industry 
    # (durable or non-durable), using original industry labels in 1980 and 
    # harmonized industry labels for subsequent years
    manuf = ifelse(
      year == 1980 & is.na(ind.orig), NA, ifelse(
        year == 1980 & ind.orig %in% c(40:46, 85, 30:34, 49), 1, ifelse(
          year != 1980 & is.na(ind1990), NA, ifelse(
            year != 1980 & ind1990 %in% 
              c(100, 101, 102, 110, 111, 112, 120, 121, 122, 130, 132, 140,
                141, 142, 150, 151, 152, 160, 161, 162, 171, 172, 180, 181,
                182, 190, 191, 192, 200, 201, 210, 211, 212, 220, 221, 222,
                230, 231, 232, 241, 242, 250, 251, 252, 261, 262, 270, 271,
                272, 280, 281, 282, 290, 291, 292, 300, 301, 310, 311, 312,
                320, 321, 322, 331, 332, 340, 341, 342, 350, 351, 352, 360,
                361, 362, 370, 371, 372, 380, 381, 390, 391, 392), 1, 0)))), 
    # Creating a measure of whether an individual works in agriculture for sample selection
    agriculture = case_when(# Coding as agriculture when:
      year %in% c(1980, 1990, 1999, 2009, 2015, 2017) & lead.ind1990 %in% c(10, 11, 30, 31, 32, 230) ~ 1, 
      year %in% c(1981, 1991, 2001, 2011, 2019) & ind1990 %in% c(10, 11, 30, 31, 32, 230) ~ 1, 
      TRUE ~ 0), # codes missing as non-agriculture- we do not impute bc we use for sample selection
    # Following similar procedure as for agriculture for military
    military = case_when(
      year %in% c(1980, 1990, 1999, 2009, 2015, 2017) & lead.ind1990 %in% c(940, 941, 942, 950, 951, 952, 960) ~ 1, 
      year %in% c(1981, 1991, 2001, 2011, 2019) & ind1990 %in% c(940, 941, 942, 950, 951, 952, 960) ~ 1,
      TRUE ~ 0))

write_csv(psid_clean, "clean_data/intermediate_psid_clean.csv")
print("Done with: Main Data file")

#------------------------------------------------------------------------------
# MARITAL STATUS MARITAL HISTORY FILES
#------------------------------------------------------------------------------

# Generating marital history from the marital history files
# The marital history files contain one record for each individual in the PSID since 1985
# Each R has at least one record, and each R has a record for each of their marriages
psid_mar <- read.dta("raw_data/psid_marital/psid_marital.dta") %>%
  transmute(
    intnum68 = MH2, pernum68 = MH3, indiv.id = paste(intnum68, pernum68, sep="_"), # Creating variables IDing R's
    # Variable indicating the order of this marriage record for R- recoding 99 to 0 marriages, 98 as missing
    marriage.order = ifelse(MH9 == 99, 0, ifelse(MH9 == 98, NA, MH9)),
    # Creating variables for the marriage date
    month.married = ifelse(MH10 %in% c(98,99), NA, str_pad(MH10, width = 2, pad = "0")),
    yr.married = ifelse(MH11 %in% c(9998, 9999), NA, MH11),
    marriagedate = mdy(paste(month.married, "01", yr.married, sep = "/")),
    # This variable indicates the status of this marriage as of the latest marital history report for this R
    status.marriage = case_when(MH12 == 9 ~ "never.married", MH12 == 7 ~ "other/bigamist",
                                MH12 == 5 ~ "separated", MH12 == 4 ~ "divorced",
                                MH12 == 3 ~ "widowed", MH12 == 1 ~ "still.married"), # Implicitly coding 8 as NA
    # Variables coding the date of the marriage's dissolution, whether divorced/widowed or separated
    # 98 or 99 indicate either an uknown date or whether the respondent was never married/marriage hadn't ended
    # 21-22-23-24 are codes for "seeasons", which are recoded to a month within that season
    month.w.d = ifelse(MH13 %in% c(98,99), NA, ifelse(MH13 == 21, 2, ifelse(MH13 == 22, 5, ifelse(MH13 == 23, 8, ifelse(
      MH13 == 24, 11, str_pad(MH13, width = 2, pad = "0")))))),
    yr.w.d = ifelse(MH14 %in% c(9998, 9999), NA, MH14),
    divwiddate = mdy(paste(month.w.d, "01", yr.w.d, sep = "/")),
    month.sep = ifelse(MH15 %in% c(98,99), NA, ifelse(MH15 == 21, 2, ifelse(MH15 == 22, 5, ifelse(MH15 == 23, 8, ifelse(
      MH15 == 24, 11, str_pad(MH15, width = 2, pad = "0")))))),
    yr.sep = ifelse(MH16 %in% c(9998, 9999), NA, MH16),
    sepdate = mdy(paste(month.sep, "01", yr.sep, sep = "/")),
    # This variable captures the year in which marital history reports were last collected for that individual
    lastyr.resp.marriage = MH17,
    # This variable reprsents the number of marriages R has reported: if num-marriages == 0, never married
    num.marriages = na_if(MH18, 98),
    # The following procedure aims to capture the status of the marriage record for each respondent as of the
    # end of the year prior to the year in our sample. The next two variables code an "end date" for the marriage- if the 
    # the record is for a respondent who never married, we code never married. If the record is for an intact marriage,
    # we code this "last date" as the last day of our wage year. If the record is for a marriage that has been
    # dissolved, it codes the date at which that marriage was dissolved
    lastdate_1980 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-1979"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_1981 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-1980"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_1990 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-1989"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_1991 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-1990"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_1999 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-1998"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_2001 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-2000"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_2009 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-2008"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_2011 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-2010"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_2015 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-2014"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_2017 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-2016"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    lastdate_2019 = as_date(ifelse(status.marriage == "never.married", NA, ifelse(
      status.marriage %in% c("still.married", "other/bigamist"), 
      mdy("12-31-2018"), ifelse(
        status.marriage %in% c("divorced", "widowed"), divwiddate, ifelse(
          status.marriage == "separated", sepdate, NA))))),
    # Here, we code the status of this marriage as of the last day in our wage year
    # If the record is for a respondent who's never married, we code it as never married:
    # if the marriage start date in that record happens after the last day of our wage year, we code this 
    # marriage as not yet married. If the marriage start date is before the end of our wage year and the "last date"
    # variable above (indicating when the marriage was dissolved, if dissolved by last obs period) is before
    # the end of our wage year, we code this marriage as the status of that marriage as of the last obs period:
    # If the marriage start date is before the end of our wage year, but the dissolution date is after the end
    # of our wage year, then we code this record as "still married" during our wage year
    status.marriage_1980 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-1979"), "notyet.married", ifelse(
        lastdate_1980 < mdy("12-31-1979"), status.marriage, ifelse(
          lastdate_1980 >= mdy("12-31-1979"), "still.married", NA)))),
    status.marriage_1981 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-1980"), "notyet.married", ifelse(
        lastdate_1981 < mdy("12-31-1980"), status.marriage, ifelse(
          lastdate_1981 >= mdy("12-31-1980"), "still.married", NA)))),
    status.marriage_1990 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-1989"), "notyet.married", ifelse(
        lastdate_1990 < mdy("12-31-1989"), status.marriage, ifelse(
          lastdate_1990 >= mdy("12-31-1989"), "still.married", NA)))),
    status.marriage_1991 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-1990"), "notyet.married", ifelse(
        lastdate_1991 < mdy("12-31-1990"), status.marriage, ifelse(
          lastdate_1991 >= mdy("12-31-1990"), "still.married", NA)))),
    status.marriage_1999 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-1998"), "notyet.married", ifelse(
        lastdate_1999 < mdy("12-31-1998"), status.marriage, ifelse(
          lastdate_1999 >= mdy("12-31-1998"), "still.married", NA)))),
    status.marriage_2001 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-2000"), "notyet.married", ifelse(
        lastdate_2001 < mdy("12-31-2000"), status.marriage, ifelse(
          lastdate_2001 >= mdy("12-31-2000"), "still.married", NA)))),
    status.marriage_2009 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-2008"), "notyet.married", ifelse(
        lastdate_2009 < mdy("12-31-2008"), status.marriage, ifelse(
          lastdate_2009 >= mdy("12-31-2008"), "still.married", NA)))),
    status.marriage_2011 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-2010"), "notyet.married", ifelse(
        lastdate_2011 < mdy("12-31-2010"), status.marriage, ifelse(
          lastdate_2011 >= mdy("12-31-2010"), "still.married", NA)))),
    status.marriage_2015 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-2014"), "notyet.married", ifelse(
        lastdate_2015 < mdy("12-31-2014"), status.marriage, ifelse(
          lastdate_2015 >= mdy("12-31-2014"), "still.married", NA)))),
    status.marriage_2017 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-2016"), "notyet.married", ifelse(
        lastdate_2017 < mdy("12-31-2016"), status.marriage, ifelse(
          lastdate_2017 >= mdy("12-31-2016"), "still.married", NA)))),
    status.marriage_2019 = ifelse(status.marriage == "never.married", "never.married", ifelse(
      marriagedate > mdy("12-31-2018"), "notyet.married", ifelse(
        lastdate_2019 < mdy("12-31-2018"), status.marriage, ifelse(
          lastdate_2019 >= mdy("12-31-2018"), "still.married", NA)))),
    # There are some missing values for the status of the marriage that we can ascertain:
    # For example, there are some records which have a reported status of divorce but no divorce date.
    # If the last year in which marital history information is recorded precedes our wage year, then
    # we know that the divorce in this marriage occurred prior to the wage year, and hence, that the 
    # status of this marriage as of our wage year is a divorce (assuming no repartnering)
    # Note that we can only make this assumption for years starting in 1985 and beyond because marital history
    # is only collected starting 1985: we are missing status at 1981 for ~5 pct of all collected marriage records
    status.marriage_1990 = ifelse(is.na(status.marriage_1990) & lastyr.resp.marriage <= 1989,
                                  status.marriage, status.marriage_1990),
    status.marriage_1991 = ifelse(is.na(status.marriage_1991) & lastyr.resp.marriage <= 1990,
                                  status.marriage, status.marriage_1991),
    status.marriage_1999 = ifelse(is.na(status.marriage_1999) & lastyr.resp.marriage <= 1998,
                                  status.marriage, status.marriage_1999),
    status.marriage_2001 = ifelse(is.na(status.marriage_2001) & lastyr.resp.marriage <= 2000,
                                  status.marriage, status.marriage_2001),
    status.marriage_2009 = ifelse(is.na(status.marriage_2009) & lastyr.resp.marriage <= 2008,
                                  status.marriage, status.marriage_2009),
    status.marriage_2011 = ifelse(is.na(status.marriage_2011) & lastyr.resp.marriage <= 2010,
                                  status.marriage, status.marriage_2011),
    status.marriage_2015 = ifelse(is.na(status.marriage_2015) & lastyr.resp.marriage <= 2014,
                                  status.marriage, status.marriage_2015),
    status.marriage_2017 = ifelse(is.na(status.marriage_2017) & lastyr.resp.marriage <= 2016,
                                  status.marriage, status.marriage_2017),
    status.marriage_2019 = ifelse(is.na(status.marriage_2019) & lastyr.resp.marriage <= 2018,
                                  status.marriage, status.marriage_2019)) %>%
  dplyr::select(-c(month.married, yr.married, month.w.d, yr.w.d, month.sep, yr.sep, 
                   intnum68, pernum68)) %>%
  # We then get the status of each individual's marriage by the target year
  # by pasting together the status of each marriage by the target year for each R
  group_by(indiv.id) %>%
  summarise(marstat_1980 = paste(unique(status.marriage_1980), collapse = ', '),
            marstat_1981 = paste(unique(status.marriage_1981), collapse = ', '),
            marstat_1990 = paste(unique(status.marriage_1990), collapse = ', '),
            marstat_1991 = paste(unique(status.marriage_1991), collapse = ', '),
            marstat_1999 = paste(unique(status.marriage_1999), collapse = ', '),
            marstat_2001 = paste(unique(status.marriage_2001), collapse = ', '),
            marstat_2009 = paste(unique(status.marriage_2009), collapse = ', '),
            marstat_2011 = paste(unique(status.marriage_2011), collapse = ', '),
            marstat_2015 = paste(unique(status.marriage_2015), collapse = ', '),
            marstat_2017 = paste(unique(status.marriage_2017), collapse = ', '),
            marstat_2019 = paste(unique(status.marriage_2019), collapse = ', ')) %>%
  gather(key, value, -indiv.id) %>%
  mutate(year = as.numeric(numextract(key))) %>%
  dplyr::select(-key) %>%
  # Using the status for each marriage by the target year for each individual, we create a variable 
  # that codes the individual's marital status at the end of the target year: if the individual's "value"
  # is only never married or only not yet married, we code as unmarried: if any of the individual's 
  # marriages have started by and are intact at the end of the wage yeaer, we code as married
  # all of the other statuses are coded accordingly
  mutate(marstat = ifelse(value %in% c("notyet.married", "never.married"), "unmarried", ifelse(
    grepl("still.married", value), "married", ifelse(
      grepl("other/bigamist", value), "other/bigamist", ifelse(
        grepl("divorced", value), "prev.married", ifelse(
          grepl("separated", value), "prev.married", ifelse(
            grepl("widowed", value), "prev.married", ifelse(
              value %in% c("notyet.married, NA", "NA, notyet.married"), NA, NA)))))))) %>%
  dplyr::select(-value)

# Joining in main family interview data with marital history data
benchmark.mar <- left_join(psid_clean, psid_mar, by = c("indiv.id", "year")) %>%
  # Creating measures of marital status
  mutate(married = case_when(marstat == "married" ~ 1, 
                             is.na(marstat) & marstat.hd == "married" ~ 1,
                             TRUE ~ 0))

write_csv(psid_mar, "clean_data/intermediate_psid_clean_mar.csv")
print("Done with: Merging Marital History to Main File")

rm(psid_mar)
#------------------------------------------------------------------------------
# CREATING DATA ON RACE BASED ON ALL AVAILABLE RACE MEASURES EVER REPORTED
#------------------------------------------------------------------------------

# Creating data on ever reported race, using heads & wives' individual race and ethnicity
# reports starting in 1985. Prior to 1985, race data was only collected on household heads: race
# was assumed to be the same for spouses/wives, as well as for splitoff families if race was carried
# over from a prior year and not re-asked in the interview year. The following section gathers all
# respondents who are ever heads/wives and report race starting in 1985
psid_race <- read.dta("raw_data/psid/psid.dta") %>% 
  transmute(
    # We repeat code for interview, person number, unique ID, and relationship to head as this is a new dataframe
    family_id = ER30001, # 1968 interview number
    person_number = ER30002, # 1968 person number
    indiv.id = paste(family_id, person_number, sep ="_"), # unique individual identifier
    rel.head_1985 = ER30465, rel.head_1986 = ER30500, rel.head_1987 = ER30537, # Relationship to head
    rel.head_1988 = ER30572, rel.head_1989 = ER30608, rel.head_1990 = ER30644, 
    rel.head_1991 = ER30691, rel.head_1992 = ER30735, rel.head_1993 = ER30808, 
    rel.head_1994 = ER33103, rel.head_1995 = ER33203, rel.head_1996 = ER33303, 
    rel.head_1997 = ER33403, rel.head_1999 = ER33503, rel.head_2001 = ER33603, 
    rel.head_2003 = ER33703, rel.head_2005 = ER33803, rel.head_2007 = ER33903, 
    rel.head_2009 = ER34003, rel.head_2011 = ER34103, rel.head_2013 = ER34203, 
    rel.head_2015 = ER34303, rel.head_2017 = ER34503, rel.head_2019 =ER34703,
    # Following Blau & Kahn's coding procedure, we use individual's reported "Spanish" ethnicity/descent
    # to code individuals as Hispanic if they report having Hispanic descent; we code individuals as
    # Black if they report being black in any race question asked that year, and as other if they 
    # report an "other" race in any race questions asked that year. We code the remaining individuals
    # as white if they report being White and do not report "having Hispanic ancestry, do not report
    # being Black in any race question, and do not report an "other" race in any race question.
    # From 1985 to 1993, individuals can report up to two races. In 1994 to 1996, individuals can 
    # report up to three races. Starting in 1997 and up to 2003, individuals can report up to four 
    # races, but "Hispanic" is not asked separately and is an option in the race questions. Starting
    # in 2005 up to 2019, the PSID asks about Hispanic ancestry separately and individuals can report
    # up to four races
    race.hd_1985 = case_when(V11937 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V11938 == 2 | V11939 == 2 ~ "Black", 
                             V11938 %in% c(3,4,7) | V11939 %in% c(3,4,7) ~ "Other", 
                             V11938 == 1 | V11939 == 1 ~ "White"), 
    race.wf_1985 = case_when(V12292 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V12293 == 2 | V12294 == 2 ~ "Black", 
                             V12293 %in% c(3,4,7) | V12294 %in% c(3,4,7) ~ "Other", 
                             V12293 == 1 | V12294 == 1 ~ "White"), 
    race.hd_1986 = case_when(V13564 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V13565 == 2 | V13566 == 2 ~ "Black", 
                             V13565 %in% c(3,4,7) | V13566 %in% c(3,4,7) ~ "Other", 
                             V13565 == 1 | V13566 == 1 ~ "White"), 
    race.wf_1986 = case_when(V13499 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V13500 == 2 | V13501 == 2 ~ "Black", 
                             V13500 %in% c(3,4,7) | V13501 %in% c(3,4,7) ~ "Other", 
                             V13500 == 1 | V13501 == 1 ~ "White"), 
    race.hd_1987 = case_when(V14611 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V14612 == 2 | V14613 == 2 ~ "Black", 
                             V14612 %in% c(3,4,7) | V14613 %in% c(3,4,7) ~ "Other", 
                             V14612 == 1 | V14613 == 1 ~ "White"), 
    race.wf_1987 = case_when(V14546 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V14547 == 2 | V14548 == 2 ~ "Black", 
                             V14547 %in% c(3,4,7) | V14548 %in% c(3,4,7) ~ "Other", 
                             V14547 == 1 | V14548 == 1 ~ "White"), 
    race.hd_1988 = case_when(V16085 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V16086 == 2 | V16087 == 2 ~ "Black", 
                             V16086 %in% c(3,4,7) | V16087 %in% c(3,4,7) ~ "Other", 
                             V16086 == 1 | V16087 == 1 ~ "White"), 
    race.wf_1988 = case_when(V16020 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V16021 == 2 | V16022 == 2 ~ "Black", 
                             V16021 %in% c(3,4,7) | V16022 %in% c(3,4,7) ~ "Other", 
                             V16021 == 1 | V16022 == 1 ~ "White"), 
    race.hd_1989 = case_when(V17482 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V17483 == 2 | V17484 == 2 ~ "Black", 
                             V17483 %in% c(3,4,7) | V17484 %in% c(3,4,7) ~ "Other", 
                             V17483 == 1 | V17484 == 1 ~ "White"), 
    race.wf_1989 = case_when(V17417 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V17418 == 2 | V17419 == 2 ~ "Black", 
                             V17418 %in% c(3,4,7) | V17419 %in% c(3,4,7) ~ "Other", 
                             V17418 == 1 | V17419 == 1 ~ "White"), 
    race.hd_1990 = case_when(V18813 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V18814 == 5 | V18815 == 5 ~ "Hispanic",
                             V18814 == 2 | V18815 == 2 ~ "Black", 
                             V18814 %in% c(3,4,6,7) | V18815 %in% c(3,4,6,7) ~ "Other", 
                             V18814 == 1 | V18815 == 1 ~ "White"), 
    race.wf_1990 = case_when(V18748 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V18749 == 5 | V18750 == 5 ~ "Hispanic",
                             V18749 == 2 | V18750 == 2 ~ "Black", 
                             V18749 %in% c(3,4,6,7) | V18750 %in% c(3,4,6,7) ~ "Other", 
                             V18749 == 1 | V18750 == 1 ~ "White"), 
    
    race.hd_1991 = case_when(V20113 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V20114 == 5 | V20115 == 5 ~ "Hispanic", 
                             V20114 == 2 | V20115 == 2 ~ "Black", 
                             V20114 %in% c(3,4,6,7) | V20115 %in% c(3,4,6,7) ~ "Other", 
                             V20114 == 1 | V20115 == 1 ~ "White"), 
    race.wf_1991 = case_when(V20048 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V20049 == 5 | V20050 == 5 ~ "Hispanic", 
                             V20049 == 2 | V20050 == 2 ~ "Black", 
                             V20049 %in% c(3,4,6,7) | V20050 %in% c(3,4,6,7) ~ "Other", 
                             V20049 == 1 | V20050 == 1 ~ "White"), 
    race.hd_1992 = case_when(V21419 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V21420 == 5 | V21421 == 5 ~ "Hispanic", 
                             V21420 == 2 | V21421 == 2 ~ "Black", 
                             V21420 %in% c(3,4,6,7) | V21421 %in% c(3,4,6,7) ~ "Other", 
                             V21420 == 1 | V21421 == 1 ~ "White"), 
    race.wf_1992 = case_when(V21354 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V21355 == 5 | V21356 == 5 ~ "Hispanic", 
                             V21355 == 2 | V21356 == 2 ~ "Black", 
                             V21355 %in% c(3,4,6,7) | V21356 %in% c(3,4,6,7) ~ "Other", 
                             V21355 == 1 | V21356 == 1 ~ "White"), 
    race.hd_1993 = case_when(V23275 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V23276 == 5 | V23277 == 5 ~ "Hispanic", 
                             V23276 == 2 | V23277 == 2 ~ "Black", 
                             V23276 %in% c(3,4,6,7) | V23277 %in% c(3,4,6,7) ~ "Other", 
                             V23276 == 1 | V23277 == 1 ~ "White"), 
    race.wf_1993 = case_when(V23211 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             V23212 == 5 | V23213 == 5 ~ "Hispanic", 
                             V23212 == 2 | V23213 == 2 ~ "Black", 
                             V23212 %in% c(3,4,6,7) | V23213 %in% c(3,4,6,7) ~ "Other", 
                             V23212 == 1 | V23213 == 1 ~ "White"), 
    race.hd_1994 = case_when(ER3941 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             ER3944 == 5 | ER3945 == 5 | ER3946 == 5 ~ "Hispanic", 
                             ER3944 == 2 | ER3945 == 2 | ER3946 == 2 ~ "Black", 
                             ER3944 %in% c(3,4,6,7) | ER3945 %in% c(3,4,6,7) | ER3946 %in% c(3,4,6,7) ~ "Other", 
                             ER3944 == 1 | ER3945 == 1 | ER3946 == 1 ~ "White"), 
    race.wf_1994 = case_when(ER3880 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             ER3883 == 5 | ER3884 == 5 | ER3885 == 5 ~ "Hispanic", 
                             ER3883 == 2 | ER3884 == 2 | ER3885 == 2 ~ "Black", 
                             ER3883 %in% c(3,4,6,7) | ER3884 %in% c(3,4,6,7) | ER3885 %in% c(3,4,6,7) ~ "Other", 
                             ER3883 == 1 | ER3884 == 1 | ER3885 == 1 ~ "White"), 
    race.hd_1995 = case_when(ER6811 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             ER6814 == 5 | ER6815 == 5 | ER6816 == 5 ~ "Hispanic", 
                             ER6814 == 2 | ER6815 == 2 | ER6816 == 2 ~ "Black", 
                             ER6814 %in% c(3,4,6,7) | ER6815 %in% c(3,4,6,7) | ER6816 %in% c(3,4,6,7) ~ "Other", 
                             ER6814 == 1 | ER6815 == 1 | ER6816 == 1 ~ "White"), 
    race.wf_1995 = case_when(ER6750 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             ER6753 == 5 | ER6754 == 5 | ER6755 == 5 ~ "Hispanic", 
                             ER6753 == 2 | ER6754 == 2 | ER6755 == 2 ~ "Black", 
                             ER6753 %in% c(3,4,6,7) | ER6754 %in% c(3,4,6,7) | ER6755 %in% c(3,4,6,7) ~ "Other", 
                             ER6753 == 1 | ER6754 == 1 | ER6755 == 1 ~ "White"), 
    race.hd_1996 = case_when(ER9057 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             ER9060 == 5 | ER9061 == 5 | ER9062 == 5 ~ "Hispanic", 
                             ER9060 == 2 | ER9061 == 2 | ER9062 == 2 ~ "Black", 
                             ER9060 %in% c(3,4,6,7) | ER9061 %in% c(3,4,6,7) | ER9062 %in% c(3,4,6,7) ~ "Other", 
                             ER9060 == 1 | ER9061 == 1 | ER9062 == 1 ~ "White"), 
    race.wf_1996 = case_when(ER8996 %in% c(1, 2, 3, 4, 5, 6, 7) ~ "Hispanic", 
                             ER8999 == 5 | ER9000 == 5 | ER9001 == 5 ~ "Hispanic", 
                             ER8999 == 2 | ER9000 == 2 | ER9001 == 2 ~ "Black", 
                             ER8999 %in% c(3,4,6,7) | ER9000 %in% c(3,4,6,7) | ER9001 %in% c(3,4,6,7) ~ "Other", 
                             ER8999 == 1 | ER9000 == 1 | ER9001 == 1 ~ "White"),
    race.hd_1997 = case_when(ER11848 == 5 | ER11849 == 5 | ER11850 == 5 | ER11851 == 5 ~ "Hispanic", 
                             ER11848 == 2 | ER11849 == 2 | ER11850 == 2 | ER11851 == 2 ~ "Black", 
                             ER11848 %in% c(3,4,6,7) | ER11849 %in% c(3,4,6,7) | ER11850 %in% c(3,4,6,7) | ER11851 %in% c(3,4,6,7) ~ "Other",
                             ER11848 == 1 | ER11849 == 1 | ER11850 == 1 | ER11851 == 1 ~ "White"),
    race.wf_1997 = case_when(ER11760 == 5 | ER11761 == 5 | ER11762 == 5 | ER11763 == 5 ~ "Hispanic", 
                             ER11760 == 2 | ER11761 == 2 | ER11762 == 2 | ER11763 == 2 ~ "Black", 
                             ER11760 %in% c(3,4,6,7) | ER11761 %in% c(3,4,6,7) | ER11762 %in% c(3,4,6,7) | ER11763 %in% c(3,4,6,7) ~ "Other",
                             ER11760 == 1 | ER11761 == 1 | ER11762 == 1 | ER11763 == 1 ~ "White"),
    race.hd_1999 = case_when(ER15928 == 5 | ER15929 == 5 | ER15930 == 5 | ER15931 == 5 ~ "Hispanic", 
                             ER15928 == 2 | ER15929 == 2 | ER15930 == 2 | ER15931 == 2 ~ "Black", 
                             ER15928 %in% c(3,4,6,7) | ER15929 %in% c(3,4,6,7) | ER15930 %in% c(3,4,6,7) | ER15931 %in% c(3,4,6,7) ~ "Other",
                             ER15928 == 1 | ER15929 == 1 | ER15930 == 1 | ER15931 == 1 ~ "White"),
    race.wf_1999 = case_when(ER15836 == 5 | ER15837 == 5 | ER15838 == 5 | ER15839 == 5 ~ "Hispanic", 
                             ER15836 == 2 | ER15837 == 2 | ER15838 == 2 | ER15839 == 2 ~ "Black", 
                             ER15836 %in% c(3,4,6,7) | ER15837 %in% c(3,4,6,7) | ER15838 %in% c(3,4,6,7) | ER15839 %in% c(3,4,6,7) ~ "Other",
                             ER15836 == 1 | ER15837 == 1 | ER15838 == 1 | ER15839 == 1 ~ "White"),
    race.hd_2001 = case_when(ER19989 == 5 | ER19990 == 5 | ER19991 == 5 | ER19992 == 5 ~ "Hispanic", 
                             ER19989 == 2 | ER19990 == 2 | ER19991 == 2 | ER19992 == 2 ~ "Black", 
                             ER19989 %in% c(3,4,6,7) | ER19990 %in% c(3,4,6,7) | ER19991 %in% c(3,4,6,7) | ER19992 %in% c(3,4,6,7) ~ "Other",
                             ER19989 == 1 | ER19990 == 1 | ER19991 == 1 | ER19992 == 1 ~ "White"),
    race.wf_2001 = case_when(ER19897 == 5 | ER19898 == 5 | ER19899 == 5 | ER19900 == 5 ~ "Hispanic", 
                             ER19897 == 2 | ER19898 == 2 | ER19899 == 2 | ER19900 == 2 ~ "Black", 
                             ER19897 %in% c(3,4,6,7) | ER19898 %in% c(3,4,6,7) | ER19899 %in% c(3,4,6,7) | ER19900 %in% c(3,4,6,7) ~ "Other",
                             ER19897 == 1 | ER19898 == 1 | ER19899 == 1 | ER19900 == 1 ~ "White"),
    race.hd_2003 = case_when(ER23426 == 5 | ER23427 == 5 | ER23428 == 5 | ER23429 == 5 ~ "Hispanic", 
                             ER23426 == 2 | ER23427 == 2 | ER23428 == 2 | ER23429 == 2 ~ "Black", 
                             ER23426 %in% c(3,4,6,7) | ER23427 %in% c(3,4,6,7) | ER23428 %in% c(3,4,6,7) | ER23429 %in% c(3,4,6,7) ~ "Other",
                             ER23426 == 1 | ER23427 == 1 | ER23428 == 1 | ER23429 == 1 ~ "White"),
    race.wf_2003 = case_when(ER23334 == 5 | ER23335 == 5 | ER23336 == 5 | ER23337 == 5 ~ "Hispanic", 
                             ER23334 == 2 | ER23335 == 2 | ER23336 == 2 | ER23337 == 2 ~ "Black", 
                             ER23334 %in% c(3,4,6,7) | ER23335 %in% c(3,4,6,7) | ER23336 %in% c(3,4,6,7) | ER23337 %in% c(3,4,6,7) ~ "Other",
                             ER23334 == 1 | ER23335 == 1 | ER23336 == 1 | ER23337 == 1 ~ "White"),
    race.hd_2005 = case_when(ER27392 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER27393 == 2 | ER27394 == 2 | ER27395 == 2 | ER27396 == 2 ~ "Black", 
                             ER27393 %in% c(3,4,5,7) | ER27394 %in% c(3,4,5,7) | ER27395 %in% c(3,4,5,7) | ER27396 %in% c(3,4,5,7) ~ "Other",
                             ER27393 == 1 | ER27394 == 1 | ER27395 == 1 | ER27396 == 1 ~ "White"), 
    race.wf_2005 = case_when(ER27296 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER27297 == 2 | ER27298 == 2 | ER27299 == 2 | ER27300 == 2 ~ "Black", 
                             ER27297 %in% c(3,4,5,7) | ER27298 %in% c(3,4,5,7) | ER27299 %in% c(3,4,5,7) | ER27300 %in% c(3,4,5,7) ~ "Other",
                             ER27297 == 1 | ER27298 == 1 | ER27299 == 1 | ER27300 == 1 ~ "White"),
    race.hd_2007 = case_when(ER40564 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER40565 == 2 | ER40566 == 2 | ER40567 == 2 | ER40568 == 2 ~ "Black", 
                             ER40565 %in% c(3,4,5,7) | ER40566 %in% c(3,4,5,7) | ER40567 %in% c(3,4,5,7) | ER40568 %in% c(3,4,5,7) ~ "Other",
                             ER40565 == 1 | ER40566 == 1 | ER40567 == 1 | ER40568 == 1 ~ "White"), 
    race.wf_2007 = case_when(ER40471 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER40472 == 2 | ER40473 == 2 | ER40474 == 2 | ER40475 == 2 ~ "Black", 
                             ER40472 %in% c(3,4,5,7) | ER40473 %in% c(3,4,5,7) | ER40474 %in% c(3,4,5,7) | ER40475 %in% c(3,4,5,7) ~ "Other",
                             ER40472 == 1 | ER40473 == 1 | ER40474 == 1 | ER40475 == 1 ~ "White"),
    race.hd_2009 = case_when(ER46542 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER46543 == 2 | ER46544 == 2 | ER46545 == 2 | ER46546 == 2 ~ "Black", 
                             ER46543 %in% c(3,4,5,7) | ER46544 %in% c(3,4,5,7) | ER46545 %in% c(3,4,5,7) | ER46546 %in% c(3,4,5,7) ~ "Other",
                             ER46543 == 1 | ER46544 == 1 | ER46545 == 1 | ER46546 == 1 ~ "White"), 
    race.wf_2009 = case_when(ER46448 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER46449 == 2 | ER46450 == 2 | ER46451 == 2 | ER46452 == 2 ~ "Black", 
                             ER46449 %in% c(3,4,5,7) | ER46450 %in% c(3,4,5,7) | ER46451 %in% c(3,4,5,7) | ER46452 %in% c(3,4,5,7) ~ "Other",
                             ER46449 == 1 | ER46450 == 1 | ER46451 == 1 | ER46452 == 1 ~ "White"),
    race.hd_2011 = case_when(ER51903 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER51904 == 2 | ER51905 == 2 | ER51906 == 2 | ER51907 == 2 ~ "Black", 
                             ER51904 %in% c(3,4,5,7) | ER51905 %in% c(3,4,5,7) | ER51906 %in% c(3,4,5,7) | ER51907 %in% c(3,4,5,7) ~ "Other",
                             ER51904 == 1 | ER51905 == 1 | ER51906 == 1 | ER51907 == 1 ~ "White"), 
    race.wf_2011 = case_when(ER51809 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER51810 == 2 | ER51811 == 2 | ER51812 == 2 | ER51813 == 2 ~ "Black", 
                             ER51810 %in% c(3,4,5,7) | ER51811 %in% c(3,4,5,7) | ER51812 %in% c(3,4,5,7) | ER51813 %in% c(3,4,5,7) ~ "Other",
                             ER51810 == 1 | ER51811 == 1 | ER51812 == 1 | ER51813 == 1 ~ "White"),
    race.hd_2013 = case_when(ER57658 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER57659 == 2 | ER57660 == 2 | ER57661 == 2 | ER57662 == 2 ~ "Black", 
                             ER57659 %in% c(3,4,5,7) | ER57660 %in% c(3,4,5,7) | ER57661 %in% c(3,4,5,7) | ER57662 %in% c(3,4,5,7) ~ "Other",
                             ER57659 == 1 | ER57660 == 1 | ER57661 == 1 | ER57662 == 1 ~ "White"), 
    race.wf_2013 = case_when(ER57548 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER57549 == 2 | ER57550 == 2 | ER57551 == 2 | ER57552 == 2 ~ "Black", 
                             ER57549 %in% c(3,4,5,7) | ER57550 %in% c(3,4,5,7) | ER57551 %in% c(3,4,5,7) | ER57552 %in% c(3,4,5,7) ~ "Other",
                             ER57549 == 1 | ER57550 == 1 | ER57551 == 1 | ER57552 == 1 ~ "White"),
    race.hd_2015 = case_when(ER64809 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER64810 == 2 | ER64811 == 2 | ER64812 == 2 | ER64813 == 2 ~ "Black", 
                             ER64810 %in% c(3,4,5,7) | ER64811 %in% c(3,4,5,7) | ER64812 %in% c(3,4,5,7) | ER64813 %in% c(3,4,5,7) ~ "Other",
                             ER64810 == 1 | ER64811 == 1 | ER64812 == 1 | ER64813 == 1 ~ "White"), 
    race.wf_2015 = case_when(ER64670 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER64671 == 2 | ER64672 == 2 | ER64673 == 2 | ER64674 == 2 ~ "Black", 
                             ER64671 %in% c(3,4,5,7) | ER64672 %in% c(3,4,5,7) | ER64673 %in% c(3,4,5,7) | ER64674 %in% c(3,4,5,7) ~ "Other",
                             ER64671 == 1 | ER64672 == 1 | ER64673 == 1 | ER64674 == 1 ~ "White"),
    race.hd_2017 = case_when(ER70881 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER70882 == 2 | ER70883 == 2 | ER70884 == 2 | ER70885 == 2 ~ "Black", 
                             ER70882 %in% c(3,4,5,7) | ER70883 %in% c(3,4,5,7) | ER70884 %in% c(3,4,5,7) | ER70885 %in% c(3,4,5,7) ~ "Other",
                             ER70882 == 1 | ER70883 == 1 | ER70884 == 1 | ER70885 == 1 ~ "White"), 
    race.wf_2017 = case_when(ER70743 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER70744 == 2 | ER70745 == 2 | ER70746 == 2 | ER70747 == 2 ~ "Black", 
                             ER70744 %in% c(3,4,5,7) | ER70745 %in% c(3,4,5,7) | ER70746 %in% c(3,4,5,7) | ER70747 %in% c(3,4,5,7) ~ "Other",
                             ER70744 == 1 | ER70745 == 1 | ER70746 == 1 | ER70747 == 1 ~ "White"),
    race.hd_2019 = case_when(ER76896 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER76897 == 2 | ER76898 == 2 | ER76899 == 2 | ER76900 == 2 ~ "Black", 
                             ER76897 %in% c(3,4,5,7) | ER76898 %in% c(3,4,5,7) | ER76899 %in% c(3,4,5,7) | ER76900 %in% c(3,4,5,7) ~ "Other",
                             ER76897 == 1 | ER76898 == 1 | ER76899 == 1 | ER76900 == 1 ~ "White"), 
    race.wf_2019 = case_when(ER76751 %in% c(1, 2, 3, 4, 5, 7) ~ "Hispanic", 
                             ER76752 == 2 | ER76753 == 2 | ER76754 == 2 | ER76755 == 2 ~ "Black", 
                             ER76752 %in% c(3,4,5,7) | ER76753 %in% c(3,4,5,7) | ER76754 %in% c(3,4,5,7) | ER76755 %in% c(3,4,5,7) ~ "Other",
                             ER76752 == 1 | ER76753 == 1 | ER76754 == 1 | ER76755 == 1 ~ "White")
  ) %>%
  gather(key, value, -c(family_id, person_number, indiv.id)) %>%
  separate(key, into = c("key", "year"), sep = "_") %>%
  # Grouping by individual id
  group_by(indiv.id) %>%
  # Turning data back to wide format, each record is a person-year
  spread(key, value, convert = T) %>%
  mutate(race = case_when(rel.head == 10 ~ race.hd, # Coding race as race of head if R is head, 
                          rel.head %in% c(20, 22) ~ race.wf)) %>% # race of wive if R is wife
  dplyr::select(indiv.id, year, race) %>%
  # This gives us a wide dataset where each row is an individual & each column is the coded race for that year
  spread(key = year, value = race) 

# Creating a new dataframe that codes a respondent's race based on all of the answers they've ever reported
psid_race_ever <- unite(psid_race, newCol, -indiv.id) %>% # Creates a variable that strings together each year's race report
  # Codes respondent's race as Hispanic if they ever report Hispanic ancestry, Black if they
  # are ever coded as Black, Other if they are ever coded as Other, and White if they are never coded as
  # any of the above but are ever coded as White. The remainder are set to missing implicitly by case_when
  mutate(race.ever = case_when(grepl("Hispanic", newCol) ~ "Hispanic", 
                               grepl("Black", newCol) ~ "Black", 
                               grepl("Other", newCol) ~ "Other", 
                               grepl("White", newCol) ~ "White")) %>%
  dplyr::select(-newCol)

# Joining ever reported race to the observed data
psid_obs <- benchmark.mar %>%
  left_join(., psid_race_ever, by = "indiv.id") %>%
  # Coding a respondent's race as the race they ever reported if available (~90%): if not, 
  # R is coded as race of the head (for ex. if respondent attrits prior to 1985 but is in 
  # the data in 1981)
  mutate(race = ifelse(is.na(race.ever), racehd, race.ever))

rm(benchmark.mar, psid_race, psid_race_ever)

write_csv(psid_obs, "clean_data/intermediate_psid_clean_mar_race.csv")
print("Done with: Merging Race to Main, Marital History File")

#------------------------------------------------------------------------------
# CREATING WORK EXPERIENCE VARIABLES USING YEARLY HOURS WORKED & REPORTED EXP
#------------------------------------------------------------------------------

# Generating the work experience variables using yearly hours worked & reported experience variables
psid_exp <- read.dta("raw_data/psid/psid.dta") %>%
  # Selecting relevant variables & renaming by year
  transmute(
    intnum68 = ER30001, pernum68 = ER30002,female = ifelse(ER32000 == 2, 1, 0), 
    int.num_1976 = ER30188, seq.num_1976 = ER30189, rel.head_1976 = ER30190, age_1976 = ER30191,
    yearswrk_hd_1976 = V4630, yearswrk.ft_hd_1976 = V4631, yearswrk_wf_1976 = V4989, yearswrk.ft_wf_1976 = V4990,
    int.num_1977 = ER30217, seq.num_1977 = ER30218, rel.head_1977 = ER30219, age_1977 = ER30220,
    ann.wrk.hrs_hd_1977 = V5232, ann.wrk.hrs_wf_1977 = V5244, 
    yearswrk_hd_1977 = V5604, yearswrk.ft_hd_1977 = V5605, yearswrk_wf_1977 = V5574, yearswrk.ft_wf_1977 = V5575,
    int.num_1978 = ER30246, seq.num_1978 = ER30247, rel.head_1978 = ER30248, age_1978 = ER30249,
    ann.wrk.hrs_hd_1978 = V5731, ann.wrk.hrs_wf_1978 = V5743,
    yearswrk_hd_1978 = V6153, yearswrk.ft_hd_1978 = V6154, yearswrk_wf_1978 = V6123, yearswrk.ft_wf_1978 = V6124,
    int.num_1979 = ER30283, seq.num_1979 = ER30284, rel.head_1979 = ER30285, age_1979 = ER30286, 
    age_1979 = ER30286, ann.wrk.hrs_hd_1979 = V6336, ann.wrk.hrs_wf_1979 = V6348,
    yearswrk_hd_1979 = V6750, yearswrk.ft_hd_1979 = V6751, yearswrk_wf_1979 = V6720, yearswrk.ft_wf_1979 = V6721,
    int.num_1980 = ER30313, seq.num_1980 = ER30314, rel.head_1980 = ER30315, age_1980 = ER30316,
    age_1980 = ER30316, ann.wrk.hrs_hd_1980 = V6934, ann.wrk.hrs_wf_1980 = V6946,
    yearswrk_hd_1980 = V7383, yearswrk.ft_hd_1980 = V7384, yearswrk_wf_1980 = V7353, yearswrk.ft_wf_1980 = V7354,
    int.num_1981 = ER30343, seq.num_1981 = ER30344, rel.head_1981 = ER30345, age_1981 = ER30346,
    age_1981 = ER30346, ann.wrk.hrs_hd_1981 = V7530, ann.wrk.hrs_wf_1981 = V7540,
    yearswrk_hd_1981 = V8035, yearswrk.ft_hd_1981 = V8036, yearswrk_wf_1981 = V8005, yearswrk.ft_wf_1981 = V8006,
    int.num_1982 = ER30373, seq.num_1982 = ER30374, rel.head_1982 = ER30375, age_1982 = ER30376,
    ann.wrk.hrs_hd_1982 = V8228, ann.wrk.hrs_wf_1982 = V8238,
    yearswrk_hd_1982 = V8659, yearswrk.ft_hd_1982 = V8660, yearswrk_wf_1982 = V8629, yearswrk.ft_wf_1982 = V8630,
    int.num_1983 = ER30399, seq.num_1983 = ER30400, rel.head_1983 = ER30401, age_1983 = ER30402,
    ann.wrk.hrs_hd_1983 = V8830, ann.wrk.hrs_wf_1983 = V8840,
    yearswrk_hd_1983 = V9345, yearswrk.ft_hd_1983 = V9346, yearswrk_wf_1983 = V9315, yearswrk.ft_wf_1983 = V9316,
    int.num_1984 = ER30429, seq.num_1984 = ER30430, rel.head_1984 = ER30431, age_1984 = ER30432, 
    ann.wrk.hrs_hd_1984 = V10037, ann.wrk.hrs_wf_1984 = V10131,
    yearswrk_hd_1984 = V10992, yearswrk.ft_hd_1984 = V10993, yearswrk_wf_1984 = V10962, yearswrk.ft_wf_1984 = V10963,
    int.num_1985 = ER30463, seq.num_1985 = ER30464, rel.head_1985 = ER30465, age_1985 = ER30466, 
    ann.wrk.hrs_hd_1985 = V11146, ann.wrk.hrs_wf_1985 = V11258,
    # For 1985 only, iniformation on years worked after age 18 was collected separately for 
    # employed vs. unemployed heads/wives, so we use information for both of these groups to
    # create a unified measure later in the pipeline
    yearswrk_hd_e_1985 = V11739, yearswrk.ft_hd_e_1985 = V11740, yearswrk_wf_e_1985 = V12102, yearswrk.ft_wf_e_1985 = V12103,
    yearswrk_hd_u_1985 = V11828, yearswrk.ft_hd_u_1985 = V11829, yearswrk_wf_u_1985 = V12191, yearswrk.ft_wf_u_1985 = V12192,
    int.num_1986 = ER30498, seq.num_1986 = ER30499, rel.head_1986 = ER30500, age_1986 = ER30501,
    ann.wrk.hrs_hd_1986 = V12545, ann.wrk.hrs_wf_1986 = V12657,
    yearswrk_hd_1986 = V13605, yearswrk.ft_hd_1986 = V13606, yearswrk_wf_1986 = V13531, yearswrk.ft_wf_1986 = V13532,
    int.num_1987 = ER30535, seq.num_1987 = ER30536, rel.head_1987 = ER30537, age_1987 = ER30538,
    ann.wrk.hrs_hd_1987 = V13745, ann.wrk.hrs_wf_1987 = V13809,
    yearswrk_hd_1987 = V14652, yearswrk.ft_hd_1987 = V14653, yearswrk_wf_1987 = V14578, yearswrk.ft_wf_1987 = V14579,
    int.num_1988 = ER30570, seq.num_1988 = ER30571, rel.head_1988 = ER30572, age_1988 = ER30573,
    ann.wrk.hrs_hd_1988 = V14835, ann.wrk.hrs_wf_1988 = V14865,
    yearswrk_hd_1988 = V16126, yearswrk.ft_hd_1988 = V16127, yearswrk_wf_1988 = V16052, yearswrk.ft_wf_1988 = V16053,
    int.num_1989 = ER30606, seq.num_1989 = ER30607, rel.head_1989 = ER30608, age_1989 = ER30609,
    ann.wrk.hrs_hd_1989 = V16335, ann.wrk.hrs_wf_1989 = V16365,
    yearswrk_hd_1989 = V17523, yearswrk.ft_hd_1989 = V17524, yearswrk_wf_1989 = V17449, yearswrk.ft_wf_1989 = V17450,
    int.num_1990 = ER30642, seq.num_1990 = ER30643, rel.head_1990 = ER30644, age_1990 = ER30645, 
    ann.wrk.hrs_hd_1990 = V17744, ann.wrk.hrs_wf_1990 = V17774,
    yearswrk_hd_1990 = V18854, yearswrk.ft_hd_1990 = V18855, yearswrk_wf_1990 = V18780, yearswrk.ft_wf_1990 = V18781,
    int.num_1991 = ER30689, seq.num_1991 = ER30690, rel.head_1991 = ER30691, age_1991 = ER30692,
    ann.wrk.hrs_hd_1991 = V19044, ann.wrk.hrs_wf_1991 = V19074,
    yearswrk_hd_1991 = V20154, yearswrk.ft_hd_1991 = V20155, yearswrk_wf_1991 = V20080, yearswrk.ft_wf_1991 = V20081,
    int.num_1992 = ER30733, seq.num_1992 = ER30734, rel.head_1992 = ER30735, age_1992 = ER30736,
    ann.wrk.hrs_hd_1992 = V20344, ann.wrk.hrs_wf_1992 = V20374,
    yearswrk_hd_1992 = V21460, yearswrk.ft_hd_1992 = V21461, yearswrk_wf_1992 = V21386, yearswrk.ft_wf_1992 = V21387,
    int.num_1993 = ER30806, seq.num_1993 = ER30807, rel.head_1993 = ER30808, age_1993 = ER30809, 
    ann.wrk.hrs_hd_1993 = V21634, ann.wrk.hrs_wf_1993 = V21670, 
    yearswrk_hd_1993 = V23316, yearswrk.ft_hd_1993 = V23317, yearswrk_wf_1993 = V23243, yearswrk.ft_wf_1993 = V23244,
    int.num_1994 = ER33101, seq.num_1994 = ER33102, rel.head_1994 = ER33103, age_1994 = ER33104, 
    ann.wrk.hrs_hd_1994 = ER4096, ann.wrk.hrs_wf_1994 = ER4107,
    yearswrk_hd_1994 = ER3985, yearswrk.ft_hd_1994 = ER3986, yearswrk_wf_1994 = ER3915, yearswrk.ft_wf_1994 = ER3916,
    int.num_1995 = ER33201, seq.num_1995 = ER33202, rel.head_1995 = ER33203, age_1995 = ER33204, 
    ann.wrk.hrs_hd_1995 = ER6936, ann.wrk.hrs_wf_1995 = ER6947, 
    yearswrk_hd_1995 = ER6855, yearswrk.ft_hd_1995 = ER6856, yearswrk_wf_1995 = ER6785, yearswrk.ft_wf_1995 = ER6786,
    int.num_1996 = ER33301, seq.num_1996 = ER33302, rel.head_1996 = ER33303, age_1996 = ER33304,
    ann.wrk.hrs_hd_1996 = ER9187, ann.wrk.hrs_wf_1996 = ER9198,
    yearswrk_hd_1996 = ER9101, yearswrk.ft_hd_1996 = ER9102, yearswrk_wf_1996 = ER9031, yearswrk.ft_wf_1996 = ER9032,
    int.num_1997 = ER33401, seq.num_1997 = ER33402, rel.head_1997 = ER33403, age_1997 = ER33404,
    ann.wrk.hrs_hd_1997 = ER12174, ann.wrk.hrs_wf_1997 = ER12185,
    yearswrk_hd_1997 = ER11897, yearswrk.ft_hd_1997 = ER11898, yearswrk_wf_1997 = ER11809, yearswrk.ft_wf_1997 = ER11810,
    # Starting in 1999, we begin having different information due to the PSID's switch to biannual interviewing
    # In different years, they collect different information on time worked during the "gap" year (that is, the year prior
    # to the year that the respondent is reporting data from in the subsequent interview)
    # In the 1999 interview, the PSID asks respondents the following information for how many hours they worked in 1997:
    # Weeks worked on any job in 1997, total number of months, and in those months, hours usually worked per week
    # This information is reported in the individual data, not the family-level data
    # We label these "1998" for consistency with other years, because each actual survey year is capturing info about the prior year
    int.num_1998 = ER33501, seq.num_1998 = ER33502, rel.head_1998 = ER33503, wkswrktot_1998 = ER33536C, monthswrk_1998 = ER33536P,
    hrsperweek_1998 = ER33536Q, yearswrk_hd_1998 = NA, yearswrk.ft_hd_1998 = NA, yearswrk_wf_1998 = NA, yearswrk.ft_wf_1998 = NA,
    ann.wrk.hrs_hd_1998 = NA, ann.wrk.hrs_wf_1998 = NA,
    # In the same 1999 interview, as with other years, the PSID also collects information about total hours worked for head/spouse 
    int.num_1999 = ER33501, seq.num_1999 = ER33502, rel.head_1999 = ER33503, age_1999 = ER33504,
    ann.wrk.hrs_hd_1999 = ER16471, ann.wrk.hrs_wf_1999 = ER16482,
    yearswrk_hd_1999 = ER15979, yearswrk.ft_hd_1999 = ER15980, yearswrk_wf_1999 = ER15886, yearswrk.ft_wf_1999 = ER15887,
    # We use information collected in the 2001 interview to fill in information about hours worked in 1999,
    # labeled here as "2000" to match the structure where each survey year reports info on hours worked in prior calendar year
    # Simliar to the 1999 interview, the 2001 interview collects information on 
    # weeks worked on any job in 1999, total number of months, and in those months, hours usually worked per week
    int.num_2000 = ER33601, seq.num_2000 = ER33602, rel.head_2000 = ER33603, wkswrktot_2000 = ER33627C, monthswrk_2000 = ER33627P,
    hrsperweek_2000 = ER33627Q, yearswrk_hd_2000 = NA, yearswrk.ft_hd_2000 = NA, yearswrk_wf_2000 = NA, yearswrk.ft_wf_2000 = NA,
    ann.wrk.hrs_hd_2000 = NA, ann.wrk.hrs_wf_2000 = NA,
    # As with 1999, in the same 2001 interview (as with other years) the PSID also collects information about total hours worked for head/spouse 
    int.num_2001 = ER33601, seq.num_2001 = ER33602, rel.head_2001 = ER33603, age_2001 = ER33604,
    ann.wrk.hrs_hd_2001 = ER20399, ann.wrk.hrs_wf_2001 = ER20410,
    yearswrk_hd_2001 = ER20040, yearswrk.ft_hd_2001 = ER20041, yearswrk_wf_2001 = ER19947, yearswrk.ft_wf_2001 = ER19948,
    # Starting in the 2003 survey wave, the PSID reports:
    # Whether heads/spouses were employed in the gap year (so in the 2003 survey wave, wether employed in 2001)
    # Average hours of week worked by heads/spouses during the gap year
    # Number of weeks employed by head and spouse in the gap year via the employment history calendar
    int.num_2002 = ER33701, seq.num_2002 = ER33702, rel.head_2002 = ER33703,
    # In survey wave 2003, 2005, and 2007, the PSID also reports 
    # weeks worked on any job in the gap year and hours usually worked per week 
    # in the individual level files. In 2003 only, they also report total number of months worked during the gap year.
    wkswrktot_2002 = ER33727C, monthswrk_2002 = ER33727P, hrsperweek_2002 = ER33727Q,
    yearswrk_hd_2002 = NA, yearswrk.ft_hd_2002 = NA,
    yearswrk_wf_2002 = NA, yearswrk.ft_wf_2002 = NA, ann.wrk.hrs_hd_2002 = NA, ann.wrk.hrs_wf_2002 = NA,
    employed_hd_2002 = ER23702D2, employed_wf_2002 = ER23702J5, wkswrk_hd_2002 = ER23702D3, wkswrk_wf_2002 = ER23702J6,
    hrs.per.wk_hd_2002 = ER23702E8, hrs.per.wk_wf_2002 = ER23702L2,
    int.num_2003 = ER33701, seq.num_2003 = ER33702, rel.head_2003 = ER33703, age_2003 = ER33704, 
    ann.wrk.hrs_hd_2003 = ER24080, ann.wrk.hrs_wf_2003 = ER24091,
    yearswrk_hd_2003 = ER23476, yearswrk.ft_hd_2003 = ER23477, yearswrk_wf_2003 = ER23384, yearswrk.ft_wf_2003 = ER23385,
    int.num_2004 = ER33801, seq.num_2004 = ER33802, rel.head_2004 = ER33803, 
    wkswrktot_2004 = ER33827S, monthswrk_2004 = NA,  hrsperweek_2004 = ER33827U,
    yearswrk_hd_2004 = NA, yearswrk.ft_hd_2004 = NA,
    yearswrk_wf_2004 = NA, yearswrk.ft_wf_2004 = NA, ann.wrk.hrs_hd_2004 = NA, ann.wrk.hrs_wf_2004 = NA,
    employed_hd_2004 = ER27711D2, employed_wf_2004 = ER27711J5, wkswrk_hd_2004 = ER27711D3, wkswrk_wf_2004 = ER27711J6,
    hrs.per.wk_hd_2004 = ER27711E8, hrs.per.wk_wf_2004 = ER27711L2,
    int.num_2005 = ER33801, seq.num_2005 = ER33802, rel.head_2005 = ER33803, age_2005 = ER33804, 
    ann.wrk.hrs_hd_2005 = ER27886, ann.wrk.hrs_wf_2005 = ER27897,
    yearswrk_hd_2005 = ER27444, yearswrk.ft_hd_2005 = ER27445, yearswrk_wf_2005 = ER27348, yearswrk.ft_wf_2005 = ER27349,
    int.num_2006 = ER33901, seq.num_2006 = ER33902, rel.head_2006 = ER33903, 
    wkswrktot_2006 = ER33927A, monthswrk_2006 = NA, hrsperweek_2006 = ER33927C,
    yearswrk_hd_2006 = NA, yearswrk.ft_hd_2006 = NA,
    yearswrk_wf_2006 = NA, yearswrk.ft_wf_2006 = NA, ann.wrk.hrs_hd_2006 = NA, ann.wrk.hrs_wf_2006 = NA, 
    employed_hd_2006 = ER40686D2, employed_wf_2006 = ER40686J5, wkswrk_hd_2006 = ER40686D3, wkswrk_wf_2006 = ER40686J6,
    hrs.per.wk_hd_2006 = ER40686E8, hrs.per.wk_wf_2006 = ER40686L2,
    int.num_2007 = ER33901, seq.num_2007 = ER33902, rel.head_2007 = ER33903, age_2007 = ER33904,
    ann.wrk.hrs_hd_2007 = ER40876, ann.wrk.hrs_wf_2007 = ER40887, 
    yearswrk_hd_2007 = ER40616, yearswrk.ft_hd_2007 = ER40617, yearswrk_wf_2007 = ER40523, yearswrk.ft_wf_2007 = ER40524,
    int.num_2008 = ER34001, seq.num_2008 = ER34002, rel.head_2008 = ER34003, yearswrk_hd_2008 = NA, yearswrk.ft_hd_2008 = NA,
    yearswrk_wf_2008 = NA, yearswrk.ft_wf_2008 = NA, ann.wrk.hrs_hd_2008 = NA, ann.wrk.hrs_wf_2008 = NA,
    employed_hd_2008 = ER46669, employed_wf_2008 = ER46680, wkswrk_hd_2008 = ER46670, wkswrk_wf_2008 = ER46681,
    hrs.per.wk_hd_2008 = ER46671, hrs.per.wk_wf_2008 = ER46682,
    int.num_2009 = ER34001, seq.num_2009 = ER34002, rel.head_2009 = ER34003, age_2009 = ER34004, 
    ann.wrk.hrs_hd_2009 = ER46767, ann.wrk.hrs_wf_2009 = ER46788, 
    yearswrk_hd_2009 = ER46594, yearswrk.ft_hd_2009 = ER46595, yearswrk_wf_2009 = ER46500, yearswrk.ft_wf_2009 = ER46501,
    int.num_2010 = ER34101, seq.num_2010 = ER34102, rel.head_2010 = ER34103, yearswrk_hd_2010 = NA, yearswrk.ft_hd_2010 = NA,
    yearswrk_wf_2010 = NA, yearswrk.ft_wf_2010 = NA, ann.wrk.hrs_hd_2010 = NA, ann.wrk.hrs_wf_2010 = NA,
    employed_hd_2010 = ER52070, employed_wf_2010 = ER52081, wkswrk_hd_2010 = ER52071, wkswrk_wf_2010 = ER52082,
    hrs.per.wk_hd_2010 = ER52072, hrs.per.wk_wf_2010 = ER52083,
    int.num_2011 = ER34101, seq.num_2011 = ER34102, rel.head_2011 = ER34103, age_2011 = ER34104, 
    ann.wrk.hrs_hd_2011 = ER52175, ann.wrk.hrs_wf_2011 = ER52196, 
    yearswrk_hd_2011 = ER51955, yearswrk.ft_hd_2011 = ER51956, yearswrk_wf_2011 = ER51861, yearswrk.ft_wf_2011 = ER51862,
    int.num_2012 = ER34201, seq.num_2012 = ER34202, rel.head_2012 = ER34203, yearswrk_hd_2012 = NA, yearswrk.ft_hd_2012 = NA,
    yearswrk_wf_2012 = NA, yearswrk.ft_wf_2012 = NA, ann.wrk.hrs_hd_2012 = NA, ann.wrk.hrs_wf_2012 = NA, 
    employed_hd_2012 = ER57824, employed_wf_2012 = ER57872, wkswrk_hd_2012 = ER57825, wkswrk_wf_2012 = ER57873,
    hrs.per.wk_hd_2012 = ER57839, hrs.per.wk_wf_2012 = ER57887,
    int.num_2013 = ER34201, seq.num_2013 = ER34202, rel.head_2013 = ER34203, age_2013 = ER34204, 
    ann.wrk.hrs_hd_2013 = ER57976, ann.wrk.hrs_wf_2013 = ER57997, 
    yearswrk_hd_2013 = ER57711, yearswrk.ft_hd_2013 = ER57712, yearswrk_wf_2013 = ER57601, yearswrk.ft_wf_2013 = ER57602,
    int.num_2014 = ER34301, seq.num_2014 = ER34302, rel.head_2014 = ER34303, yearswrk_hd_2014 = NA, yearswrk.ft_hd_2014 = NA,
    yearswrk_wf_2014 = NA, yearswrk.ft_wf_2014 = NA, ann.wrk.hrs_hd_2014 = NA, ann.wrk.hrs_wf_2014 = NA,
    employed_hd_2014 = ER65004, employed_wf_2014 = ER65052, wkswrk_hd_2014 = ER65005, wkswrk_wf_2014 = ER65053,
    hrs.per.wk_hd_2014 = ER65019, hrs.per.wk_wf_2014 = ER65067,
    int.num_2015 = ER34301, seq.num_2015 = ER34302, rel.head_2015 = ER34303, age_2015 = ER34305,
    ann.wrk.hrs_hd_2015 = ER65156, ann.wrk.hrs_wf_2015 = ER65177, 
    yearswrk_hd_2015 = ER64871, yearswrk.ft_hd_2015 = ER64872, yearswrk_wf_2015 = ER64732, yearswrk.ft_wf_2015 = ER64733,
    int.num_2016 = ER34501, seq.num_2016 = ER34502, rel.head_2016 = ER34503, yearswrk_hd_2016 = NA, yearswrk.ft_hd_2016 = NA,
    yearswrk_wf_2016 = NA, yearswrk.ft_wf_2016 = NA, ann.wrk.hrs_hd_2016 = NA, ann.wrk.hrs_wf_2016 = NA,
    employed_hd_2016 = ER71096, employed_wf_2016 = ER71144, wkswrk_hd_2016 = ER71097, wkswrk_wf_2016 = ER71145,
    hrs.per.wk_hd_2016 = ER71111, hrs.per.wk_wf_2016 = ER71159,
    int.num_2017 = ER34501, seq.num_2017 = ER34502, rel.head_2017 = ER34503, age_2017 = ER34504, 
    ann.wrk.hrs_hd_2017 = ER71233, ann.wrk.hrs_wf_2017 = ER71254, 
    yearswrk_hd_2017 = ER70943, yearswrk.ft_hd_2017 = ER70944, yearswrk_wf_2017 = ER70805, yearswrk.ft_wf_2017 = ER70806,
    int.num_2018 = ER34701, seq.num_2018 = ER34702, rel.head_2018 = ER34703, yearswrk_hd_2018 = NA, yearswrk.ft_hd_2018 = NA,
    yearswrk_wf_2018 = NA, yearswrk.ft_wf_2018 = NA, ann.wrk.hrs_hd_2018 = NA, ann.wrk.hrs_wf_2018 = NA,
    employed_hd_2018 = ER77118, employed_wf_2018 = ER77166, wkswrk_hd_2018 = ER77119, wkswrk_wf_2018 = ER77167,
    hrs.per.wk_hd_2018 = ER77133, hrs.per.wk_wf_2018 = ER77181,
    int.num_2019 = ER34701, seq.num_2019 = ER34702, rel.head_2019 = ER34703, age_2019 = ER34704, 
    ann.wrk.hrs_hd_2019 = ER77255, ann.wrk.hrs_wf_2019 = ER77276, 
    yearswrk_hd_2019 = ER76961, yearswrk.ft_hd_2019 = ER76962, yearswrk_wf_2019 = ER76816, yearswrk.ft_wf_2019 = ER76817
  ) %>%
  # Turning to long format where key = varname for all vars except 
  # sex, 1968 household and person numbers
  gather(key, value, -c(pernum68, intnum68, female)) %>%
  # Create individual id, year column, removing year from variable label
  mutate(indiv.id = paste(intnum68, pernum68, sep = "_"),
         year = as.numeric(str_match(key, "[0-9]+")),
         var = str_remove(key, "_[0-9]+[0-9]+")) %>%
  dplyr::select(-key) %>%
  # Grouping by individual id
  group_by(indiv.id) %>%
  # Turning data back to wide format, each record is a person-year
  spread(var, value, convert = T) %>%
  mutate(rel.head =  ifelse(rel.head %in% c(1, 10), "head", ifelse(rel.head %in% c(2, 20, 22), "wife", "other")),
         wkswrktot = ifelse(wkswrktot > 52, NA, wkswrktot), # Recoding missing values as missing
         hrsperweek = ifelse(hrsperweek > 112, NA, hrsperweek), # Recoding missing values as missing
         monthswrk = ifelse(monthswrk > 12, NA, monthswrk),
         # Recoding years worked information for 1985 that capture this measure separately by employment status
         # to align with our measures for all other years
         yearswrk_hd = ifelse(year == 1985 & complete.cases(yearswrk_hd_u) & yearswrk_hd_u != 0, yearswrk_hd_u, 
                              ifelse(year == 1985, yearswrk_hd_e, yearswrk_hd)), 
         yearswrk_wf = ifelse(year == 1985 & complete.cases(yearswrk_wf_u) & yearswrk_wf_u != 0, yearswrk_wf_u, 
                              ifelse(year == 1985, yearswrk_wf_e, yearswrk_wf)), 
         yearswrk.ft_hd = ifelse(year == 1985 & complete.cases(yearswrk.ft_hd_u) & yearswrk.ft_hd_u != 0, yearswrk.ft_hd_u, 
                                 ifelse(year == 1985, yearswrk.ft_hd_e, yearswrk.ft_hd)), 
         yearswrk.ft_wf = ifelse(year == 1985 & complete.cases(yearswrk.ft_wf_u) & yearswrk.ft_wf_u != 0, yearswrk.ft_wf_u, 
                                 ifelse(year == 1985, yearswrk.ft_wf_e, yearswrk.ft_wf)) 
  ) %>% 
  # Selecting only the individuals in our sample
  filter(indiv.id %in% unique(psid_obs$indiv.id))

# Labeling gap years for annual hours worked predictions: two separate
# labels because for each group of years, we use a different set of available
# variables to generate annual hours worked in the gap year predictions
gapyr.initial <- c(2000, 1998)
gapyr.later <- c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018)

# Creating a new object cleaning the experience variables
psid_exp.clean <- psid_exp %>%
  mutate(
    age = na_if(age, 999),
    # Assigning variables corresponding to heads to Rs who are heads, same with wives 
    # Also setting missing codes to missing
    yrswrk = case_when(rel.head == "head" ~ yearswrk_hd, 
                       rel.head == "wife" ~ yearswrk_wf), 
    yrswrk.clean = ifelse(yrswrk > 97, NA, yrswrk), 
    yrswrk.ft = case_when(rel.head == "head" ~ yearswrk.ft_hd,
                          rel.head == "wife" ~ yearswrk.ft_wf), 
    yrswrk.ft.clean = ifelse(yrswrk.ft > 97, NA, yrswrk.ft), 
    # Following Blau & Khan procedure to censor measured years of experience to max out at years worked after age 18
    yrswrk.clean.cen = ifelse(yrswrk.clean > age-18, age-18, yrswrk.clean),
    yrswrk.ft.clean.cen = ifelse(yrswrk.ft.clean > age-18, age-18, yrswrk.ft.clean),
    yrswrk.clean.cen = ifelse(yrswrk.clean.cen < 0, 0, yrswrk.clean.cen),
    yrswrk.ft.clean.cen = ifelse(yrswrk.ft.clean.cen < 0, 0, yrswrk.ft.clean.cen),
    # Cleaning employed, hours per week and weeks per year later gap year vars
    wkswrk = case_when(rel.head == "head" ~ wkswrk_hd, 
                       rel.head == "wife" ~ wkswrk_wf),
    wkswrk.clean = ifelse(wkswrk > 52, NA, wkswrk), 
    hrswk = case_when(rel.head == "head" ~ hrs.per.wk_hd, 
                      rel.head == "wife" ~ hrs.per.wk_wf),
    hrswk.clean = ifelse(hrswk >= 998, NA, hrswk), 
    employed = case_when(rel.head == "head" ~ employed_hd,
                         rel.head == "wife" ~ employed_wf), 
    ann.wrk.hrs = case_when(rel.head == "head" ~ ann.wrk.hrs_hd, 
                            rel.head == "wife" ~ ann.wrk.hrs_wf),
    # Estimating annual work hours for the gap years
    # If the gap year is in years for which we have individual-level data, we use that: 1998, 2000, 2002
    ann.wrk.hrs.pred = ifelse(year %in% c(1998, 2000) &
                                is.na(wkswrktot) & complete.cases(monthswrk), monthswrk*4.33*hrsperweek,
                              ifelse(year %in% c(1998, 2000) &
                                       complete.cases(wkswrktot),wkswrktot*hrsperweek,
                                     # For years 2004-2006, we only have the individual level data for other members in FU, 
                                     # and heads and wives' information is coded separately
                                     ifelse(year %in% c(2002, 2004, 2006) &
                                              rel.head %!in% c("head", "wife"), wkswrktot*hrsperweek,
                                            # For heads  and wives starting in 2002 and all subsequent gap years, we use the wks worked and 
                                            # hours worked measures to generate predicted annual work hours
                                            ifelse(year %in% c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018) &
                                                     employed == 1,  wkswrk.clean*hrswk.clean, 
                                                   # For heads and wives that report not working for money in the gap year
                                                   # according to the employment history calendar, we set their work hours to zero
                                                   ifelse(year %in% c(2002, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018) &
                                                            employed == 5, 0, 
                                                          # For heads and wives in non-gap years, we use the annual hours worked reported 
                                                          # For individuals who are not heads or wives, this essentially defaults this measure to NA
                                                          ann.wrk.hrs)))))) %>% 
  # Selecting relevant variables
  dplyr::select(indiv.id, intnum68, pernum68, year, female, rel.head, age, ann.wrk.hrs,
                employed,
                ann.wrk.hrs.pred, yrswrk.clean.cen, yrswrk.ft.clean.cen) %>%
  arrange(indiv.id, year) %>%
  mutate(
    # Filling in age variable in the gap year
    # We use this because in the very last step of the cumulative sum, we link these measures to the preceding wave, not their survey wave, 
    age = ifelse(is.na(age) & lag(age) != 0, lag(age) + 1, age), 
    # The gap year fill variables take the measured years of experience variable as of the year
    # prior to the gap year and assigns that value to the gap year. Reminder that these variables
    # are only updated in the PSID in 1985 or once R joins/establishes a new household
    yrswrk.gapyrfill = ifelse(year %in% c(gapyr.initial, gapyr.later), lag(yrswrk.clean.cen), yrswrk.clean.cen),
    yrswrk.ft.gapyrfill = ifelse(year %in% c(gapyr.initial, gapyr.later), lag(yrswrk.ft.clean.cen), yrswrk.ft.clean.cen),
    # Creating measures of working positive hours, working full-time hours, or working part-time hours 
    # based on the annual hours worked variable created w/ information on the gap years
    wrk.pos = ifelse(ann.wrk.hrs.pred > 0, 1, 0), 
    wrk.ft = ifelse(ann.wrk.hrs.pred >= 1500, 1, 0),
    wrk.pt = ifelse(wrk.pos == 1 & wrk.ft != 1, 1, 0)) %>%
  # Selecting relevant variables
  dplyr::select(indiv.id, intnum68, pernum68, year, female, age, rel.head, ann.wrk.hrs, ann.wrk.hrs.pred, 
                employed,
                yrswrk.gapyrfill, yrswrk.ft.gapyrfill, wrk.pos, wrk.ft, wrk.pt) %>%
  ungroup() %>%
  mutate(rowid = row_number(indiv.id)) # Creating a measure of number of observations by individual

# This creates a new object that uses cumulative sums to generate the final experience measures for R by year
psid_exp.clean.2 <- psid_exp.clean %>%
  mutate(
    # The next two variable takes the observed value for yrswrk or yrswrk.ft the FIRST time a new value is
    # observed for a respondent. The PSID asks respondents about their years of total and full-time experience 
    # when they first join the survey or when they become new heads. They also ask this question of all Rs in 1985:
    # These "fill" variables set any variable subsequent to a newly observed value as missing: so if a respondent 
    # has observed values for years of experience in 1978, 1985, and 2000, these variables capture the measured
    # years of experience only in these "updated" years for that respondent, setting the other years for that R as NA
    yrswrk_fill = ifelse(rowid %in% distinct(psid_exp.clean, indiv.id, yrswrk.gapyrfill, .keep_all = T)$rowid, 
                         yrswrk.gapyrfill, NA),
    yrswrk.ft_fill = ifelse(rowid %in% distinct(psid_exp.clean, indiv.id, yrswrk.ft.gapyrfill, .keep_all = T)$rowid,
                            yrswrk.ft.gapyrfill, NA)) %>%
  dplyr::select(-rowid) %>% # Removes the row id variable 
  mutate(
    # These next two variables use the variables above as a baseline, 
    # which have missing values for that year UNLESS new information is collected on R's work experience in that year.
    # If no new information is collected on R's experience that year, the variables below are filled as follows:
    # if the respondent reports working positive hours/ft hours that year, the value for that year/variable takes a value of 1.
    # If R is coded as NOT having worked positive/ft hours that year, the value for that year/variable takes a value of 0
    yrswrk_final = ifelse(is.na(yrswrk_fill) & wrk.pos == 1, 1, ifelse(is.na(yrswrk_fill) & wrk.pos == 0, 0, yrswrk_fill)),
    yrswrk.ft_final = ifelse(is.na(yrswrk.ft_fill) & wrk.ft == 1, 1, ifelse(is.na(yrswrk.ft_fill) & wrk.ft == 0, 0, yrswrk.ft_fill))) %>%
  # This groups respondent by individual and by the "group" of years for that R with matching work experience info,
  # meaning that no new work experience info was collected. The following variables cumulatively sum the variables
  # that record new information on work experience, if recorded, and if not, count whether the respondent was 
  # working that year- this gives us our year-by-year measure of work experience- this measure is defined as the 
  # years of work experience LAST reported by R prior to that year PLUS the subsequent years in which R worked
  # positive/ft hours up to the baseline year. For example, in 1990, if the individual's work experience was last
  # updated in 1985, these variables will measure work experience as of 1985 + additional years of work experience
  # for each year up to 1990, defined by the observed number of hours worked in those years
  group_by(indiv.id, yrswrk.gapyrfill) %>%
  mutate(expt = cumsum(yrswrk_final),
         expf = cumsum(yrswrk.ft_final))

# This final object restricts the object above to our target outcome and covariate years and merges 
# the experience data to the rest of our outcome/covariate data. 
benchmark.exp <- psid_exp.clean.2 %>%
  filter(year %in% c(1980, 1981, 1990, 1991, 1999, 2001, 2009, 2011, 2017, 2019)) %>%
  dplyr::select(-c(ann.wrk.hrs, female, rel.head, age)) %>% # Remove variables already in the covariate data
  # Merge in to the covariate data by individual id and year
  left_join(psid_obs, ., by = c("indiv.id", "year")) %>%
  mutate(expf = ifelse(expf > age-17 & age !=0, age-17, expf), 
         expt = ifelse(expt > age-17 & age !=0, age-17, expt))

rm(psid_exp, psid_exp.clean, psid_exp.clean.2)

write_csv(benchmark.exp, "clean_data/intermediate_psid_clean_mar_race_exp.csv")
print("Done with: Merging Experience to Main, Marital History, Race File")

#------------------------------------------------------------------------------
# FERTILITY USING FERTILITY HISTORY FILES
#------------------------------------------------------------------------------

# Generating fertility variables using the fertility history data
# Fertility history data has an id for each child 
psid_fert <- read_xlsx("raw_data/psid_fertility/J295915.xlsx") %>%
  # Selecting relevant columns
  dplyr::select(intnum68 = CAH3, pernum68 = CAH4, record.type = CAH2, chnum1 = CAH10, 
                chnum2 = CAH11, yr.child.born = CAH15, total.children = CAH106) %>%
  mutate(
    bio.record = ifelse(record.type == 1, 1, 0), # Records whether child is biological or not
    yr.child.born = ifelse(yr.child.born %in% c(9998, 9999), NA, yr.child.born), # Assings NAs to missing codes for year in which child was born
    indiv.id = paste(intnum68, pernum68, sep = "_"), # R id
    child.id = paste(chnum1, chnum2, sep = "."), # Unique id for each child
    total.children = ifelse(total.children == 98, NA, total.children)) %>% # Total number of chidren as of date last collected
  # The row below expands the data to create a row for each individual id for each year 
  # in the range of years in which children were born- the child id column has the child id in 
  # the year in which that child was born to that individual. For other years the child id variable is set to zero
  tidyr::complete(indiv.id, yr.child.born, fill = list(child.id = "0.0")) %>%
  group_by(indiv.id) %>%
  arrange(indiv.id, yr.child.born) %>%
  mutate(counter = row_number(indiv.id)) %>%
  mutate(
    number = ifelse(child.id != "0.0", 1, 0), # This variable labels whether a child was born in that year
    counter = cumsum(number), # This cumulates the number of children by year for each individual
    # This variable documents the cumulative number of children by each year: if the cumulative number of children 
    # is greater than the measured total number of children, it takes the value of the total number of children
    num.children = ifelse(total.children < counter & complete.cases(total.children), total.children, counter),
    year = yr.child.born) %>%
  dplyr::select(-number) %>%
  dplyr::select(indiv.id, year, num.children) 

benchmark.fert <- psid_fert %>%
  # Selecting years prior to when other vars measured because we want to make sure the child was born in the calendar year 
  # before the covariates were measured, so that they for sure were born by the time the covariates were measured
  filter(year %in% c(1979, 1980, 1989, 1990, 1998, 2000, 2008, 2010, 2014, 2016, 2018)) %>% 
  mutate(year = year + 1) %>%
  # Joining with the covariate data
  left_join(benchmark.exp, .,  by = c("indiv.id", "year")) %>% 
  # Creating variable: if age at first birth missing in the covariate data and R does not have 
  # any children as per the PSID fertility files, then age at first birth is set to 9999
  mutate(afb = ifelse(is.na(age.first.birth) & num.children == 0, 9999, age.first.birth)) %>%
  # Note: if R has more than one child born in a target year, then R will have more than one record
  # for the target year in the fertility data and will thus have two rows in the merged data. 
  # In the procedure below, we keep only the last row for a given R in the merged data, which 
  # counts all children born in the target year if more than one child was born 
  group_by(year, indiv.id) %>%
  # First we count the total number of person-year observations for each R-year combination
  # and for each PY record, we label whether it is the first record, second, or so on: the last
  # record has the total number of children born as of that year, if more than one child was born
  mutate(n = n(), rownum = row_number()) %>%
  ungroup() %>% 
  # The following variable labels the last row number of the total PY-observations observed for that person
  # as the row to keep: this keeps the row that captures the total number of respondents born to that person
  # as of that year, and drops the extra rows which capture if more than one child was born that year
  mutate(keep = ifelse(rownum == n, "keep", "drop")) %>%
  filter(keep == "keep") %>%
  dplyr::select(-c(keep, rownum, n))

# PSID Fertility History files only started being collected in 1985. As a result, there is a subset
# of respondents observed in the early years who have attrited by 1985 (473, ~9 % of Rs) and are not in the fertility
# history file. For these respondents, we use data collected on the total number of kids and the 
# age of the oldest child born to the head (all respondents in later years are observed in Fertility History file)
psid.misskids <- read.dta("raw_data/psid/psid.dta") %>% 
  # Selecting relevant variables & renaming by year- 1980
  transmute(family_id = ER30001, person_number = ER30002, female = ifelse(ER32000 == 2, 1, 0), 
            indiv.id = paste(family_id, person_number, sep = "_"),
            rel.head_1980 = ifelse(ER30315 == 1, "head", ifelse(ER30315 == 2, "wife", "other")),
            rel.head_1981 = ifelse(ER30345 == 1, "head", ifelse(ER30345 == 2, "wife", "other")),
            age_1980 = na_if(ER30316, 999), age_1981 = na_if(ER30346, 999), 
            # Getting total number of children of head age of oldest kid of head 
            totkids.head_1980 = na_if(V7368, 99), age.oldest.kid.head_1980 = na_if(V7365,99), 
            totkids.head_1981 = na_if(V8020, 99), age.oldest.kid.head_1981  = na_if(V8017,99)) %>%
  gather(key, value, -c(family_id, person_number, indiv.id, female)) %>% # Turning to long format
  # Creating year variable based on the covariate label
  mutate(year = case_when(grepl("_1980", key) ~ 1980, 
                          grepl("_1981", key) ~ 1981),
         var = str_remove(key, "_[0-9]+[0-9]+")) %>%
  dplyr::select(-key) %>% # Removing extra key variable 
  # Grouping by individual id
  group_by(indiv.id) %>%
  # Turning data back to wide format, each record is a person-year
  spread(var, value, convert = T) %>%
  filter(rel.head %in% c("head", "wife")) %>%
  mutate(num.children.hd = totkids.head, # For heads and wives, assigns total # of children of head to heads & their wives
         # Generating year born for head's oldest child by subtracting age at 1980 interview from 1980
         yr.fb.hd = ifelse(num.children.hd == 0, NA, 1980-age.oldest.kid.head), 
         yr.born = 1980-age, # Getting year born for respondents, subtracting their age from 1980
         # Creating a measure for head/spouse's age when head's first child was born by subtracting 
         # the year head/wife was born from the year in which head's first child was born
         afb.synth = ifelse(num.children.hd == 0, 9999, yr.fb.hd - yr.born)) %>%
  dplyr::select(indiv.id, year, num.children.hd, yr.fb.hd, yr.born, afb.synth, 
                age.oldest.kid.head)

# Merging this additional data to the main data by individual id and year
benchmark.fert.synth <- left_join(benchmark.fert, psid.misskids,
                                  by = c("indiv.id", "year")) %>%
  mutate( # Creating additional variables:
    # If number of children missing, assigns number of children born to head in 1980
    # This leaves us with a missing rate of .5 percent for 1980
    num.children.synth = ifelse(is.na(num.children), num.children.hd, num.children), 
    # Creating an indicator variable for whether number of children comes from this imputing procedure:
    # of R's in 1980, 91.2 % (4910) have the number of kids assigned with fertility history files, 
    # 8.3 % (446) use the imputed value for head, and .5 % (27) remain missing
    dummy.nkids.synth = ifelse(is.na(num.children) & !is.na(num.children.synth), 1, 0), 
    # If age at first birth missing in the data, assigns age when head's oldest child was born
    afb.synth = ifelse(is.na(afb), afb.synth, afb),
    # Some Rs with >0 children have a value of 999 for afb.synth- this wrongly indicates they have no children
    # This is the case for individuals with missing data on AFB and get assigned AFB == 9999 because the head
    # in their household in 1980 has had no kids. But according to reports in the fertility files, these individuals
    # themselves have had children. We update the variable above by coding age at first birth as missing for
    # individuals that have missing age at first birth, but report having >0 children according to the fertility files
    afb.synth = ifelse(afb.synth == 9999 & complete.cases(num.children) & num.children > 0, NA, afb.synth),
    # Creating an indicator variable for whether age at first birth comes from this imputing procedure:
    # of R's in 1980, 88 % 4751 have the number of kids assigned with fertility history files, 
    # 9.9 % (533) use the value imputed based on age of oldest child of head, and 1.8 % (99) remain missing
    dummy.afb.synth = ifelse(is.na(afb) & !is.na(afb.synth), 1, 0))

# We then use the fertility history files to create an alternative measure of first birth
# for individuals with missing data on age at first birth. This procedure uses the fertility
# history files by observing the first year in which a respondent transitions from having no
# children to having a child and taking the respondent's age in that year as the age of first birth

# We first create a vector of ids with missing data on our previous measure of age at first birth ("afb.synth")
# Recall that this measure uses the year when the respondent was born and the year in which a respondent's
# first child was born from the family files. Given that individuals who attrit from the sample before 1985 do
# not have this information in the family files, we use the reported age of the head's oldest child
# to generate a measure of age at first birth as R's age when the respondent's oldest child was born for those
# who attrit from the sample before 1985 (for details on this procedure refer to the code chunk above, lines 803-846)
ids.miss <- benchmark.fert.synth %>% filter(is.na(afb.synth)) %>% dplyr::select(indiv.id) %>% pull()

# This takes an object with a record for each individual id in each year for which a child could be born 
test <- psid_fert %>% 
  ungroup() %>%
  # We filter out "missing year" records for individuals with no children recorded
  mutate(drop = ifelse(is.na(year) & num.children == 0, "drop", "keep")) %>%
  filter(drop == "keep") %>%
  dplyr::select(-drop) %>%
  # This creates a variable with labeled row numbers for the entire data (not row number by person id)
  mutate(rownum = row_number()) 

# We then create a new object that records the row number in which the number of children in the data changes
# The "lengths" column captures the number of rows between a change in value in number of children
# So "lengths" 27, 6, and 3 mean that the value of number of children first changes after 27 rows, then changes
# again 6 rows after that, then again 3 rows after that, and so on
t <- rle(test$num.children)[1] %>% 
  as.data.frame() %>%
  # The "pos" column cumulatively sums the length column to get the specific row numbers in the data at which
  # the number of children changes: "lengths" 27, 6, and 3 indicate that the value for number of children change
  # after 27 rows, then 6 rows after, then 3 rows after. By cumulatively summing "lengths", we get the specific
  # row number at which the number of children changes (27+6 = 33, 33+3 = 36, and so on)
  mutate(pos = cumsum(lengths))

# We then go back to the object that has a record for each individual in each year in which a child is born
# and select only the row numbers in this dataframe at which the number of children changes 
test2 <- test %>%
  filter(rownum %in% t$pos) %>%
  # We then select rows where the number of children == 0. These rows represent the last year in which 
  # the respondent is reported to have no children. In other words, the respondent had their first child
  # in the time span between the year when num.children == 0 and the subsequent year where num.children == 1
  filter(num.children == 0) %>%
  # We then create a variable that marks this year as the "year of first birth" for that respondent. This
  # measure captures that the respondent had their first kid prior to the subsequent year
  mutate(yr.firstchild.born.fert = year) %>%
  dplyr::select(indiv.id, yr.firstchild.born.fert)

# We then begin the process of merging this data with the rest of the observed data
# First, we get the individual ids, year, and age columns from the observed data
psid.obs.id.yr.age <- psid_obs %>% dplyr::select(indiv.id, year, age)
# We also create a vector of unique ids in the observed data
benchmark.ids <- as.vector(unique(psid_obs$indiv.id))

# We then create a new object that uses the observed data and merges it with our
# synthetic data on year in which a child is first reported according to the fertility files
# First, we create a dataframe that creates an observation for each individual in our synthetic 
# fertility data file above, for each of the covariate and wage years in our observed data
test3 <- crossing(indiv.id = test2$indiv.id, 
                  year = c(1979, 1980, 1989, 1990, 1998, 2000, 2008, 2010, 2014, 2016, 2018)) %>%
  mutate(year = year + 1) %>%
  # We select only individuals in the synthetic fertility data file that are also in the observed data
  # (due to sample restrictions like being a head/wife in the outcome year, being present in the outcome yr)
  filter(indiv.id %in% benchmark.ids) %>%
  # We then merge this data structure with our synthetic fertililty file: This gives us a 
  # data structure that has an observation for each person in our synthetic fertility file who is also in 
  # our observed data, for each covariate/wage year, and a variable that captures the last year in which 
  # that respondent reported zero children before transitioning to reporting one child 
  left_join(., test2, by = "indiv.id") %>%
  # We then merge this data with our id-year-age column subset of the observed data to 
  # generate a new variable that captures the respondent's age in the year in which they 
  # are last observed with no children before transitioning to having their first child
  left_join(., psid.obs.id.yr.age, by = c("indiv.id", "year")) %>%
  mutate(afb.ferhist = yr.firstchild.born.fert - (year - age)) %>%
  dplyr::select(-age) # We select out age as to not have it repeated when we merge this object to our observed data

# We then merge in this data object to our observed data
benchmark.fert.final <- benchmark.fert.synth %>%
  left_join(., test3, by = c("indiv.id", "year")) %>%
  # We create our final measures of fertility below
  mutate(
    # If age at first birth missing in the data, assigns age when head's oldest child was born
    # This variable takes on the value of our first "synthetic" measure of fertility (using age when
    # head's oldest child was born for the early year) when available. If this "synthetic" measure is
    # missing, this variable takes on the value of our second "synthetic" measure, based on the last year
    # in which a respondent was observed as childless when transitioning to having one child based on their 
    # fertility history. If both values are missing, this value is missing
    afb.final = case_when(is.na(afb.synth) ~ afb.ferhist, complete.cases(afb.synth) ~ afb.synth), 
    # This variable repeats the procedure above, but omits the step where we assign age at first birth
    # based on the respondent's age when the head's oldest child was born for the early covariate year
    afb.nosynth = ifelse(is.na(afb), afb.ferhist, afb),
    # We then create categorical variables for age at first birth based on these vars
    afb.cat = ifelse(afb.final == 9999, "nokids", ifelse(
      num.children.synth == 0, "nokids", ifelse(
        afb.final <= 21, "21minus", ifelse(
          afb.final <= 27, "23to27", "28plus")))))

rm(benchmark.fert.synth, test3)

write_csv(benchmark.fert.final, "clean_data/intermediate_psid_clean_mar_race_exp_fert.csv")
print("Done with: Merging Fertility to Main, Marital History, Race, Experience File")

#------------------------------------------------------------------------------
# CLEANED PSID DATA
#------------------------------------------------------------------------------

# Joining with the rest of our observed data
benchmark.final <- benchmark.fert.final %>%
  mutate(
    # Creating additional categorical variables
    region = case_when(region == 1 ~ "Northeast", 
                       region == 2 ~ "Northcentral",
                       region == 3 ~ "South",
                       # We combine individuals in Alaska/Hawaii with those in the Western US
                       region %in% c(4, 5) ~ "West", 
                       region == 6 ~ "Foreign.Country"),
    emp.tenure = emp.tenure / 12, 
    manuf = case_when(ind1990 %in% c(100, 101, 102, 110, 111, 112, 120, 121, 122, 130, 132, 140, 141,
                                     142, 150, 151, 152, 160, 161, 162, 171, 172, 180, 181, 182, 190, 191,
                                     192, 200, 201, 210, 211, 212, 220, 221, 222) & year != 1980 |
                        ind.orig %in% c(40:46, 85) & year == 1980 ~ 1,
                      ind1990 %in% c(230, 231, 232, 241, 242, 250, 251, 252, 261, 262, 270, 271, 272, 280,
                                     281, 282, 290, 291, 292, 300, 301, 310, 311, 312, 320, 321, 322, 331, 
                                     332, 340, 341, 342, 350, 351, 352, 360, 361, 362, 370, 371, 372, 380,
                                     381, 390, 391, 392) & year != 1980 |
                        ind.orig %in% c(30:34, 49) & year == 1980 ~ 1, 
                      TRUE ~ 0),
    occ.managers = ifelse(is.na(occ2010), NA, ifelse(
      occ2010 <= 3650, 1, 0))) %>%
  dplyr::select(indiv.id, year, female, family.id68, person.number68, 
                family.id, imm.sample.97, imm.sample.17, latino.sample,
                perwt, perwt.long, age, lead.age, rel.head, race, 
                region, lead.region, yrs.ed.fam,
                ann.wrk.hrs, ann.wrk.hrs.pred, lead.ann.wrk.hrs, 
                expt, expf, govt.job, union, self.emp, manuf, occ.managers,
                wages, lead.wage, emp.tenure, military, agriculture, 
                num.children, afb.cat, married, self.emp.lead, 
                lead.ann.wrk.hrs, age.youngest, occ2010)

rm(benchmark.fert.final)

#------------------------------------------------------------------------------
# CREATING OCCUPATIONAL CHARACTERISTICS USING IPUMS DATA
#------------------------------------------------------------------------------

# Loading data from IPUMS files to merge in occupational characteristics
# NOTE: To load data, you must download both the extract's data and the DDI and
# also set the working directory to the folder with these files 
# (or change the path below).

if (!require("ipumsr")) stop(
  "Reading IPUMS data into R requires the ipumsr package. 
  It can be installed using the following command: 
  install.packages('ipumsr')")

# Reading in the data
ddi <- read_ipums_ddi("raw_data/ipums_occs/usa_00054.xml")
ipums.occs.raw <- read_ipums_micro(ddi)

# Selecting the relevant data:
ipums.occs <- ipums.occs.raw %>%
  mutate(female = ifelse(SEX ==2, 1, 0),
         # Creating indicator for whether individual meets our hours/weeks worked ft criteria
         wrk.ft = ifelse(WKSWORK2 == 6 & UHRSWORK >= 35, 1, 0)) %>%
  filter(CLASSWKR==2) %>% # EXCLUDING THOSE NOT WORKING FOR WAGES (SELF-EMP)
  filter(EMPSTAT ==1) %>% # INCLUDING ONLY THOSE WHO ARE EMPLOYED
  filter(AGE >= 30 & AGE <= 55) # Defining occupational characteristics only for those in our analytical sample's age range

rm(ipums.occs.raw) 

ipums.data <- ipums.occs %>%
  group_by(YEAR, OCC2010) %>%
  # Creating a measure of pct female in a given occupation during our wage years
  summarise(occ.pct.female = wtd.mean(female, w = PERWT)) %>%
  rename(year = "YEAR", occ2010 = "OCC2010") %>%
  ungroup() %>%
  # Here, we're aligning the year labels in IPUMS with the first year we match to in PSID
  # So since we match data from the ACS file labelled 2019 starting in 2017, we relabel 2019 to 2017,
  # 2000 to 1999, and 2012 to 2009
  mutate(year = ifelse(year == 2019, 2017, ifelse(
    year == 2000, 1999, ifelse(year == 2012, 2009, year))),
    occ2010 = as.numeric(occ2010))

rm(ipums.occs) 

# Creating the object to merge to our observed data
# Note: this object will create a row for each year in our observed data 
# and each IPUMS occupation. Each year pair (1980/1981, 1990/1991 and so on) 
# will be assigned the average characteristics of the occupation in 1980, 1990, and so on
# We merge this to our time-varying PSID occupation dummies
benchmark.ipums <- crossing(occ2010 = ipums.data$occ2010,
                            year = c(1980, 1981, 1990, 1991, 1999, 2001, 2009, 2011, 2017, 2019)) %>%
  left_join(., ipums.data, by = c("occ2010", "year")) %>%
  # This indicator makes sure that we are filling in the ipums data for each year-pair in our PSID data
  mutate(indicator = case_when(year %in% c(1980, 1981) ~ 1, 
                               year %in% c(1990, 1991) ~ 2, 
                               year %in% c(1999, 2001) ~ 3, 
                               year %in% c(2009, 2011) ~ 4, 
                               year %in% c(2017, 2019) ~ 5)) %>%
  group_by(occ2010, indicator) %>%
  fill(starts_with("occ."), .direction = "down") %>%
  ungroup() %>% dplyr::select(-indicator) %>%
  # Joining the IPUMS data with our PSID data by year
  # Note that we merge the same occupation characteristics (1980 Census, and from the 2014-2019 ACS)
  # to the different years in the same period- so 1980 census characteristics are matched to occupation dummies
  # as measured in 1980 and 1981, 1990 census characteristics to occupation dummies as measured in 1990 and 1991, 
  # 2000 census characteristics to occupation dummies as measured in 1999 and 2001, ACS 2008-2012 characteristics are 
  # matched to occupation dummies as measured in 2009 and 2010, and ACS 2014-2019 characteristics are matched 
  # into occupation dummies in 2017 and 2019
  right_join(., benchmark.final, by = c("year", "occ2010")) %>%
  # Creating indicators for different kinds of sample restrictions:
  # Excluding agriculture and military, excluding the self-employed, including only FT workers, 
  # excluding individuals based on wage restrictions (non-zero wages, above 4000 annual earnings)
  mutate(
    wages = ifelse(year %in% c(1980, 1990, 
                               1999, 2009, 2017), lead.wage, wages),
    ann.wrk.hrs = ifelse(year %in% c(1980, 1990, 
                                     1999, 2009, 2017), lead.ann.wrk.hrs, ann.wrk.hrs),
    # Because military and agricultural employments are rare,
    # we assume individuals with missing values 
    samp.exc.mil.ag = case_when(
      military == 1 | agriculture == 1 ~ 1,
      TRUE ~ 0),
    samp.exc.region = case_when(
      region %in% c("Foreign.Country") ~ 1,
      TRUE ~ 0),
    samp.exc.selfemp = case_when(
      year %in% c(1980, 1990, 1999, 2009, 2017) & self.emp.lead == 1 ~ 1,
      year %!in% c(1980, 1990, 1999, 2009, 2017) & self.emp == 1 ~ 1, 
      TRUE ~ 0),
    samp.exc.zerowage = case_when(
      year %in% c(1980, 1990, 1999, 2009, 2017) & lead.wage  == 0 ~ 1, 
      year %!in% c(1980, 1990, 1999, 2009, 2017) & wages == 0 ~ 1,
      TRUE ~ 0),
    samp.inc.ft = case_when(
      year %in% c(1980, 1990, 1999, 2009, 2017) & lead.ann.wrk.hrs >= 1500 ~ 1, 
      year %!in% c(1980, 1990, 1999, 2009, 2017) & ann.wrk.hrs.pred  >= 1500 ~ 1,
      TRUE ~ 0),
    samp.inc.age = case_when(
      year %in% c(1980, 1990, 1999, 2009, 2017) & lead.age >= 30 & lead.age <= 55 ~ 1, 
      year %!in% c(1980, 1990, 1999, 2009, 2017) & age >= 30 & age <= 55 ~ 1,
      TRUE ~ 0),
    samp.inc.final = ifelse(samp.exc.mil.ag != 1 & samp.exc.selfemp != 1 & samp.exc.region != 1 &
                              samp.exc.zerowage != 1 & samp.inc.age == 1 & ann.wrk.hrs > 0,
                            1, 0)) %>%
  # We set wages for both the main timing and the alternate timing
  # to equal wages as measured in 1981, 2017
  mutate(wages = ifelse(year %in% c(1980, 1990, 1999, 2009, 2017), 
                        lead.wage, wages),
         ann.wrk.hrs = ifelse(year %in% c(1980, 1990, 
                                          1999, 2009, 2017), 
                              lead.ann.wrk.hrs, ann.wrk.hrs)) %>%
  # Removing "lead" variables used to generate the sample restriction +
  # annual work hours and wages for the alternate variable timings, 
  # extraneous id vars
  dplyr::select(-c(starts_with("lead"), self.emp.lead, 
                   military, agriculture, occ2010, ann.wrk.hrs.pred,
                   family.id68, person.number68, family.id))

#------------------------------------------------------------------------------
# SAVE FILE TO THE CLEAN DATA FOLDER
#------------------------------------------------------------------------------
write_csv(benchmark.ipums, paste0("clean_data/", "psid_clean", ".csv"))
print("Done with: All Preprocessing")
