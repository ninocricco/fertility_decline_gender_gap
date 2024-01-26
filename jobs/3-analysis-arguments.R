#********************************************************
# PROJECT: CAN DECLINING FERTILITY HELP EXPLAIN THE NARROWING GENDER PAY GAP?
# FILE: CREATES VECTORS USED AS ARGUMENTS FOR ALL MAIN ANALYSES
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/01/2023 (mdy)
#********************************************************

# Setting vector arguments for main analyses
data = psid_imp
years = c(1981, 2019)
sample_conditions = list("samp.inc.age == 1",
                         "samp.exc.mil.ag != 1", 
                         "samp.exc.selfemp != 1", 
                         "samp.exc.region != 1",
                         "samp.exc.zerowage != 1", 
                         "ann.wrk.hrs > 0")
weights = "perwt"
outcome = "lnhrlywage"
group = "female"
covariates = c("num.kids.cont", "age", "agesq",
               "Black", "Hispanic", "Other",
               "Northeast", "Northcentral", "South", 
               "HighSchool", "SomeCollege", "ba.avdeg", 
               "married", "log.expf", "emp.tenure", 
               "ftormore", "overwork", 
               "union", "govt.job", "occ.pct.female", 
               "occ.managers", "manuf")

age <- c("age", "agesq")
race <- c("Black", "Hispanic", "Other")
region <- c("Northeast", "Northcentral", "South")
ed <- c("HighSchool", "SomeCollege", "ba.avdeg")
marstat <- c("married")
wrkhrs <- c( "ftormore", "overwork")
laborsupply <- c("log.expf", "emp.tenure", "ftormore", "overwork")
jobchar <- c("union", "govt.job", "occ.pct.female", "occ.managers", "manuf")

# Create a single combined expression for the filtering conditions
conditions_expr <- glue::glue("({paste(sample_conditions, collapse = ') & (')})")

# Defines covariates and labels for each model we want to estimate
covariates_to_exclude <- list(
  c(ed, marstat, laborsupply, jobchar),
  c(marstat, laborsupply, jobchar),
  c(laborsupply, jobchar),
  c(jobchar),
  c()
)

model_labels <- c("Model 1: Demographic Controls",
                  "Model 2: + Education", 
                  "Model 3: + Marital Status", 
                  "Model 4: + Labor Supply",
                  "Model 5: + Job Characteristics")
