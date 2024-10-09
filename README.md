# Replication Code for "Can Fertility Decline Help Explain Gender Pay Convergence?"
##  Nino Cricco and Alexandra Killewald

These files contain the replication code for the article "Can Fertility Decline Help Explain Gender Pay Convergence?". This project uses publicly available data from the Panel Study of Income Dynamics (PSID), which are available to researchers at <https://psidonline.isr.umich.edu/>, and the U.S. Decennial Census and the American Community Survey, which are available to researchers at <https://usa.ipums.org/usa/>. This repository contains all the code files needed to create the output in this article. The data can be donwloaded directly by users (instructions provided below), or can be accessed via the PSID Public Extract data repository at <https://www.openicpsr.org/openicpsr/project/209567/version/V1/view>, which contains both raw and clean data files. Please note that the user will have to create an account with ICPSR to access the files.

The files in the PSID data repository are organized in the following folders:

- `raw_data`: contains the raw data downloaded from the PSID, IPUMS, and crosswalks provided by the census bureau. 
- `clean_data`: contains the transformed data after data processing and missing data imputation. All of the analysis files draw on the files in this folder.  
- `jobs`: contains the code files used to process the data, conduct the analyses, and create the output for the article. The code files are organized according to files needed for setup, data processing, data analysis, and output creation. For more details, see the Code files section below.

Some of the code files save the output to named folders- to ensure easy replication, make sure there are empty folders in your directory named "tables" and "figures", or change the paths in the code files to save the output in a different directory. 

## Data

### PSID Data

There are three data files from the PSID under the following directories. To access the raw data from the PSID website, users must first create an account, go to the Data Center, and click on "Previous Carts". Users can then retrieve carts created by other users- for the data files used in this project, enter the email address <ncricco@g.harvard.edu>. The user can then download these data files directly from the PSID website using each file's job identifier number. 

- `raw_data/psid`: Primary data file from the Family Interview. Job Identifier 321006
- `raw_data/psid_fertility`: Fertility history file. Job Identifier 314662
- `raw_data/psid_marital`: Marital history file. Job Identifier 314661

Note that for the data coming from the Family Interview, the researcher has multiple options to generate the data file. We downloaded the data extract compatible with Stata, then used the Stata do-file to generate and save the "psid.dta" file in the raw_data folder. Note that users must edit the automatically generated do-file to reflect the file path where the downloaded files are stored, and then manually save the data to the raw_data folder. 

### IPUMS Data

To download the IPUMS-USA data, create a data extract at <https://usa.ipums.org/usa/>. 

The data extract should contain the following variables:
- SEX
- YEAR
- WKSWORK2
- UHRSWORK
- CLASSWKR
- EMPSTAT
- AGE
- OCC2010

Select the following samples:

- 1980 5% state	5.0%	
- 1990 5% state	5.0%	
- 2000 5%	5.0%	
- 2000 1%	1.0%	
- 2012 ACS 5yr 5.0%	
- 2019 ACS 5yr

### Crosswalks

The raw_data folder also contains crosswalks created by the U.S. Census bureau to ensure the compatibility of occupation and industry codes across years. The crosswalks in the raw_data folder were manually edited to include only relevant codes and draw on the IPUMS integrated crosswalk, accessible here:

<https://usa.ipums.org/usa/resources/volii/documents/integrated_ind_occ_crosswalks.xlsx>

ipums_occ_crosswalk.xlsx comes from the occ2010 tab in the IPUMS integrated crosswalk. 
ipums_industry_crosswalk.xlsx comes from the ind1990 tab in the IPUMS integrated crosswalk. 

Some occupation codes are in the PSID codebook, but not included in the crosswalk. For information on these codes, see <https://www.bls.gov/cps/cenocc2010.htm>. 

The PSID also uses 2012 industry codes in some survey years, which are not included in the integrated IPUMS crosswalk. We obtain a crosswalk for these codes (file census_industry_crosswalk_2012.1990.xlsx) from the Census Bureau website at the following link at the time of writing:

<https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/industry-crosswalk-90-00-02-07-12.xls>

## Code Files

### Setup
- `0-setup.R`: If downloading the replication file, this is not needed- this sets up the project environment with the appropriate package dependencies
- `1-load-libraries.R`: Read in the required libraries
- `1-functions.R`: Creates functions that are used across the processing and analysis files

### Data Processing

- `2-processing_rawtoclean.R`: Cleans, merges, and transforms data from the raw_data folder to create an analytic dataset. 
- `3-processing_imputation.R`: Uses multiple imputation via chained equations (MICE) to impute missing data in the analytic dataset and outputs a table specifying rates of missing data.

### Data Analysis

- `4-analysis_arguments.R`: Specifies arguments that are used repeatedly throughout the analysis pipeline
- `5-analysis_main.R`: Runs the analyses for all tables and figures in the main text and ancillary appendix tables related to results from the main analyses.
- `5-analysis_appendix-sampspecs.R`: Generates point estimates for alternative sample specifications, shown in Table A3 in the appendix. 
- `5-analysis_appendix-altrefyear.R`: Generates point estimates for alternative reference years, shown in Table A4 in the appendix. 
- `5-analysis_appendix-fertility.R`: Generates point estimates for alternative fertility measures, shown in Table A5 in the appendix. 
- `5-analysis_appendix-lfselection.R`: Generates sample means, coefficients, and decomposition point estimates adjusting for changes in observed patterns of labor force selection, shown in Table A6-A7 in the appendix. 
- `5-analysis_appendix-figures.R`: Generates appendix figures.
- `6-analysis_main-bootstrapci.R`: Generates bootstrapped estimates for decomposition to compute confidence intervals shown in Table 3 in the paper and Table A1 in the appendix. 
- `6-analysis_appendix-sampspecs-bootstrapci.R`: Generates bootstrapped estimates for decomposition with alternative sample specifications to compute confidence intervals shown in Table A3 in the appendix.
- `6-analysis_appendix-altrefyear-bootstrapci.R`: Generates bootstrapped estimates for decomposition with alternative reference year as the wage structure to compute confidence intervals shown in Table A4 in the appendix.
- `6-analysis_appendix-fertility-bootstrapci.R`: Generates bootstrapped estimates for decomposition with alternative fertility measures to compute confidence intervals shown in Table A5 in the appendix.
- `6-analysis_appendix-lfselection-bootstrapci.R`: Generates bootstrapped estimates for decomposition adjusting for changing patterns of observed labor force selection to compute confidence intervals shown in Table A7 in the appendix.
- `6-analysis_appendix-race-bootstrapci.R`: Generates bootstrapped estimates for decomposition by race, showin in Figure A4 in the appendix.

Previous versions of this project contain files for analyses that are not included in the final paper. 

## References

Killewald, Alexandra and Nino José Cricco. 2023. "Can Fertility Decline Help Explain the Narrowing Gender Pay Gap?"
