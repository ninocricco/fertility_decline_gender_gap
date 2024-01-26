# Replication Code for "Can Fertility Decline Help Explain the Narrowing Gender Wage Gap?"
##  Nino Cricco and Alexandra Killewald

These files contain the replication code for the article "Can Fertility Decline Help Explain the Narrowing Gender Pay Gap?". This project uses publicly available data from the Panel Study of Income Dynamics (PSID), which are available to researchers at <https://psidonline.isr.umich.edu/>, and the U.S. Decennial Census and the American Community Survey, which are available to researchers at <https://usa.ipums.org/usa/>. This replication file is organized as an R project and contains all the code and data files needed to create the output in this article. The files are organized in the following folders:

- `raw_data`: contains the raw data downloaded from the PSID, IPUMS, and crosswalks provided by the census bureau. The user can choose to initiate the replication pipeline using the files contained in this folder, or the user can choose to download the files included in this folder directly (instructions provided below). Please note that if the user chooses to manually download the files, they should update or edit the file names and directories accordingly. 
- `clean_data`: contains the transformed data after data processing and missing data imputation. All of the analysis files draw on the files in this folder.  
- `jobs`: contains the code files used to process the data, conduct the analyses, and create the output for the article. The code files are organized according to files needed for setup, data processing, data analysis, and output creation. For more details, see the Code files section below. 
- `tables`: contains raw as well as formatted table output
- `figures`: contains all figures 

## Data

### PSID Data

There are three data files from the PSID under the following directories. To access the raw data from the PSID directly, users must first create an account, go to the Data Center, and click on "Previous Carts". Users can then retrieve carts created by other users- for the data files used in this project, enter the email address <ncricco@g.harvard.edu>. The user can then download these data files directly from the PSID website using each file's job identifier number. 

- `raw_data/psid`: Primary data file from the Family Interview. Job Identifier 321006
- `raw_data/psid_fertility`: Fertility history file. Job Identifier 314662
- `raw_data/psid_marital`: Marital history file. Job Identifier 314661

Note that for the data coming from the Family Interview, the researcher has multiple options to generate the data file. We downloaded the data extract compatible with Stata, then used the Stata do-file to generate and save the "psid.dta" file in the raw_data folder. Note that users must edit the automatically generated do-file to reflect the file path where the downloaded files are stored, and then manually save the data to the raw_data folder. 

### IPUMS Data

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
- `0-libraries.R`: Read in the required libraries
- `0-functions.R`: Creates functions that are used across the processing and analysis files

### Data Processing

- `1-processing_rawtoclean.R`: Cleans, merges, and transforms data from the raw_data folder to create an analytic dataset. 
- `2-processing_imputation.R`: Uses multiple imputation via chained equations (MICE) to impute missing data in the analytic dataset and outputs a table specifying rates of missing data.

### Data Analysis

- `3-analysis_arguments.R`: Specifies arguments that are used repeatedly throughout the analysis pipeline
- `4-analysis_main.R`: Runs the analyses for all tables and figures in the main text and ancillary appendix tables related to results from the main analyses.
- `4-analysis_figure-a1.R`: Creates output for the first figure in the appendix showing fertility means and coefficients by subgroup
- `4-analysis_table_a6.R`: Runs analyses for Table A6, change within decades
- `4-analysis_table_a7.R`: Runs analyses for Table A7, using alternative sample specifications
- `4-analysis_table_a8.R`: Runs analyses for Table A8, using alternative fertility measures
- `4-analysis_table_a9.R`: Runs analyses for Table A9, using alternative reference years
- `4-analysis_table_a11-a12-a13.R`: Runs analyses for Tables A11-13, reweighting the sample by labor force selection
- `4-analysis_table_3_bootstrapci.R`: Creates bootstrapped estimates for Table 3 and outputs confidence intervals
- `4-analysis_table_a8_bootstrapci.R`: Creates bootstrapped estimates for Table A8 and outputs confidence intervals

### Not in the paper
- `5-analysis-cohabitation.R`: Splits marital status group into married-single-cohabiting and re-estimates the models
- `5-analysis-cross-sectionaldecomp.R`: Decomposes the contribution of fertility differences in the cross-section
- `5-analysis-talkfigures.R`: Outputs figures visualizing decomposition results for conference presentations (outdated)
- `5-analysis-counterfactuals-wedge.R`: Creates analyses based on counterfactual decompositions distinguishing convergence from composition pathways (outdated)


## References

Killewald, Alexandra and Nino José Cricco. 2023. "Can Fertility Decline Help Explain the Narrowing Gender Pay Gap?"