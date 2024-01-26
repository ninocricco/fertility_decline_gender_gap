#**********************************************************
# PROJECT: KILLEWALD- GENDER WAGE DISTRIBUTION
# FILE: ANALYSES, COHORT-SPECIFIC FIGURE A4
# AUTHOR: NINO CRICCO
# LAST UPDATED: 08/02/2023 (mdy)
#**********************************************************

opts <- options(knitr.kable.NA = "")

#*# Loading helper functions
source("jobs/0-functions.R")
source("jobs/0-libraries.R")

# Loading the imputed dataset
psid_imp <- read_csv("clean_data/psid_final.csv")

# Loading the imputed dataset
source("jobs/3-analysis-arguments.R")

# Creating an object to use in the decompositions using the sample means by birth cohort
means.year <- data %>%
  mutate(birth.cohort = case_when(year - age <= 1929 ~ "1920s",
                           year - age <= 1939 ~ "1930s",
                           year - age <= 1949 ~ "1940s",
                           year - age <= 1959 ~ "1950s",
                           year - age <= 1969 ~ "1960s",
                           year - age <= 1979 ~ "1970s",
                           TRUE ~ "1980s")) %>%
  group_by(year, female, .imp, birth.cohort) %>% # For each year, sex, and imputation,
  # Compute the weighted means for the variables below, using the weights specified in the function
  dplyr::select(wages.hrly, num.kids.cont, perwt) %>%
  summarize_all(list(wmean = ~wtd.mean(., w = perwt))) %>% 
  filter(.imp != 0) %>% # Filtering out the non-imputed data
  dplyr::select(year, female, everything()) %>% # Ordering the columns
  dplyr::select(-c(.imp, perwt_wmean)) %>% # Removing weight and imputation column
  ungroup() %>%
  group_by(year, female, birth.cohort) %>% # For each year and female, compute means mean across imputations
  summarise_all(mean) 

# Setting names to the weighted means table
names(means.year) <- gsub("_wmean", "", names(means.year))

# We use this table of year-and-sex specific means for the decomposition analyses
# Each row is a year X sex combination, columns are year-sex specific means of selected variables
means.year_table <- means.year %>% 
  filter(year %in% c(1981, 1991, 2001, 2011, 2019))

# Setting the color palette for the figure
my_blues = RColorBrewer::brewer.pal(n = 9, "Blues")[3:9]

# Creating figure from means table
figa4 <- 
  means.year_table %>%
  pivot_wider(names_from = female, values_from = c(wages.hrly, num.kids.cont)) %>%
  mutate(ratio = wages.hrly_1/wages.hrly_0, 
         year = year-1) %>%
  dplyr::select(year, birth.cohort, num.kids.cont_1, ratio) %>%
  gather(key, value, -c(birth.cohort, year)) %>%
  mutate(label = case_when(year == 1980 & birth.cohort == "1920s" ~ birth.cohort,
                           year == 1990 & birth.cohort %in% c("1930s", "1960s") ~ birth.cohort,
                           year == 2000 & birth.cohort %in% c("1940s", "1950s") ~ birth.cohort,
                           year == 2010 & birth.cohort == "1970s" ~ birth.cohort,
                           year == 2018 & birth.cohort == "1980s" ~ birth.cohort)) %>%
  ggplot(aes(x = year, y = value, color = birth.cohort, 
             linetype = birth.cohort, label = label)) +
  geom_point() +
  geom_line() +
  geom_label_repel(box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   show.legend = FALSE) +
  facet_wrap(~key, scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(num.kids.cont_1 = "Average Number of Children",
                                      ratio = "Ratio of Women's to Men's Average Hourly Wage") )) +
  theme_bw() +
  scale_colour_manual(values = my_blues, 
                      name = "Birth Cohort") +
  labs(title = "Figure A4: Changing Fertility and the Changing Gender Pay Gap, by Birth Cohort", 
       y = "", 
       x = "") +
  scale_x_continuous(breaks=c(1980, 1990, 2000, 2010, 2018)) +
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        legend.position = "bottom",
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14),
        strip.text = element_text(size = 16),
        legend.key.width=unit(3,"line"),
        legend.title = element_text(size = 14)) +
  guides(linetype = "none", label = "none") 

ggsave(plot = figa4, "figures/figa4.jpg", 
       width = 10.5, height = 6, units = "in", device='jpeg', dpi=700)
