# Creating cross-sectional decomposition
means.men.t1 <- means.year_table %>% filter(female == 0, year == 1980) %>% select(year, female, ends_with("mean"))
means.women.t1 <- means.year_table %>% filter(female == 1, year == 1980) %>% select(year, female, ends_with("mean"))

means.men.t2 <- means.year_table %>% filter(female == 0, year == 2018) %>% select(year, female, ends_with("mean"))
means.women.t2 <- means.year_table %>% filter(female == 1, year == 2018) %>% select(year, female, ends_with("mean"))

mens_wages_t1 <- means.men.t1$lnhrlywage_mean
womens_wages_t1 <- means.women.t1$lnhrlywage_mean
mens_wages_t2 <- means.men.t2$lnhrlywage_mean
womens_wages_t2 <- means.women.t2$lnhrlywage_mean

mens_parenthood_baseline_t1 <- coeftable_all %>% 
  filter(female == 0, model == "Model 1: Baseline", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t1$num.kids.cont_mean

womens_parenthood_baseline_t1 <- coeftable_all %>% 
  filter(female == 1, model == "Model 1: Baseline", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t1$num.kids.cont_mean

baseline_cs_men_t1 <- (mens_wages_t1- mens_parenthood_baseline_t1)
baseline_cs_women_t1 <- (womens_wages_t1 - womens_parenthood_baseline_t1)

pct.exp_baseline_t1 <- 1-((baseline_cs_men_t1-baseline_cs_women_t1)/ (mens_wages_t1-womens_wages_t1))

mens_parenthood_baseline_t2 <- coeftable_all %>% 
  filter(female == 0, model == "Model 1: Baseline", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t2$num.kids.cont_mean

womens_parenthood_baseline_t2 <- coeftable_all %>% 
  filter(female == 1, model == "Model 1: Baseline", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t2$num.kids.cont_mean

baseline_cs_men_t2 <- (mens_wages_t2- mens_parenthood_baseline_t2)
baseline_cs_women_t2 <- (womens_wages_t2- womens_parenthood_baseline_t2)

pct.exp_baseline_t2 <- 1-((baseline_cs_men_t2-baseline_cs_women_t2)/ (mens_wages_t2-womens_wages_t2))

mens_parenthood_Background_t1 <- coeftable_all %>% 
  filter(female == 0, model == "Model 2: + Background", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t1$num.kids.cont_mean

womens_parenthood_Background_t1 <- coeftable_all %>% 
  filter(female == 1, model == "Model 2: + Background", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t1$num.kids.cont_mean

Background_cs_men_t1 <- (mens_wages_t1- mens_parenthood_Background_t1)
Background_cs_women_t1 <- (womens_wages_t1-womens_parenthood_Background_t1)

pct.exp_Background_t1 <- 1-((Background_cs_men_t1-Background_cs_women_t1)/ (mens_wages_t1-womens_wages_t1))

mens_parenthood_Background_t2 <- coeftable_all %>% 
  filter(female == 0, model == "Model 2: + Background", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t2$num.kids.cont_mean

womens_parenthood_Background_t2 <- coeftable_all %>% 
  filter(female == 1, model == "Model 2: + Background", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t2$num.kids.cont_mean

Background_cs_men_t2 <- (mens_wages_t2- mens_parenthood_Background_t2)
Background_cs_women_t2 <- (womens_wages_t2-womens_parenthood_Background_t2)

pct.exp_Background_t2 <- 1-((Background_cs_men_t2-Background_cs_women_t2)/ (mens_wages_t2-womens_wages_t2))

mens_parenthood_Education_t1 <- coeftable_all %>% 
  filter(female == 0, model == "Model 3: + Education", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t1$num.kids.cont_mean

womens_parenthood_Education_t1 <- coeftable_all %>% 
  filter(female == 1, model == "Model 3: + Education", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t1$num.kids.cont_mean

Education_cs_men_t1 <- (mens_wages_t1- mens_parenthood_Education_t1)
Education_cs_women_t1 <- (womens_wages_t1-womens_parenthood_Education_t1)

pct.exp_Education_t1 <- 1-((Education_cs_men_t1-Education_cs_women_t1)/ (mens_wages_t1-womens_wages_t1))

mens_parenthood_Education_t2 <- coeftable_all %>% 
  filter(female == 0, model == "Model 3: + Education", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t2$num.kids.cont_mean

womens_parenthood_Education_t2 <- coeftable_all %>% 
  filter(female == 1, model == "Model 3: + Education", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t2$num.kids.cont_mean

Education_cs_men_t2 <- (mens_wages_t2- mens_parenthood_Education_t2)
Education_cs_women_t2 <- (womens_wages_t2-womens_parenthood_Education_t2)

pct.exp_Education_t2 <- 1-((Education_cs_men_t2-Education_cs_women_t2)/ (mens_wages_t2-womens_wages_t2))

mens_parenthood_WEJT_t1 <- coeftable_all %>% 
  filter(female == 0, model == "Model 4: + Work Experience and Job Tenure", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t1$num.kids.cont_mean

womens_parenthood_WEJT_t1 <- coeftable_all %>% 
  filter(female == 1, model == "Model 4: + Work Experience and Job Tenure", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t1$num.kids.cont_mean

WEJT_cs_men_t1 <- (mens_wages_t1- mens_parenthood_WEJT_t1)
WEJT_cs_women_t1 <- (womens_wages_t1-womens_parenthood_WEJT_t1)

pct.exp_WEJT_t1 <- 1-((WEJT_cs_men_t1-WEJT_cs_women_t1)/ (mens_wages_t1-womens_wages_t1))

mens_parenthood_WEJT_t2 <- coeftable_all %>% 
  filter(female == 0, model == "Model 4: + Work Experience and Job Tenure", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t2$num.kids.cont_mean

womens_parenthood_WEJT_t2 <- coeftable_all %>% 
  filter(female == 1, model == "Model 4: + Work Experience and Job Tenure", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t2$num.kids.cont_mean

WEJT_cs_men_t2 <- (mens_wages_t2- mens_parenthood_WEJT_t2)
WEJT_cs_women_t2 <- (womens_wages_t2-womens_parenthood_WEJT_t2)

pct.exp_WEJT_t2 <- 1-((WEJT_cs_men_t2-WEJT_cs_women_t2)/ (mens_wages_t2-womens_wages_t2))

mens_parenthood_Full_t1 <- coeftable_all %>% 
  filter(female == 0, model == "Model 5: Full", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t1$num.kids.cont_mean

womens_parenthood_Full_t1 <- coeftable_all %>% 
  filter(female == 1, model == "Model 5: Full", year == 1981, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t1$num.kids.cont_mean

Full_cs_men_t1 <- (mens_wages_t1- mens_parenthood_Full_t1)
Full_cs_women_t1 <- (womens_wages_t1-womens_parenthood_Full_t1)

pct.exp_Full_t1 <- 1-((Full_cs_men_t1-Full_cs_women_t1)/ (mens_wages_t1-womens_wages_t1))

mens_parenthood_Full_t2 <- coeftable_all %>% 
  filter(female == 0, model == "Model 5: Full", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.men.t2$num.kids.cont_mean

womens_parenthood_Full_t2 <- coeftable_all %>% 
  filter(female == 1, model == "Model 5: Full", year == 2019, estimate == "coef") %>%
  select(num.kids.cont) *
  means.women.t2$num.kids.cont_mean

Full_cs_men_t2 <- (mens_wages_t2- mens_parenthood_Full_t2)
Full_cs_women_t2 <- (womens_wages_t2-womens_parenthood_Full_t2)

pct.exp_Full_t2 <- 1-((Full_cs_men_t2-Full_cs_women_t2)/ (mens_wages_t2-womens_wages_t2))


as.data.frame(bind_cols(c("Baseline", "Baseline", "+ Background", "+ Background", "+ Education", "+ Education", "+ WEJB", "+ WEJB", "Full", "Full"), c(1981, 2019, 1981, 2019, 1981, 2019, 1981, 2019, 1981, 2019), bind_rows(pct.exp_baseline_t1, pct.exp_baseline_t2, pct.exp_Background_t1, pct.exp_Background_t2, pct.exp_Education_t1, pct.exp_Education_t2, pct.exp_WEJT_t1, pct.exp_WEJT_t2, pct.exp_Full_t1, pct.exp_Full_t2), )) %>% rename(model = "...1", year = "...2")
