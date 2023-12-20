library(tidyverse)
library(gt)
library(haven)
library(dlookr)
library(stringr)
library(fixest)
library(foreign)
library(glue)
library(modelsummary)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(pROC)

# DATA CLEANING AND SETUP -------
lei <- read_dta("data/subway_analysis_use.dta")
citynames <- read.csv("data/citynames_english.csv")

walkscore <- read.dbf("data/WalkScore3.dbf")
walkscore$PINYIN <- walkscore$PINYIN |> 
  iconv(to = "UTF-8", sub = "byte") |> 
  as.character() |> 
  toupper()

walkscore_m <- walkscore |> 
  rename(cityname_caps = "PINYIN") |> 
  mutate(
    cityname_caps = gsub(" SHI", "", walkscore$PINYIN),
    s_length_width = LENGTH_M * width_m,
    cityname_caps = case_when(
      cityname_caps == "HUAI<A1><AF>AN" ~ "HUAI'AN",
      cityname_caps == "JI<A1><AF>AN" ~ "JI'AN",
      cityname_caps == "LU<A1><AF>AN" ~ "LU'AN",
      cityname_caps == "MA<A1><AF>ANSHAN" ~ "MA'ANSHAN",
      cityname_caps == "TAI<A1><AF>AN" ~ "TAI'AN",
      cityname_caps == "ULANQAB MENG" ~ "ULANQAB",
      cityname_caps == "XI<A1><AF>AN" ~ "XI'AN",
      cityname_caps == "YA<A1><AF>AN" ~ "YA'AN",
      cityname_caps == "YAN<A1><AF>AN" ~ "YAN'AN",
      cityname_caps == "BAYANNUR MENG" ~ "BAYANNUR",
      cityname_caps == "HULUN BUIR MENG" ~ "HULUNBEL",
      cityname_caps == "JIAYUQUAN" ~ "JIAYUGUAN",
      cityname_caps == "PUER" ~ "PU'ER",
      TRUE ~ cityname_caps
      )
    ) |> 
  group_by(cityname_caps) |> 
  summarize(ws_mean_w = weighted.mean(WS, s_length_width),
            ws_mean = mean(WS))
  
# join all four data sets together, more data cleaning
sbway <- lei |> 
  left_join(citynames, by = "City_Code")

sbway$cityname_caps <- sbway$cityname_caps |> trimws()

sbway <- sbway |> 
  mutate(
    cityname_caps = case_when(
      cityname_caps == "OF HARBIN" ~ "HARBIN",
      cityname_caps == "OF GUILIN" ~ "GUILIN",
      cityname_caps == "WHITE" ~ "BAICHENG",
      TRUE ~ cityname_caps
      )
    ) |> 
  left_join(walkscore_m, by = "cityname_caps", relationship = "many-to-many")

# Lei & Zhou eliminate Vice-Province Level cities from their analysis
sbway <- sbway |> filter(fsj2 == 0)

# TABLE 1: Summary Statistics -------
# this table groups by outcome, treatment, and covariates, and but I could not get the formatting to correctly transfer over to LaTeX.
sumtable <- sbway |> 
  select(Mayor_promotion3y, Mayor_connection_work, Mayor_age, Per_pop, gdp, rev, GRP_growth, Mayor_plan, inv1_per, GRP_per, land_per, rev_per, ws_mean_w) |> 
  mutate(
    gdp = gdp / 10,
    GRP_per = GRP_per / 1000,
    rev_per = rev_per / 1000,
    inv1_per = inv1_per / 100,
    Per_pop = Per_pop / 100,
    land_per = land_per / 100) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) |> 
  mutate(category = recode(
    name,
    Mayor_promotion3y = "Outcome",
    Mayor_plan = "Treatment",
    .default = "Covariate"
  )) |> 
  mutate(name = recode_factor(
    name,
    Mayor_promotion3y = "Mayor promoted within three years",
    Mayor_plan = "Mayor obtaining subway approval",
    gdp = "City GDP (100 million ¥)",
    GRP_per = "City GDP per capita (1000 ¥)",
    GRP_growth = "City GDP growth rate (%)",
    rev = "City fiscal revenue (billion ¥)",
    rev_per = "City fiscal revenue per capita (1000 ¥)",
    inv1_per = "City investment in infrastructure per capita (100 ¥)",
    land_per = "City land sales revenue per capita (100 ¥)",
    Per_pop = "City population (million)",
    Mayor_connection_work = "Mayor connection",
    Mayor_age = "Mayor age",
    ws_mean_w = "Weighted walk score")
  ) |> 
  arrange(name) |> 
  group_by(category) |> 
  gt() |> 
  cols_align("left", columns = name) |> 
  cols_label(mean = "Mean", 
             sd = "Std. Dev.", 
             n = "N", 
             name = "Variable") |> 
  fmt_number(columns = c(mean, sd), decimals = 2) |> 
  fmt_integer(columns = n) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |> 
  gt::gtsave("tables/sumtable.png")

# this summary table appears in the paper
sumtable1 <- sbway |> 
  select(Mayor_promotion3y, Mayor_connection_work, Mayor_age, Per_pop, gdp, rev, GRP_growth, Mayor_plan, inv1_per, GRP_per, land_per, rev_per, ws_mean_w) |> 
  mutate(
    gdp = gdp / 10,
    GRP_per = GRP_per / 1000,
    rev_per = rev_per / 1000,
    inv1_per = inv1_per / 100,
    Per_pop = Per_pop / 100,
    land_per = land_per / 100) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  ) |> 
  mutate(name = recode_factor(
    name,
    Mayor_promotion3y = "Mayor promoted within three years",
    Mayor_plan = "Mayor obtaining subway approval",
    gdp = "City GDP (100 million ¥)",
    GRP_per = "City GDP per capita (1000 ¥)",
    GRP_growth = "City GDP growth rate (%)",
    rev = "City fiscal revenue (billion ¥)",
    rev_per = "City fiscal revenue per capita (1000 ¥)",
    inv1_per = "City investment in infrastructure per capita (100 ¥)",
    land_per = "City land sales revenue per capita (100 ¥)",
    Per_pop = "City population (million)",
    Mayor_connection_work = "Mayor connection",
    Mayor_age = "Mayor age",
    ws_mean_w = "Weighted walk score")
  ) |> 
  arrange(name) |> 
  gt() |> 
  cols_align("left", columns = name) |> 
  cols_label(mean = "Mean", 
             sd = "Std. Dev.", 
             n = "N", 
             name = "Variable") |> 
  fmt_number(columns = c(mean, sd), decimals = 2) |> 
  fmt_integer(columns = n) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) |> 
  gt::gtsave("tables/sumtable.tex")

# add column to data set indicating whether city is eligible to apply for subway approval
sbway <- sbway |> 
  mutate(
    eligible = case_when(
      rev >= 10 & gdp >= 100 & Per_pop > 300 ~ "eligible",
      TRUE ~ "not eligible"
    )
  )

# create new dataframe with only eligible cities and add lagged variable for infrastructure investment per capita
sbway_e <- sbway |> 
  filter(
    eligible == "eligible"
  ) |> 
  arrange(City_Code, Year) |> 
  group_by(City_Code) |> 
  mutate(lag_inv1_per = lag(inv1_per))

# visualization of new walk score variable among eligible cities
walk_box <- sbway_e |> 
  ggplot(aes(x = as.factor(Mayor_plan), y = ws_mean)) + 
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Subway Approval (1 = Yes)",
    y = "Mean Walk Score"
  ) +
  theme(axis.text = element_text(size = 30), 
        axis.title = element_text(size = 35),
  )

ggsave("figures/walkbox.png")

# t-test of difference in mean weighted walk score across approval groups, among eligible cities
sbway0_e <- sbway_e |> 
  filter(Mayor_plan == 0)

sbway1_e <- sbway_e |> 
  filter(Mayor_plan == 1)

t.test(sbway1_e$ws_mean, sbway0_e$ws_mean)

# REPLICATION of DIFF-IN-DIFF DESIGN -------

# replication of STATA control variable vectors
mayor_cont <- c("gender2", "race6", "Mayor_age", "Mayor_c_edu", "Mayor_c_central_exp", "Mayor_c_prov_exp", "Mayor_c_county_exp", "Mayor_c_soe_exp", "Mayor_c_univ_exp", "Mayor_c_league", "Mayor_connection_work")
base_cont <- c("lpop_1", "lgdp_1", "lrev_1", "GRP_growth_1")

fit1 <- feols(Mayor_promotion3y ~ Mayor_plan | Year + City_Code, 
              data = sbway,
              cluster = "City_Code")

reg_eqn2 <- glue("Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} | Year + City_Code")
fit2 <- feols(as.formula(reg_eqn2), data = sbway, cluster = "City_Code")

reg_eqn3 <- glue("Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | Year + City_Code")
fit3 <- feols(as.formula(reg_eqn3), data = sbway, cluster = "City_Code")

reg_eqn4 <- glue("Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} | Year^pro_code + Year + City_Code")
fit4 <- feols(as.formula(reg_eqn4), data = sbway, cluster = "City_Code")

reg_eqn5 <- glue("Mayor_promotion3y ~ Mayor_plan + {str_c(mayor_cont, collapse = ' + ')} + {str_c(base_cont, collapse = ' + ')} + ws_mean | Year^pro_code + Year")
fit5 <- feols(as.formula(reg_eqn5), data = sbway, cluster = "City_Code")

# trend annotation
trendspecs <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~ `(5)`,
  "City FE", "x", "x", "x", "x", "",
  "Year FE", "x", "x", "x", "x", "x",
  "Mayor controls", "", "x", "x", "x", "x",
  "City controls", "", "", "x", "x", "x",
  "Province-year FE", "", "", "", "x", "x"
)

regtable <- modelsummary(
  models = list(
    "(1)" = fit1,
    "(2)" = fit2,
    "(3)" = fit3,
    "(4)" = fit4,
    "(5)" = fit5
    ),
  coef_rename = c(Mayor_plan = "Subway approval"),
  coef_omit = glue("{str_c(mayor_cont, collapse = '|')}|{str_c(base_cont, collapse = '|')}|ws_mean"),
  gof_map = c("nobs"),
  add_rows = trendspecs,
  output = "gt"
  ) |> 
  tab_spanner(label = "Mayor Promoted within Three Years", columns = 2:6) |> 
  gt::gtsave("tables/regtable.tex")


# EVENT STUDY PLOT
lei_es <- sbway |> 
  group_by(City_Code) |>
  mutate(
    plan_lag1 = dplyr::lag(Mayor_plan, 1),
    plan_lag2 = dplyr::lag(Mayor_plan, 2),
    plan_lag3 = dplyr::lag(Mayor_plan, 3),
    plan_lag4 = dplyr::lag(Mayor_plan, 4),
    plan = Mayor_plan,
    plan_lead1 = dplyr::lead(Mayor_plan, 1),
    plan_lead2 = dplyr::lead(Mayor_plan, 2),
    plan_lead3 = dplyr::lead(Mayor_plan, 3),
    plan_lead4 = dplyr::lead(Mayor_plan, 4),
    plan_lead5 = dplyr::lead(Mayor_plan, 5),
    mpprior2 = case_when(
      plan == 0 & plan_lead1 == 0 & plan_lead2 == 1 ~ 1,
      TRUE ~ 0),
    mpprior3 = case_when(
      plan == 0 & plan_lead1 == 0 & plan_lead2 == 0 & plan_lead3 == 1 ~ 1,
      TRUE ~ 0),
    mpprior4 = case_when(
      plan == 0 & plan_lead1 == 0 & plan_lead2 == 0 & plan_lead3 == 0 & plan_lead4 == 1 ~ 1,
      TRUE ~ 0),
    mpprior5 = case_when(
      plan == 0 & plan_lead1 == 0 & plan_lead2 == 0 & plan_lead3 == 0 & plan_lead4 == 0 & plan_lead5 == 0 ~ 1,
      TRUE ~ 0),
    mppost1 = case_when(
      plan == 0 & plan_lag1 == 1 ~ 1,
      TRUE ~ 0),
    mppost2 = case_when(
      plan == 0 & plan_lag1 == 0 & plan_lag2 == 1 ~ 1,
      TRUE ~ 0),
    mppost3 = case_when(
      plan == 0 & plan_lag1 == 0 & plan_lag2 == 0 & plan_lag3 == 1 ~ 1,
      TRUE ~ 0),
    mppost4 = case_when(
      plan == 0 & plan_lag1 == 0 & plan_lag2 == 0 & plan_lag3 == 0 & plan_lag4 == 0 ~ 1,
      TRUE ~ 0),
    mpconn1 = case_when(
      plan == 1 & plan_lag1 == 0 ~ 1,
      TRUE ~ 0),
    mpconn2 = case_when(
      plan == 1 & plan_lag1 == 1 & plan_lag2 == 0 ~ 1,
      TRUE ~ 0),
    mpconn3 = case_when(
      plan == 1 & plan_lag1 == 1 & plan_lag2 == 1 & plan_lag3 == 0 ~ 1,
      TRUE ~ 0),
    mpconn4 = case_when(
      plan == 1 & plan_lag1 == 1 & plan_lag2 == 1 & plan_lag3 == 1 & plan_lag4 == 0 ~ 1,
      TRUE ~ 0),
    mpconn5 = case_when(
      plan == 1 & mpconn1 == 0 & mpconn2 == 0 & mpconn3 == 0 & mpconn4 == 0 ~ 1,
      TRUE ~ 0),
    mpprior1 = case_when(
      plan == 0 & plan_lead1 == 1 ~ 1,
      TRUE ~ 0)
  )

fit_es <- feols(Mayor_promotion3y ~ mpconn1 + mpconn2 + mpconn3 + mpconn4 + mpconn5 + mpprior5 + mpprior4 + mpprior3 + mpprior2 | Year + City_Code, data = lei_es, cluster = "City_Code")

event_study <- fit_es |> 
  broom::tidy() |> 
  filter(term %in% c("mpprior5", "mpprior4", "mpprior3", "mpprior2", "mpconn1", "mpconn2", "mpconn3", "mpconn4", "mpconn5")) |> 
  mutate(term = recode_factor(
    term,
    mpprior5 = -5,
    mpprior4 = -4,
    mpprior3 = -3,
    mpprior2 = -2,
    mpconn1 = 0,
    mpconn2 = 1,
    mpconn3 = 2,
    mpconn4 = 3,
    mpconn5 = 4)
  ) |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2*std.error, ymax = estimate + 2*std.error), width = 0) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 4.5, linetype = "dashed") +
  theme_bw() +
  labs(y = "Effect of Approval on Mayor Promotion in 3 Years",
       x = "Years Since Subway Approval"
  ) +
  coord_fixed(ratio = 6) +
  theme(axis.text = element_text(size = 30), 
        axis.title = element_text(size = 35),
        plot.margin=grid::unit(c(0,0,0,0), "mm")
        )

ggsave("figures/eventstudy.png", plot = event_study)

## EXTENSION: Predictive Model for Subway Approval -------
set.seed(06511)

sbway_e <- ungroup(sbway_e)

sbway_split <- initial_split(sbway_e, prop = 0.7)
sbway_train <- training(sbway_split)
sbway_test <- testing(sbway_split)

kitch_vars <- c("Per_pop", "Per_pop_1", "Per_pop_2", "gdp", "GRP_1", "GRP_2", "rev", "Budget_income_1", "Budget_income_2", "GRP_growth", "GRP_growth_1", "GRP_per", "GRP_per_1", "area_total", "lnarea", "inv1_per", "lag_inv1_per", "land_per", "rev_per", "uerate", "land_price", "area_plot", "value_plot", "ws_mean")

form_kitch <- as.formula(glue("Mayor_plan ~ {str_c(kitch_vars, collapse = ' + ')}"))

mod_kitch <- lm(form_kitch, data = sbway_train)
mod_lasso <- cv.glmnet(form_kitch, data = sbway_train)

# need to fit LOGIT model since Mayor_plan outcome is binary

lfit_kitch <- glm(form_kitch, sbway_train, family = binomial)
lfit_lasso <- cv.glmnet(form_kitch, sbway_train, family = binomial)

sum(coef(lfit_lasso, s = "lambda.min") != 0)

logit_preds <- sbway_test |> 
  transmute(
    y = factor(Mayor_plan),
    pred_kitch = predict(lfit_kitch, sbway_test, type = "response", na.action = na.pass),
    pred_lasso = predict(lfit_lasso, sbway_test, s = "lambda.min", type = "response", na.action = na.pass)
  )

# the following plot does not appear in the paper
scatter1 <- logit_preds |> 
  ggplot(aes(x = pred_kitch, y = pred_lasso)) +
  geom_point(aes(shape = y, color = y), alpha = 0.8) +
  scale_color_manual(
    values = c(`0` = "darkgray", `1` = "red"),
    labels = c(`0` = "No Approval", `1` = "Got Approval")
  ) +
  scale_shape_discrete(
    labels = c(`0` = "No Approval", `1` = "Got Approval")
  ) +
  labs(
    x = "Prediction (Kitchen Sink Model)",
    y = "Prediction (LASSO Model)",
    color = NULL,
    shape = NULL
  ) +
  coord_equal() +
  theme_bw()

# the following code creates the ROC plot
roc_kitch <- roc(sbway_test$Mayor_plan, logit_preds$pred_kitch)
roc_lasso <- roc(sbway_test$Mayor_plan, logit_preds$pred_lasso)

roc_kitch$auc
roc_lasso$auc

roc_plot <- ggroc(data = list(roc_kitch, roc_lasso), legacy.axes = TRUE, size = 1, alpha = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  coord_equal() +
  labs(
    y = "True positive rate",
    x = "False positive rate",
    color = "Model",
  ) +
  scale_color_manual(values = c("blue", "red"), labels = c("Kitchen Sink", "LASSO")) +
  theme_minimal() +
  theme(axis.text = element_text(size = 30), 
        axis.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 35),
        plot.margin = unit(c(-5, 0.5, -5, 0.5), "cm"))

ggsave("figures/rocplot.png", plot = roc_plot)
