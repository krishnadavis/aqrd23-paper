library(tidyverse)
library(scales)
library(broom)
library(lme4)
library(rstanarm)
library(gt)
library(haven)
library(dlookr)
library(stringr)
library(fixest)

sbway <- read_dta("data/subway_analysis_use.dta")
load("data/ppp_panel.RData")
load("data/ppp_contract.RData")
citynames <- read.csv("data/citynames_english.csv")
city_geo <- read.csv("data/city_geo_area.csv")
city_geo <- city_geo |> 
  rename("geo_area" = Area)
city_geo$geo_area <- as.numeric(gsub(",", "", city_geo$geo_area))

names(ppp)
mean(contract$invest_l)

sbway <- sbway |> 
  left_join(citynames, by = "City_Code") |> 
  left_join(city_geo, by = "cityname_caps", relationship = "many-to-many")

cortbl <- sbway |>
  select(Mayor_plan, cityname_caps) |> 
  group_by(cityname_caps) |>
  summarize(plan = sum(Mayor_plan)) |> 
  left_join(city_geo, by = "cityname_caps") |> 
  select(cityname_caps, plan, geo_area) 

cor(cortbl$plan, log(cortbl$geo_area), use = "complete.obs")
plot(cortbl$geo_area, cortbl$plan)


sbway1 <- sbway |> 
  filter(
    Per_pop > 300,
    rev > 10,
    gdp >= 100
)

feols(Mayor_promotion3y ~ 1 | Mayor_plan ~ logltavg, sbway1)

summary(lm(sbway$Mayor_plan ~ sbway$logltavg))
summary(lm(sbway$Mayor_promotion3y ~ sbway$logltavg))
cor(sbway$Mayor_promotion3y, sbway$logltavg, use = "complete.obs")




sumtable <- sbway |> 
  select(Mayor_promotion3y, Mayor_connection_work, Mayor_age, Per_pop, gdp, rev, GRP_growth, Mayor_plan, inv1_per, GRP_per, land_per, rev_per) |> 
  mutate(GRP_per = GRP_per / 1000,
         rev_per = rev_per / 1000,
         inv1_per = inv1_per / 1000,
         land_per = land_per / 1000) |> 
  pivot_longer(cols = everything()) |> 
  group_by(name) |> 
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value)),
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
    Mayor_connection_work = "Mayor connection",
    GRP_growth = "City GDP growth rate (%)",
    GRP_per = "City GDP per capita (1000 ¥)",
    Mayor_age = "Mayor age",
    Per_pop = "City population",
    gdp = "City GDP (billion ¥)",
    inv1_per = "City investment in infrastructure per capita (1000 ¥)",
    land_per = "City land sales revenue per capita (1000 ¥)",
    rev = "City fiscal revenue (billion ¥)",
    rev_per = "City fiscal revenue per capita (1000 ¥)"
  )) |> 
  arrange(name) |> 
  group_by(category) |> 
  gt() |> 
  cols_align("left", columns = name) |> 
  cols_label(mean = "Mean", sd = "Std. Dev.", n = "N", name = "Variable") |> 
  fmt_number(columns = c(mean, sd), decimals = 2) |> 
  fmt_integer(columns = n) |> 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups())



