# Clean data ----

# Setup ----
## Load packages ----
if (!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "rio", "here", "sjlabelled", "eurostat", "janitor", "countrycode",
            "conflicted")

# Conflicts
conflict_prefer("filter", "dplyr")

## Load data ----
# Cumulative ALLBUS
eb.orig <- import(here("data", "ZA7847_v1-0-0.sav"))

# Overview of variables
eb.vars <- eb.orig %>%
  colnames() %>%
  enframe(name = NULL,
          value = "colname") %>%
  bind_cols(description = get_label(eb.orig),
            label = get_labels(eb.orig) %>% map_chr(., ~paste0(., collapse = "; ")))
  

# Data wrangling ----
eb.df <- eb.orig %>%
  mutate(isocntry = if_else(isocntry %in% c("DE-W", "DE-E"), "DE", isocntry),
         isocntry = countrycode(isocntry, "eurostat", "iso3c", custom_match = c("GR" = "GRC")))

## Eurostat: Share of foreigners ----
migpop.orig <- get_eurostat("migr_pop1ctz")

# Compute share of third-country nationals of population 
migpop.df <- migpop.orig %>% 
  filter(citizen %in% c("NEU27_2020_FOR", "TOTAL"), age == "TOTAL", sex == "T", 
         time == "2021-01-01") %>%
  select(-c(age, unit, sex, time)) %>%
  pivot_wider(names_from = citizen, values_from = values) %>%
  clean_names() %>%
  mutate(share_foreign = neu27_2020_for / total * 100,
         isocntry = countrycode(geo, "eurostat", "iso3c"),
         country = countrycode(geo, "eurostat", "country.name.en")) %>%
  select(isocntry, foreign_pop = neu27_2020_for, pop = total, share_foreign)

## Join: EB & Eurostat ----
eb.df <- eb.df %>%
  left_join(y = migpop.df) 

## Categorize empirical share of third-country foreigners in country
eb.df <- eb.df %>%
  mutate(share_foreign_cat = case_when(
    share_foreign >= 0 & share_foreign < 3 ~ 1,
    share_foreign >= 3 & share_foreign < 6 ~ 2,
    share_foreign >= 6 & share_foreign < 9 ~ 3,
    share_foreign >= 9 & share_foreign < 12 ~ 4,
    share_foreign >= 12 & share_foreign < 15 ~ 5,
    share_foreign >= 15 & share_foreign < 20 ~ 6,
    share_foreign >= 20 & share_foreign < 25 ~ 7,
    share_foreign >= 25 & share_foreign < 35 ~ 8,
    share_foreign >= 35 & share_foreign < 50 ~ 9,
    share_foreign >= 50 ~ 10,
    TRUE ~ NA_real_
  ))