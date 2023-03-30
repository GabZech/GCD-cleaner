library(readr)
library(dplyr)
library(stringr)
library(stringi)
library(janitor)
library(readxl)
library(tidyr)

# set desired country or city
des_country = "germany"
des_city = "berlin"
des_company = "bayer healthcare"

# import raw dataset
companies_raw <- read_csv("data/gcd_raw.csv")

# select relevant columns
companies <- companies_raw %>%
  select(name, locality, country, "current employee estimate")

# filter for specific country
companies_country <- companies %>%
  filter(country == des_country) %>%
  filter(grepl(des_country, locality)) %>%
  mutate(city = str_trim(str_extract(locality, '[^,]+(?=,)'))) %>%
  mutate(state = str_trim(str_extract(locality, '(?<=,)[^,]+(?=,)'))) %>%
  select(-c(locality, country, city))

# filter for specific city
companies_city <- companies %>% 
  filter(country == des_country) %>%
  mutate(city = str_trim(str_extract(locality, '[^,]+(?=,)'))) %>%
  mutate(state = str_trim(str_extract(locality, '(?<=,)[^,]+(?=,)'))) %>%
  select(-c(locality, country)) %>% 
  filter(city == des_city)

# filter for specific company
company <- companies %>% 
  filter(name == des_company)

# check unique regions
unique(companies_country$state)
unique(companies_city$city)


# remove unwanted rows
companies_country <- companies_country %>%
  filter(!state == "missouri")

# fix names
ori_names = c()
new_names = c()

companies_country$state <- stri_replace_all_regex(df$name,
                                                  pattern=ori_names,
                                                  replacement=new_names,
                                                  vectorize=FALSE)

# save file
write.csv(companies_city,"output\\companies_berlin.csv", row.names = FALSE)

