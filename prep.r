library(readxl)
library(tidyverse)
library(sf)
library(fs)
library(janitor)
library(fuzzyjoin)
library(wbstats)
library(countrycode)

dir_create("raw-data")
dir_create("clean-data")

x <- read_xlsx("raw-data/International Students Places of Origin 2015-2019.xlsx") %>%
  clean_names() %>%
  set_names(c("country_name", "2017", "2018", "2016", "2015"))
na.omit()

y <- read_xlsx("raw-data/International Students Places of Origin 2009-2014.xlsx") %>%
  clean_names() %>%
  na.omit()

y$x2013_14 <- as.double(y$x2013_14)
y$x2014_15 <- as.double(y$x2014_15)
y$x2011_12 <- as.double(y$x2011_12)

y <- y %>% 
  set_names(c("country_name", "2013", "2014","2011", "2012", "2009", "2010"))

country <- x %>%
  inner_join(y, by = c("country_name"  = "country_name")) %>%
  pivot_longer(cols = 2:11) %>%
  set_names(c("country_name", "year", "num_students"))

country$wb_code <- countrycode(country$country_name, origin = 'country.name', destination = 'wb')

country$region <- countrycode(country$country_name, origin = 'country.name', destination = 'region')

wbdata <- wb(indicator = c('SP.POP.TOTL', 'NY.GDP.PCAP.CD'), startdate  = 2009, enddate = 2018) %>%
  select(iso3c, indicator, country, value, date) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  set_names(c("wb_code", "country", "year", "population", "gdp_per_capita")) 

vdem <- read_csv("raw-data/V-Dem-CY-Full+Others-v9.csv") %>%
  select(country_name, country_text_id, year, e_peaveduc, v2peedueq, v2clfmove, v2clacfree) %>%
  filter(year >= 2009) %>%
  set_names(c("country_name", "wb_code", "year", "education_15_plus",
              "education_equality", "freedom_foreign_movement", "freedom_expression"))

vdem$year <- as.character(vdem$year)

field_by_country <- read_xlsx("raw-data/International-Students-Fields-of-Study.xlsx", skip = 3) %>%
  clean_names() %>%
  head(25)

field_by_country$wb_code <- countrycode(field_by_country$place_of_origin, origin = 'country.name', destination = 'wb')

country_indicators <- country %>%
  left_join(vdem, by = c("year", "wb_code")) %>%
  left_join(wbdata, by = c("year", "wb_code")) %>%
  left_join(field_by_country, by = "wb_code") %>%
  select(-country_name.y, -country)

country_indicators$year <- as.integer(country_indicators$year)

funding <- read_xlsx("raw-data/International-Students-Primary-Source-of-Funding.xlsx", skip = 5) %>%
  clean_names() %>%
  select(x1, x2017_18) %>%
  set_names(c("funding_source", "num_students")) %>%
  na.omit()

write_rds(country_indicators, "clean-data/country_indicators.rds")

write_rds(funding, "clean-data/funding.rds")

#conflicts <- country %>% 
#  filter(is.na(wb_code)) %>%
#  distinct(country_name) %>%
#  pull(country_name)
