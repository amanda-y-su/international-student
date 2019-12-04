# load relevant packages 

library(readxl)
library(tidyverse)
library(sf)
library(fs)
library(janitor)
library(fuzzyjoin)
library(wbstats)
library(countrycode)

#create directors for raw data and cleaned data

dir_create("raw-data")
dir_create("clean-data")

# read in international student countries of origin data from 2015-2019 excel spreadsheet

x <- read_xlsx("raw-data/International Students Places of Origin 2015-2019.xlsx") %>%
  
  # format and clean names of columns 
  
  clean_names() %>%
  
  # rename columns 
  
  set_names(c("country_name", "2017", "2018", "2016", "2015")) %>%
  
  # omit missing value observations
  
  na.omit()

# read in international student countries of origin data from 2009-2014 excel spreadsheet

y <- read_xlsx("raw-data/International Students Places of Origin 2009-2014.xlsx") %>%
  
  # format and clean names of columns 
  
  clean_names() %>%
  
  # omit missing value observations
  
  na.omit()

#convert columns originally coded as characters into double numeric valuue observations

y$x2013_14 <- as.double(y$x2013_14)
y$x2014_15 <- as.double(y$x2014_15)
y$x2011_12 <- as.double(y$x2011_12)

# rename columns from y tibble

y <- y %>% 
  
  set_names(c("country_name", "2013", "2014","2011", "2012", "2009", "2010"))

# create country tibble by joining the x and y tibbles

country <- x %>%
  
  # join the x and y tibbles by their country_name column 
  
  inner_join(y, by = c("country_name"  = "country_name")) %>%
  
  # lengthen data by increasing number of rows, converting columns 2-11 into observations 
  
  pivot_longer(cols = 2:11) %>%
  
  # rename the columns 
  
  set_names(c("country_name", "year", "num_students"))

# create a country code variable that assigns a country code to each country observations using the world bank package
# adding a country code variable will help with joining the various tibbles so that there is a standardized way of formatting each country
# since country names can sometimes be written differently depending on the data set

country$wb_code <- countrycode(country$country_name, origin = 'country.name', destination = 'wb')

# create a region variable that assigns a region to each country observations using the world bank package

country$region <- countrycode(country$country_name, origin = 'country.name', destination = 'region')

# create a world bank tibble with the variales of gdp per capita, population from 2009 to 2018

wbdata <- wb(indicator = c('SP.POP.TOTL', 'NY.GDP.PCAP.CD'), startdate  = 2009, enddate = 2018) %>%
  
  # select only relevant columns
  
  select(iso3c, indicator, country, value, date) %>%
  
  # widen the data by by creating an indicator variable that takes values from the indicator column and converts them into columns
  # and assigns correspondinding values from the value column to those indicator columns
  #instead of one indicator column with gdp per capita and population as variables, there are now two new columns for gdp per and population
  
  pivot_wider(names_from = indicator, values_from = value) %>%
  
  # rename the columns 
  
  set_names(c("wb_code", "country", "year", "population", "gdp_per_capita")) 

# create a vdem tibble that reads in data from the V-Dem csv

vdem <- read_csv("raw-data/V-Dem-CY-Full+Others-v9.csv") %>%
  
  #select relevant columns with variables of interest, including country name, country code, year, educational equality, 
  #freedom of movement, and freedom of academic and cultural expression
  
  select(country_name, country_text_id, year, e_peaveduc, v2peedueq, v2clfmove, v2clacfree) %>%
  
  # select only observations past 2009
  
  filter(year >= 2009) %>%
  
  # rename the columns so they're easier to understand
  
  set_names(c("country_name", "wb_code", "year", "education_15_plus",
              "education_equality", "freedom_foreign_movement", "freedom_expression"))

# read in field of study data from excel file, skipping the first three lines of blank or irrelevant information

field_by_country <- read_xlsx("raw-data/International-Students-Fields-of-Study.xlsx", skip = 3) %>%
  
  # clean the names of the columns 
  
  clean_names() %>%
  
  #select only the first 25 observations (omitting miscellaneous, irrelevant information at the bottom of the dataset)
  
  head(25)

# create a country code variable for field_by_country using the world bank package 

field_by_country$wb_code <- countrycode(field_by_country$place_of_origin, origin = 'country.name', destination = 'wb')

# create country_indicators tibble which consolidates all the data from the various tibbles related to country of origin and year

country_indicators <- country %>%
  
  #join the vdem tibble to the country tibble by country code
  
  left_join(vdem, by = c("year", "wb_code")) %>%
  
  #join the wb tibble to the country and vdem tibbles by country code
  
  left_join(wbdata, by = c("year", "wb_code")) %>%
  
  # join the field_by_country tibble to the country, vdem, and wdata tibbles by country code
  
  left_join(field_by_country, by = "wb_code") %>%
  
  #omit country_name.y and country columns since they are redundant with country_name.x
  
  select(-country_name.y, -country) %>%
  
  # filter out Mexico and Central America observations since they are duplicates of the Mexico observation
  
  filter(country_name.x != "Mexico and Central America")

# convert year in country_indicators to integer numeric variale 

country_indicators$year <- as.integer(country_indicators$year)


# read in funding data from excel spreadsheet, skip first five observations

funding <- read_xlsx("raw-data/International-Students-Primary-Source-of-Funding.xlsx", skip = 5) %>%
  
  #clean names for formatting purposes
  
  clean_names() %>%
  
  #select only relevant columns 
  
  select(x1, x2018_19) %>%
  
  #rename columns so they're easier to understand 
  
  set_names(c("funding_source", "num_students")) %>%
  
  #omit missing values 
  
  na.omit()

#read in enrollment data from an excel file, skipping the first four lines 

enrollment <- read_xlsx("raw-data/International-Students-Enrollment.xlsx", skip =4) %>%
  
  #clean the names so they're easier to understand 
  
  clean_names() %>%
  
  #select only observations past 2000/01
  
  slice(53:71)

#convert percent_change in enrollment data to a numeric variable 

enrollment$percent_change <- as.numeric(enrollment$percent_change)

# write enrollment data into an rds file in clean-data directory 

write_rds(enrollment, "clean-data/enrollment.rds")

# write country indicators data into an rds file in clean-data directory 

write_rds(country_indicators, "clean-data/country_indicators.rds")

# write funding data into an rds file in clean-data directory 

write_rds(funding, "clean-data/funding.rds")
