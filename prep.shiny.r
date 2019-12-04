#load tidyverse package 

library(tidyverse)

# read country_indicators data from rds file in clean-data

country_indicators <- read_rds("clean-data/country_indicators.rds")

# write country_indicators data into an rds file in the app directory

write_rds(country_indicators, "app/country_indicators.rds")

# read funding data from rds file in clean-data

funding <- read_rds("clean-data/funding.rds")

# write funding data into an rds file in the app directory

write_rds(funding, "app/funding.rds")


# read enrollment data from rds file in clean-data

enrollment <- read_rds("clean-data/enrollment.rds")

# write enrollment data into an rds file in the app directory

write_rds(enrollment, "app/enrollment.rds")










