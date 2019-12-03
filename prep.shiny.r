library(tidyverse)

country_indicators <- read_rds("clean-data/country_indicators.rds")
write_rds(country_indicators, "app/country_indicators.rds")

funding <- read_rds("clean-data/funding.rds")
write_rds(funding, "app/funding.rds")

enrollment <- read_rds("clean-data/enrollment.rds")
write_rds(enrollment, "app/enrollment.rds")











