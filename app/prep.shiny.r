library(fs)
library(sf)
library(gganimate)
library(tidyverse)
library(save.r)

file_copy("graphics/region_graph.rds", "app/region_graph.rds")

file_copy("graphics/gdp_map.rds", "app/gdp_map.rds")

file_copy("graphics/free_expression_map.rds", "app/free_expression_map.rds")

file_copy("graphics/edu_equality_map.rds", "app/edu_equality_map.rds")

file_copy("graphics/funding_map.rds", "app/funding_map.rds")

region_map <- readRDS("app/region_graph.rds")
gdp_map <- readRDS("app/gdp_map.rds")
free_expression_map <- readRDS("app/free_expression_map.rds")
edu_equality_map <- readRDS("app/edu_equality_map.rds")
funding_map <- readRDS("app/funding_map.rds")

ggsave("app/region_graph.png", region_map)
anim_save("app/gdp_map.gif")
ggsave("app/free_expression_map.png", free_expression_map)
ggsave("app/edu_equality_map.png", edu_equality_map)
ggsave("app/funding_map.png", funding_map)







