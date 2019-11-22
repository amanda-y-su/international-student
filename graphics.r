library(tidyverse)
library(gganimate)
library(gt)
library(scales)

dir_create("graphics")

#something about regions (bar graph)
region_graph <- country_indicators %>%
  filter(!is.na(population)) %>%
  filter(!is.na(region)) %>%
  group_by(region, year) %>%
  summarize(prop_students = sum(num_students)/sum(population)) %>%
  ggplot(aes(x = year, y = prop_students, fill = year)) + 
  geom_col() +
  facet_wrap(~region) + 
  theme(strip.text = element_text(size=4.4), axis.text.x = element_text(angle = 90, size = 4), legend.position = "none") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  labs(title = "Proportion of Origin Region's Population that are \nInternational Students in the United States", x = "Year", 
       y = "Proportion") 

write_rds(region_graph, "graphics/region_graph.rds")

# scatterplot: gdp
gdp_map <- country_indicators %>%
  ggplot(aes(x = gdp_per_capita, y = num_students, size = population, frame = year)) +
  geom_point() +
  ylim(0, 30000) + 
  transition_time(year) +
  labs(title = "Number of International Students Depending on Origin Country's GDP", subtitle = "Year: {frame_time}",
       x = "Gross Domestic Product ($)", y = "Number of Students", size = "Population") +
  scale_size_continuous(labels=comma) 

write_rds(gdp_map, "graphics/gdp_map.rds")

# scatterplot: freedom of expression

free_expression_map <- country_indicators %>%
  filter(year == "2017") %>%
  ggplot(aes(x = freedom_expression, y = num_students, size = population)) +
  ylim(0, 30000) + 
  geom_point(aes(colour = "red")) + 
  geom_smooth(method = "lm", se = F, colour = "gray") + 
  labs(title = "Number of International Students Depending on \nOrigin Country's Freedom of Expression", 
       subtitle = "Year: 2017", x = "Freedom of Expression", y = "Number of Students", population = "Population") + 
  scale_size_continuous(labels=comma) +
  scale_colour_discrete(guide = 'none')

write_rds(free_expression_map, "graphics/free_expression_map.rds")

#scatterplot: educational equality 
edu_equality_map <- country_indicators %>%
  filter(year == "2017") %>%
  ggplot(aes(x = education_equality, y = num_students, size = population, colour = "blue")) +
  ylim(0, 30000) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F, colour = "gray") + 
  labs(title = "Number of International Students Depending on \nOrigin Country's Educational Equality", 
       subtitle = "Year: 2017", x = "Education Equality", y = "Number of Students", population = "Population") + 
  scale_size_continuous(labels=comma) +
  scale_colour_discrete(guide = 'none')

write_rds(edu_equality_map, "graphics/edu_equality_map.rds")

funding_map <- funding %>% 
  filter(funding_source != "Total Students" &
           funding_source != "International Funding Sources" &
           funding_source != "U.S. Funding Sources") %>%
  mutate(funding_source = fct_relevel(funding_source, 
                                      c("U.S. Government",
                                        "U.S. Private Sponsor",
                                        "U.S. College or University",
                                        "Foreign Private Sponsor",
                                        "Foreign Government or University", 
                                        "International Organization", 
                                        "Current Employment", 
                                        "Personal and Family", 
                                        "Other Sources"))) %>%
  ggplot(aes(x=funding_source, y = num_students, fill=funding_source)) +
  geom_col() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(title = "Primary Sources of Funding for International \nStudents in the U.S.", subtitle = "2017-2018 School Year", 
       x = "Funding Source", fill = "Funding Source", y = "Number of International Students in the U.S.") + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

write_rds(funding_map, "graphics/funding_map.rds")

#most common fields of study for countries arranged in order from highest to lowest 

#field_table <- field_by_country %>%
#  arrange(desc(total_students)) %>%
#  head(25) %>%
#  select(-total_students, -wb_code) %>%
#  pivot_longer(cols = 2:13) %>%
#  group_by(place_of_origin) %>% 
#  top_n(1) %>%
#  select(place_of_origin, name) 

#gt(field_table) %>%
#  tab_header(title = "Most Popular Field of Study for International Students in the U.S. By Country of Origin") %>%
#  cols_label(place_of_origin = "Country of Origin", name = "Field of Study") %>%
#  cols_align(align = c("center")) %>%

#  tab_options(
#    table.width = 500,
##    table.font.size = 15,
#    heading.title.font.size = 20,
#    heading.subtitle.font.size = 14,
#    column_labels.font.size = 14
#  )