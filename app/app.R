library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(shiny)
library(shinythemes)
library(coefplot)
library(plotly)

country_indicators <- read_rds("app/country_indicators.rds")
funding <- read_rds("app/funding.rds")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "International Students in the United States",

    tabPanel(
      "About the Project",

      p("This project looks to analyze various patterns in the experiences and motivations of international students in the United States. I visualize in several ways the distribution of international students across regions of origin from 2009 to 2019, the different indicators in countries of origin that may influence how many international students study in the U.S., and the distribution of international students’ primary sources of funding."),
      p("I used data from the Institute of International Education, the World Bank, and Varieties of Democracy. The IIE dataset tracks the number of international students in the U.S. and their countries of origin throughout the last decade. The World Bank dataset has hundreds of statistics for different country indicators, including Gross Domestic Product and population. The Varieties of Democracy dataset measures democracy in different countries by hundreds of factors, including my variables of interest: education equality and freedom of academic expression."),
      p("Though I could easily download all the data as Excel files, there was a significant amount of data cleaning that could occur before I could join my various datasets. The final IIE dataset required me to join two datasets for the years 2009-2014 and the years 2015-2019. I also had to clean the wbdata dataset by only selecting my two variables of interest — population and gross domestic product per capita, along with country code country name, and country year. For the vdem dataset, I selected only my two variables of interest — education equality and freedom of academic expression, along with country name, country code, and year. I also filtered for only observations past 2009 in the wbdata and vdem datasets since I only had IIE data past 2009. I then used the wbstats package to create a country code variable for the original IIE dataset so that I could easily join the three datasets together by country code since spelling and formatting for different country names varied drastically across datasets.")
    ),


    tabPanel(
      "Geographical Regions of Origin",
      plotlyOutput("region")
    ),

    tabPanel(
      "Indicators of Countries of Origin",
      p("Select from the following plots to see the correlation between various country of origin indicators and the number of international students in the U.S. from those countries"),
      selectInput("indicator",
        label = h5("Select Plot"),
        choices = list(
          "Gross Domestic Product Per Capita" = "gdp",
          "Freedom of Expression" = "free_expression",
          "Educational Equality" = "edu_equality"
        ),
        selected = "gdp",
        multiple = FALSE,
        selectize = FALSE,
        width = "400px",
        size = 1
      ),
      plotlyOutput("country_indicator")
    ),

    tabPanel(
      "Sources of Funding",

      plotlyOutput("funding")
    ),
    tabPanel(
      "About the Author",
      h1("Amanda Y. Su"),
      p("I'm a sophomore at Harvard College studying History & Literature. I'm originally from the Bay Area. I'm interested in issues of race, ethnicity, immigration and education. I'm planning to pursue career in journalism.")
    )
  )
)


server <- function(input, output) {
  output$country_indicator <- renderPlotly({
    if (input$indicator == "gdp") {
      ggplotly(country_indicators %>%

        ggplot(aes(x = gdp_per_capita, y = num_students, size = population)) +
        geom_point() +
        ylim(0, 30000) +
        labs(
          title = "Number of International Students Depending on Origin Country's GDP", subtitle = "Year: {frame_time}",
          x = "Gross Domestic Product Per Capita ($)", y = "Number of Students", size = "Population"
        ) +
        scale_size_continuous(labels = comma))
    }

    else if (input$indicator == "free_expression") {
      ggplotly(country_indicators %>%
        filter(year == "2017") %>%
        ggplot(aes(x = freedom_expression, y = num_students, size = population)) +
        ylim(0, 30000) +
        geom_point(aes(colour = "red")) +
        geom_smooth(method = "lm", se = F, colour = "gray") +
        labs(
          title = "Number of International Students Depending on Origin Country's Freedom of Expression",
          subtitle = "Year: 2017", x = "Freedom of Expression", y = "Number of Students", population = "Population"
        ) +
        scale_size_continuous(labels = comma) +
        scale_colour_discrete(guide = "none") +
        theme(legend.position = "none"))
    }

    else if (input$indicator == "edu_equality") {
      ggplotly(country_indicators %>%
        filter(year == "2017") %>%
        ggplot(aes(x = education_equality, y = num_students, size = population)) +
        ylim(0, 30000) +
        geom_point() +
        geom_smooth(method = "lm", se = F, colour = "gray") +
        labs(
          title = "Number of International Students Depending on Origin Country's Educational Equality",
          subtitle = "Year: 2017", x = "Education Equality", y = "Number of Students", population = "Population"
        ) +
        scale_size_continuous(labels = comma) +
        scale_colour_discrete(guide = "none"))
    }
  })

  output$region <- renderPlotly({
    ggplotly(country_indicators %>%
      filter(!is.na(population)) %>%
      filter(!is.na(region)) %>%
      group_by(region, year) %>%
      summarize(prop_students = sum(num_students) / sum(population)) %>%
      ggplot(aes(x = year, y = prop_students, fill = year)) +
      geom_col() +
      facet_wrap(~region) +
      theme(
        strip.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90, size = 5),
        axis.text.y = element_text(size = 5),
        legend.position = "none"
      ) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      labs(
        title = "Proportion of Origin Region's Population that are International Students in the United States", x = "Year",
        y = "Proportion"
      ))
  })

  output$funding <- renderPlotly({
    ggplotly(funding %>%
      filter(funding_source != "Total Students" &
        funding_source != "International Funding Sources" &
        funding_source != "U.S. Funding Sources") %>%
      mutate(funding_source = fct_relevel(
        funding_source,
        c(
          "U.S. Government",
          "U.S. Private Sponsor",
          "U.S. College or University",
          "Foreign Private Sponsor",
          "Foreign Government or University",
          "International Organization",
          "Current Employment",
          "Personal and Family",
          "Other Sources"
        )
      )) %>%
      ggplot(aes(x = funding_source, y = num_students, fill = funding_source)) +
      geom_col() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(
        title = "Primary Sources of Funding for International Students in the U.S.", subtitle = "2017-2018 School Year",
        x = "Funding Source", fill = "Funding Source", y = "Number of International Students in the U.S."
      ) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)))
  })
}


shinyApp(ui = ui, server = server)
