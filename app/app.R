library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(scales)
library(RDocumentation)
library(wesanderson)
library(viridis)
library(RColorBrewer)

country_indicators <- read_rds("country_indicators.rds")
funding <- read_rds("funding.rds")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "International Students in the United States",

    tabPanel(
      "Background",
      
      h2("A History"),
      p("This project looks to analyze various patterns in the experiences and motivations of international students in the United States. I visualize in several ways the distribution of international students across regions of origin from 2009 to 2019, the different indicators in countries of origin that may influence how many international students study in the U.S., and the distribution of international students’ primary sources of funding."),
      
      plotlyOutput("total"),
      
      h2("Purpose"),
      p("This project looks to analyze various patterns in the experiences and motivations of international students in the United States. I visualize in several ways the distribution of international students across regions of origin from 2009 to 2019, the different indicators in countries of origin that may influence how many international students study in the U.S., and the distribution of international students’ primary sources of funding."),
    
      h3("Data"),
      p("I used data from the Institute of International Education, the World Bank, and Varieties of Democracy. The IIE dataset tracks the number of international students in the U.S. and their countries of origin throughout the last decade. The World Bank dataset has hundreds of statistics for different country indicators, including Gross Domestic Product and population. The Varieties of Democracy dataset measures democracy in different countries by hundreds of factors, including my variables of interest: education equality and freedom of academic expression."),
      p("Though I could easily download all the data as Excel files, there was a significant amount of data cleaning that could occur before I could join my various datasets. The final IIE dataset required me to join two datasets for the years 2009-2014 and the years 2015-2019. I also had to clean the wbdata dataset by only selecting my two variables of interest — population and gross domestic product per capita, along with country code country name, and country year. For the vdem dataset, I selected only my two variables of interest — education equality and freedom of academic expression, along with country name, country code, and year. I also filtered for only observations past 2009 in the wbdata and vdem datasets since I only had IIE data past 2009. I then used the wbstats package to create a country code variable for the original IIE dataset so that I could easily join the three datasets together by country code since spelling and formatting for different country names varied drastically across datasets.")
    ),


    tabPanel(
      "Geographical Regions of Origin",
      plotlyOutput("region")
    ),

    tabPanel(
      "Indicators of Countries of Origin",
      selectInput("indicator",
        label = h5("Select from the following plots to see the correlation between various country of origin indicators and the number of international students in the U.S. from those countries"),
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
      "Fields of Study",
      
      plotlyOutput("field")
    ),

    tabPanel(
      "Sources of Funding",

      plotlyOutput("funding")
    ),
    tabPanel(
      "About Me",
      h2("Amanda Y. Su"),
      p("I'm a sophomore at Harvard College studying History & Literature and pursuing a career in journalism. Though I am originally from the Bay Area, I currently live in Zurich, Switzerland. I'm intellectually and personally interested in issues of race, ethnicity, immigration, education, and their intersections. As a student, I'm always looking for opportunities to understand these topics more deeply through my work in academia, journalism, photography, and data science."),
      p("To learn more about me and what I do, you can:"),
        p("— Reach me at ",
        a("amandasu@college.harvard.edu",
          href = "amandasu@college.harvard.edu")),
      p("— Connect with me on ",
        a("LinkedIn",
          href = "https://www.linkedin.com/in/amandaysu/")),
      p("— Visit my ",
        a("personal website",
        href = "https://www.amanda-su.com/")),
      
      imageOutput("portrait")
    )
  )
)




server <- function(input, output) {
  
  output$total <- renderPlotly ({
    
    total_plot <- country_indicators %>%
              filter(!is.na(wb_code)) %>%
              group_by(year) %>%
              summarize(total = sum(num_students)) %>%
              ggplot(aes(as.factor(year), total, fill = I("#A1C6B7"))) + 
              geom_col() + 
              scale_y_continuous(label = comma) +
              labs(
                 title = "Number of International Students in the U.S.", 
                 x = "Year",
                 y = "Number of Students"
               ) + 
      theme(legend.position = "none")
            
  
  ggplotly(total_plot, width = 800, height = 400, tooltip = "total") %>%
      layout(annotations = 
               list(x = 1, y = -0.16, text = "Source: Institute of International Education", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10, color="black")), 
             margin = list(l = 150, r = -20, b = 20, t = 50)) %>%
    config(displayModeBar = F)
  })
  
  output$country_indicator <- renderPlotly ({
  
      if (input$indicator == "gdp") {
      plot_ly(
          data = country_indicators,
          x = ~gdp_per_capita, 
          y = ~num_students,
          frame = ~year,
          text = ~country_name.x, 
          color = I("#ae3918"),
          size = ~population,
          hoverinfo = 'none',
          type = 'scatter',
          width = 900, 
          height = 600,
          hovertemplate = paste(
            '<i>Country</i>: %{text}',
            '<br><i>GDP Per Capita</i>: %{x}',
            '<br><i># Students</i>: %{y}</br>'
          )
        ) %>%
        animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
          layout(title = 'Number of International Students in the United States',
                 xaxis = list(title = "Gross Domestic Product Per Capita ($)", zeroline = F, dtick = 25000, tickformat = ",d"),
                 yaxis = list(title = "Number of Students", zeroline = F, tickformat = ",d"), 
                 margin = list(l = 150, r = -20, b = 20, t = -10)) %>%
          config(displayModeBar = F)
        }

    else if (input$indicator == "free_expression") {
      
        plot_ly(data = country_indicators,
        x = ~freedom_expression, 
        y = ~num_students,
        frame = ~year,
        text = ~country_name.x, 
        color = I("#027ab0"), 
        size = ~population,
        hoverinfo = "country_name.x",
        type = 'scatter',
        width = 900, 
        height = 600
      ) %>%
        animation_opts(
          1000, easing = "elastic", redraw = FALSE
        ) %>%
        layout(
          title = 'Number of International Students in the United States',
          xaxis = list(
            title = "Freedom of Academic and Cultural Expression",
            zeroline = F
          ),
          yaxis = list(
            title = "Number of Students",
            zeroline = F,
            tickformat = ",d"
          ),
          margin = list(l = 150, r = -20, b = 20, t = -10)
          
        ) %>%
        config(displayModeBar = F) %>%
        add_trace(
          type = 'scatter',
          hovertemplate = paste('<i>Country</i>: %{text}',
                                '<br><i>Freedom of Expression</i>: %{x}',
                                '<br><i># Students</i>: %{y}</br>'),
          hoverinfo = "none",
          showlegend = FALSE
        )
    }

    else if (input$indicator == "edu_equality") {
      plot_ly(data = country_indicators, x = ~education_equality, y = ~num_students,
        frame = ~year,
        text = ~country_name.x,
        color = I("#d19c2f"),
        size = ~population,
        hoverinfo = "country_name.x",
        type = 'scatter',
        width = 900, 
        height = 600)  %>%
        animation_opts(
          1000, easing = "elastic", redraw = FALSE) %>%
        layout(
          title = 'Number of International Students in the United States',
          xaxis = list(
            title = "Educational Equality",
            zeroline = F
          ),
          yaxis = list(
            title = "Number of Students",
            zeroline = F, 
            tickformat = ",d"
          
          ),
          margin = list(l = 150, r = -20, b = 20, t = -10)
        ) %>%
        config(displayModeBar = F) %>%
        add_trace(
          type = 'scatter',
          hovertemplate = paste('<i>Country</i>: %{text}',
                                '<br><i>Education Equality</i>: %{x}',
                                '<br><i># Students</i>: %{y}</br>'),
          showlegend = FALSE
        )
        
    }
  }
)

  
  output$region <- renderPlotly({
     region_ggplot <- country_indicators %>%
      filter(!is.na(population)) %>%
      filter(!is.na(region)) %>%
      group_by(region, year) %>%
      summarize(prop_students = sum(num_students) / sum(population)) %>%
      ggplot(aes(x = factor(year), y = prop_students, fill = year)) +
      geom_col() +
      facet_wrap(~region) +
      theme(
        strip.text = element_text(size = 6),
        axis.text.x = element_text(angle = 60, size = 6),
        axis.text.y = element_text(size = 7),
        legend.position = "none"
      ) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      labs(
        title = "Proportion of Origin Region's Population that are International Students in the U.S.", 
        x = "Year",
        y = ""
      ) +
       scale_fill_gradientn(colors = (wes_palette("Darjeeling2", type = "discrete")))
     
  ggplotly(region_ggplot, tooltip = "prop_students", width = 920, height = 720) %>%
    layout(margin = list(l = 50, r = -20, b = 20, t = -10)) %>%
    config(displayModeBar = F)

  
  }
)

  output$funding <- renderPlotly({
    funding_ggplot <- funding %>%
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
        axis.text.x = element_text(angle = 45, size = 8),
        axis.ticks.x = element_blank()
      ) +
      labs(
        title = "Primary Sources of Funding for International Students in the U.S.", subtitle = "2017-2018 School Year",
        x = "Funding Source", fill = "Funding Source", y = "Number of International Students in the U.S."
      ) +
      scale_y_continuous(label = comma) +
      scale_fill_brewer(palette = "BrBG", direction = -1)

    ggplotly(funding_ggplot, tooltip = "num_students", width = 900, height = 700) %>%
      layout(margin = list(l = 50, r = -20, b = 20, t = -5)) %>%
      config(displayModeBar = F)
    
    })
  
  output$field <- renderPlotly({
    
    field_ggplot <- field_by_country %>%
      pivot_longer(cols = 3:14, names_to = "field") %>%
      mutate(field = recode(field, 
                           business_mgmt = "Business Management",
                                  education = "Education",
                                  engineering = "Engineering",
                                  fine_applied_arts = "Fine Arts",
                                  health_professions = "Health Professions",
                                  humanities = "Humanities",
                                  intensive_english = "Intensive English",
                                  math_computer_science = "Math/CS",
                                  physical_life_sciences = "Physical & Life Sciences",
                                  social_sciences = "Social Sciences",
                                  other_fields_of_study = "Other",
                                  undeclared = "Undeclared")) %>%
      ggplot(aes(x = field, y = fct_rev(place_of_origin))) +
      geom_tile(aes(fill = value), color = "white") + 
      scale_fill_gradient(low = "white", high = I("#AB8A5C")) +
      labs(y = "Country of Origin", x = "Field of Study", fill = "Percent")
    
    ggplotly(field_ggplot, width = 700, height = 700) %>%
      layout(xaxis = list(side ="bottom", tickangle = -45)) %>% 
      config(displayModeBar = FALSE) %>%
      add_trace(
        type = 'heatmap',
        hovertemplate = paste('<i>Country</i>: %{y}',
                              '<br><i>Field</i>: %{x}</br>'),
        showlegend = FALSE
      )
  })
  
  # %>%
  #   mutate(field = relevel(field, c(
  #     "Business Management",
  #     "Education",
  #     "Engineering",
  #     "Math/CS",
  #     "Physical & Life Sciences",
  #     "Social Sciences",
  #     "Humanities",
  #     "Fine Arts",
  #     "Health Professions",
  #     "Intensive English",
  #     "Undeclared",
  #     "Other"
  #   ))) 
  
}

shinyApp(ui = ui, server = server)
