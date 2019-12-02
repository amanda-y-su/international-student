library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(scales)
library(RDocumentation)
library(wesanderson)
library(viridis)
library(RColorBrewer)
library(janitor)

country_indicators <- read_rds("country_indicators.rds")
funding <- read_rds("funding.rds")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "International Students in the United States",

    tabPanel(
      "Background",
      
      h2("A History"),
      p("The United States currently has the world’s largest international student population, hosting about 1.1 million of the 4.6 million Internationals students enrolled worldwide in 2017. Since the 1950s, this number has experienced steady growth. In the 2016-2017 school year, international students constituted 5 percent of the total population enrolled in U.S. higher education."),
      
      plotlyOutput("total"),
      
      br(),
      
      p("Despite previous trends of growth, the number of international students newly enrolling at a U.S. institution dropped for the first time in recent years in fall 2016. A national survey of staff at more than 500 U.S. higher education institutions showed a 7 percent decline in new international student enrollment in fall 2017, according to the Migration Policy Institute. Survey participants attributed the drop to a combination of factors including visa delays and denials, the costs of U.S. higher education, the shifting social and political climate, notably after the election of President Donald Trump, competition from institutions in other countries, and prospective students’ concerns about securing a job in the United States after graduation."),
      p("In recent years, China has been the top origin country for international students in the U.S, followed by India, South Korea, and Saudi Arabia. However, this wasn’t always the case. Prior to the passage of the Immigration Act of 1965, students from Canada, Taiwan, India, and several European and Latin American countries predominated in international student enrollment. The Immigration Act, which abolished the National Origins Formula that had largely been used to restrict immigration from Asia, Southern Europe, and Eastern Europe, reversed this trend. The U.S. subsequently saw an influx of international students from Asia. "),
      
      plotlyOutput("enrollment"),
      
      br(),
      
      h2("A Study"),
      p("This project looks to analyze various patterns in the experiences and motivations of international students in the United States. I visualize in several ways the distribution of international students across regions of origin from 2009 to 2019, the different indicators in countries of origin that may influence how many international students study in the U.S., the distribution of international students' fields of study, and the distribution of their primary sources of funding."),
      p("I used data from the Institute of International Education, the World Bank, and Varieties of Democracy. IIE tracked the number of international students in the U.S. and their countries of origin throughout the last decade and also provided the numbers I used to visualize the distribution of primary funding sources and fields of study.The World Bank dataset provides hundreds of statistics for different country indicators, including Gross Domestic Product per capita and population. The Varieties of Democracy dataset measures democracy in different countries by hundreds of factors, including my variables of interest: education equality, freedom of academic and cultural expression, and freedom of foreign movement."),
      br(),
      br()

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
          "Educational Equality" = "edu_equality",
          "Freedom of Foreign Movement" = "free_movement"
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
        href = "https://www.amanda-su.com/"))
      
    )
  )
)




server <- function(input, output) {
  
  output$total <- renderPlotly ({
    
    total_plot <- country_indicators %>%
              filter(!is.na(wb_code),
                     country_name.x != "Mexico and Central America") %>%
              group_by(year) %>%
              summarize(total = sum(num_students)) %>%
              ggplot(aes(as.factor(year), total, fill = I("#A1C6B7"))) + 
              geom_col() + 
              scale_y_continuous(label = comma) +
              labs(
                 title = "Number of International Students in the U.S., 2009-2018", 
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
  
  output$enrollment <- renderPlotly({
    enrollment_ggplot <- enrollment %>%
      ggplot(aes(year, percent_change)) +
      geom_bar(stat = "identity", aes(fill = I("#E7C899")), legend = FALSE) +
      labs(
        title = "Annual Percent Change of International Students Enrolled in U.S.", 
        x = "Academic Year",
        y = "Percent Change"
      ) + 
      theme(
        axis.text.x = element_text(angle = 60, size = 6),
        legend.position = "none"
      ) 
    
    
    ggplotly(enrollment_ggplot, width = 800, height = 400, tooltip = "total") %>%
      layout(annotations = 
               list(x = 1, y = -0.25, text = "Source: Institute of International Education", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10, color="black")), 
             margin = list(l = 150, r = -20, b = 20, t = 50)) %>%
      config(displayModeBar = F)
      
  })
  
  output$region <- renderPlotly({
    region_ggplot <- country_indicators %>%
      filter(!is.na(population)) %>%
      filter(!is.na(region)) %>%
      group_by(region, year) %>%
      summarize(prop_students = sum(num_students) / sum(population)) %>%
      ggplot(aes(x = factor(year), y = prop_students, fill = year, 
                 text = paste("<b>Year</b>:", year, '<br>', '<b>Proportion:</b>', prop_students, '</br>'))) +
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
        y = "Percent Change"
      ) +
      scale_fill_gradientn(colors = (wes_palette("Darjeeling2", type = "discrete")))
    
    ggplotly(region_ggplot, tooltip = "text", width = 920, height = 720) %>%
      layout(margin = list(l = 50, r = -20, b = 20, t = -10)) %>%
      config(displayModeBar = F)
    
    
  }
  )
  
  output$country_indicator <- renderPlotly ({
  
      if (input$indicator == "gdp") {
      plot_ly(
          data = country_indicators,
          x = ~gdp_per_capita, 
          y = ~num_students,
          frame = ~year,
          color = I("#405568"),
          size = ~population,
          hoverinfo = 'text',
          text = ~paste('<b>Country:</b>', country_name.x, 
                        '</br>', "<b>GDP Per Capita:</b>", gdp_per_capita, '<br>',
                        '<b># of Students:</b>', num_students, '</br>'),
          type = 'scatter',
          width = 900, 
          height = 600
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
        color = I("#E3AB4A"), 
        size = ~population,
        hoverinfo = "text",
        text = ~paste('<b>Country:</b>', country_name.x, 
                      '</br>', "<b>Freedom of Expression:</b>", freedom_expression, '<br>',
                      '<b># of Students:</b>', num_students, '</br>'),
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
        config(displayModeBar = F) 
    }

    else if (input$indicator == "edu_equality") {
      plot_ly(data = country_indicators, x = ~education_equality, y = ~num_students,
        frame = ~year,
        color = I("#A1C6B7"),
        size = ~population,
        hoverinfo = "text",
        text = ~paste('<b>Country:</b>', country_name.x, 
                             '</br>', "<b>Educational Equality</b>", education_equality, '<br>',
                             '<b># of Students:</b>', num_students, '</br>'),
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
        config(displayModeBar = F)
        
    }
    
    else if (input$indicator == "free_movement") {
      plot_ly(data = country_indicators, x = ~freedom_foreign_movement, 
              y = ~num_students,
              frame = ~year,
              color = I("#CF4E3E"),
              size = ~population,
              hoverinfo = "text",
              text = ~paste('<b>Country:</b>', country_name.x, 
                            '</br>', "<b>Freedom of Foreign Movement:</b>", freedom_foreign_movement, '<br>',
                            '<b># of Students:</b>', num_students, '</br>'),
              type = 'scatter',
              width = 900, 
              height = 600)  %>%
        animation_opts(
          1000, easing = "elastic", redraw = FALSE) %>%
        layout(
          title = 'Number of International Students in the United States',
          xaxis = list(
            title = "Freedom of Foreign Movement",
            zeroline = F
          ),
          yaxis = list(
            title = "Number of Students",
            zeroline = F, 
            tickformat = ",d"
            
          ),
          margin = list(l = 150, r = -20, b = 20, t = -10)
        ) %>%
        config(displayModeBar = F)
    }
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
      ggplot(aes(x = funding_source, y = num_students, fill = funding_source, 
                 text = paste("<b>Source:</b>", funding_source, '<br>', '<b># Students:</b>', num_students, '</br>'))) +
      geom_col() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 8),
        axis.ticks.x = element_blank()
      ) +
      labs(x = "Funding Source", fill = "Funding Source", y = "Number of International Students in the U.S."
      ) +
      scale_y_continuous(label = comma) +
      scale_fill_brewer(palette = "BrBG", direction = -1)

    ggplotly(funding_ggplot, tooltip = "text", width = 900, height = 700) %>%
      layout(margin = list(l = 50, r = -20, b = 20, t = -5),
             title = list(text = paste0("Primary Sources of Funding for International Students in the U.S.",
                                        '<br>',
                                        '<sup>',
                                        'How many students primarily used a particular source of funding to fund their U.S. education in 2018?',
                                        '</sup>'))) %>%
      config(displayModeBar = F)
    
    })
  
  output$field <- renderPlotly({
    
    field_ggplot <- country_indicators %>%
      select(country_name.x, year, total_students, business_mgmt, education, engineering, 
             fine_applied_arts, health_professions,
             humanities, intensive_english, math_computer_science, 
             physical_life_sciences, social_sciences, 
             other_fields_of_study, undeclared) %>% 
      na.omit() %>%
      filter(year == 2018) %>%
      filter(country_name.x != "Mexico and Central America") %>%
      select(-year) %>%
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
      mutate(field = fct_relevel(field, c(
        "Business Management",
        "Education",
        "Engineering",
        "Math/CS",
        "Physical & Life Sciences",
        "Social Sciences",
        "Humanities",
        "Fine Arts",
        "Health Professions",
        "Intensive English",
        "Undeclared",
        "Other"
      ))) %>%
      ggplot(aes(x = field, y = fct_rev(country_name.x), 
                 text = paste("<b>Country:</b>", country_name.x, '<br>', 
                              '<b>Field:</b>', field, '<br>', 
                              '<b>Percent:</b>', value, '%',
                              '</br>'))) +
      geom_tile(aes(fill = value), color = "white") + 
      scale_fill_gradient(low = "white", high = I("#4B8A7E")) +
      labs(y = "Country of Origin", x = "Field of Study", fill = "Percent")
    
    ggplotly(field_ggplot, width = 700, height = 700, tooltip = "text") %>%
      layout(xaxis = list(side ="bottom", tickangle = -45), 
             margin = list(l = 50, r = -20, b = 0, t = -5),
             title = list(text = paste0("International Students' Fields of Study in the U.S. by Country",
                                               '<br>',
                                               '<sup>',
                                               'What percent of international students from a country chose a particular field of study in 2018?',
                                               '</sup>'))) %>% 
      config(displayModeBar = FALSE)
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
