# load relevant packages

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
library(networkD3)
library(infer)
library(broom)

# read in country_indicators data from rds file in app directory

country_indicators <- read_rds("country_indicators.rds")

# read in funding data from rds file in app directory

funding <- read_rds("funding.rds")

# read in enrollment data from rds file in app directory

enrollment <- read_rds("enrollment.rds")

# create user interface for shiny app

ui <- fluidPage(

  # use flatly shiny theme
  theme = shinytheme("flatly"),

  # create navigation buy with a header title
  
  navbarPage(
    "International Students in the United States",

    # create a panel in the navigation bar named "Background"

    tabPanel(
      "Background",

      # add a header and text about the brief recent history of international students in the U.S.

      h2("A History"),
      p("The United States currently has the world’s largest international student population, 
        hosting about 1.1 million of the 4.6 million Internationals students enrolled worldwide 
        in 2017. Since the 1950s, this number has experienced steady growth. 
        In the 2016-2017 school year, international students constituted 
        5 percent of the total population enrolled in U.S. higher education."),

      # output "total" plot created in the server below

      plotlyOutput("total"),

      br(),
      
      p("Despite previous trends of growth, the number of international students 
        newly enrolling at a U.S. higher education institution dropped for the first time in recent years 
        in fall 2016. A national survey of staff at more than 500 U.S. higher 
        education institutions showed a 7 percent decline in new international 
        student enrollment in fall 2017, according to the Migration Policy Institute. 
        Survey participants attributed the drop to a combination of factors including 
        visa delays and denials, the costs of U.S. higher education, the shifting 
        social and political climate, notably after the election of President Donald 
        Trump, competition from institutions in other countries, and prospective 
        students’ concerns about securing a job in the United States after graduation."),
      
      br(),

      # output enrollment plot created in server below
      
      plotlyOutput("enrollment"),
      
      br(),

      # add a header and text about my project
      
      h2("A Study"),
      p("This project looks to analyze various patterns in the experiences and motivations of international students pursuing higher education
        in the United States. I visualize in several ways the distribution of international students across regions of origin from 2009 to 2019, the different indicators in countries of origin that may influence how many international students study in the U.S., the distribution of international students' fields of study, and the distribution of their primary sources of funding."),
      p("I used data from the Institute of International Education, the World Bank, and Varieties of Democracy. IIE tracked the number of international students in the U.S. and their countries of origin throughout the last decade and also provided the numbers I used to visualize the distribution of primary funding sources and fields of study. The World Bank dataset provides hundreds of statistics for different country indicators, including Gross Domestic Product per capita and population. The Varieties of Democracy dataset measures democracy in different countries by hundreds of factors, including my variables of interest: educational equality, freedom of academic and cultural expression, and freedom of foreign movement."),
      p(
        "The source code for this project can be found at my GitHub ",

        # add a hyperlink to my github

        a("here",
          href = "https://github.com/amanda-y-su/international-student"
        ), "."
      ),
      br()
    ),

    # create a tab panel with the title "Geographical Regions of Origin"

    tabPanel(
      "Geographical Regions of Origin",
      
      # add text about background and explaining the graph 
      
      fixedRow(
        column(
          4,
          h3("Background"),
          p("In recent years, China has been the top origin country 
                 for international students in the U.S, followed by India, 
                 South Korea, and Saudi Arabia. However, this wasn’t always the case. 
                 Prior to the passage of the Immigration Act of 1965, 
                 students from Canada, Taiwan, India, and several 
                 European and Latin American countries predominated in 
                 international student enrollment. The Immigration Act, 
                 which abolished the National Origins Formula that had 
                 largely been used to restrict immigration from Asia, Southern Europe, 
                 and Eastern Europe, reversed this trend. The U.S. subsequently saw 
                 an influx of international students from Asia. "),
          h3("Explanation"),
          p("The plot indicates that the origin regions with the highest proportion of international students 
                 in the U.S. are North America and Micronesia. This makes sense because North America only includes 
                 Canada where a large proportion of its population are international students in the U.S., its neighboring country. Canada is also 
                 linguistically, culturally, and socioeconomically similar to the U.S., so barriers for Canadian international students are minimal.
                 Micronesia, which is a subregion of Oceania composed of thousands of small islands, has a high proportion of international students
                 in the U.S. because its population itself is already quite small at around 100,000. This also explains the variability of the 
                 proportion over the past decade. Though East Asia includes China, which has the highest number of international students in the U.S., it 
                 does not have the highest proportion of international students because its population is also the largest in the world. I calculated
                 the proportion instead of raw numbers for this very reason, so that countries with already large populations wouldn't skew and
                 bias the visual representation.")
        ),
      
        #display the region plot
        
        column(
          5,
          plotlyOutput("region")
        )
      )
    ),

    # create a tab panel which explains what each of the indicators means

    tabPanel(
      "Indicators of Countries of Origin",
      
      tabsetPanel(
        tabPanel(
          "Background",
          
          h3("Gross Domestic Product Per Capita"),
          h4("Indicator Explanation"),
          fixedRow(
            column(
              7,
              p("GDP per capita is a measure of a country's economic output that accounts for its number of people. 
          It divides the country's gross domestic product by its total population. That makes it a good measurement of a
            country's standard of living. It tells you how prosperous a country feels to each of its citizens."))),

          h3("Freedom of Academic and Cultural Expression"),
          h4("Indicator Explanation"),
          fixedRow(
            column(
              2,
              p("Question: Is there academic freedom and freedom of cultural expression related to political issues?")
            ),
            column(
              7,
              p("Responses:"),
              p("0: Not respected by public authorities. Censorship and intimidation are frequent. 
      Academic activities and cultural expressions are severely restricted or controlled by the government."),
              p("1: Weakly respected by public authorities. Academic freedom and freedom of cultural expression 
      are practiced occasionally, but direct criticism of the government is mostly met with repression."),
              p("2: Somewhat respected by public authorities. Academic freedom and freedom of cultural 
      expression are practiced routinely, but strong criticism of the government is sometimes met with repression."),
              p("3: Mostly respected by public authorities. There are few limitations on academic freedom 
      and freedom of cultural expression, and resulting sanctions tend to be infrequent and soft."),
              p("4: Fully respected by public authorities. There are no restrictions on academic freedom or cultural expression.")
            )
          ),
          
          h3("Edcuational Equality"),
          h4("Indicator Explanation"),
          fixedRow(
            column(
              2,
              p("Question: To what extent is high quality basic education guaranteed to all, 
        sufficient to enable them to exercise their basic rights as adult citizens? 
        Basic education refers to ages typically between 6 and 16 years of age 
          but this varies slightly among countries.")
            ),
            column(
              7,
              p("Responses:"),
              p("0: Extreme. Provision of high quality basic education is extremely unequal 
      and at least 75 percent (%) of children receive such low-quality education that 
      undermines their ability to exercise their basic rights as adult citizens."),
              p("1: Unequal. Provision of high quality basic education is extremely unequal and 
      at least 25 percent (%) of children receive such low-quality education that 
      undermines their ability to exercise their basic rights as adult citizens."),
              p("2: Somewhat equal. Basic education is relatively equal in quality 
      but ten to 25 percent (%) of children receive such low-quality education 
      that undermines their ability to exercise their basic rights as adult citizens."),
              p("3: Relatively equal. Basic education is overall equal in quality but five 
      to ten percent (%) of children receive such low-quality education that probably 
      undermines their ability to exercise their basic rights as adult citizens."),
              p("4: Equal. Basic education is equal in quality and less than five percent (%) 
        of children receive such low-quality education that probably undermines 
        their ability to exercise their basic rights as adult citizens.")
            )
          ),
          
          h3("Freedom of Foreign Movement"),
          h4("Indicator Explanation"),
          fixedRow(
            column(
              2,
              p("Question: Is there freedom of foreign travel and emigration? 
      This indicator specifies the extent to which citizens are able to travel 
      freely to and from the country and to emigrate without being subject to 
      restrictions by public authorities.")
            ),
            column(
              7,
              p("Responses:"),
              p("0: Not respected by public authorities. Citizens are rarely allowed to emigrate 
      or travel out of the country. Transgressors (or their families) are severely punished. 
      People discredited by the public authorities are routinely exiled or prohibited from traveling."),
              p("1: Weakly respected by public authorities. The public authorities systematically 
      restrict the right to travel, especially for political opponents or particular social 
      groups. This can take the form of general restrictions on the duration of stays 
      abroad or delays/refusals of visas."),
              p("2: Somewhat respected by the public authorities. The right to travel for leading political 
      opponents or particular social groups is occasionally restricted but ordinary citizens 
      only met minor restrictions."),
              p("3: Mostly respected by public authorities. Limitations on freedom of movement 
      and residence are not directed at political opponents but minor restrictions 
      exist. For example, exit visas may be required and citizens may be prohibited 
      from traveling outside the country when accompanied by other members of their family."),
              p("4: Fully respected by the government. The freedom of citizens to travel from 
        and to the country, and to emigrate and repatriate, is not restricted by 
        public authorities.")
            )
          )
          ),
          
        # add a panel which will display the scatterplots explaining proportion of international students
        # as a function of a country indicator 
        
        tabPanel(
          "Graphs",

          # create a selection bar where users can choose an indicator scatterplot to view

          selectInput("indicator",
            label = h5("Select from the following plots to see the correlation between various country of origin indicators and the number of international students in the U.S. from those countries"),

            # list available choices which each match a plot created in server below

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

          # output plot from country_indicator in server below (whichever one was selected)

          plotlyOutput("country_indicator")
        ),
        
        # add panel which will include analyses of the regression lines for the indicator plots
        
        tabPanel(
          "Analysis",
          
          br(),
          
          p("Each of the four regression lines has a positive slope, 
          indicating that there is a positive correlation between the 
          indicators and the proportion of a country’s population that 
          are international student in the U.S."),
          
          h4("GDP Per Capita"),
          
          p("The regression line 
          with the steepest slope is the one that explains the proportion 
          as a function of GDP per capita. This may be due to the fact that 
          the more prosperous a country is, the more likely its citizens 
          are able to afford the increasingly exorbitant costs of studying 
          abroad in the U.S."),
          
          h4("Freedom of Academic and Cultural Expression"),
          
          p("The regression line with the flattest slope 
          is the one that explains the proportion as a function of 
          freedom of academic and cultural expression, revealing 
          that there is little to no correlation between the two 
          variables. I had assumed that there would be a more extreme 
          positive correlation because limits on free cultural and 
          academic expression could hinder access to and suppress 
          desires for Western education."),
          
          h4("Freedom of Foreign Movement"),
          
          p("It made sense that the correlation 
          between freedom of foreign movement and the proportion would be 
          positive because countries that have more lax emigration laws 
          would allow for more movement of international students out 
          of the country."),
          
          h4("Educational Equality"),
          
          p("I was surprised by the 
          positive slope of educational equality regression line because 
          I had assumed that the more accessible education is in a country, 
          the less likely it is many students would want to leave that 
          country for the U.S. However, it’s likely that there is a 
          positive relationship because students in countries with 
          accessible basic education would be more likely to want to 
          continue that education at U.S. higher education institutes 
          whereas countries with many children who don’t even
          have access to basic education would be extremely unlikely to 
          pursue higher education in the U.S.")
          
        )
      )
    ),

    # create a tab panel named "Fields of Study"

    tabPanel(
      "Fields of Study",
      
      # add text explaining the field of study graph 
      
      fixedRow(
        column(
          4,
          h3("Explanation"),
          p("This heat map visually represents the distribution of 
               fields of study among international students in the U.S. 
               from particular countries of origin. The darker the cell 
               the higher the percentage of international students from a country 
               (y axis) who chose a certain field (x axis). The heat map indicates
               that the overall most popular fields of study among the represented 
               major sending countries of international students to the U.S. are 
               Engineering and Business Management. The cell at the intersection 
               of Engineering and Kuwait is the darkest one of the whole graph. 
               Engineering constitutes 63.5 percent of the chosen fields of study 
               of international students from Kuwait."),
          p("Based on the density of color in the heat map cells, Science, 
                 Technology, Engineering, and Math fields appear to be noticeably 
                 more popular than fields in the Arts and Humanities among international 
                 students. After graduating, STEM international students are eligible 
                 for an extended 36-months of Optional Practical Training, during which 
                 students can pursue valuable internships and work experience before 
                 students return to their home countries.")
        ),
        column(
          5,
          
          # output field of study plot created in server below
          
          plotlyOutput("field")
        )
      )
    ),

    # create a tab panel named funding

    tabPanel(
      "Sources of Funding",

      # add text explaining the sources of funding graph 
      
      fixedRow(
        column(
          4,
          h3("Explanation"),
          p("The most popular primary source of funding among international 
                 students in the U.S. in 2018 was Personal and Family, 
                 followed by Current Employment, and U.S. College or University. 
                 Most full-time international students have F-1 status, which 
                 allows for part time, on-campus employment. In addition to 
                 financial aid, funding from U.S. college or university also 
                 includes teaching and research assistantships, which are often 
                 federal government research grants disbursed to the student 
                 through the institution. The least popular was International 
                 Organizations, which can include the United Nations, World 
                 Health Organization, and other organizations that offer aid. 
                 They, however, require you to be in your home country 
                 when you apply and can be very competitive.")
        ),
        column(
          5,

          # output funding plot created in server below

          plotlyOutput("funding")
        )
      )
    ),

    # create a tab panel titled About which will include information about me and hyperlinks to my website, social media profiles, etc.

    tabPanel(
      "About",
      h2("Amanda Y. Su"),
      p("I'm a sophomore at Harvard College studying History & Literature and pursuing a career in journalism. Though I am originally from the Bay Area, I currently live in Zurich, Switzerland. I'm intellectually and personally interested in issues of race, ethnicity, immigration, education, and their intersections. As a student, I'm always looking for opportunities to understand these topics more deeply through my work in academia, journalism, photography, and data science."),
      p("To learn more about me and what I do, you can:"),
      p(
        "— Reach me at ",
        a("amandasu@college.harvard.edu",
          href = "amandasu@college.harvard.edu"
        )
      ),
      p(
        "— Connect with me on ",
        a("LinkedIn",
          href = "https://www.linkedin.com/in/amandaysu/"
        )
      ),
      p(
        "— Visit my ",
        a("personal website",
          href = "https://www.amanda-su.com/"
        )
      )
    )
  )
)

# create server with code for shiny app

server <- function(input, output) {
  
  # create plot that displays the number of international students in the U.S. over the last decade
  
  output$total <- renderPlotly({
    
    # use country_indicators data to make plot 
    
    total_plot <- country_indicators %>%
      
      # filter out observations with missing country codes (i.e., observations that aren't countries)
      # filter out "Mexico and Central America" observations because they are double
      # counted in the "Central America" osbervations
      
      filter(
        !is.na(wb_code),
        country_name.x != "Mexico and Central America"
      ) %>%
      
      # group the data by year so you can see the changes over time 
      
      group_by(year) %>%
      
      # create a varialbe named total which finds the total number of international students in the U.S. per year 
      
      summarize(total = sum(num_students)) %>%
      
      # create a ggplot with the x axis being year and the y axis being total number of international students 
      # change the color of the plot to green 
      # format the hover text
      
      ggplot(aes(as.factor(year), total, fill = I("#A1C6B7"), text = paste("<b>Total:</b>", total))) +
      
      # create a bar graph 
      
      geom_col() +
      
      # format the x axis so that it displays in comma style
      
      scale_y_continuous(label = comma) +
      
      # add titles and labels
      
      labs(
        title = "Number of International Students in the U.S., 2009-2018",
        x = "Year",
        y = "Number of Students"
      ) +
      
      # remove the legend 
      
      theme(legend.position = "none")

    # wrap the ggplot total_plot in ggplotly to create a more dynamic, sleek plotly graph 
    # set the hovertext to the text argument in the ggplot function above
    
    ggplotly(total_plot, width = 800, height = 400, tooltip = "text") %>%
      
      # add a source note at the bottom of the graph 
      layout(
        annotations =
          list(
            x = 1, y = -0.16, text = "Source: Institute of International Education",
            showarrow = F, xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
            font = list(size = 10, color = "black")
          ),
        
        # adjust the margins of the graph so nothing gets cut off 
        
        margin = list(l = 150, r = -20, b = 20, t = 50)
      ) %>%
      
      
      # remove distracting ModeBar
      
      config(displayModeBar = F)
  }
)


  # create a plot that displays percent change in number of international students in the U.S. over the last decade
  # to indicate trends in enrollment
  
  output$enrollment <- renderPlotly({
    
    # use enrollment data to make plot 
    
    enrollment_ggplot <- enrollment %>%
      
      # create a ggplot with the x axis being year and the y axis being percent change 
      # create bar graph 
      # change the color of the plot to yellow
      # format the hover text
      
      ggplot(aes(year, percent_change, text = paste("<b>Percent Change:</b>", percent_change, "%"))) +
      geom_bar(stat = "identity", aes(fill = I("#E7C899"))) +
      
      # add titles and labels 
      
      labs(
        title = "Annual Percent Change of International Students Enrolled in U.S.",
        x = "Academic Year",
        y = "Percent Change"
      ) +
      
      # rotate tick labels on x axis for readability purposes 
      # remove legend 
      
      theme(
        axis.text.x = element_text(angle = 60, size = 6),
        legend.position = "none"
      )

    # wrap the ggplot enrollment_ggplot in ggplotly to create plotly graphic 
    # set the hovertext to the text argument in the ggplot function above
    
    ggplotly(enrollment_ggplot, width = 800, height = 400, tooltip = "text") %>%
      layout(
        annotations =
          list(
            
            # add source note 
            
            x = 1, y = -0.25, text = "Source: Institute of International Education",
            showarrow = F, xref = "paper", yref = "paper",
            xanchor = "right", yanchor = "auto", xshift = 0, yshift = 0,
            font = list(size = 10, color = "black")
          ),
        
        # adjust margins 
        
        margin = list(l = 150, r = -20, b = 20, t = 50)
      ) %>%
      
      # remove ModeBar
      
      config(displayModeBar = F)
  })

  
  # create graph that displays the proportion of an origin region's population that are international students 
  # in the U.S., faceted by origin region, over the last decade
  
  output$region <- renderPlotly({
    
    # create ggplot using country_indicators data 
    
    region_ggplot <- country_indicators %>%
      
      # filter out missing population values and region values 
      
      filter(!is.na(population)) %>%
      filter(!is.na(region)) %>%
      
      # group by region and year 
      
      group_by(region, year) %>%
      
      # create prop_students variable which divides the total number of students in a year and region by the population
      # of that region in specific year
      
      summarize(prop_students = sum(num_students) / sum(population)) %>%
      
      # create ggplot with x axis as year and y axis as prop_students variable 
      # each year in the graph is a different color 
      
      ggplot(aes(
        x = factor(year), y = prop_students, fill = year,
        
        # format hover text 
        
        text = paste("<b>Year</b>:", year, "<br>", "<b>Proportion:</b>", prop_students, "</br>")
      )) +
      
      # create a bar graph 
      # facet by region so that each geographical region has its own bar graph 
      
      geom_col() +
      facet_wrap(~region) +
      
      # adjust font size and angle of axes labels and tick labels for readability purposes 
      
      theme(
        strip.text = element_text(size = 6),
        axis.text.x = element_text(angle = 60, size = 6),
        axis.text.y = element_text(size = 7),
        
        # remove legend 
        
        legend.position = "none"
      ) +
      
      # format y axis tick labels so that they don't display in scientific notatation
      
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      
      # add labels and titles to the plot
      
      labs(
        title = "Proportion of Origin Region's Population that are International Students in the U.S.",
        x = "Year",
        y = ""
      ) +
      
      # set color palette of bar graphs to Darjeeling 2 from Wes Andersen color palette package
      scale_fill_gradientn(colors = (wes_palette("Darjeeling2", type = "discrete")))

    ggplotly(region_ggplot, tooltip = "text", width = 920, height = 720) %>%
      layout(margin = list(l = 50, r = -20, b = 20, t = -10)) %>%
      config(displayModeBar = F)
  })

  # create graphs that display the correlation between proportion of country's population that are international students
  # in the U.S. and country indicators 
  
  output$country_indicator <- renderPlotly({
    
    # perform the following actions if the indicator selected in ui input is "gdp"
    
    if (input$indicator == "gdp") {
      
      # clean country_indicators data and assign to prop_gdp
      
      prop_gdp <- country_indicators %>%
        
        # create prop_students variable which divides number of international students in the U.S. by a 
        # country's population
        
        mutate(prop_students = num_students / population) %>%
        
        # filter out missing values for prop_students and gdp_per_capita, the variables of interest for this graph 
        
        filter(
          !is.na(prop_students),
          !is.na(gdp_per_capita)
        )

      # create a regression which explains prop_students as a function of log(gdp_per_capita) 
      # using the cleaned prop_gdp data
      # extract fitted values from model
      
      fit <- glm(prop_students ~ gdp_per_capita, data = prop_gdp) %>%
        fitted.values()
      
      # create plotly graph using data from prop_gdp 
      # set x axis to gdp_per_capita 
      # set y axis to prop_students 
      # set the frame to year so that you can see the different data points for each year in the past decade
      # set the hoverinfo to the formatted text argument, which will display the country name, gdp_per_capita, 
      # and proportion of population that are international students in the U.S. 
      # set type to scatter to create a scatterplot 
      # adjust width and height for aesthetic purposes 
      
      plot_ly(
        data = prop_gdp,
        x = ~ gdp_per_capita,
        y = ~prop_students,
        frame = ~year,
        color = I("#405568"),
        size = ~population,
        hoverinfo = "text",
        text = ~ paste(
          "<b>Country:</b>", country_name.x,
          "</br>", "<b>GDP Per Capita:</b>", gdp_per_capita, "<br>",
          "<b>Proportion:</b>", prop_students, "</br>"
        ),
        type = "scatter",
        width = 900,
        height = 600,
        mode = "markers"
      ) %>%
        
        # make animation elastic so that it "bounces" between the frames and isn't continuous
        # I wanted to make the changes between frames discrete since we can't know what points are in between years
        
        animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
        
        # add titles and axes labels
        # format margins so things don't get cut off 
        # don't display legend 
        
        layout(
          title = "Proportion of Population that are International Students in the U.S.",
          xaxis = list(title = "Gross Domestic Product Per Capita ($)", zeroline = F, tickformat = ",d"),
          yaxis = list(title = "Proportion", zeroline = F),
          margin = list(l = 150, r = -20, b = 20, t = -10),
          showlegend = F
        ) %>%
        
        # remove ModeBar
        
        config(displayModeBar = F) %>%
        
        # add regression line to scatterplot using the fit model created above
        
        add_markers(y = ~prop_students) %>%
        add_trace(x = ~ gdp_per_capita, y = ~fit, mode = "line")
    }

    # perform the following actions if the indicator selected in ui input is "free_expression"
    
    else if (input$indicator == "free_expression") {
      
      # clean country_indicators data and assign to prop_expression
      
      prop_expression <- country_indicators %>%
        
        # create prop_students variable which divides number of international students in the U.S. by a 
        # country's population
        
        mutate(prop_students = num_students / population) %>%

        # filter out missing values for prop_students and freedom_expression, the variables of interest for this graph 
        
        filter(
          !is.na(prop_students),
          !is.na(freedom_expression)
        )

      # create a regression which explains prop_students as a function of freedom_expression
      # using the cleaned prop_expression data
      # extract fitted values from model
      
      fit <- lm(prop_students ~ freedom_expression, data = prop_expression) %>%
        fitted.values()

      # create plotly graph using data from prop_expression 
      # set x axis to freedom_expression 
      # set y axis to prop_students 
      # set the frame to year so that you can see the different data points for each year in the past decade
      # set the hoverinfo to the formatted text argument, which will display the country name, freedom_expression, 
      # and proportion of population that are international students in the U.S. 
      # set type to scatter to create a scatterplot 
      # adjust width and height for aesthetic purposes 
      
      plot_ly(
        data = prop_expression,
        x = ~freedom_expression,
        y = ~prop_students,
        frame = ~year,
        color = I("#E3AB4A"),
        size = ~population,
        hoverinfo = "text",
        text = ~ paste(
          "<b>Country:</b>", country_name.x,
          "</br>", "<b>Freedom of Expression:</b>", freedom_expression, "<br>",
          "<b>Proportion:</b>", prop_students, "</br>"
        ),
        type = "scatter",
        width = 900,
        height = 600
      ) %>%
        
        # make animation elastic so that it "bounces" between the frames and isn't continuous

        animation_opts(
          1000,
          easing = "elastic", redraw = FALSE
        ) %>%
        
        # add titles and axes labels
        # format margins so things don't get cut off 
        # don't display legend 
        
        layout(
          title = "Proportion of Population that are International Students in the U.S.",
          xaxis = list(
            title = "Freedom of Academic and Cultural Expression",
            zeroline = F
          ),
          yaxis = list(
            title = "Proportion of Country Population",
            zeroline = F
          ),
          margin = list(l = 150, r = -20, b = 20, t = -10),
          showlegend = F
        ) %>%
        
        # remove ModeBar
        
        config(displayModeBar = F) %>%
        
        # add regression line to scatterplot using the fit model created above
        
        add_markers(y = ~prop_students) %>%
        add_trace(x = ~freedom_expression, y = ~fit, mode = "lines")
    }

    
    # perform the following actions if the indicator selected in ui input is "edu_equality"
    
    else if (input$indicator == "edu_equality") {
      
      # clean country_indicators data and assign to prop_edueq
      
      prop_edueq <- country_indicators %>%
        
        # create prop_students variable which divides number of international students in the U.S. by a 
        # country's population
        
        mutate(prop_students = num_students / population) %>%
        
        # filter out missing values for prop_students and education_equality, the variables of interest for this graph 
        
        filter(
          !is.na(prop_students),
          !is.na(education_equality)
        )

      # create a regression which explains prop_students as a function of education_equality
      # using the cleaned prop_edueq data
      # extract fitted values from model
      
      fit <- lm(prop_students ~ education_equality, data = prop_edueq) %>%
        fitted.values()

      # create plotly graph using data from prop_edueq
      # set x axis to education_equality
      # set y axis to prop_students 
      # set the frame to year so that you can see the different data points for each year in the past decade
      # set the hoverinfo to the formatted text argument, which will display the country name, education_equality, 
      # and proportion of population that are international students in the U.S. 
      # set type to scatter to create a scatterplot 
      # adjust width and height for aesthetic purposes 
      
      plot_ly(
        data = prop_edueq, x = ~education_equality, y = ~prop_students,
        frame = ~year,
        color = I("#A1C6B7"),
        size = ~population,
        hoverinfo = "text",
        text = ~ paste(
          "<b>Country:</b>", country_name.x,
          "</br>", "<b>Educational Equality</b>", education_equality, "<br>",
          "<b>Proportion:</b>", prop_students, "</br>"
        ),
        type = "scatter",
        width = 900,
        height = 600
      ) %>%
        
        # make animation elastic so that it "bounces" between the frames and isn't continuous
        
        animation_opts(
          1000,
          easing = "elastic", redraw = FALSE
        ) %>%
        
        # add titles and axes labels
        # format margins so things don't get cut off 
        # don't display legend 
        
        layout(
          title = "Proportion of Population that are International Students in the U.S.",
          xaxis = list(
            title = "Educational Equality",
            zeroline = F
          ),
          yaxis = list(
            title = "Proportion of Country Population",
            zeroline = F
          ),
          margin = list(l = 150, r = -20, b = 20, t = -10),
          showlegend = F
        ) %>%
        
        # remove ModeBar
        
        config(displayModeBar = F) %>%
        
        # add regression line to scatterplot using the fit model created above
        
        add_markers(y = ~prop_students) %>%
        add_trace(x = ~education_equality, y = ~fit, mode = "lines")
    }

    # perform the following actions if the indicator selected in ui input is "free_movement"
    
    else if (input$indicator == "free_movement") {
      
      # clean country_indicators data and assign to prop_movement
      
      prop_movement <- country_indicators %>%
        
        # create prop_students variable which divides number of international students in the U.S. by a 
        # country's population
        
        mutate(prop_students = num_students / population) %>%
        
        # filter out missing values for prop_students and freedom_foreign_movement, the variables of interest for this graph 
        
        filter(
          !is.na(prop_students),
          !is.na(freedom_foreign_movement)
        )
      
      # create a regression which explains prop_students as a function of freedom_foreign_movement
      # using the cleaned prop_movement data
      # extract fitted values from model

      fit <- lm(prop_students ~ freedom_foreign_movement, data = prop_movement) %>%
        fitted.values()

      # create plotly graph using data from prop_movement
      # set x axis to freedom_foreign_movement
      # set y axis to prop_students 
      # set the frame to year so that you can see the different data points for each year in the past decade
      # set the hoverinfo to the formatted text argument, which will display the country name, freedom_foreign_movement, 
      # and proportion of population that are international students in the U.S. 
      # set type to scatter to create a scatterplot 
      # adjust width and height for aesthetic purposes 
      
      plot_ly(
        data = prop_movement, x = ~freedom_foreign_movement,
        y = ~prop_students,
        frame = ~year,
        color = I("#CF4E3E"),
        size = ~population,
        hoverinfo = "text",
        text = ~ paste(
          "<b>Country:</b>", country_name.x,
          "</br>", "<b>Freedom of Foreign Movement:</b>", freedom_foreign_movement, "<br>",
          "<b>Proportion:</b>", prop_students, "</br>"
        ),
        type = "scatter",
        width = 900,
        height = 600
      ) %>%
        
        # make animation elastic so that it "bounces" between the frames and isn't continuous
        
        animation_opts(
          1000,
          easing = "elastic", redraw = FALSE
        ) %>%
        
        # add titles and axes labels
        # format margins so things don't get cut off 
        # don't display legend 
        
        layout(
          title = "Proportion of Population that are International Students in the U.S.",
          xaxis = list(
            title = "Freedom of Foreign Movement",
            zeroline = F
          ),
          yaxis = list(
            title = "Proportion of Country Population",
            zeroline = F
          ),
          margin = list(l = 150, r = -20, b = 20, t = -10),
          showlegend = F
        ) %>%
        
        # remove ModeBar
        
        config(displayModeBar = F) %>%
        
        # add regression line to scatterplot using the fit model created above
        
        add_markers(y = ~prop_students) %>%
        add_trace(x = ~freedom_foreign_movement, y = ~fit, mode = "lines")
    }
  })

  # create graph that displays the primary sources of funding for international students in the U.S.
  
  output$funding <- renderPlotly({
    
    # create funding ggplot using funding data 
    
    funding_ggplot <- funding %>%
      
      # filter out total students, international funding sources, U.S. funding sources since they are either 
      # irrelevant to the graph or double counted 
      
      filter(funding_source != "Total Students" &
        funding_source != "International Funding Sources" &
        funding_source != "U.S. Funding Sources") %>%
      
      # reorder the levels of the funding_source variable so that other sources is the last one 
      
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
      
      # create a ggplot with the x axis being funding_source and the y axis being number of students for which a specific
      # funding source is their primary source of funding 
      # format hover text 
      
      ggplot(aes(
        x = funding_source, y = num_students, fill = funding_source,
        text = paste("<b>Source:</b>", funding_source, "<br>", "<b># Students:</b>", num_students, "</br>")
      )) +
      
      # create a bar graph 
      
      geom_col() +
      
      # format x axis label, tick marks, and tick labels for readability purposes 
      
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, size = 8),
        axis.ticks.x = element_blank()
      ) +
      
      # add labels 
      
      labs(x = "Funding Source", fill = "Funding Source", y = "Number of International Students in the U.S.") +
      
      # format y axis tick marks in comma style
      
      scale_y_continuous(label = comma) +
      
      # set color of bars in the plot to BrBG color balette from brewer package 
      
      scale_fill_brewer(palette = "BrBG", direction = -1)

    # wrap ggplot in ggplotly 
    # set hover text to text argument in ggplot 
    
    ggplotly(funding_ggplot, tooltip = "text", width = 900, height = 700) %>%
      
      # adjust margins of plot 
      # add title and subtitle 
      layout(
        margin = list(l = 50, r = -20, b = 20, t = -5),
        title = list(text = paste0(
          "Primary Sources of Funding for International Students in the U.S.",
          "<br>",
          "<sup>",
          "How many students primarily used a particular source of funding to fund their U.S. education in 2018?",
          "</sup>"
        ))
      ) %>%
      
      # remove modeBar 
      
      config(displayModeBar = F)
  })

  # create field plot that displays distribution of fields of study among international students
  
  output$field <- renderPlotly({
    
    # create a field of study ggplot using country_indicators data
    
    field_ggplot <- country_indicators %>%
      
      # select only the columns of interest: country name, year, number of students, fields of study 
      
      select(
        country_name.x, year, total_students, business_mgmt, education, engineering,
        fine_applied_arts, health_professions,
        humanities, intensive_english, math_computer_science,
        physical_life_sciences, social_sciences,
        other_fields_of_study, undeclared
      ) %>%
      
      # omit any missing values 
      
      na.omit() %>%
      
      # filter for only the most recent year 2018 
      
      filter(year == 2018) %>%
      
      # filter out Mexico and Central America observations since they are double_counted in Central America
      
      filter(country_name.x != "Mexico and Central America") %>%
      
      # omit year column since it is no longer relevant 
      
      select(-year) %>%
      
      # lengthen data by making all field of study columns into levels within the one new column named "field"
      
      pivot_longer(cols = 3:14, names_to = "field") %>%
      
      # rename and reformat all levels within the field column into standard written text 
      
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
        undeclared = "Undeclared"
      )) %>%
      
      # relevel the field variable so that undeclared and other observations are at the end 
      
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
      
      # create a ggplot 
      # set x axis to field 
      # set y axis to country name 
      # format hover text 
      
      ggplot(aes(
        x = field, y = fct_rev(country_name.x),
        text = paste(
          "<b>Country:</b>", country_name.x, "<br>",
          "<b>Field:</b>", field, "<br>",
          "<b>Percent:</b>", value, "%",
          "</br>"
        )
      )) +
      
      # create a geom_tile "heatmap" where the cell fill is value (i.e., the percent of a specific country's population
      # international students in the U.S. that chose a certain field of study)
      
      geom_tile(aes(fill = value), color = "white") +
      
      # set the gradient colors of the heatmap
      
      scale_fill_gradient(low = "white", high = I("#4B8A7E")) +
      
      # add titles and labels 
      
      labs(y = "Country of Origin", x = "Field of Study", fill = "Percent")

    ggplotly(field_ggplot, width = 700, height = 700, tooltip = "text") %>%
      layout(
        xaxis = list(side = "bottom", tickangle = -45),
        margin = list(l = 50, r = -20, b = 0, t = -5),
        
        # add a title and subtitle 
        
        title = list(text = paste0(
          "International Students' Fields of Study in the U.S. by Country",
          "<br>",
          "<sup>",
          "What percent of international students from a country chose a particular field of study in 2018?",
          "</sup>"
        ))
      ) %>%
      
      # remove modeBar
      config(displayModeBar = FALSE)
  })
}

# run the application 

shinyApp(ui = ui, server = server)
