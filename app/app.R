library(tidyverse)
library(ggplot2)
library(formattable)
library(ggalluvial)
library(shiny)
library(shinythemes)
library(coefplot)

ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage("International Students in the United States",
                           
                           tabPanel("About the Project",
                                    
                                    p("This project looks to analyze how individuals employed by Harvard University donate their political dollars. I visualize in multiple different ways the size of the donations, the support different candidates recieve, and how political ideology impacts donations. I also look at how these donations differ within the graduate schools and how all of the donations compare to the general population's donations."),
                                    p("I accessed data on the political donations of individuals employed by Harvard University on the website of the Federal Election Committee, which releases information on every donation made to a presidential, senate, or house political campaign, including the name of the donor, their occupation, their employer, the donated amount, their state, and the recipient campaign."),
                                    p("Although this data was easily accessible by downloading a csv file, there was much data cleaning to be done to ensure that my analyses are accurate. First, I only observed donations made to 2020 presidential campaigns, excluding senate and house donations. Second, I filtered for donations made after January 1st, 2017, as there was no presidential donations to 2020 campaigns in 2016 or earlier. Third, I had to parse through the contributor employer data and filter out donations whose donors were not actually employed by Harvard University. For example, the donations from the employees of Harvard-Westlake School were present in the data, as they contain the word “Harvard.” Donations like this were removed."),
                                    p("I also used the Federal Election Committee website to find data on the campaign donations of the general population, the total campaign sizes, and the political party affiliations of the different committees."),
                                    p("Additionally, I sourced data from a DataForProgress.org article, which offered ideology scores for 2020 candidates who served in Congress. The specific data was not publicly availible, and I had to reach out to the article author to access it.")
                           ),
                           
                           tabPanel("About the Author",
                                    h1("Amanda Y. Su"),
                                    p("I'm a sophomore at Harvard College studying History & Literature. I'm originally from the Bay Area. I'm interested in issues of race, ethnicity, immigration and education. I'm planning to pursue career in journalism.")
                           ),
                           
                           tabPanel("Geographical Regions of Origin",
                                    imageOutput("region")
                           ),
                           
                           tabPanel("Indicators of Countries of Origin", 
                                    p("Select from the following plots to see how employees of Harvard's graduate schools donate to 2020 presidential campaigns."),
                                    selectInput("indicator", label = h5("Select Plot"), 
                                                choices = list("Gross Domestic Product" = "gdp",
                                                               "Freedom of Expression" = "free_expression",
                                                               "Educational Equality" = "edu_equality"), 
                                                selected = "gdp",
                                                multiple = FALSE,
                                                selectize = FALSE,
                                                width = '400px',
                                                size = 1),
                                    imageOutput("country_indicator")
                                    
                           ),
                           
                           tabPanel("Sources of Funding",
                                    
                                    imageOutput("funding")
                           )))


server <- function(input, output) {
    
    output$country_indicator <- renderImage({
        
        if(input$indicator == "gdp") {     
            list(src = "gdp_map.gif",
                 contentType = "image/gif")
        } 
        
        else if(input$indicator == "free_expression") {
            list(src = "free_expression_map.png",
                 contentType = "image/png")
        }
        
        else if(input$indicator == "edu_equality"){
            list(src = "edu_equality_map.png",
                 contentType = "image/png")
        }
    }
    )
    
    output$region <- renderImage({
        list(src = "region_graph.png",
             contentType = "image/png")
    })
    
    output$funding <- renderImage({
        list(src = "funding_map.png",
             contentType = "image/png")
    }
    )
}


shinyApp(ui = ui, server = server)
