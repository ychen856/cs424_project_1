#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(class="p-0 m-0",
  includeCSS("www/dashboard.css"),
  tags$head(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")  
  ),
  tags$nav(class="head buttom-shadow p-0 m-0 pl-0",
           tags$ul(class="title p-0 mr-auto", 
                   tags$li(
                     tags$h1("CS424 Project 1")
                   )),
           tags$ul(class="name p-0", 
                   tags$li(class="text-lg mt-auto mb-auto", "Yi-Chun Chen"))
           ),
    tags$div( class="nav-card",
    #tags$i(class="fab fa-accessible-icon card"),
    navbarPage("wtf", 
        tabPanel("Plot",
                 mainPanel(
                   tags$div("FFFFFFF", style="heigh: 200px; width: 200px; background-color: pink"
                      
                            
                            )
                 )), 
        tabPanel("Summary"),
        tabPanel("Table")
    )
  ),
  
  # Application title
  title = "Diamonds Explorer",
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 26)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(3, h4("ffffff")
        ),
        column(4, h4("KKKKKK"))
      )
    )
  )
)

