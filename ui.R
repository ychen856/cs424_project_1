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
  tags$nav(class="head shadow p-0 m-0 pl-0",
           tags$ul(class="title p-0 mr-auto", 
                   tags$li(
                     tags$h1("CS424 Project 1")
                   )),
           tags$ul(class="name p-0", 
                   tags$li(class="text-lg mt-auto mb-auto", "Yi-Chun Chen"))
           ),
    tags$div( class="nav-card p-0",
    #tags$i(class="fab fa-accessible-icon card"),
    navbarPage("", 
        tabPanel("Plot", class="p-0", style="background-color: grey",
                 mainPanel( class="panel p-0",
                   fluidRow(
                     column(8, class="p-0",
                        tags$div(class="card border-title shadow",
                            tags$div(class="card-body",
                                     
                                     tags$div(class="title",
                                              tags$span(
                                              "Total Amount of Energy Generation")
                                        ),
                            
                            fluidRow(style="margin: 2px",
                              column(3, style="background-color: white",
                                     tags$div(
                                       tags$div(class="subtitle",
                                         tags$i(class="fas fa-search"),
                                         "Data Filter:"
                                       ),
                                       tags$div(class="filter",
                                         checkboxGroupInput("variable", "Energy source: ",
                                                                   c("Cylinders" = "cyl",
                                                                     "Transmission" = "am",
                                                                     "Gears" = "gear"))
                                        ),
                                       tags$div(class="filter",
                                                selectInput("variable2", "Start Year:",
                                                            c("Cylinders" = "cyl",
                                                              "Transmission" = "am",
                                                              "Gears" = "gear"))
                                       ),
                                       tags$div(class="filter",
                                                selectInput("variable3", "End Year:",
                                                            c("Cylinders" = "cyl",
                                                              "Transmission" = "am",
                                                              "Gears" = "gear"))
                                       ),
                                       tags$div(class="filter",
                                                selectInput("variable4", "First State: ",
                                                                   c("Cylinders" = "cyl",
                                                                     "Transmission" = "am",
                                                                     "Gears" = "gear",
                                                                     "Alabama"="AL",	
                                                                     "Alaska"="AK",	
                                                                     "Arizona"="AZ",	
                                                                     "Arkansas"="AR",	
                                                                     "California"="CA",	
                                                                     "Colorado"="CO",	
                                                                     "Connecticut"="CT",	
                                                                     "Delaware"="DE",	
                                                                     "Florida"="FL",	
                                                                     "Georgia"="GA",	
                                                                     "Hawaii"="HI",	
                                                                     "Idaho"="ID",	
                                                                     "Illinois"="IL",	
                                                                     "Indiana"="IN",	
                                                                     "Iowa"="IA",	
                                                                     "Kansas"="KS",	
                                                                     "Kentucky"="KY",	
                                                                     "Louisiana"="LA",	
                                                                     "Maine"="ME",	
                                                                     "Maryland"="MD",	
                                                                     "Massachusetts"="MA",	
                                                                     "Michigan"="MI",	
                                                                     "Minnesota"="MN",	
                                                                     "Mississippi"="MS",	
                                                                     "Missouri"="MO",	
                                                                     "Montana"="MT",	
                                                                     "Nebraska"="NE",	
                                                                     "Nevada"="NV",	
                                                                     "New Hampshire"="NH",	
                                                                     "New Jersey"="NJ",	
                                                                     "New Mexico"="NM",	
                                                                     "New York"="NY",	
                                                                     "North Carolina"="NC",	
                                                                     "North Dakota"="ND",	
                                                                     "Ohio"="OH",	
                                                                     "Oklahoma"="OK",	
                                                                     "Oregon"="OR",	
                                                                     "Pennsylvania"="PA",	
                                                                     "Rhode Island"="RI",	
                                                                     "South Carolina"="SC",	
                                                                     "South Dakota"="SD",	
                                                                     "Tennessee"="TN",	
                                                                     "Texas"="TX",	
                                                                     "Utah"="UT",	
                                                                     "Vermont"="VT",	
                                                                     "Virginia"="VA",	
                                                                     "Washington"="WA",	
                                                                     "West Virginia"="WV",	
                                                                     "Wisconsin"="WI",	
                                                                     "Wyoming"="WY"
                                                                     ))
                                       ),
                                       tags$div(class="filter",
                                                selectInput("variable5", "Second State: ",
                                                            c("Cylinders" = "cyl",
                                                              "Transmission" = "am",
                                                              "Gears" = "gear",
                                                              "Alabama"="AL",	
                                                              "Alaska"="AK",	
                                                              "Arizona"="AZ",	
                                                              "Arkansas"="AR",	
                                                              "California"="CA",	
                                                              "Colorado"="CO",	
                                                              "Connecticut"="CT",	
                                                              "Delaware"="DE",	
                                                              "Florida"="FL",	
                                                              "Georgia"="GA",	
                                                              "Hawaii"="HI",	
                                                              "Idaho"="ID",	
                                                              "Illinois"="IL",	
                                                              "Indiana"="IN",	
                                                              "Iowa"="IA",	
                                                              "Kansas"="KS",	
                                                              "Kentucky"="KY",	
                                                              "Louisiana"="LA",	
                                                              "Maine"="ME",	
                                                              "Maryland"="MD",	
                                                              "Massachusetts"="MA",	
                                                              "Michigan"="MI",	
                                                              "Minnesota"="MN",	
                                                              "Mississippi"="MS",	
                                                              "Missouri"="MO",	
                                                              "Montana"="MT",	
                                                              "Nebraska"="NE",	
                                                              "Nevada"="NV",	
                                                              "New Hampshire"="NH",	
                                                              "New Jersey"="NJ",	
                                                              "New Mexico"="NM",	
                                                              "New York"="NY",	
                                                              "North Carolina"="NC",	
                                                              "North Dakota"="ND",	
                                                              "Ohio"="OH",	
                                                              "Oklahoma"="OK",	
                                                              "Oregon"="OR",	
                                                              "Pennsylvania"="PA",	
                                                              "Rhode Island"="RI",	
                                                              "South Carolina"="SC",	
                                                              "South Dakota"="SD",	
                                                              "Tennessee"="TN",	
                                                              "Texas"="TX",	
                                                              "Utah"="UT",	
                                                              "Vermont"="VT",	
                                                              "Virginia"="VA",	
                                                              "Washington"="WA",	
                                                              "West Virginia"="WV",	
                                                              "Wisconsin"="WI",	
                                                              "Wyoming"="WY"
                                                            ))
                                       )
                                     )
                              ),
                              column(9, style="background-color: pink",
                                     tableOutput("data"))
                            )
                            )
                        )
                     ),
                     column(4, class="p-0", style="background-color: blue",
                            tags$div(class="card",
                                     "zzzzz"
                        )
            
                     )
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

