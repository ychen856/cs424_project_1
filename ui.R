#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(class="p-0 m-0",
  includeCSS("www/dashboard.css"),
  tags$head(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);")  
  ),
  # Application title
  title = "CS424 Project 1",
  tags$nav(class="head shadow p-0 m-0 pl-0",
           tags$ul(class="title p-0 mr-auto mt-0 mb-0", 
                   tags$li(
                     tags$p("CS424 Project 1")
                   )),
           tags$ul(class="name p-0", 
                   tags$li(class="text-lg mt-auto mb-auto", "Yi-Chun Chen"))
           ),
    tags$div( class="p-0",
    #tags$i(class="fab fa-accessible-icon card"),
    navbarPage("", 
        #Total Amount page start
        tabPanel("Total Amount", class="p-0",
                 mainPanel( class="panel p-0",
                   fluidRow(
                     
                     #Total Amount of Energy generation start
                     column(9, class="p-0",
                        tags$div(class="card border-title shadow",
                            #card Start
                            tags$div(class="card-body",
                                     
                                     tags$div(class="title",
                                              tags$span(
                                              "Total Amount of Energy Generation")
                                        ),
                            
                            fluidRow(style="margin: 2px",
                              column(2, style="background-color: white",
                                     tags$div(
                                       tags$div(class="subtitle",
                                         tags$i(class="fas fa-search"),
                                         "Data Filter:"
                                       ),
                                       
                                       
                                       #Energy source filter start
                                       tags$div(class="filter",
                                         checkboxGroupInput("energySourceInput", "Energy source: ", choices = c("Select All", energySource_dist), selected="Select All")
                                        ) #energy source filter end
                                       
                                       
                                       #date input start
                                       #tags$div(class="filter",
                                      #          tags$table(class="select-year",
                                      #              tags$tr(
                                      #                  tags$td(class="start-year",  numericInput(inputId="startYear", label = "Year: ", value = 1990, min = 1990, max = 2020, step = NA)),
                                      #                  tags$td(class="text", "to"),
                                      #                  tags$td(class="end-year", numericInput(inputId="endYear", label = "", value = 2020, min = 1990, max = 2020, step = NA))
                                      #              )
                                      #          )
                                      # ), #date input end
                                      # tags$div(class="filter",
                                      #          selectizeInput(
                                      #            'request', 'States: ', choices = c("All States", state.name), selected="All States", multiple = TRUE
                                      #          )
                                      # )
                                     )
                              ), 
                              
                              column(10, 
                                  tags$div(class="row",
                                    column(12, 
                                           tags$div(class="subtitle",
                                                    tags$i(class="fas fa-chart-line"),
                                                    "Line Chart:"
                                           ),
                                           tags$div(style="height: 390px;  background-color: pink", plotOutput("lineChart", height = 390))
                                           
                                    )
                                    #column(6,
                                    #       tags$div(class="subtitle",
                                    #                tags$i(class="fas fa-chart-bar"),
                                    #                "Stack Chart:"
                                    #       ),
                                    #       tags$div(style="height: 400px; background-color: pink", plotOutput("stackChart", height = 400))
                                    #)
                                  ),
                                  tags$div(class="row",
                                           column(12,
                                                  tags$div(class="subtitle",
                                                           tags$i(class="fas fa-chart-bar"),
                                                           "Stack Chart:"
                                                  ),
                                                  tags$div(style="height: 390px; background-color: pink", plotOutput("stackChart", height = 390))
                                           )
                                  )
                                  #tags$div(class="row pt-5",
                                  #         column(12, 
                                  #                tags$div(class="subtitle",
                                  #                         tags$i(class="fas fa-flag-usa"),
                                  #                         "Heat Map:"
                                  #                ),
                                  #                tags$div(style="height: 300px; background-color: pink", plotOutput("usMap", height = 350))
                                  #         )
                                  #)
                              )
                              
                            )#End of fluid row
                            )#End of card
                        )
                     ),#Total Amount of Energy Generation end
                     
                    #Energy Generation Detail start
                    column(3, class="p-0",
                        tags$div(class="card border-title shadow",
                            tags$div(class="card-body",
                                tags$div(class="title",
                                    tags$span(
                                        "Energy Generation Detail")
                                ),
                                tags$div(style="height:850px; padding: 15px",
                                    DT::dataTableOutput("myTable")
                                )
                            )
                        )
                    ) #Energy Generation Detail end

                )
            )
        ), #Total Amount page end
        
        
        #Percentage page start
        tabPanel("Percentage", class="p-0",
                 mainPanel( class="panel p-0",
                            fluidRow(
                              
                              #Total Amount of Energy generation start
                              column(9, class="p-0",
                                     tags$div(class="card border-title shadow",
                                              #card Start
                                              tags$div(class="card-body",
                                                       
                                                       tags$div(class="title",
                                                                tags$span(
                                                                  "Energy Generation in Percentage")
                                                       ),
                                                       
                                                       fluidRow(style="margin: 2px",
                                                                column(2, style="background-color: white",
                                                                       tags$div(
                                                                         tags$div(class="subtitle",
                                                                                  tags$i(class="fas fa-search"),
                                                                                  "Data Filter:"
                                                                         ),
                                                                         
                                                                         
                                                                         #Energy source filter start
                                                                         tags$div(class="filter",
                                                                                  checkboxGroupInput("energySourceInput_per", "Energy source: ", choices = c("Select All", energySource_dist), selected="Select All")
                                                                         ) #energy source filter end
                                                                    )
                                                                ), 
                                                                
                                                                column(10, 
                                                                       tags$div(class="row",
                                                                                column(12, 
                                                                                       tags$div(class="subtitle",
                                                                                                tags$i(class="fas fa-chart-line"),
                                                                                                "Line Chart:"
                                                                                       ),
                                                                                       tags$div(style="height: 390px;  background-color: pink", plotOutput("lineChart_per", height = 390))
                                                                                       
                                                                                )
                                                                                #column(6,
                                                                                #       tags$div(class="subtitle",
                                                                                #                tags$i(class="fas fa-chart-bar"),
                                                                                #                "Stack Chart:"
                                                                                #       ),
                                                                                #       tags$div(style="height: 400px; background-color: pink", plotOutput("stackChart", height = 400))
                                                                                #)
                                                                       ),
                                                                       tags$div(class="row",
                                                                                column(12,
                                                                                       tags$div(class="subtitle",
                                                                                                tags$i(class="fas fa-chart-bar"),
                                                                                                "Stack Chart:"
                                                                                       ),
                                                                                       tags$div(style="height: 390px; background-color: pink", plotOutput("stackChart_per", height = 390))
                                                                                )
                                                                       )
                                                                       #tags$div(class="row pt-5",
                                                                       #         column(12, 
                                                                       #                tags$div(class="subtitle",
                                                                       #                         tags$i(class="fas fa-flag-usa"),
                                                                       #                         "Heat Map:"
                                                                       #                ),
                                                                       #                tags$div(style="height: 300px; background-color: pink", plotOutput("usMap", height = 350))
                                                                       #         )
                                                                       #)
                                                                )
                                                                
                                                       )#End of fluid row
                                              )#End of card
                                     )
                              ),#Total Amount of Energy Generation end
                              
                              #Energy Generation Detail start
                              column(3, class="p-0",
                                     tags$div(class="card border-title shadow",
                                              tags$div(class="card-body",
                                                       tags$div(class="title",
                                                                tags$span(
                                                                  "Energy Generation Detail (%)")
                                                       ),
                                                       tags$div(style="height:850px; padding: 15px",
                                                                DT::dataTableOutput("myTable_per")
                                                       )
                                              )
                                     )
                              ) #Energy Generation Detail end
                              
                            )
                 )
        
                 
                 
                 
        ), #Percentage page end
        
        #Camparison page start
        tabPanel("Comparison", class="p-0",
                 mainPanel( class="panel p-0",
                            #Filter Start
                            fluidRow(
                              column(6),
                              column(6, class = "p-0",
                                      tags$div(
                                          column(3, class = "p-5",
                                              tags$div(
                                                  tags$div(class="filter, cust-text",
                                                      selectizeInput(
                                                          'energySourceInputCom', 'Select a energy source: ', choices = c("All", energySource_dist), selected="All", multiple = FALSE
                                                      )
                                                   ) 
                                              )
                                          ),
                                          
                                          column(9, class = "p-5",   
                                              #first state input start
                                              column(4, class = "p-5",       
                                                  tags$div(
                                                      tags$div(class="filter, cust-text",
                                                          selectizeInput(
                                                              'fisrtStateInput', 'Select the first states: ', choices = c("All States", state.name, "Washington DC"), selected="All States", multiple = FALSE
                                                          )
                                                      ) 
                                                  )       
                                                     
                                              ),#first state input end
                                              
                                              #first year input start
                                              column(2, class = "p-5 pl-0",
                                                  tags$div(
                                                      tags$div(class="filter, cust-text",
                                                          tags$table(class="select-year",
                                                              tags$tr(
                                                                  tags$td(class="start-year",  numericInput(inputId="firstYearInput", label = "Year: ", value = 1990, min = 1990, max = 2019, step = NA)),
                                                                  #tags$td(class="text", "to"),
                                                                  #tags$td(class="end-year", numericInput(inputId="endYear", label = "", value = 2019, min = 1990, max = 2019, step = NA))
                                                              )
                                                          )         
                                                      ) 
                                                  )
                                              ), #first year input end
                                              #Second State input start
                                              column(4, class = "p-5",
                                                     tags$div(
                                                       tags$div(class="filter, cust-text",
                                                                selectizeInput(
                                                                  'secondStateInput', 'Select the second states: ', choices = c("All States", state.name, "Washington DC"), selected="All States", multiple = FALSE
                                                                )
                                                       ) 
                                                     )
                                              ),
                                              #Second state input end
                                              #Second year input start
                                              column(2, class = "p-5 pl-0",
                                                  tags$div(
                                                      tags$div(class="filter, cust-text",
                                                          tags$table(class="select-year",
                                                              tags$tr(
                                                                  tags$td(class="start-year",  numericInput(inputId="secondYearInput", label = "Year: ", value = 1990, min = 1990, max = 2019, step = NA)),
                                                                  #tags$td(class="text", "to"),
                                                                  #tags$td(class="end-year", numericInput(inputId="endYear", label = "", value = 2019, min = 1990, max = 2019, step = NA))
                                                              )
                                                          )
                                                      ) 
                                                  )
                                              ) #Second year input end
                                          ),#date input end
                                      )
                                )
                            ), #Filter End
                            
                            #State 1 Row start
                            fluidRow(
                              #Total Amount of Energy generation start
                              column(12, class="p-0",
                                     tags$div(class="card border-title shadow",
                                              #card Start
                                              tags$div(class="card-body",
                                                       
                                                       tags$div(class="title",
                                                                tags$span(
                                                                  "First State")
                                                       ),
                                                       
                                                       fluidRow(style="margin: 2px",
                                                                #Line Chart start
                                                                column(4, style="background-color: white",
                                                                    tags$div(
                                                                          tags$div(class="subtitle",
                                                                                  tags$i(class="fas fa-chart-line"),
                                                                                  "Line Chart:"
                                                                          ),
                                                                          tags$div(style="height: 300px;  background-color: pink", 
                                                                              plotOutput("firstStateLineChart", height = 300)
                                                                          )
                                                                      )
                                                                ), #Line Chart end
                                                                
                                                                #Stack Chart Start
                                                                column(4, 
                                                                    tags$div(
                                                                        tags$div(class="subtitle",
                                                                            tags$i(class="fas fa-chart-bar"),
                                                                                "Stack Chart:"
                                                                            ),
                                                                            tags$div(style="height: 300px;  background-color: pink", 
                                                                                plotOutput("firstStateStackChart", height = 300)
                                                                            )
                                                                        )
                                                                ), #Stack Chart End
                                                                
                                                                #Heap Map Start
                                                                column(4, 
                                                                    tags$div(
                                                                        tags$div(class="subtitle",
                                                                            tags$i(class="fas fa-flag-usa"),
                                                                                "Heat Map:"
                                                                            ),
                                                                            tags$div(style="height: 300px;  background-color: pink", 
                                                                                plotOutput("firstStateHeatMap", height = 300)
                                                                            )
                                                                      )
                                                                ) #Heat Map End
                                                                
                                                       )#End of fluid row
                                              )#End of card
                                     )
                              )#Total Amount of Energy Generation end
                          ), #State 1 Row end
                          #State 2 Row Start
                          fluidRow(
                            #Total Amount of Energy generation start
                            column(12, class="p-0",
                                   tags$div(class="card border-title shadow",
                                            #card Start
                                            tags$div(class="card-body",
                                                     tags$div(class="title",
                                                              tags$span(
                                                                "First State")
                                                     ),
                                                     
                                                     fluidRow(style="margin: 2px",
                                                              #Line Chart start
                                                              column(4, style="background-color: white",
                                                                     tags$div(
                                                                       tags$div(class="subtitle",
                                                                                tags$i(class="fas fa-chart-line"),
                                                                                "Line Chart:"
                                                                       ),
                                                                       tags$div(style="height: 300px;  background-color: pink", 
                                                                            plotOutput("secondStateLineChart", height = 300)
                                                                       ) 
                                                                  )
                                                              ), #Line Chart end
                                                              
                                                              #Stack Chart Start
                                                              column(4, 
                                                                  tags$div(
                                                                      tags$div(class="subtitle",
                                                                          tags$i(class="fas fa-chart-bar"),
                                                                              "Stack Chart:"
                                                                      ),
                                                                      tags$div(style="height: 300px;  background-color: pink", 
                                                                          plotOutput("secondStateStackChart", height = 300)
                                                                      )
                                                                  )
                                                              ), #Stack Chart End
                                                              
                                                              #Heap Map Start
                                                              column(4, 
                                                                  tags$div(
                                                                      tags$div(class="subtitle",
                                                                          tags$i(class="fas fa-flag-usa"),
                                                                              "Heat Map:"
                                                                      ),
                                                                      tags$div(style="height: 300px;  background-color: pink", 
                                                                          #plotOutput("lineChart_per", height = 390)
                                                                      )
                                                                  )
                                                              ) #Heat Map End
                                                              
                                                     )#End of fluid row    
                                                     
                                            )#End of card
                                   )
                            )#Total Amount of Energy Generation end
                          )#State 2 Row End
                 )
                 
        ), #Camparison page start
        
        #5 Interesting Things page start
        tabPanel("5 Interesting Things"), #5 Interesting Things page end
        
        #About page start
        tabPanel("About",
                 tableOutput("data2")
        ) #About page end
    )
  ),
  
  # Sidebar with a slider input for number of bins
  #sidebarLayout(
  #  sidebarPanel(
  #    sliderInput("bins",
  #              "Number of bins:",
  #                min = 1,
  #                max = 50,
  #                value = 26)
  #  ),
    
    # Show a plot of the generated distribution
  #  mainPanel(
  #    fluidRow(
  #      column(3, h4("ffffff")
  #      ),
  #      column(4, h4("KKKKKK"))
  #    )
  #  )
  #)
)

