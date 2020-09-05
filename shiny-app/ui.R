library(shiny)
library(shinydashboard)
library(shinycustomloader)
library(aWhereAPI)
library(DT)
library(gdata)
library(stringr)
library(lubridate)
library(DBI)
library(RMySQL)
library(DT)
library(maps)
library(ggmap)
library(ggplot2)
library(rgdal)
library(shinyTree)  
library(dismo)
library(rJava)
library(stringr)
library(leaflet)
library(rsconnect)
library(shiny.i18n)
library(leaflet.extras)
library(data.table)
library(maptools)
library(tidyr)
library(dplyr)
library(svglite)
library(slickR)
library(openair)

#####

ui <- dashboardPage(skin = "green",
                    
                    #CABECERA DE PAGINA
                    dashboardHeader(
                      title = "LATE BLIGHT",
                      
                      dropdownMenu(type = "messages",
                                   messageItem(
                                     from = "Sales Dept",
                                     message = "Sales are steady this month."
                                   ),
                                   
                                   messageItem(
                                     from = "New User",
                                     message = "How do I register?",
                                     icon = icon("question"),
                                     time = "13:45"
                                   ),
                                   
                                   messageItem(
                                     from = "Support",
                                     message = "The new server is ready.",
                                     icon = icon("life-ring"),
                                     time = "2014-12-01"
                                   )
                                   
                      )
                      
                    ),
                    
                    #BARRA LATERAL
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
                        
                        menuItem("Language", tabName = "language", icon = icon("globe"),
                                 radioButtons("selected_language", "Select language:",
                                              c("English" = "en",
                                                "Spanish" = "sp"
                                              ))),
                        
                        menuItem("About", tabName = "about", icon = icon("th"),badgeLabel = "new", badgeColor = "green")
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "dashboard",
                          fluidRow(
                            tabBox(
                              title = "POTATO LATE BLIGHT FORECASTING",
                              id = "tabset1", 
                              height = "1000px", 
                              width = 12,
                              
                              #INGRESAR LOS ARCHIVOS DE ENTRADA
                              tabPanel(
                                title = "INPUT DATA",
                                icon  =  icon ( "file-import" ),
                                fluidRow(
                                  tabBox(
                                    width = 12,
                                    tabPanel(title = "Data by aWhere",
                                             column(
                                               12,
                                               h3("Crop information"),
                                               br(),
                                               column(
                                                 5,
                                                 dateInput("date0", "Emergency day:", Sys.Date()-7)
                                               ),
                                               column(
                                                 5,
                                                 dateInput("daten", "Harvest day:", Sys.Date()-1, max = Sys.Date()+14)
                                               ),
                                               column(
                                                 2
                                               )
                                               
                                             ),
                                             
                                             column(
                                               12,
                                               column(
                                                 5,
                                                 selectInput("res", "Level of resistance to variety:", 
                                                             c("Susceptible" = "s","Moderately susceptible" = "ms","Moderately resistant" = "mr","Resistant" = "r", "Highly resistant" = "hr"))
                                               ),
                                               column(
                                                 5,
                                                 selectInput("lim", "Leaf wetness threshold:", 
                                                             c("90% Relative humidity" = 90,"85% Relative humidity" = 85))
                                               ),
                                               column(
                                                 2
                                               )
                                             ),
                                             
                                             column(
                                               12,
                                               h3("Area of interest"),
                                               br(),
                                               fluidRow(
                                                 tabBox(
                                                   height = "750px",
                                                   tabPanel(title = "By map",
                                                            h4("Select the location(s) of interest on the map"),
                                                            br(),
                                                            DT::dataTableOutput("inputMarkers")
                                                   ),
                                                   
                                                   tabPanel(title = "By file",
                                                            h4("Select a file (.csv) located on your computer"),
                                                            p(class = "text-muted", style="text-align:justify",
                                                              paste("Sample input file: "),
                                                              #tags$a(href='modelAPP/points.csv', target='blank', 'points.csv', download = 'points.csv')
                                                            ),
                                                            
                                                            uiOutput('resetfileInput'),
                                                            DT::dataTableOutput("fileInputPoints")
                                                   ),
                                                   
                                                   tabPanel(title = "By box"
                                                            
                                                            
                                                   )
                                                   
                                                 ),
                                                 
                                                 box(
                                                   #h4("MAP" ,align = "center"), height = "750px",
                                                   leafletOutput("mymap1a", "100%", "550px"),
                                                   #br(),
                                                   actionButton("clearMap1a", "Clear", class="btn-warning", width = "120px" )
                                                 )
                                                 
                                               )
                                               
                                             )
                                             
                                    ),
                                    
                                    
                                    
                                    tabPanel(
                                      
                                      width = 12,
                                      title = "Data by Weather Station",
                                      
                                      column(
                                        12,
                                        h3("Crop information"),
                                        br(),
                                        column(
                                          5,
                                          dateInput("date02", "Emergency day:", Sys.Date()-7)
                                        ),
                                        column(
                                          5,
                                          dateInput("daten2", "Harvest day:", Sys.Date()-1, max = Sys.Date()+14)
                                        ),
                                        column(
                                          2
                                        )
                                        
                                      ),
                                      
                                      column(
                                        12,
                                        column(
                                          5,
                                          selectInput("res2", "Level of resistance to variety:", 
                                                      c("Susceptible" = "s","Moderately susceptible" = "ms","Moderately resistant" = "mr","Resistant" = "r", "Highly resistant" = "hr"))
                                        ),
                                        column(
                                          5,
                                          selectInput("lim2", "Leaf wetness threshold:", 
                                                      c("90% Relative humidity" = 90,"85% Relative humidity" = 85))
                                        ),
                                        column(
                                          2
                                        )
                                        
                                      ),
                                      
                                      column(
                                        12,
                                        h3("Area of interest"),
                                        br(),
                                        column(
                                          5,
                                          textInput("lat2", "Latitude:", "-12")
                                          
                                        ),
                                        column(
                                          5,
                                          textInput("long2", "Longitude:", "-76")
                                          
                                        ),
                                        column(
                                          2
                                        )
                                        
                                      ),
                                      
                                      column(
                                        12,
                                        h3("Weather Data"),
                                        column(
                                          4,
                                          
                                          fileInput("fileWD", " ",
                                                    multiple = FALSE,
                                                    accept = c("text/csv",
                                                               "text/comma-separated-values,text/plain",
                                                               ".csv"))),
                                        column(
                                          2,
                                          br(),
                                          actionButton("view1", "  View table ", icon = icon ( "clipboard-check" ))),
                                        
                                        column(
                                          3,
                                          br(),
                                          actionButton("run2", "  Run model by personal data ", icon = icon ( "play" ))
                                        ),
                                        
                                        column(
                                          2,
                                          br(),
                                          downloadButton("downloadData2", " Download data")
                                        ),
                                        
                                        column(
                                          1
                                        )
                                        
                                      ),
                                      
                                      column(
                                        12,
                                        
                                        column(
                                          4,
                                          style = "overflow-x: scroll",
                                          
                                          br(),
                                          withLoader(
                                            #br(),
                                            DT::dataTableOutput("table3", width = "100%", height = "auto"), 
                                            type = "html", loader = "loader4")
                                          
                                        ),
                                        
                                        column(
                                          1,
                                        ),
                                        
                                        column(
                                          
                                          6,
                                          style = "overflow-x: scroll" ,
                                          withLoader(
                                            #br(),
                                            DT::dataTableOutput("tableWD", width = "100%", height = "auto"), 
                                            type = "html", loader = "loader4")
                                          
                                        )
                                        
                                        
                                        
                                      )
                                      
                                      
                                      
                                      
                                      
                                    )
                                    
                                  )
                                  
                                )
                                
                                
                                
                                
                                
                              ),
                              
                              #PANEL PARA VISUALIZAR Y DESCARGAR LOS DATOS     
                              tabPanel("RESULTS",
                                       icon  =  icon ( "poll" ),
                                       style = "overflow-x: scroll" ,
                                       actionButton("run1", " Run model by aWhere", icon = icon ( "play" )),
                                       
                                       br(),
                                       br(),
                                       withLoader(
                                         #br(),
                                         slickROutput("calendars", width="80%"), 
                                         type = "html", loader = "loader4"),
                                       
                                       downloadButton("downloadData", " Download data")
                              )
                              
                            )
                            
                            
                          ) 
                          
                          
                        ),
                        
                        
                        tabItem(tabName = "about",
                                h3("Forecasting Model for Potato Late Blight Management"),
                                h4("International Potato Center")
                                
                        )
                        
                        
                        
                        
                      )
                      
                      
                      
                    ) 
                    
)
