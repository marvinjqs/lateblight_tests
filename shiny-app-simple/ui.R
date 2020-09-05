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
                        
                        menuItem("About", tabName = "about", icon = icon("th"), #,badgeLabel = "new", 
                                 badgeColor = "green")
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(
                          tabName = "dashboard",
                          fluidRow(
                            tabBox(
                              title = "POTATO LATE BLIGHT - DSS",
                              id = "tabset1", 
                              height = "1000px", 
                              width = 12,
                              
                              #INGRESAR LOS ARCHIVOS DE ENTRADA
                              tabPanel(
                                title = "INPUT DATA",
                                icon  =  icon ( "file-import" ),
                               
                                    
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
                                                 uiOutput("var_names1")
                                               ),
                                               
                                               column(
                                                 5
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
                                                 box(
                                                   height = "750px",
                                                   
                                                            h4("Select the location(s) of interest on the map"),
                                                            br(),
                                                            DT::dataTableOutput("inputMarkers")
                                                   
                                                   
                                                 
                                                 
                                                   
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
