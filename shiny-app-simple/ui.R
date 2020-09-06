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
                      
                      title = tags$a(href='https://marvinjonathcn.shinyapps.io/DSSA-LATEBLIGHT/',
                                     tags$img(src='logo.png',
                                              width = "200px",
                                              height = "50px"))
                      ),
                    
                    #BARRA LATERAL
                    dashboardSidebar(
                      
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("th")),
                        
                        menuItem("About", tabName = "about", icon = icon("info-circle")),
                        
                        # \u2007 es para un espacio en blanco
                        menuItem("\u2007Github", 
                                 icon = icon("github"), 
                                 href = "https://github.com/")
                      
                        )
                      ),
                    
                    
                    dashboardBody(
                      
                      # Obtener mediante Javascript el ancho y largo de sesion
                      tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
                      
                      
                      tabItems(
                        tabItem(
                          tabName = "dashboard",
                          fluidRow(
                            tabBox(
                              title = "POTATO LATE BLIGHT - DSS",
                              id = "tabset1",
                              width = 12,
                           
                              
                              #INGRESAR LOS ARCHIVOS DE ENTRADA
                              tabPanel(
                                title = "INPUT DATA",
                                icon  =  icon ( "file-import" ),
                               
                                    fluidRow(
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
                                             )),
                                            
                                fluidRow( 
                                             column(
                                               12,
                                               h3("Area of interest"),
                                               br(),
                                               fluidRow(
                                                 box(
                                                   width = 6,
                                                   
                                                            h4("Select the location(s) of interest on the map"),
                                                            br(),
                                                            DT::dataTableOutput("inputMarkers")
                                                   
                                                   
                                                 
                                                 
                                                   
                                                 ),
                                                 
                                                 box(
                                                   width = 6,
                                                   #h4("MAP" ,align = "center"), height = "750px",
                                                   leafletOutput("mymap1a"),
                                                   #br(),
                                                   actionButton("clearMap1a", "Clear", class="btn-warning", width = "120px" )
                                                 )
                                                 
                                               )
                                               
                                             ))
                                          
                                
                                
                                
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
                                         slickROutput("calendars", width="90%"),
                                         type = "html", loader = "loader4"),
                                       br(),
                                       br(),
                                       br(),
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
