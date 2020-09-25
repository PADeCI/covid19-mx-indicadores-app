# User Interface ----------------------------------------------------------



# Title left side ---------------------------------------------------------

title <- tags$a(href = "https://padeci.org",
                tags$img(src = "padeci.png", height = "80", width = "80")

)

# title <- tags$a("HOLAS", 
#                 tags$img(src = "padeci.png", height = "80", width = "80")
#                 
# )

#tags$a(href = "https://github.com/PADeCI", "GitHub"),
ui <- 
  fluidPage(theme = shinytheme("sandstone"),
            
            dashboardPage(
              title = "IndicadoresCovid",
              dashboardHeader(title = title,
                              
                              # Set height of dashboardHeader
                              tags$li(class = "dropdown",
                                      tags$style(".main-header {max-height: 80px}"),
                                      tags$style(".main-header .logo {height: 80px;}"),
                                      tags$style(".sidebar-toggle {height: 80px; padding-top: 1px !important;}"),
                                      tags$style(".navbar {min-height:80px !important}")
                              ) ),
              
              # dashboard SideBar -------------------------------------------------------
              
              dashboardSidebar(
                # Adjust the sidebar
                tags$style(".left-side, .main-sidebar {padding-top: 80px}"),
                sidebarMenu(
                  #SideBarMenu Items
                  
                  # First MenuItem
                  menuItem("Tablero", tabName = "dashboard", icon = icon("dashboard")),
                  
                  # Second MenuItem
                  menuItem("Gráficas", tabName = "graficas", icon = icon ("fas fa-chart-bar"),
                           menuSubItem("Indicadores en el tiempo",
                                       tabName = "graph1"),
                           menuSubItem("Acumulados/Incidentes",
                                       tabName = "graph11"),
                           menuSubItem("Tasa de positividad",
                                       tabName = "graph2"),
                           menuSubItem("Tasa de letalidad",
                                       tabName = "graph3"),
                           menuSubItem("Tiempos de duplicación",
                                       tabName = "graph4")
                           #menuSubItem("Mapas",
                                       #tabName = "graph5")
                  ),
                  
                  menuItem("Ficha técnica", tabName = "ficha", icon = icon("fas fa-book-open")),
                  
                  # Third MenuItem
                  menuItem("Acerca de", tabName = "about", icon = icon("fas fa-address-book")),
                  
                  # Fourth MenuItem 
                  menuItem("Github", icon =icon( "fab fa-github"), href = "https://github.com/PADeCI")
                  
                  #Fifth MenuItem
                  #menuItem("Table", tabName = "table")
                )
              ),
              
              # Dashboard Body ----------------------------------------------------------
              
              
              dashboardBody(
                #Logo part
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href= "custom.css"),
                  #tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css"),
                ),
                
                tags$head(tags$style(HTML(
                  '.myClass { 
                    font-size: 30px;
                    line-height: 80px;
                    text-align: center;
                    font-family: Source Sans Pro,Calibri,Candara,Arial,sans-serif;
                    font-weight: 500;
                    color: white;
                    padding: 0 15px;
                    overflow: hidden;
                    
      }
    '))),
                tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Monitor de indicadores COVID-19 </span>\');
      })
     ')),
                tabItems(
                  #Page gráficas
                  
                  # Graph 1 "Indicadores COVID-19 a través del tiempo" ----------------------
                  
                  tabItem(tabName = "graph1",
                          h2("Indicadores COVID-19 en el tiempo"),
                          hr(),
                          
                          p("La siguiente gráfica permite comparar uno o distintos indicadores COVID-19 para una
                           o más entidades federativas. En los menús desplegables que se encuentran debajo de la gráfica, 
                           podrás seleccionar la(s) entidad(es) y los indicadores que desees observar. 
                           Además, puedes seleccionar si deseas ver los datos acumulados o los datos incidentes. 
                          El tipo de escala permite apreciar, de mejor manera, las tendencias deseadas,"),
                          fluidRow(
                            
                            box(
                              class = "btn_trans",
                              collapsible = F, width = 12,
                              column( 12, align="center" , tableOutput('top5')),
                              plotOutput("plot1", height = 500) %>% withSpinner(color="#2980b9"),
                              
                              # downloadButton(outputId = "down", label = "Download the plot"),
                              dropdownButton(
                                tags$h4("Transformación"),
                                tags$b("Selecciona el tipo de transformación"),
                                tags$br(),
                                
                                prettyRadioButtons(inputId = "radioB1",
                                                   label = NULL,
                                                   choices = c("Identidad" = "identity", "Logarítmica" = "log")),
                                
                                circle = TRUE, status = "danger", icon = icon("gear"), width = "200px",
                                tooltip = tooltipOptions(title = "Cambia la transformación !")
                              ),
                              
                            ),
                            
                          ),
                          fluidRow(
                            
                            #Selector box 1 "Estados"
                            box(
                              
                              title = "Estados",
                              status = "primary",
                              solidHeader = TRUE,
                              width = 6,
                              
                              selectInput(inputId = "n_lag_inf",
                                          label = "Selecciona estados", 
                                          choices = v_states_zmvm , 
                                          selected = "Ciudad de México",
                                          multiple = TRUE),
                              checkboxInput("all12", "Seleccionar Todos/Ninguno", value = TRUE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_states1",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                            ),
                            
                            #Selector box 2 "Indicador"
                            box(
                              
                              width = 6,
                              
                              title = "Indicadores",
                              status = "primary",
                              solidHeader = TRUE,
                              #width = 5,
                              postition = "left",
                              selectInput(inputId = "indicator_1", 
                                          label = "Selecciona un indicador",
                                          choices = c(v_outcome, 
                                                      "Tasa Fatalidad" = "TLetal",
                                                      "Tasa de Positividad" = "TPos",
                                                      "Tasa de Pruebas" = "TP",
                                                      "Tasa de Incidentes" = "TI"
                                                      
                                                      ),
                                          multiple = TRUE),
                              checkboxInput("all_ind_1", "Seleccionar Todos/Ninguno", value = FALSE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_indicator",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                  
                                ),
                              
                            ),#End box 2
                            
                            
                          ),#End FluidRow 1  
                          
                          #Fluid Row 2
                          fluidRow(
                            #Selector box 3  "Tipo de Caso"
                            box(
                              
                              #width = 6,
                              
                              title = "Tipo de caso",
                              status = "primary",
                              solidHeader = TRUE,
                              selectInput(inputId = "type",
                                          label = "Selecciona el tipo de caso",
                                          choices = c("Acumulados"= "cum_cases", "Incidentes" = "new_cases"),
                                          multiple = FALSE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_type",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                              
                            ),
                            
                            #Selector box 4 "Escala"
                            box(
                              width = 6,
                              title = "Escala",
                              status = "primary",
                              solidHeader = TRUE,
                              selectInput(inputId = "scale_cum_inc",
                                          label = "Selecciona una escala",
                                          choices = c( "Fija" = "fixed", "Libre" = "free","Libre x"= "free_x",  "Libre y "= "free_y"),
                                          multiple = FALSE,
                                          
                                          
                              ),
                              footer =
                                
                                actionButton(
                                  inputId = "help_scale1",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                            ),
                            # #Box 5 "Transformaciom"
                            # 
                            # box(
                            #   width = 6,
                            #   title = "Transformación",
                            #   status = "primary",
                            #   solidHeader = TRUE,
                            #   selectInput(inputId = "select_trans",
                            #               label = "Selecciona una transformación",
                            #               choices = v_scale,
                            #               
                            #               multiple = FALSE,
                            #   ),
                            #   
                            # ),
                          ), #FluidRow 2 
                          #   #Fluid Row 3
                          #   fluidRow(
                          #     box(
                          #       title = "Guardar Imágen",
                          #       status ="warning",
                          #     actionButton("saveButton", "Go!")
                          #     )
                          # 
                          # )#End fluid Row 3
                  ),
                  
                  
                  # Graph 1.1 "Indicadores COVID-19 No-facets" ------------------------------
                  tabItem( tabName = "graph11",
                           h2("Indicadores COVID-19 no facets"),
                           hr(),
                           p("De manera similar, la siguiente gráfica permite comparar 
                             las tendencias de distintos estados para un solo indicador."),
                           fluidRow(
                             
                             box(
                               collapsible = F, width = 12,
                               column( 12,align="center" ,tableOutput('table11')),
                               plotOutput("plot11", height = 500) %>% withSpinner(color="#2980b9"),
                               #radioButtons(inputId = "var3", label = "Select the file type", choices = list("png", "pdf")),
                               #downloadButton(outputId = "down", label = "Download the plot")
                               dropdownButton(
                                 tags$h4("Transformación"),
                                 tags$b("Selecciona el tipo de transformación"),
                                 tags$br(),
                                 
                                 prettyRadioButtons(inputId = "radioB2",
                                                    label = NULL,
                                                    choices = c("Identity" = "identity", "Logarítmica" = "log")),
                                 
                                 circle = TRUE, status = "danger", icon = icon("gear"), width = "200px",
                                 tooltip = tooltipOptions(title = "Cambia la transformación !")
                               ),
                               
                               
                             ),
                             
                           ),
                           
                           fluidRow(
                             #Selector box 1 "Estados"
                             box(
                               title = "Estados",
                               status = "primary",
                               solidHeader = TRUE,
                               #width = 6,
                               
                               selectInput(inputId = "states_cum_inc_nofacet",
                                           label = "Selecciona estados", 
                                           choices = v_states_zmvm, 
                                           selected = "Ciudad de México",
                                           multiple = TRUE),
                               checkboxInput("all_states_11", "Seleccionar Todos/Ninguno", value = TRUE),
                               footer =
                                 
                                 actionButton(
                                   inputId = "help_states11",
                                   label = "Ayuda",
                                   icon = icon("fas fa-question-circle")
                                 )
                             ),
                             #Selector box 2 "Indicador"
                             box(
                               
                               width = 6,
                               
                               title = "Indicador",
                               status = "primary",
                               solidHeader = TRUE,
                               #width = 5,
                               postition = "left",
                               selectInput(inputId = "v_outcome_cum_inc_nofacet", 
                                           label = "Selecciona un indicador",
                                           choices = v_outcome,
                                           multiple = FALSE),
                               footer =
                                 
                                 actionButton(
                                   inputId = "help_indicator11",
                                   label = "Ayuda",
                                   icon = icon("fas fa-question-circle")
                                 )
                             ),#End box 2
                             #Selector box 3  "Tipo de Caso"
                             box(
                               
                               width = 6,
                               
                               title = "Tipo de caso",
                               status = "primary",
                               solidHeader = TRUE,
                               selectInput(inputId = "v_type_cum_inc_nofacet",
                                           label = "Selecciona el tipo de caso",
                                           choices = c("Acumulados"= "cum_cases", "Incidentes" = "new_cases"),
                                           multiple = FALSE),
                               footer =
                                 
                                 actionButton(
                                   inputId = "help_type11",
                                   label = "Ayuda",
                                   icon = icon("fas fa-question-circle")
                                 )
                               
                             ),
                           )# End Fluid Row 1
                           
                  ),
                  
                  # Graph 2 "Pruebas y tasas de positividad" -----------------------------------------------------------------
                  
                  tabItem(tabName = "graph2",
                          h2("Indicadores COVID-19 en el tiempo para varios estados"),
                          hr(),
                          p("La siguiente gráfica muestra las pruebas acumuladas que se han realizado en una o varias entidades federativas,
                          así como las proporciones de pruebas positivas y negativas, respecto al total. 
                          Además, el número entre paréntesis indica la “Tasa de positividad acumulada”,
                           es decir, el porcentaje de las pruebas que han resultado positivas, respecto al total de las realizadas."),
                          fluidRow(
                            box(
                              collapsible = F, width = 12,
                              column( 12, align="center" ,tableOutput('top6')),
                              plotOutput("plot2", height = 500) %>% withSpinner(color="#2980b9"))
                            
                          ),
                          fluidRow(
                            #Selector box 1 "Estados"
                            box(
                              
                              title = "Estados",
                              status = "primary",
                              solidHeader = TRUE,
                              #width = 6,
                              
                              selectInput(inputId = "states_pos_neg_test",
                                          label = "Selecciona estados", 
                                          choices = v_states_zmvm , 
                                          selected = "Ciudad de México",
                                          multiple = TRUE),
                              checkboxInput("all_states_2", "Seleccionar Todos/Ninguno", value = TRUE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_states2",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                            ),
                            
                            #Selector box 2  "Fecha"
                            box(
                              
                              width = 6,
                              
                              title = "Fecha",
                              status = "primary",
                              solidHeader = TRUE,
                              dateInput(  inputId = "date_pos_neg_test",
                                          label = "Selecciona una fecha",
                                          
                                          value = Sys.Date(),
                                          min =  "2020-02-27",
                                          max = Sys.Date() ),
                              footer =
                                
                                actionButton(
                                  inputId = "help_date2",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                              
                            ),
                          ),# End Fluid Row 1
                          fluidRow(
                            #Selector box 4 "Escala"
                            
                            
                            
                            
                            
                          ),
                          
                  ),
                  
                  # Graph 3 "Tasa de letalidad acumulada" -----------------------------------------------------------------
                  
                  tabItem(tabName = "graph3",
                          h2("Tasa de letalidad acumulada"),
                          hr(),
                          p("La siguiente gráfica muestra los casos confirmados acumulados en una o varias entidades federativas, 
                          así como las proporciones de los casos que han resultado en una defunción.  
                          El número en el paréntesis indica la “Tasa de letalidad acumulada”, es decir, 
                          el porcentaje de casos confirmados que han terminado en defunción."),
                          fluidRow(
                            box(
                              collapsible = F, width = 12,
                              column( 12, align="center" ,tableOutput('top7')),
                              plotOutput("plot3", height = 500) %>% withSpinner(color="#2980b9"))
                          ),
                          
                          fluidRow(
                            #Selector box 1 "Estados"
                            box(
                              
                              title = "Estados",
                              status = "primary",
                              solidHeader = TRUE,
                              #width = 6,
                              
                              selectInput(inputId = "states_cum_fatality",
                                          label = "Selecciona estados", 
                                          choices = v_states_zmvm, 
                                          selected = "Ciudad de México",
                                          multiple = TRUE),
                              checkboxInput("all_states_3", "Seleccionar Todos/Ninguno", value = TRUE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_states3",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                            ),
                            
                            #Selector box 2  "Fecha"
                            box(
                              
                              width = 6,
                              
                              title = "Fecha",
                              status = "primary",
                              solidHeader = TRUE,
                              dateInput(  inputId = "date_cum_fatality",
                                          label = "Selecciona una fecha",
                                          
                                          value = Sys.Date(),
                                          min =  "2020-02-27",
                                          max = Sys.Date() ),
                              footer =
                                
                                actionButton(
                                  inputId = "help_date3",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                              
                            ),
                          ),
                          
                  ),
                  
                  # Graph 4 "Tiempos de duplicación" ----------------------------------------
                  
                  tabItem(tabName = "graph4",
                          h2("Tiempos de duplicación"), 
                          hr(),
                          p("Los tiempos de duplicación reflejan el tiempo en el que un indicador tardaría en duplicar su tamaño,
                asumiendo un crecimiento exponencial constante.  
                La siguiente gráfica muestra la tendencia de algún indicador para una o varias entidades federativas, 
                así como tres tiempos de duplicación hipotéticos."),
                          fluidRow(
                            box(
                              collapsible = F, width = 12,
                              column( 12, align="center" ,tableOutput('top8')),
                              plotlyOutput("plot4", height = 500) %>% withSpinner(color="#2980b9"))
                          ),
                          fluidRow(
                            #Selector box 1 (Estados)
                            box(
                              title = "Estados",
                              status = "primary",
                              solidHeader = TRUE,
                              selectInput(inputId = "states",
                                          label = "Selecciona estados", 
                                          selected = "Ciudad de México",
                                          choices = v_states_zmvm, 
                                          multiple = TRUE),
                              checkboxInput("all13", "Seleccionar Todos/Ninguno", value = FALSE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_states_dt",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                            ),
                            #Selector box 2
                            box(
                              title = "Indicador",
                              status = "primary",
                              solidHeader = TRUE,
                              selectInput(inputId = "outcome2", 
                                          label = "Selecciona un indicador",
                                          choices = v_outcome,
                                          multiple = FALSE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_indicator_dt",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                              
                            ),#End box 
                          ),
                          
                  ),
                  
                  # Graph 5 "Mapas por estado" ---------------------------------------------------------
                  
                  tabItem(tabName = "graph5",
                          h2("Tasas COVID-19 por estado"), 
                          hr(),
                          p("La siguiente herramienta permite visualizar la distribución de distintas tasas en los municipios de una entidad federativa determinada. 
              La Tasa de positividad acumulada refleja el porcentaje de pruebas positivas, respecto al total de pruebas realizadas. 
              La Tasa de incidencia señala el número de casos confirmados de COVID-19 por cada 100,000 habitantes.
                La Tasa de pruebas refleja el número de pruebas realizadas por cada 100,000 habitantes. 
                La Tasa de letalidad muestra el porcentaje de los casos confirmados que han concluido como defunciones."),
                          fluidRow(
                            box(
                              collapsible = F, width = 12,
                              column( 12, align="center" ,tableOutput('top9')),
                              plotlyOutput("plot5", height = 500) %>% withSpinner(color="#2980b9") )
                          ),
                          fluidRow(
                            #Selector box 1 (Estados)
                            box(
                              title = "Estados",
                              status = "primary",
                              solidHeader = TRUE,
                              selectInput(inputId = "states5",
                                          label = "Selecciona estados", 
                                          choices = v_states,
                                          
                                          selected = "Aguascalientes",
                                          multiple = FALSE,
                              ),
                              footer =
                                
                                actionButton(
                                  inputId = "help_states_maps",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                              
                            ),
                            #Selector box 2
                            box(
                              title = "Indicador",
                              status = "primary",
                              solidHeader = TRUE,
                              selectInput(inputId = "outcome3", 
                                          label = "Selecciona un indicador",
                                          choices = c("Tasa de positividad acumulada"= "TPosA", 
                                                      "Tasa de incidencia acumulada" = "TIA",
                                                      "Tasa de pruebas acumulada" = "TPA",
                                                      "Tasa de letalidad acumulada" = "TLetal" ),
                                          multiple = FALSE),
                              footer =
                                
                                actionButton(
                                  inputId = "help_indicator_maps",
                                  label = "Ayuda",
                                  icon = icon("fas fa-question-circle")
                                )
                            ),#End box 
                          ),
                          
                          
                  ),
                  
                  
                  # PAGE DASHBOARD ----------------------------------------------------------
                  tabItem(
                    h1("Tablero de Datos"),
                     #   class = "tablero",
                     #   style = "font-family: 'Lobster', cursive;
                     #   font-weight: 500; line-height: 1.1; 
                     # color: #4d3a7d;"),
                    h4(textOutput("estado")),
                    tabName = "dashboard", 
                    
                    fluidRow(
                      tags$head(tags$style(HTML(".small-box {height: 130px}"))),
                      
                      box(
                        status = "warning",
                        width = 12,
                        # Dynamic valueBoxes
                        valueBoxOutput("confirmados", width = 4), 
                        tags$style(".small-box.bg-yellow { background-color: #FFC9200 !important; color: #ffff  !important; }"),
                        #valueBoxOutput("negativos", width = 4), 
                        # valueBoxOutput("sospechosos", width = 4), 
                        
                        valueBoxOutput("defunciones", width = 4), 
                        tags$style(".small-box.bg-teal { background-color: #D77B5F !important; color: #ffff !important; }"),
                        #valueBoxOutput("porcHospitalizados", width = 4), 
                        
                        valueBoxOutput("hospitalizados", width = 4), 
                        tags$style(".small-box.bg-blue { background-color: #506432 !important; color: #ffff  !important; }"),
                        
                        valueBoxOutput("uci", width = 4),
                        tags$style(".small-box.bg-light-blue { background-color: #80003A !important; color: #ffff  !important; }"),
                        
                        valueBoxOutput("intubados", width = 4),
                        tags$style(".small-box.bg-orange { background-color: #696B7E !important; color: #ffff  !important; }"),
                        
                        valueBoxOutput("pruebas", width = 4), 
                        tags$style(".small-box.bg-olive { background-color: #6C3400 !important; color: #ffff  !important; }"),
                        
                        
                        
                        div(class= "lastUpdate", "Ultima Actualización:", n_time_stamp),
                        
                        
                      )
                    ),#End FluidRow 1
                    
                    fluidRow(
                      # Sidebar with a slider input
                      box(
                        
                        status = "warning",
                        width = 4,
                        height = 110,
                        
                        selectInput(inputId = "indicatorTableState", 
                                    label = "Selecciona un estado",
                                    selected = "Nacional",
                                    choices = v_states_zmvm,
                                    
                                    multiple = FALSE)
                      ),
                      box(
                        status = "warning",
                        width = 4,
                        height = 110,
                        selectInput(inputId = "indicatorTable", 
                                    label = "Selecciona una opción",
                                    choices = c("Acumulados"= "cum_cases", 
                                                "Incidentes" = "new_cases"
                                    ),
                                    multiple = FALSE),
                      ),
                      box(
                        width = 4,
                        status = "warning",
                        dateInput(  inputId = "date_cum_outcomes",
                                    label = "Selecciona una fecha",
                                    
                                    value = Sys.Date() -1 ,
                                    min =  "2020-02-27",
                                    max = Sys.Date()  ),
                        
                      )
                    ),
                    
                    fluidRow(
                      # Show a plot of the generated distribution
                      box(
                        width = 12,
                        formattableOutput("indTable"),
                        div(class= "lastUpdate", "Ultima Actualización:", n_time_stamp_county),
                        #Button
                        footer = 
                          actionButton(
                            inputId = "help_tasas",
                            label = "Ayuda",
                            icon = icon("fas fa-question-circle")
                          )
                          
                      )
                      
                    )
                    
                    
                    
                    # tabBox(
                    #   width = 12,
                    #   title = tagList(shiny::icon("fas fa-database"), "Tablas"),
                    #   side = "right", height = "250px",
                    #   selected = "Tab3",
                    #   tabPanel("Confirmados", dataTableOutput("tableConf")),
                    #   tabPanel("Tab2", "Tab content 2"),
                    #   tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                    # )
                    
                    
                    #)
                    
                    
                    
                  ),#End Page Dashboard
                  # PAGE ABOUT  --------------------------------------------------------------
                  
                  tabItem(
                    # "about", page_about, value = "page-about"
                    tabName = "about",
                    h2("Acerca de"),
                    hr(),
                    
                 
                    fluidRow(
                      fluidRow(
                        column(
                          box(
                            #title = div("Sobre este proyecto", style = "padding-left: 20px", class = "h2")
                            background = "olive",
                            column(
                              h3("Monitor de Indicadores COVID-19"),
                              "El Monitor de Indicadores COVID-19 tiene como propósito mostrar información sobre la pandemia de COVID-19 en
                              México a nivel municipal, estatal y nacional, con información actualizada día con día proveniente de 
                              la Secretaría de Salud Federal, a través de la Dirección General de Epidemiología. 
                              Si deseas obtener información más detallada sobre la metodología y resultados utilizados en esta aplicación, accede a 
                              nuestro repositorio público de GitHub (https://github.com/PADeCI).",


                              # tags$br(),
                              # h3("Disclaimer"),
                              # "
                              # 
                              # “Para desarrollar la aplicación se han utilizado datos abiertos publicados por la Secretaría de 
                              # Salud Federal, a través de la Dirección General de Epidemiología. La información que 
                              # presenta esta aplicación es meramente descriptiva y no refleja opiniones ni valoraciones de 
                              # los investigadores del Proyecto de Análisis de Decisiones en Contextos Inciertos (PADeCI) ni 
                              # del Centro de Investigación y Docencia Económicas”.
                              # 
                              # 
                              # ",
                              tags$br(),
                              h3("Contacto"),
                              "PADeCI",
                              tags$br(),
                              tags$a(href = "https://padeci.org/", "Sitio Oficial"), " | ",
                              tags$a(href = "https://github.com/PADeCI", "GitHub"), " | ",
                              tags$br(),
                              # "Responsable Fernando Alarid-Escudero",
                              # tags$br(),
                              # tags$a(href = "https://github.com/feralaes", "GitHub"), " | ",
                              # tags$a(href = "http://www.politicadedrogas.org/PPD/index.php/site/colaborador/id/36.html", "LinkedIn"), " | ",
                              # tags$a(href = "https://scholar.google.com/citations?hl=es&user=JwrkxocAAAAJ", "Google Scholar"), " | ",
                              # tags$br(),
                              # "Responsable Yadira Peralta Torres",
                              # tags$br(),
                              # tags$a(href = "https://github.com/AndreaLuviano", "GitHub"), " | ",
                              # tags$a(href = "https://www.cide.edu/nosotros/comunidad/profesores/perfil/?id=279", "LinkedIn"), " | ",
                              # tags$a(href = "https://scholar.google.com/citations?hl=es&user=JwrkxocAAAAJ", "Google Scholar"), " | ",
                              width = 12
                            ),
                            width = 12,
                          ),
                          width = 12,
                          style = "padding: 15px"
                        )
                      )
                    ),#End Fluid Row 1
                    
                        flipBox(
                          id = 1,
                          main_img = "https://image.flaticon.com/icons/svg/149/149076.svg",
                          header_img = "https://image.flaticon.com/icons/svg/119/119595.svg",
                          width = 6,
                          front_title = "Disclaimer",
                          back_title = 
                            h3("Powered by"),
                          align = "justify",
                          "Para desarrollar la aplicación se han utilizado datos abiertos publicados por la Secretaría de 
                              Salud Federal, a través de la Dirección General de Epidemiología. La información que 
                              presenta esta aplicación es meramente descriptiva y no refleja opiniones ni valoraciones de 
                              los investigadores del Proyecto de Análisis de Decisiones en Contextos Inciertos (PADeCI) ni 
                              del Centro de Investigación y Docencia Económicas.",
                          back_content = 
                            column(
                              width = 12,
                              align = "center",
                              tags$img(src = "padeci.png", height=200, width = 300)
                              # tags$br(),
                              # tags$a(href = "https://padeci.org/", "Sitio Oficial"), " | ",
                              # tags$a(href = "https://github.com/PADeCI", "GitHub"), " | ",
                              # tags$br()
                       
                              )
                            
                        )

                   


                    
                  ),#End tab Item About
                  
                  # Page Github -------------------------------------------------------------
                  tabItem(
                    tabName = "github",
                    h2("Github")
                  ),#End Github
                  
                  
                  # PAGE FICHA TECNICA ------------------------------------------------------
                  tabItem(
                    tabName = "ficha",
                    h2("Ficha Técnica"),
                    hr(),
                    
                    p("La información utilizada en esta aplicación proviene de la base de datos pública de la Dirección General de Epidemiología de la Secretaría de Salud. Por favor referir
                        al link que se proporciona." ),
                    tags$a(href="https://www.gob.mx/salud/documentos/datos-abiertos-152127", "Datos Abiertos"),
                    
                    tags$br(),
                    tags$br(),
                    fluidRow(
                      #Height info Boxes
                      tags$head(tags$style(HTML('.info-box {min-height: 160px;} .info-box-icon {height: 150px; line-height: 100px;} .info-box-content {padding-top: 10px; padding-bottom: 10px;}'))),
                      
                      #Tasa de positividad acumulada
                      infoBox(tags$b("Tasa de positividad Acumulada"),
                              hr(),
                              "Para calcular la tasa de positividad acumulada, 
                              se divide el número de casos confirmados acumulados sobre el número de 
                              pruebas de COVID-19 realizadas, hasta una fecha determinada. 
                              El número resultante se multiplica por 100, para obtener un porcentaje.
                              
                              ",
                              tags$br(),
                              width = 6,
                              icon = icon("credit-card"), 
                              fill = FALSE,
                              color = "purple"),
                      #Tasa de incidencia acumulada
                      infoBox(tags$b("Tasa de Incidencia Acumulada"),
                              hr(),
                              "Para calcular la tasa de incidencia acumulada, se divide el número de 
                              casos confirmados de COVID-19 sobre el total de la población de un 
                              estado/municipio, hasta una fecha determinada. El número resultante 
                              se multiplica por 100,000 para obtener un valor estandarizado por 
                              cada 100,000 habitantes. ", 
                              width = 6,icon = icon("credit-card"), fill = FALSE,color = "purple"),
                      
                    ),#End Fluid Row 1
                    hr(),
                    fluidRow(
                      #Tasa de pruebas acumulada
                      infoBox(tags$b("Tasa de Pruebas acumulada"),
                              hr(),
                              "Para calcular la tasa de pruebas acumulada, se divide el número de pruebas de 
                              COVID-19 realizadas sobre el total de la población de un estado/municipio, 
                              hasta una fecha determinada. El número resultante se multiplica por 100,000 para 
                              obtener un valor estandarizado por cada 100,000 habitantes. ", 
                              width = 6,icon = icon("credit-card"), fill = FALSE,color = "purple"),
                      #Tasa de letalidad Acumulada
                      infoBox(tags$b("Tasa de Letalidad acumulada"),
                              hr(),
                              " Para calcular la tasa de letalidad acumulada, se divide el 
                              número de fallecimientos acumulados sobre el número de casos confirmados de COVID-19, 
                              hasta una fecha determinada. El número resultante se multiplica por 100, 
                              para obtener un porcentaje. ", 
                              width = 6,icon = icon("credit-card"), fill = FALSE,color = "purple"),
                    ),#End Fluid Row 2
                    fluidRow(
                    box(
                      title = "Advertencia",
                      width = 12,
                      status = "warning",
                      collapsible = TRUE,
                      p("Los datos reportados diariamente por la Secretaría de Salud, a través de la Dirección General de Epidemiología pueden presentar un rezago en su recolección. Es decir, la información reportada cada 
                        día rara vez es reportada el mismo día de su ocurrencia.")
                    )
                    ),
                    
                  ),#End Ficha Tecnica
                  
                  
                  # Page Table --------------------------------------------------------------
                  tabItem(
                    tabName = "table",
                    h2("Tablas"),
                    
                    fluidRow(
                      box(
                        width = 12,
                        reactableOutput("table")
                      )
                    )
                    
                    
                    
                    
                  )#End Page Table
                  
                )#End tabItems  
              ) #End Body
            )
  )
