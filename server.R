# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # DASHBOARD PAGE ----------------------------------------------------------
  
  #Title State
  output$estado <- renderText({ input$indicatorTableState})
  
  #Confirmados 
  output$confirmados <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(confirmedNacional),
      subtitle = "Confirmados",
      icon = icon("fas fa-plus-square"),
      color    = "yellow", 
    )
  })
  
  #Negativos
  output$negativos <- renderValueBox({
    valueBox(
      
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(confirmedNacional),
      subtitle = "Negativos",
      icon = icon("fas fa-plus-square"),
      color    = "red", 
    )
  })
  
  # #Sospechosos
  # output$sospechosos <- renderValueBox({
  #   valueBox(
  #     #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
  #     #color = "purple",
  #     paste0(confirmedNacional),
  #     subtitle = "Sospechosos",
  #     icon = icon("fas fa-question"),
  #     color    = "red", 
  #   )
  # })
  # 
  #Defunciones
  output$defunciones <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(deathNacional),
      subtitle = "Defunciones",
      icon = icon("fas fa-ribbon"),
      color    = "teal", 
    )
  })
  
  #Porcentaje Hospitalizados
  output$porcHospitalizados <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(porcNacional),
      subtitle = " Porcentaje de Hospitalizados",
      icon = icon("fas fa-percent"),
      color    = "maroon", 
    )
  })
  
  #Hospitalizados
  output$hospitalizados <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(hospiNacional),
      subtitle = "Hospitalizados",
      icon = icon("fas fa-hospital"),
      color    = "blue", 
    )
  })
  
  #UCI
  output$uci <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(uciNacional),
      subtitle = "UCI",
      icon = icon("fas fa-briefcase-medical"),
      color    = "light-blue", 
    )
  })
  
  #Intubados
  output$intubados <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(intuNacional),
      subtitle = "Intubados",
      icon = icon("fas fa-ambulance"),
      color    = "orange", 
    )
  })
  
  #Pruebas
  output$pruebas <- renderValueBox({
    valueBox(
      #paste0(25 + input$count, "%"), "Confirmados", icon = icon("list"),
      #color = "purple",
      paste0(testNacional),
      subtitle = "Pruebas",
      icon = icon("fas fa-vials"),
      color    = "olive", 
    )
  })
  
  
  # Data Tables Nacional Municipios -----------------------------------------
  
  
  observe({
    x <- input$indicatorTableState
    
    if (x == "Nacional"){
      #Data Table Nacional
      out_table_rate <- reactive({
        cum_outcomes_state(
          #select_state = input$indicatorTableState,
          select_date = input$date_cum_outcomes, 
          type = input$indicatorTable, 
          save_table = FALSE, 
          return_table = TRUE)
        
      })
      
      
      output$indTable  <- renderFormattable({
        out_table_rate()
      })
    } else {
      #Data Table Municipios
      out_table_munc <- reactive({
        cum_outcomes_county(data = "df_covid_ssa_county",
                            select_date = input$date_cum_outcomes,
                            type = input$indicatorTable,
                            select_state = input$indicatorTableState,
                            save_table = FALSE,
                            return_table = TRUE)
      })
      
      output$indTable  <- renderFormattable({
        out_table_munc()
      })
      
    }
  }) 
  
  
  # ValueBoxes --------------------------------------------------------------
  
  
  #CONFIRMADOS
  observe({
    x <- input$indicatorTableState
    switch(x, 
           "Aguascalientes"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[1]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })
           },
           
           "Baja California"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[2]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Baja California Sur"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[3]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Campeche"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[4]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Chiapas"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[5]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Chihuahua"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[6]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Coahuila"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[7]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Colima"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[8]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Durango"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[9]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Guanajuato"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[10]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Guerrero"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[11]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Hidalgo"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[12]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Jalisco"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[13]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Ciudad de México"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[14]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Michoacán"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[15]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Morelos"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[16]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Nayarit"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[17]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           
           "Nuevo León"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[18]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Oaxaca"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[19]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           
           "Puebla"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[20]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Querétaro"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[21]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Quintana Roo"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[22]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           
           "San Luis Potosí"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[23]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Sinaloa"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[24]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Sonora"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[25]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Estado de México"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[26]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Tabasco"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[27]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Tamaulipas"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[28]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Tlaxcala"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[29]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           "Veracruz"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[30]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })   
           },
           
           
           "Yucatán"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[31]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })
             
           },
           
           "Zacatecas"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[32]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })
             
           },
           
           
           "Nacional"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[33]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })
             
           },
           
           "ZMVM"={
             output$confirmados <- renderValueBox({
               valueBox(
                 paste0(confirmadosVector[34]),
                 subtitle = "Confirmados",
                 icon = icon("fas fa-plus-square"),
                 color    = "yellow", 
               )
             })
             
           },
           {
             print('default')
           }
    )
  })
  
  #DEFUNCIONES
  observe({
    x <- input$indicatorTableState
    switch(x, 
           "Aguascalientes"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[1]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })
           },
           
           "Baja California"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[2]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Baja California Sur"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[3]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Campeche"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[4]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Chiapas"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[5]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Chihuahua"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[6]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Coahuila"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[7]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Colima"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[8]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Durango"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[9]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Guanajuato"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[10]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Guerrero"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[11]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Hidalgo"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[12]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Jalisco"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[13]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Ciudad de México"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[14]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal",  
               )
             })   
           },
           
           "Michoacán"={
             output$defunciones<- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[15]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Morelos"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[16]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Nayarit"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[17]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           
           "Nuevo León"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[18]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Oaxaca"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[19]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           
           "Puebla"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[20]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Querétaro"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[21]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Quintana Roo"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[22]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           
           "San Luis Potosí"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[23]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Sinaloa"={
             output$defunciones<- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[24]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Sonora"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[25]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Estado de México"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[26]),
                 subtitle = "Defuncioness",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Tabasco"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[27]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Tamaulipas"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[28]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Tlaxcala"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[29]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           "Veracruz"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[30]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })   
           },
           
           
           "Yucatán"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[31]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })
             
           },
           
           "Zacatecas"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[32]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })
             
           },
           
           
           "Nacional"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[33]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })
             
           },
           
           "ZMVM"={
             output$defunciones <- renderValueBox({
               valueBox(
                 paste0(defuncionesVector[34]),
                 subtitle = "Defunciones",
                 icon = icon("fas fa-ribbon"),
                 color    = "teal", 
               )
             })
             
           },
           {
             print('default')
           }
    )
  })
  
  #HOSPITALIZADOS
  observe({
    x <- input$indicatorTableState
    switch(x, 
           "Aguascalientes"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[1]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })
           },
           
           "Baja California"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[2]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Baja California Sur"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[3]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Campeche"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[4]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Chiapas"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[5]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Chihuahua"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[6]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue",  
               )
             })   
           },
           
           "Coahuila"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[7]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Colima"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[8]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Durango"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[9]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Guanajuato"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[10]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Guerrero"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[11]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Hidalgo"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[12]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Jalisco"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[13]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Ciudad de México"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[14]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Michoacán"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[15]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Morelos"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[16]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Nayarit"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[17]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    ="blue", 
               )
             })   
           },
           
           
           "Nuevo León"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[18]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Oaxaca"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[19]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           
           "Puebla"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[20]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Querétaro"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[21]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Quintana Roo"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[22]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           
           "San Luis Potosí"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[23]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Sinaloa"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[24]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue",  
               )
             })   
           },
           
           "Sonora"={
             output$hospitalizados<- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[25]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Estado de México"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[26]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue",  
               )
             })   
           },
           
           "Tabasco"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[27]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Tamaulipas"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[28]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue",  
               )
             })   
           },
           
           "Tlaxcala"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[29]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           "Veracruz"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[30]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })   
           },
           
           
           "Yucatán"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[31]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })
             
           },
           
           "Zacatecas"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[32]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })
             
           },
           
           
           "Nacional"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[33]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })
             
           },
           
           "ZMVM"={
             output$hospitalizados <- renderValueBox({
               valueBox(
                 paste0(hospitalizadosVector[34]),
                 subtitle = "Hospitalizados",
                 icon = icon("fas fa-hospital"),
                 color    = "blue", 
               )
             })
             
           },
           {
             print('default')
           }
    )
  })
  
  #UCI
  observe({
    x <- input$indicatorTableState
    switch(x, 
           "Aguascalientes"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[1]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })
           },
           
           "Baja California"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[2]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Baja California Sur"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[3]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Campeche"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[4]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })   
           },
           
           "Chiapas"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[5]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Chihuahua"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[6]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Coahuila"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[7]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Colima"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[8]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Durango"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[9]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Guanajuato"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[10]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Guerrero"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[11]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Hidalgo"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[12]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Jalisco"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[13]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Ciudad de México"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[14]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             }) 
           },
           
           "Michoacán"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[15]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             }) 
           },
           
           "Morelos"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[16]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })   
           },
           
           "Nayarit"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[17]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           
           "Nuevo León"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[18]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Oaxaca"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[19]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           
           "Puebla"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[20]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Querétaro"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[21]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Quintana Roo"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[22]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           
           "San Luis Potosí"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[23]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             }) 
           },
           
           "Sinaloa"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[24]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Sonora"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[25]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             }) 
           },
           
           "Estado de México"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[26]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Tabasco"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[27]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             }) 
           },
           
           "Tamaulipas"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[28]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })   
           },
           
           "Tlaxcala"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[29]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })  
           },
           
           "Veracruz"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[30]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })   
           },
           
           
           "Yucatán"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[31]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })
             
           },
           
           "Zacatecas"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[32]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })
             
           },
           
           
           "Nacional"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[33]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })
             
           },
           
           "ZMVM"={
             output$uci <- renderValueBox({
               valueBox(
                 paste0(uciVector[34]),
                 subtitle = "UCI",
                 icon = icon("fas fa-briefcase-medical"),
                 color    = "light-blue", 
               )
             })
             
           },
           {
             print('default')
           }
    )
  })
  
  #INTUBADOS
  observe({
    x <- input$indicatorTableState
    switch(x, 
           "Aguascalientes"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[1]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Baja California"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[2]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Baja California Sur"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[3]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Campeche"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[4]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Chiapas"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[5]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Chihuahua"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[6]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Coahuila"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[7]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Colima"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[8]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Durango"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[9]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Guanajuato"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[10]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Guerrero"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[11]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Hidalgo"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[12]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Jalisco"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[13]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Ciudad de México"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[14]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Michoacán"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[15]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Morelos"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[16]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })   
           },
           
           "Nayarit"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[17]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           
           "Nuevo León"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[18]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Oaxaca"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[19]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           
           "Puebla"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[20]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Querétaro"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[21]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Quintana Roo"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[22]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           
           "San Luis Potosí"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[23]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Sinaloa"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[24]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Sonora"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[25]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
           },
           
           "Estado de México"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[26]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Tabasco"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[27]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             }) 
           },
           
           "Tamaulipas"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[28]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Tlaxcala"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[29]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })  
           },
           
           "Veracruz"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[30]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })   
           },
           
           
           "Yucatán"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[31]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
             
           },
           
           "Zacatecas"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[32]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
             
           },
           
           
           "Nacional"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[33]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
             
           },
           
           "ZMVM"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(intubadosVector[34]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
             
           },
           {
             print('default')
           }
    )
  })
  
  #PRUEBAS
  observe({
    x <- input$indicatorTableState
    switch(x, 
           "Aguascalientes"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[1]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Baja California"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[2]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Baja California Sur"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[3]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Campeche"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[4]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Chiapas"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[5]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Chihuahua"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[6]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             }) 
           },
           
           "Coahuila"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[7]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Colima"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[8]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Durango"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[9]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Guanajuato"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[10]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Guerrero"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[11]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Hidalgo"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[12]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             }) 
           },
           
           "Jalisco"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[13]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Ciudad de México"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[14]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Michoacán"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[15]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Morelos"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[16]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })  
           },
           
           "Nayarit"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[17]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           
           "Nuevo León"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[18]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Oaxaca"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[19]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           
           "Puebla"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[20]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Querétaro"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[21]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Quintana Roo"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[22]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           
           "San Luis Potosí"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[23]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Sinaloa"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[24]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             }) 
           },
           
           "Sonora"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[25]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Estado de México"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[26]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Tabasco"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[27]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Tamaulipas"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[28]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             }) 
           },
           
           "Tlaxcala"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[29]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
           },
           
           "Veracruz"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[30]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })  
           },
           
           
           "Yucatán"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[31]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
             
           },
           
           "Zacatecas"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[33]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
             
           },
           
           
           "Nacional"={
             output$pruebas <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[34]),
                 subtitle = "Pruebas",
                 icon = icon("fas fa-vials"),
                 color    = "olive", 
               )
             })
             
           },
           
           "ZMVM"={
             output$intubados <- renderValueBox({
               valueBox(
                 paste0(pruebasVector[34]),
                 subtitle = "Intubados",
                 icon = icon("fas fa-ambulance"),
                 color    = "orange", 
               )
             })
             
           },
           {
             print('default')
           }
    )
  })
  
  # Graph 1 "Indicadores COVID-19 a través del tiempo --------------------------------------------------------------
  
  out_grapy <- reactive({
    cum_inc(select_state = input$n_lag_inf,
            data = "df_covid_ssa_state",
            outcome = input$indicator_1,
            type = input$type,
            select_trans = input$radioB1,
            scales = input$scale_cum_inc,
            save_plot = FALSE)
  })
  
  
  
  
  output$plot1  <- renderPlot({
    out_grapy()
  })
  
  #Download Plot 1 ""
  #downloadHandler contains 2 arguments as functions, namely filename, content
  output$down <- downloadHandler(
    filename = function() { paste("plot", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = out_grapy(), device = "png")    }
  )
  
  # Graph 1.1 ---------------------------------------------------------------
  
  out_graph_cum_inc_nofacet <- reactive({
    cum_inc_nofacet(select_state = input$states_cum_inc_nofacet,
                    data = "df_covid_ssa_state",
                    outcome = input$v_outcome_cum_inc_nofacet,
                    type = input$v_type_cum_inc_nofacet,
                    select_trans = input$radioB2,
                    save_plot = FALSE,
                    print_plot = TRUE)
  })
  
  output$plot11  <- renderPlot({
    out_graph_cum_inc_nofacet()
  })
  
  
  
  # Graph 2 "Pruebas y tasas de positividad" -----------------------------------------------------------------
  out_pos_neg <- reactive({
    pos_neg_test(data = "df_covid_ssa_state",
                 select_state = input$states_pos_neg_test,
                 select_date = input$date_pos_neg_test,
                 type = "cum_cases",
                 select_trans = input$v_select_trans_pos_neg_test,
                 print_plot = TRUE,
                 save_plot = FALSE)
  })
  
  
  output$plot2  <- renderPlot({
    out_pos_neg()
  })
  
  
  # Graph 3 "Tasa de letalidad acumulada"-----------------------------------------------------------------
  
  out_cum_fatality <- reactive({
    cum_fatality(data = "df_covid_ssa_state",
                 select_state = input$states_cum_fatality,
                 select_date = input$date_cum_fatality,
                 print_plot = TRUE,
                 save_plot = FALSE)
    
  })
  
  
  output$plot3  <- renderPlot({
    out_cum_fatality()
  })
  
  
  # Graph 4 "Tiempos de duplicación" ----------------------------------------
  out_doubling_times <- reactive({
    doubling_times(select_state = input$states,
                   #data = "df_covid_ssa_state",
                   outcome = input$outcome2, 
                   print_plot = TRUE,
                   save_plot = FALSE,
                   return_plot = TRUE)
  })
  
  output$plot4 <- renderPlotly({
    out_doubling_times()
  })
  
  
  # Graph 5 "Mapas por estado" ----------------------------------------------
  
  out_map_outcome <- reactive({
    map_outcome(select_state = input$states5,
                data = "df_covid_ssa_county",
                outcome = input$outcome3, 
                print_plot = TRUE,
                save_plot = FALSE,
                return_plot = TRUE)
  })
  
  output$plot5 <- renderPlotly({
    out_map_outcome()
  })
  
  
  # Page Tables ------------------------------------------------------------------
  
  
  output$table <- renderReactable({
    sparkline = colDef(cell = function(confirmed, index) {
      sparkline(confirmed$weight[[index]])
    })
    reactable(confirmed)
  })
  
  
  
  # Pop-up windows ----------------------------------------------------------
  
  
  # Graph 1 -----------------------------------------------------------------
  
  observeEvent(input$help_states1, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Estados",
                style = "color: steelblue;"),
        #tags$b("La república mexicana"), "está conformada",
        tags$br(),
        "Selecciona una de las 32 entidades federativas que componen a la República Mexicana.", 
        tags$br(),
        "Además, puedes seleccionar el agregado nacional o la Zona Metropolitana del Valle de México (ZMVM), 
        la cual está compuesta de la Ciudad de México y algunos municipios del Estado de México e Hidalgo",
        # tags$br(),
        # "ZMVM - Zona Metropolitana del Valle de México: CDMX, EDOMEX, Hidalgo",
        # tags$br(),
        # "Nacional - ",
        tags$br(),
        icon("map")
      ),
      html = TRUE
    )
  })
  
  #Help Indicador
  observeEvent(input$help_indicator, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Indicadores",
                style = "color: steelblue;"),
        
        tags$br(),
        tags$b("Confirmados: "),
        "Casos que hayan resultado Positivos a una prueba de COVID-19",
        
        tags$br(),
        tags$b("Hospitalizados:"), 
        "Casos confirmados que hayan requerido de hospitalización.",
        
        tags$br(),
        tags$b("Intubado:") ,
        "Casos confirmados que hayan requerido ser intubados.",
        
        tags$br(),
        tags$b("Muertes:"),
        "Casos confirmados que hayan resultado en un fallecimiento",
        
        tags$br(),
        tags$b("Síntomas: "),
        "Casos que, para una fecha determinada, hayan desarrollado síntomas de COVID-19.",
        
        tags$br(),
        tags$b("UCI: "),
        "Casos confirmados que hayan requerido atención médica en una Unidad de Cuidados Intensivos (UCI). ",
        tags$br(),
        icon("far fa-hand-point-up")
      ),
      html = TRUE
    )
  })
  
  # #Help "Tipo"
  observeEvent(input$help_type, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Tipo",
                style = "color: steelblue;"),
        tags$br(),
        tags$b("Acumulados: "),
        "Indica la suma de casos para una fecha determinada.",
        tags$br(),
        tags$b("Incidentes: "),
        "Refleja los casos ocurridos en una fecha determinada.",
        tags$br(),
        icon("fas fa-cubes")
      ),
      html = TRUE
    )
  })
  # Help "Scale"
  observeEvent(input$help_scale1, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Indicadores",
                style = "color: steelblue;"),
        
        tags$br(),
        tags$b("Fija: "),
        "Utiliza las mismas escalas de medición para todos los sub gráficos.",
        tags$br(),
        tags$b("Libre: "),
        "Utiliza una escala distinta para cada uno de los sub gráficos, dependiendo de los valores del Estado/Indicador.",
        tags$br(),
        tags$b("Libre X:"),
        "Permite una variación en la escala de medición para el eje horizontal, dependiendo de los valores del Estado/Indicador.",
        tags$br(),
        tags$b("Libre Y: "),
        "Permite una variación de la escala de medición para el eje vertical, dependiendo de los calores del Estado/Indicador.",
        
        tags$br(),
        icon("far fa-hand-point-up")
      ),
      html = TRUE
    )
  }) 
  
  # Graph 1.1 "" ------------------------------------------------------------
  
  #Help States
  observeEvent(input$help_states11, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Estados",
                style = "color: steelblue;"),
        tags$br(),
        "Selecciona una de las 32 entidades federativas que componen a la República Mexicana.", 
        tags$br(),
        "Además, puedes seleccionar el agregado nacional o la Zona Metropolitana del Valle de México (ZMVM), 
        la cual está compuesta de la Ciudad de México y algunos municipios del Estado de México e Hidalgo",
        # tags$br(),
        # "ZMVM - Zona Metropolitana del Valle de México: CDMX, EDOMEX, Hidalgo",
        # tags$br(),
        # "Nacional - ",
        icon("map")
      ),
      html = TRUE
    )
  })
  
  # Help "Indicador"
  observeEvent(input$help_indicator11, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Indicadores",
                style = "color: steelblue;"),
        
        tags$br(),
        tags$b("Confirmados: "),
        "Casos que hayan resultado Positivos a una prueba de COVID-19",
        tags$br(),
        tags$b("Hospitalizados:"), 
        "Casos confirmados que hayan requerido de hospitalización.",
        
        tags$br(),
        tags$b("Intubado:") ,
        "Casos confirmados que hayan requerido ser intubados.",
        tags$br(),
        tags$b("Muertes:"),
        "Casos confirmados que hayan resultado en un fallecimiento",
        tags$br(),
        tags$b("Síntomas: "),
        "Casos que, para una fecha determinada, hayan desarrollado síntomas de COVID-19.",
        tags$br(),
        tags$b("UCI: "),
        "Casos confirmados que hayan requerido atención médica en una Unidad de Cuidados Intensivos (UCI). ",
        tags$br(),
        icon("far fa-hand-point-up")
      ),
      html = TRUE
    )
  })
  
  # Help "Tipo"
  observeEvent(input$help_type11, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Tipo",
                style = "color: steelblue;"),
        tags$br(),
        tags$b("Acumulados: "),
        "Indica la suma de casos para una fecha determinada.",
        tags$br(),
        tags$b("Incidentes: "),
        "Refleja los casos ocurridos en una fecha determinada.",
        tags$br(),
        icon("fas fa-cubes")
      ),
      html = TRUE
    )
  })
  
  
  
  # Graph 2 "Pruebas y tasas de positividad" ----------------------------------------------------
  
  #Help States
  observeEvent(input$help_states2, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Estados",
                style = "color: steelblue;"),
        #tags$b("La república mexicana"), "está conformada",
        tags$br(),
        "Selecciona una de las 32 entidades federativas que componen a la República Mexicana.", 
        tags$br(),
        "Además, puedes seleccionar el agregado nacional o la Zona Metropolitana del Valle de México (ZMVM), 
        la cual está compuesta de la Ciudad de México y algunos municipios del Estado de México e Hidalgo",
        # tags$br(),
        # "ZMVM - Zona Metropolitana del Valle de México: CDMX, EDOMEX, Hidalgo",
        # tags$br(),
        # "Nacional - ",
        tags$br(),
        icon("map")
      ),
      html = TRUE
    )
  })
  
  #Help Date 
  observeEvent(input$help_date2, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Fecha",
                style = "color: steelblue;"),
        tags$br(),
        "Selecciona una fecha para la cual desees obtener los datos acumulados/incidentes.",
        tags$br(),
        tags$b("Nota:"), "no podrás seleccionar una fecha posterior al límite de la base de datos.",
        tags$br(),
        icon("calendar")
      ),
      html = TRUE
    )
  })
  
  
  
  # Graph 3 "Tasa de letalidad" ---------------------------------------------
  #Help States
  observeEvent(input$help_states3, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Estados",
                style = "color: steelblue;"),
        tags$br(),
        "Selecciona una de las 32 entidades federativas que componen a la República Mexicana.", 
        tags$br(),
        "Además, puedes seleccionar el agregado nacional o la Zona Metropolitana del Valle de México (ZMVM), 
        la cual está compuesta de la Ciudad de México y algunos municipios del Estado de México e Hidalgo",
        # tags$br(),
        # "ZMVM - Zona Metropolitana del Valle de México: CDMX, EDOMEX, Hidalgo",
        # tags$br(),
        # "Nacional - ",
        tags$br(),
        icon("map")
      ),
      html = TRUE
    )
  })
  
  
  #Help Date 
  observeEvent(input$help_date3, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Fecha",
                style = "color: steelblue;"),
        tags$br(),
        "Selecciona una fecha para la cual desees obtener los datos acumulados/incidentes.",
        tags$br(),
        tags$b("Nota:"), "no podrás seleccionar una fecha posterior al límite de la base de datos.",
        tags$br(),
        icon("calendar")
      ),
      html = TRUE
    )
  })
  
  # Graph Tiempos de Ducplicación -------------------------------------------
  
  #Help States "Tiempos de duplicación"
  observeEvent(input$help_states_dt, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Estados",
                style = "color: steelblue;"),
        #tags$b("La república mexicana"), "está conformada",
        tags$br(),
        "Selecciona una de las 32 entidades federativas que componen a la República Mexicana.", 
        tags$br(),
        "Además, puedes seleccionar el agregado nacional o la Zona Metropolitana del Valle de México (ZMVM), 
        la cual está compuesta de la Ciudad de México y algunos municipios del Estado de México e Hidalgo",
        # tags$br(),
        # "ZMVM - Zona Metropolitana del Valle de México: CDMX, EDOMEX, Hidalgo",
        # tags$br(),
        # "Nacional - ",
        tags$br(),
        icon("map")
      ),
      html = TRUE
    )
  })
  
  # #Help States "Mapas"
  observeEvent(input$help_states_maps, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Estados",
                style = "color: steelblue;"),
        #tags$b("La república mexicana"), "está conformada",
        tags$br(),
        "Selecciona una de las 32 entidades federativas que componen a la República Mexicana. 
        Además, puedes seleccionar el agregado nacional o la Zona Metropolitana del Valle de México (ZMVM), 
        la cual está compuesta de la Ciudad de México y algunos municipios del Estado de México e Hidalgo",
        # tags$br(),
        # "ZMVM - Zona Metropolitana del Valle de México: CDMX, EDOMEX, Hidalgo",
        # tags$br(),
        # "Nacional - ",
        tags$br(),
        icon("map")
      ),
      html = TRUE
    )
  })
  
  
  # #Help Indicator "Tiempos de duplicación"
  observeEvent(input$help_indicator_dt, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Indicadores",
                style = "color: steelblue;"),
        
        tags$br(),
        tags$br(),
        tags$b("Confirmados: "),
        "Casos que hayan resultado Positivos a una prueba de COVID-19",
        tags$br(),
        tags$b("Hospitalizados:"), 
        "Casos confirmados que hayan requerido de hospitalización.",
        
        tags$br(),
        tags$b("Intubado:") ,
        "Casos confirmados que hayan requerido ser intubados.",
        tags$br(),
        tags$b("Muertes:"),
        "Casos confirmados que hayan resultado en un fallecimiento",
        tags$br(),
        tags$b("Síntomas: "),
        "Casos que, para una fecha determinada, hayan desarrollado síntomas de COVID-19.",
        tags$br(),
        tags$b("UCI: "),
        "Casos confirmados que hayan requerido atención médica en una Unidad de Cuidados Intensivos (UCI). ",
        tags$br(),
        icon("far fa-hand-point-up")
      ),
      html = TRUE
    )
  })
  # 
  #Help Indicator "Tiempos Mapas"
  
  
  observeEvent(input$help_indicator_maps, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Indicadores",
                style = "color: steelblue;"),
        
        tags$br(),
        tags$br(),
        tags$b("Tasa de positividad acumulada: "),
        "Porcentaje de pruebas de COVID-19 que resultaron positivas hasta una fecha determinada.",
        tags$br(),
        tags$b("Tasa de incidencia acumulada:"), 
        "Casos positivos de COVID-19, respecto al total de la población de un estado/municipio. Los valores indican el número de casos positivos por cada 100,000 habitantes. ",
        
        tags$br(),
        tags$b("Tasa de pruebas acumulada:") ,
        "Número de pruebas de COVID-19 realizadas, respecto al total de la población de un estado/municipio. Los valores indican el número de pruebas realizadas por cada 100,000 habitantes. 
        ",
        tags$br(),
        tags$b("Tasa de letalidad acumulada:"),
        "Porcentaje de casos confirmados de COVID-19 que resultaron en fallecimiento hasta una fecha determinada. ",
        
        tags$br(),
        icon("far fa-hand-point-up")
      ),
      html = TRUE
    )
  })
  #
  
  #Help Page DASHBOARD Tasas
  observeEvent(input$help_tasas, {
    show_alert(
      title = NULL,
      text = tags$span(
        tags$h3("Indicadores",
                style = "color: steelblue;"),
        
        tags$br(),
        tags$br(),
        tags$b("Tasa de positividad acumulada: "),
        "Porcentaje de pruebas de COVID-19 que resultaron positivas hasta una fecha determinada.",
        tags$br(),
        tags$b("Tasa de incidencia acumulada:"), 
        "Casos positivos de COVID-19, respecto al total de la población de un estado/municipio. Los valores indican el número de casos positivos por cada 100,000 habitantes. ",
        
        tags$br(),
        tags$b("Tasa de pruebas acumulada:") ,
        "Número de pruebas de COVID-19 realizadas, respecto al total de la población de un estado/municipio. Los valores indican el número de pruebas realizadas por cada 100,000 habitantes. 
        ",
        tags$br(),
        tags$b("Tasa de letalidad acumulada:"),
        "Porcentaje de casos confirmados de COVID-19 que resultaron en fallecimiento hasta una fecha determinada. ",
  
        tags$br(),
        icon("far fa-hand-point-up")
      ),
      html = TRUE
    )
  })
  
  
  
  
  # Select All/None ---------------------------------------------------------
  
  #Graph 1 
  
  observe({
    updateSelectInput(
      session, "n_lag_inf", choices = v_states_zmvm,
      selected = if(input$all12) v_states_zmvm
    )
  })
  
  
  
  observe({
    updateSelectInput(
      session, "indicator_1", choices = v_outcome,
      selected = if(input$all_ind_1) v_outcome
    )
  })
  
  #Graph 1.1
  
  #States
  observe({
    updateSelectInput(
      session, "states_cum_inc_nofacet", choices = v_states_zmvm,
      selected = if(input$all_states_11) v_states_zmvm
    )
  })
  
  #Graph 2 "Pruebas y tasas de positividad"
  
  #States
  observe({
    updateSelectInput(
      session, "states_pos_neg_test", choices = v_states,
      selected = if(input$all_states_2) v_states
    )
  })
  
  
  #Graph 3 "Tasa de letalidad"
  
  #States
  observe({
    updateSelectInput(
      session, "states_cum_fatality", choices = v_states,
      selected = if(input$all_states_3) v_states
    )
  })
  
  
  
  
  # Select All/None States "Tiempos de duplicaación"
  
  observe({
    updateSelectInput(
      session, "states", choices = v_states_zmvm,
      selected = if(input$all13) v_states_zmvm
    )
  })
  
}
