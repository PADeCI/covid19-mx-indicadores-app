#****************************************************************# 
# Purpose: Plot VR Cum/Inc cases by and foreach state(s)         #
#                                                                #
# Created: June 01, 2020 / 20:12                                 #
# Depends on:                                                    #
#   Authors: Fernando Alarid-Escudero                            #
#            Manuel Cardona                                      # 
#            Yadira Peralta                                      #
#   e-mails: fernando.alarid@cide.edu                            #
#            mcardona@poverty-action.org                         #
#            yadira.peralta@cide.edu                             #
#                                                                #
#****************************************************************# 

#' Function to plot outcome(s) for selected states
#' 
#' \code{cum_inc} plots the outcome(s) of interest for states of 
#' interest.
#'
#' @param data Dataframe to be used
#' @param select_state A scalar or vector of states for plotting
#' @param outcome A scalar or vector of covid19-related outcomes for plotting
#' @param type A scalar with type of variable for plotting
#' @param select_trans A scalar indicating the type of scale to be used
#' @param scales A scalar defining if scales should be homogeneus or not
#' @param print_plot Logical. Prints plot if TRUE
#' @param save_plot Logical. Saves the plot if TRUE
#' @param return_plot Logical. Returns plot if TRUE
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
#' @export
cum_inc <- function(select_state = "Ciudad de México", 
                    data = "df_covid_ssa_state",
                    outcome = c("Confirmados", "Hospitalizados", 
                                "Intubados", "Muertes", "Pruebas", 
                                "Síntomas", "UCI", "TPos", "TI", "TP", "TLetal"), 
                    type = c("cum_cases", "new_cases"),
                    select_trans = c("identity", "log"),
                    scales = c("fixed", "free", "free_y", "free_x"),
                    print_plot = TRUE,
                    save_plot = FALSE,
                    return_plot = TRUE){
  
  outcome      <- match.arg(outcome, several.ok = TRUE)
  type         <- match.arg(type, several.ok = FALSE)
  select_trans <- match.arg(select_trans, several.ok = FALSE)
  data         <- match.arg(data, several.ok = FALSE)
  scales       <- match.arg(scales, several.ok = FALSE)
  
  
  df_outcome <- df_covid_ssa_state %>%
    group_by(entidad, var_resultado) %>%
    mutate(inc_cases = c(1, diff(cum_cases)),
           last_case = max(time_cases),
           send_label = case_when(time_cases ==last_case ~ entidad)) 
  
  
  if (type == "cum_cases") {
    types = "cum_cases"
  } else if (type == "new_cases") {
    types = "new_cases"
  }

  
  # Positivity rates (cum and inc)
  
        # Confirmed cases
        confirmed <- subset(df_outcome, var_resultado == "Confirmados") #We are focusing on confirmed cases
        confirmed <- confirmed %>%
          group_by(entidad) %>%
          rename(Confirmados_cum = cum_cases,
                 Confirmados_inc = inc_cases)
        confirmed$entidad[confirmed$entidad=='México'] <- 'Nacional'
        confirmed <- confirmed[c("entidad", "date", "Confirmados_cum", "Confirmados_inc")]
        
        # Tests
        test <- subset(df_outcome, var_resultado == "Pruebas") #We are focusing on tests
        test <- test %>%
          group_by(entidad) %>%
          rename(Pruebas_cum = cum_cases, 
                 Pruebas_inc = inc_cases)
        test$entidad[test$entidad=='México'] <- 'Nacional'
        test <- test[c("entidad", "date", "Pruebas_cum", "Pruebas_inc")]
        
        tposa <- left_join(confirmed, test, by = c("entidad", "date")) 
        tposa$cum_cases <- (tposa$Confirmados_cum/tposa$Pruebas_cum)*100
        tposa$new_cases <- (tposa$Confirmados_inc/tposa$Pruebas_inc)*100
        tposa <- tposa[c("entidad", "date", "cum_cases", "new_cases")]
        tposa$var_resultado <- "TPos"
        
        
  # Incidence rates (cum and inc)
        
        # Confirmed cases
        inci <- subset(df_outcome, var_resultado == "Confirmados") #We are focusing on confirmed cases
        inci <- inci %>%
          group_by(entidad) %>%
          rename(Confirmados_cum = cum_cases,
                 Confirmados_inc = inc_cases)
        inci$entidad[inci$entidad=='México'] <- 'Nacional'
        inci <- inci[c("entidad", "date", "Confirmados_cum", "Confirmados_inc", "population")]
        
        inci$cum_cases <- (inci$Confirmados_cum/inci$population)*100000
        inci$new_cases <- (inci$Confirmados_inc/inci$population)*100000
        inci <- inci[c("entidad", "date", "cum_cases", "new_cases")]
        inci$var_resultado <- "TI"
        
  # Test rates (cum and inc)
        
        # Tests
        trate <- subset(df_outcome, var_resultado == "Pruebas") #We are focusing on confirmed cases
        trate <- trate %>%
          group_by(entidad) %>%
          rename(Pruebas_cum = cum_cases,
                 Pruebas_inc = inc_cases)
        trate$entidad[trate$entidad=='México'] <- 'Nacional'
        trate <- trate[c("entidad", "date", "Pruebas_cum", "Pruebas_inc", "population")]
        
        trate$cum_cases <- (trate$Pruebas_cum/trate$population)*100000
        trate$new_cases <- (trate$Pruebas_inc/trate$population)*100000
        trate <- trate[c("entidad", "date", "cum_cases", "new_cases")]       
        trate$var_resultado <- "TP"
        
  # Case fatality rates (cum and inc)
        
        # Deaths
        death <- subset(df_outcome, var_resultado == "Muertes") #We are focusing on tests
        death <- death %>%
          group_by(entidad) %>%
          rename(Muertes_cum = cum_cases, 
                 Muertes_inc = inc_cases)
        death$entidad[death$entidad=='México'] <- 'Nacional'
        death <- death[c("entidad", "date", "Muertes_cum", "Muertes_inc")]
        
        tletal <- left_join(confirmed, death, by = c("entidad", "date")) 
        tletal$cum_cases <- (tletal$Muertes_cum/tletal$Confirmados_cum)*100
        tletal$new_cases <- (tletal$Muertes_inc/tletal$Confirmados_inc)*100
        tletal <- tletal[c("entidad", "date", "cum_cases", "new_cases")]
        tletal$var_resultado <- "TLetal"
        
        
  remove(confirmed, death, test)
  
  df_outcome <- df_outcome[c("entidad", "date", "var_resultado", "cum_cases", "new_cases", "time_stamp")]
  
  df_outcome <- rbind(df_outcomes, tposa, inci, trate, tletal)
  
  df_outcome$time_stamp <- df_covid_ssa_state$time_stamp[1]

  
  df_outcome$colors[df_outcome$var_resultado == "Confirmados"] <- "#D1495B"
  df_outcome$colors[df_outcome$var_resultado == "Hospitalizados"] <- "#00798C"
  df_outcome$colors[df_outcome$var_resultado == "Intubados"] <- "#30638E" 
  df_outcome$colors[df_outcome$var_resultado == "Muertes"] <- "#003D5B"
  df_outcome$colors[df_outcome$var_resultado == "Pruebas"] <- "#EDAE49" 
  df_outcome$colors[df_outcome$var_resultado == "Síntomas"] <- "#ff715b"
  df_outcome$colors[df_outcome$var_resultado == "UCI"] <- "#432371"
  df_outcome$colors[df_outcome$var_resultado == "TIA"] <- "#eb1ab3"
  df_outcome$colors[df_outcome$var_resultado == "TPosA"] <- "#8d10e0"
  df_outcome$colors[df_outcome$var_resultado == "TLetal"] <- "#0895bd"
  df_outcome$colors[df_outcome$var_resultado == "TPA"] <- "#b3d90b"
  
  
  n_time_stamp <- df_covid_ssa_state$time_stamp[1]
  
  cgroup_cols <- c(clr_darken(paletteer_d("ggsci::category20_d3"), 0.1)[1:(length(select_state))], "gray93")
  
  df_outcome <- df_outcome %>%
    filter(var_resultado %in% outcome & entidad %in% select_state) 
  
  if (type=="cum_cases" & length(select_state) >= 11) {
    pre_ <- "01"
  } else if (type=="new_cases" & length(select_state) >= 11) {
    pre_ <- "02"
  } else if (type=="cum_cases" & length(select_state) < 11) {
    pre_ <- "03"
  } else if (type=="new_cases" & length(select_state) < 11) {
    pre_ <- "04"
  }
  
  prefix <- switch(pre_,
                   "01" = "01",
                   "02" = "02", 
                   "03" = "03",
                   "04" = "04")
  prefix_type <- switch(type,
                        "cum_cases" = "cum",
                        "new_cases" = "inc")
  
  scale_used <- switch(scales,
                       "fixed"  = "fixed",
                       "free_x" = "free_x",
                       "free_y" = "free_y",
                       "free"   = "free")
  
  v_states <- c("Aguascalientes", "Baja California", "Baja California Sur", 
                "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
                "Hidalgo", "Jalisco", "Estado de México", "Michoacán",
                "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
                "Yucatán", "Zacatecas", "ZMVM", "Nacional")
  v_states_ab <- c("AGU", "BC", "BCS",
                   "CA", "COA", "COL", "CHP", "CHH",
                   "CDMX", "DUR", "GUA", "GRO", 
                   "HGO", "JAL", "MEX", "MICH",
                   "MOR", "NAY", "NLE", "OAX", "PUE",
                   "QRO", "QROO", "SLP", "SIN",
                   "SON", "TAB", "TAM", "TLA", "VER",
                   "YUC", "ZAC", "ZMVM", "NAC")
  states_list <- c()
  for (i in 1:34) {
    if (v_states[i] %in% select_state){
      states_list <- paste0(states_list, "_", v_states_ab[i])
    }
  }
  
  if(length(outcome)==1 & length(select_state)<11 & outcome[1]=="Confirmados" & type=="cum_cases"){
    titles <- "Casos confirmados acumulados de COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Hospitalizados" & type=="cum_cases"){
    titles <- "Hospitalizaciones acumuladas por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Intubados" & type=="cum_cases"){
    titles <- "Pacientes intubados acumulados por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Muertes" & type=="cum_cases"){
    titles <- "Muertes acumuladas por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Pruebas" & type=="cum_cases"){
    titles <- "Pruebas acumuladas de COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Síntomas" & type=="cum_cases"){
    titles <- "Casos sintomáticos acumulados de COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="UCI" & type=="cum_cases"){
    titles <- "Casos acumulados en unidad de cuidados intensivos (UCI) por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TI" & type=="cum_cases"){
    titles <- "Tasa de incidencia acumulada para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TPos" & type=="cum_cases"){
    titles <- "Tasa de positividad acumulada para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TLetal" & type=="cum_cases"){
    titles <- "Tasa de fatalidad acumulada para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TP" & type=="cum_cases"){
    titles <- "Tasa de pruebas acumulada para varias entidades"
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Confirmados" & type=="cum_cases"){
    titles <- paste("Casos confirmados acumulados de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Hospitalizados" & type=="cum_cases"){
    titles <- paste("Hospitalizaciones acumuladas por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Intubados" & type=="cum_cases"){
    titles <- paste("Pacientes intubados acumulados por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Muertes" & type=="cum_cases"){
    titles <- paste("Muertes acumuladas por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Pruebas" & type=="cum_cases"){
    titles <- paste("Pruebas acumuladas de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Síntomas" & type=="cum_cases"){
    titles <- paste("Casos sintomáticos acumulados de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="UCI" & type=="cum_cases"){
    titles <-paste("Casos acumulados en unidad de cuidados intensivos (UCI) en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TI" & type=="cum_cases"){
    titles <-paste("Tasa de incidencia acumulada en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TPos" & type=="cum_cases"){
    titles <-paste("Tasa de positividad acumulada en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TLetal" & type=="cum_cases"){
    titles <-paste("Tasa de letalidad acumulada en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TP" & type=="cum_cases"){
    titles <-paste("Tasa de pruebas acumulada en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Confirmados" & type=="new_cases"){
    titles <- "Casos confirmados incidentes de COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Hospitalizados" & type=="new_cases"){
    titles <- "Hospitalizaciones incidentes por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Intubados" & type=="new_cases"){
    titles <- "Pacientes intubados incidentes por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Muertes" & type=="new_cases"){
    titles <- "Muertes incidentes por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Pruebas" & type=="new_cases"){
    titles <- "Pruebas incidentes de COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="Síntomas" & type=="new_cases"){
    titles <- "Casos sintomáticos incidentes de COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="UCI" & type=="new_cases"){
    titles <- "Casos incidentes en unidad de cuidados intensivos (UCI) por COVID-19 para varias entidades"
  } else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TI" & type=="new_cases"){
    titles <- "Tasa de incidencia diaria para varias entidades"
  }else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TPos" & type=="new_cases"){
    titles <- "Tasa de positividad diaria para varias entidades"
  }else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TLetal" & type=="new_cases"){
    titles <- "Tasa de letalidad diaria para varias entidades"
  }else if (length(outcome)==1 & length(select_state)<11 & outcome[1]=="TP" & type=="new_cases"){
    titles <- "Tasa de pruebas diaria para varias entidades"
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Confirmados" & type=="new_cases"){
    titles <- paste("Casos confirmados incidentes de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Hospitalizados" & type=="new_cases"){
    titles <- paste("Hospitalizaciones incidentes por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Intubados" & type=="new_cases"){
    titles <- paste("Pacientes intubados incidentes por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Muertes" & type=="new_cases"){
    titles <- paste("Muertes incidentes por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Pruebas" & type=="new_cases"){
    titles <- paste("Pruebas incidentes de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="Síntomas" & type=="new_cases"){
    titles <- paste("Casos sintomáticos incidentes de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="UCI" & type=="new_cases"){
    titles <-paste("Casos incidentes en unidad de cuidados intensivos (UCI) en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TI" & type=="new_cases"){
    titles <-paste("Tasa de incidencia diaria en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TPos" & type=="new_cases"){
    titles <-paste("Tasa de positividad diaria en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TLetal" & type=="new_cases"){
    titles <-paste("Tasa de letalidad diaria en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)==1 & outcome[1]=="TP" & type=="new_cases"){
    titles <-paste("Tasa de pruebas diaria en ", select_state, ".", sep = "")
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Confirmados" & type=="new_cases"){
    titles <- "Casos confirmados incidentes de COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Hospitalizados" & type=="new_cases"){
    titles <- "Hospitalizaciones incidentes por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Intubados" & type=="new_cases"){
    titles <- "Pacientes intubados incidentes por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Muertes" & type=="new_cases"){
    titles <- "Muertes incidentes por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Pruebas" & type=="new_cases"){
    titles <- "Pruebas incidentes de COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Síntomas" & type=="new_cases"){
    titles <- "Casos sintomáticos incidentes de COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="UCI" & type=="new_cases"){
    titles <- "Casos incidentes en unidad de cuidados intensivos (UCI) por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TI" & type=="new_cases"){
    titles <- "Tasa de incidencia diaria por entidad federativa"
  }else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TPos" & type=="new_cases"){
    titles <- "Tasa de positividad diaria por entidad federativa"
  }else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TLetal" & type=="new_cases"){
    titles <- "Tasa de letalidad diaria por entidad federativa"
  }else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TP" & type=="new_cases"){
    titles <- "Tasa de pruebas diaria por entidad federativa"
  } else if(length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Confirmados" & type=="cum_cases"){
    titles <- "Casos confirmados acumulados de COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Hospitalizados" & type=="cum_cases"){
    titles <- "Hospitalizaciones acumuladas por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Intubados" & type=="cum_cases"){
    titles <- "Pacientes intubados acumulados por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Muertes" & type=="cum_cases"){
    titles <- "Muertes acumuladas por COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Pruebas" & type=="cum_cases"){
    titles <- "Pruebas acumuladas de COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="Síntomas" & type=="cum_cases"){
    titles <- "Casos sintomáticos acumulados de COVID-19 por entidad federativa"
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="UCI" & type=="cum_cases"){
    titles <- "Casos acumulados en unidad de cuidados intensivos (UCI) por COVID-19 por entidad federativa"  
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TI" & type=="cum_cases"){
    titles <- "Tasa de incidencia acumulada por entidad federativa"  
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TPos" & type=="cum_cases"){
    titles <- "Tasa de positividad acumulada por entidad federativa"  
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TLetal" & type=="cum_cases"){
    titles <- "Tasa de letalidad acumulada por entidad federativa"  
  } else if (length(outcome)==1 & length(select_state)>=11 & outcome[1]=="TP" & type=="cum_cases"){
    titles <- "Tasa de pruebas acumulada por entidad federativa"  
  }
  
  
  
  if (length(outcome)==1 & outcome[1]=="Confirmados"){
    color_outcome <- "#D1495B"
  } else if (length(outcome)==1 & outcome[1]=="Hospitalizados") {
    color_outcome <- "#00798C"
  } else if (length(outcome)==1 & outcome[1]=="Intubados") {
    color_outcome <- "#30638E"
  } else if (length(outcome)==1 & outcome[1]=="Muertes") {
    color_outcome <- "#003D5B"
  } else if (length(outcome)==1 & outcome[1]=="Pruebas") {
    color_outcome <- "#EDAE49"
  } else if (length(outcome)==1 & outcome[1]=="Síntomas") {
    color_outcome <- "#ff715b"
  } else if (length(outcome)==1 & outcome[1]=="UCI") {
    color_outcome <- "#432371"
  } else if (length(outcome)==1 & outcome[1]=="TIA") {
    color_outcome <- "#eb1ab3"
  } else if (length(outcome)==1 & outcome[1]=="TPosA") {
    color_outcome <- "#8d10e0"
  } else if (length(outcome)==1 & outcome[1]=="TLetal") {
    color_outcome <- "#0895bd"
  } else if (length(outcome)==1 & outcome[1]=="TPA") {
    color_outcome <- "##b3d90b"
  }

  
  type_ <- switch(type,
                  "cum_cases" = "acumuladas",
                  "new_cases" = "incidentes")
  
  
  if ((length(select_state)*length(outcome))>=14){
    multi = 1
  } else {
    multi = 4-log(length(select_state)*length(outcome))
    if (12 + multi >= 16){
      multi = 4
    } else {
      multi = multi
    }
  }
  
  if (length(select_state) > 1 & length(outcome) > 1){
    
    gg_outcome <- df_outcome %>%
      ggplot(mapping = aes(x = date,
                           y = get(type),
                           color = df_outcome$colors
      )) +
      geom_line(size = .8) +
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      facet_wrap(var_resultado ~ entidad, scales = scale_used) +
      guides(linetype = FALSE,
             color = FALSE) +
      #scale_color_manual(values = cgroup_cols) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         trans = select_trans,
                         breaks = number_ticks(5)) +
      scale_x_date(date_labels = "%d/%m", #Change the format of the date in the x axis
                   breaks = number_ticks(10)) + #Include 10 breaks in the x axis
      labs(title = paste0("Variables ", type_, " de COVID-19 para varias entidades"),
           hjust = 0,
           x = "",
           y = "", 
           caption = paste("Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), 
                           "a las",  format(Sys.time(), "%H:%M"), "horas.",
                           "Fuente: Datos de la Dirección de Epidemiología de la", 
                           "Secretaría de Salud con fecha", n_time_stamp, ".")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold",
                                      size = 16,
                                      family = "Arial"),
            plot.subtitle = element_text(size = 12,
                                         face = "plain", 
                                         family = "Arial"),
            plot.caption = element_text(hjust = 0, 
                                        face = "plain", 
                                        family = "Arial",
                                        size = 8,
                                        colour = "#777777"),
            panel.background = element_rect(fill = "white", 
                                            colour = "white", 
                                            size = 0.15, 
                                            linetype = "solid"),
            panel.grid.major = element_line(size = 0.15, 
                                            linetype = 'solid',
                                            colour = "gray40"), 
            panel.grid.minor = element_line(size = 0.15, 
                                            linetype = 'dotted',
                                            colour = "gray40"), 
            axis.title.x = (element_text(size = 12,
                                         family = "Arial")),
            axis.title.y = (element_text(size =12,
                                         family = "Arial")),
            element_line(linetype = "dotted",
                         colour = "gray99",
                         size = .1),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8, 
                                       family = ""),
            axis.text.y = element_text(size = 8+multi,
                                       family = ""),
            legend.text = element_text(size = 12+multi,
                                       family = ""),
            strip.text = element_text(size = 12+multi,
                                      family = "")) +
      if (save_plot == TRUE) {
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), states_list, ".jpeg"), 
               width = 14, height = 6)}
  } else if (length(select_state) > 1 & length(select_state) < 11 & length(outcome) == 1){
    gg_outcome <- df_outcome %>%
      ggplot(mapping = aes(x = date,
                           y = get(type)
      )) +
      geom_line(size = .8,
                color = color_outcome) +
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      facet_wrap(~ entidad, scales = scale_used) +
      guides(linetype = FALSE,
             color = FALSE) +
      scale_color_manual(values = cgroup_cols) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         breaks = number_ticks(5),
                         trans = select_trans) +
      scale_x_date(date_labels = "%d/%m", #Change the format of the date in the x axis
                   breaks = number_ticks(10)) + #Include 10 breaks in the x axis
      labs(title = titles,
           hjust = 0,
           x = "",
           y = "", 
           caption = paste("Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), 
                           "a las",  format(Sys.time(), "%H:%M"), "horas.",
                           "Fuente: Datos de la Dirección de Epidemiología de la", 
                           "Secretaría de Salud con fecha", n_time_stamp, ".")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold",
                                      size = 16,
                                      family = "Arial"),
            plot.subtitle = element_text(size = 12,
                                         face = "plain", 
                                         family = "Arial"),
            plot.caption = element_text(hjust = 0, 
                                        face = "plain", 
                                        family = "Arial",
                                        size = 8,
                                        colour = "#777777"),
            panel.background = element_rect(fill = "white", 
                                            colour = "white", 
                                            size = 0.15, 
                                            linetype = "solid"),
            panel.grid.major = element_line(size = 0.15, 
                                            linetype = 'solid',
                                            colour = "gray40"), 
            panel.grid.minor = element_line(size = 0.15, 
                                            linetype = 'dotted',
                                            colour = "gray40"), 
            axis.title.x = (element_text(size = 12,
                                         family = "Arial")),
            axis.title.y = (element_text(size =12,
                                         family = "Arial")),
            element_line(linetype = "dotted",
                         colour = "gray99",
                         size = .1),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8, 
                                       family = ""),
            axis.text.y = element_text(size = 8+multi,
                                       family = ""),
            legend.text = element_text(size = 12+multi,
                                       family = ""),
            strip.text = element_text(size = 12+multi,
                                      family = ""))+
      if (save_plot == TRUE){
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), states_list, ".jpeg"), 
               width = 14, height = 6)}
  } else if (length(select_state) >= 11 & length(outcome) == 1){
    
    state_rank <- df_outcome %>%
      group_by(entidad) %>%
      summarise(rank = max(get(type))) %>% 
      arrange(desc(rank))
    
    df_outcome <- df_outcome %>%
      left_join(state_rank, #Add ranking to original data frame
                by = c("entidad" = "entidad")) 
    
    df_outcome <- df_outcome %>% 
      arrange(desc(rank))
    #Fix the state order 
    df_outcome$entidad <- ordered(df_outcome$entidad, 
                                  unique(df_outcome$entidad))
    
    gg_outcome <- df_outcome %>%
      ggplot(mapping = aes(x = date,
                           y = get(type)
      )) +
      geom_line(size = .8,
                color = color_outcome) +
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      facet_wrap(~ entidad, ncol = 8, scales = scale_used) +
      guides(linetype = FALSE,
             color = FALSE) +
      scale_color_manual(values = cgroup_cols) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         breaks = number_ticks(5),
                         trans = select_trans) +
      scale_x_date(date_labels = "%d/%m", #Change the format of the date in the x axis
                   breaks = number_ticks(10)) + #Include 10 breaks in the x axis
      labs(title = titles,
           hjust = 0,
           x = "",
           y = "", 
           caption = paste("Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), 
                           "a las",  format(Sys.time(), "%H:%M"), "horas.",
                           "Fuente: Datos de la Dirección de Epidemiología de la", 
                           "Secretaría de Salud con fecha", n_time_stamp, ".")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold",
                                      size = 16,
                                      family = "Arial"),
            plot.subtitle = element_text(size = 12,
                                         face = "plain", 
                                         family = "Arial"),
            plot.caption = element_text(hjust = 0, 
                                        face = "plain", 
                                        family = "Arial",
                                        size = 8,
                                        colour = "#777777"),
            panel.background = element_rect(fill = "white", 
                                            colour = "white", 
                                            size = 0.15, 
                                            linetype = "solid"),
            panel.grid.major = element_line(size = 0.15, 
                                            linetype = 'solid',
                                            colour = "gray40"), 
            panel.grid.minor = element_line(size = 0.15, 
                                            linetype = 'dotted',
                                            colour = "gray40"), 
            axis.title.x = (element_text(size = 12,
                                         family = "Arial")),
            axis.title.y = (element_text(size =12,
                                         family = "Arial")),
            element_line(linetype = "dotted",
                         colour = "gray99",
                         size = .1),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8, 
                                       family = ""),
            axis.text.y = element_text(size = 8+multi,
                                       family = ""),
            legend.text = element_text(size = 12+multi,
                                       family = ""),
            strip.text = element_text(size = 12+multi,
                                      family = "")) +
      if (save_plot == TRUE){
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), "_", 
                      "by_state", ".jpeg"), 
               width = 14, height = 6)}
  } else if (length(select_state) == 1 & length(outcome) > 1){
    gg_outcome <- df_outcome %>%
      ggplot(mapping = aes(x = date,
                           y = get(type),
                           color = df_outcome$colors
      )) +
      geom_line(size = .8) +
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      facet_wrap(~ var_resultado, scales = scale_used) +
      guides(linetype = FALSE,
             color = FALSE) +
      #scale_color_manual(values = cgroup_cols) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         breaks = number_ticks(5),
                         trans = select_trans) +
      scale_x_date(date_labels = "%d/%m", #Change the format of the date in the x axis
                   breaks = number_ticks(10)) + #Include 10 breaks in the x axis
      labs(title = paste0("Variables ", type_ ," de COVID-19 en ", 
                          select_state),
           hjust = 0,
           x = "",
           y = "", 
           caption = paste("Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), 
                           "a las",  format(Sys.time(), "%H:%M"), "horas.",
                           "Fuente: Datos de la Dirección de Epidemiología de la", 
                           "Secretaría de Salud con fecha", n_time_stamp, ".")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold",
                                      size = 16,
                                      family = "Arial"),
            plot.subtitle = element_text(size = 12,
                                         face = "plain", 
                                         family = "Arial"),
            plot.caption = element_text(hjust = 0, 
                                        face = "plain", 
                                        family = "Arial",
                                        size = 8,
                                        colour = "#777777"),
            panel.background = element_rect(fill = "white", 
                                            colour = "white", 
                                            size = 0.15, 
                                            linetype = "solid"),
            panel.grid.major = element_line(size = 0.15, 
                                            linetype = 'solid',
                                            colour = "gray40"), 
            panel.grid.minor = element_line(size = 0.15, 
                                            linetype = 'dotted',
                                            colour = "gray40"), 
            axis.title.x = (element_text(size = 12,
                                         family = "Arial")),
            axis.title.y = (element_text(size =12,
                                         family = "Arial")),
            element_line(linetype = "dotted",
                         colour = "gray99",
                         size = .1),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8, 
                                       family = ""),
            axis.text.y = element_text(size = 8+multi,
                                       family = ""),
            legend.text = element_text(size = 12+multi,
                                       family = ""),
            strip.text = element_text(size = 12+multi,
                                      family = ""))+
      if (save_plot == TRUE){
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), states_list, ".jpeg"), 
               width = 14, height = 6)}
  } else if (length(select_state) == 1 & length(outcome) == 1){
    gg_outcome <- df_outcome %>%
      ggplot(mapping = aes(x = date,
                           y = get(type)
      )) +
      geom_line(size = .8,
                color = color_outcome) +
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      guides(linetype = FALSE) +
      guides(color = FALSE) +
      scale_color_manual(values = cgroup_cols) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         breaks = number_ticks(5),
                         trans = select_trans) +
      scale_x_date(date_labels = "%d/%m", #Change the format of the date in the x axis
                   breaks = number_ticks(10)) + #Include 10 breaks in the x axis
      labs(title = titles,
           hjust = 0,
           x = "",
           y = "", 
           caption = paste("Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), 
                           "a las",  format(Sys.time(), "%H:%M"), "horas.",
                           "Fuente: Datos de la Dirección de Epidemiología de la", 
                           "Secretaría de Salud con fecha", n_time_stamp, ".")) +
      theme_minimal() +
      theme(plot.title = element_text(face = "bold",
                                      size = 16,
                                      family = "Arial"),
            plot.subtitle = element_text(size = 12,
                                         face = "plain", 
                                         family = "Arial"),
            plot.caption = element_text(hjust = 0, 
                                        face = "plain", 
                                        family = "Arial",
                                        size = 8,
                                        colour = "#777777"),
            panel.background = element_rect(fill = "white", 
                                            colour = "white", 
                                            size = 0.15, 
                                            linetype = "solid"),
            panel.grid.major = element_line(size = 0.15, 
                                            linetype = 'solid',
                                            colour = "gray40"), 
            panel.grid.minor = element_line(size = 0.15, 
                                            linetype = 'dotted',
                                            colour = "gray40"), 
            axis.title.x = (element_text(size = 12,
                                         family = "Arial")),
            axis.title.y = (element_text(size =12,
                                         family = "Arial")),
            element_line(linetype = "dotted",
                         colour = "gray99",
                         size = .1),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       size = 8, 
                                       family = ""),
            axis.text.y = element_text(size = 8+multi,
                                       family = ""),
            legend.text = element_text(size = 12+multi,
                                       family = ""),
            strip.text = element_text(size = 12+multi,
                                      family = ""))+
      if (save_plot == TRUE){
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), states_list, ".jpeg"), 
               width = 14, height = 6)}
  }
  if(print_plot == T){
    print(gg_outcome)
  }
  if(return_plot){
    return(gg_outcome)  
  }
}