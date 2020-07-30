#****************************************************************# 
# Purpose: Plot VR Cum/Inc cases                                 #
#                                                                #
# Created: June 02, 2020 / 20:12                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' Function to plot outcome(s) for selected states
#' 
#' \code{cum_inc_nofacet} plots the outcome(s) of interest for states of 
#' interest.
#'
#' @param data Dataframe to be used
#' @param select_state A scalar or vector of states for plotting; More than one state should be selected
#' @param outcome A scalar or vector of covid19-related outcomes for plotting
#' @param type A scalar with type of variable for plotting
#' @param print_plot Logical. Prints plot if TRUE
#' @param save_plot Logical. Saves plot if TRUE
#' @param return_plot Logical. Returns plot if TRUE
#' @param select_trans Identity or log
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
#' @export
cum_inc_nofacet <- function(select_state = "Ciudad de México",
                            data = "df_covid_ssa_state",
                            outcome = c("Confirmados", "Hospitalizados", 
                                        "Intubados", "Muertes", "Pruebas", 
                                        "Síntomas", "UCI"), 
                            type = c("cum_cases", "new_cases"),
                            select_trans = c("identity", "log"),
                            print_plot = TRUE,
                            save_plot = FALSE,
                            return_plot = TRUE){
  
  outcome      <- match.arg(outcome, several.ok = FALSE)
  type         <- match.arg(type, several.ok = FALSE)
  select_trans <- match.arg(select_trans, several.ok = FALSE)
  data         <- match.arg(data, several.ok = FALSE)
  
  
  ## "Not in" operator: This function will allow us to exclude irrelevant series from the graph
  "%nin%" <- function(x, y) {
    return( !(x %in% y) )
  }
  
  df_outcome <- df_covid_ssa_state %>%
    group_by(entidad, var_resultado) %>%
    mutate(inc_cases = c(1, diff(cum_cases)),
           last_case = max(time_cases))
  
  
  df_outcome$end_label <- case_when(df_outcome$time_cases == df_outcome$last_case ~ df_outcome$entidad) 
  
  
  df_outcome$entidad2 <- paste("01", df_outcome$entidad, sep="")
  df_outcome$entidad2 <- case_when(df_outcome$entidad %in% select_state ~ df_outcome$entidad,
                                   df_outcome$entidad %nin% select_state ~ df_outcome$entidad2)
  
  
  n_time_stamp <- df_covid_ssa_state$time_stamp[1]
  
  prefix <- switch(type,
                   "cum_cases" = "05",
                   "new_cases" = "06")
  prefix_type <- switch(type,
                        "cum_cases" = "cum",
                        "new_cases" = "inc")
  
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
  v_states_colors <- c("#4C191B", "#963D5A", "#C589E8",
                       "#E3DAFF", "#ECFFF8", "#048BA8", "#16DB93", "#EFEA5A",
                       "#F29E4C", "#49416D", "#DBCFB0", "#BFC8AD",
                       "#90B494", "#718F94", "#545775", "#CE84AD",
                       "#CE96A6", "#D1A7A0", "#9EB7E5", "#648DE5", "#1C3738",
                       "#4D4847", "#8BAAAD", "#23B5D3", "#75ABBC",
                       "#A2AEBB", "#E3E7AF", "#7D1D3F", "#512500", "#8A4F7D",
                       "#73AB84", "#99D19C", "#E83151", "#4B7F52")
  
  colors_list <- ""
  for (i in 1:34) {
    if (v_states[i] %in% select_state){
      colors_list <- append(colors_list, v_states_colors[i])
    }
  }
  colors_list <- colors_list[-1]
  
  if(length(select_state)!=1 & outcome[1]=="Confirmados" & type=="cum_cases"){
    titles <- "Casos confirmados acumulados de COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Hospitalizados" & type=="cum_cases"){
    titles <- "Hospitalizaciones acumuladas por COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Intubados" & type=="cum_cases"){
    titles <- "Pacientes intubados acumulados por COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Muertes" & type=="cum_cases"){
    titles <- "Muertes acumuladas por COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Pruebas" & type=="cum_cases"){
    titles <- "Pruebas acumuladas de COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Síntomas" & type=="cum_cases"){
    titles <- "Casos sintomáticos acumulados de COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="UCI" & type=="cum_cases"){
    titles <- "Casos acumulados en unidad de cuidados intensivos (UCI) por COVID-19 para varias entidades"
  } else if (length(select_state)==1 & outcome[1]=="Confirmados" & type=="cum_cases"){
    titles <- paste("Casos confirmados acumulados de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Hospitalizados" & type=="cum_cases"){
    titles <- paste("Hospitalizaciones acumuladas por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Intubados" & type=="cum_cases"){
    titles <- paste("Pacientes intubados acumulados por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Muertes" & type=="cum_cases"){
    titles <- paste("Muertes acumuladas por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Pruebas" & type=="cum_cases"){
    titles <- paste("Pruebas acumuladas de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Síntomas" & type=="cum_cases"){
    titles <- paste("Casos sintomáticos acumulados de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="UCI" & type=="cum_cases"){
    titles <-paste("Casos acumulados en unidad de cuidados intensivos (UCI) en ", select_state, ".", sep = "")
  } else if (length(select_state)!=1 & outcome[1]=="Confirmados" & type=="new_cases"){
    titles <- "Casos confirmados incidentes de COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Hospitalizados" & type=="new_cases"){
    titles <- "Hospitalizaciones incidentes por COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Intubados" & type=="new_cases"){
    titles <- "Pacientes intubados incidentes por COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Muertes" & type=="new_cases"){
    titles <- "Muertes incidentes por COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Pruebas" & type=="new_cases"){
    titles <- "Pruebas incidentes de COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="Síntomas" & type=="new_cases"){
    titles <- "Casos sintomáticos incidentes de COVID-19 para varias entidades"
  } else if (length(select_state)!=1 & outcome[1]=="UCI" & type=="new_cases"){
    titles <- "Casos incidentes en unidad de cuidados intensivos (UCI) por COVID-19 para varias entidades"
  } else if (length(select_state)==1 & outcome[1]=="Confirmados" & type=="new_cases"){
    titles <- paste("Casos confirmados incidentes de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Hospitalizados" & type=="new_cases"){
    titles <- paste("Hospitalizaciones incidentes por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Intubados" & type=="new_cases"){
    titles <- paste("Pacientes intubados incidentes por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Muertes" & type=="new_cases"){
    titles <- paste("Muertes incidentes por COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Pruebas" & type=="new_cases"){
    titles <- paste("Pruebas incidentes de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="Síntomas" & type=="new_cases"){
    titles <- paste("Casos sintomáticos incidentes de COVID-19 en ", select_state, ".", sep = "")
  } else if (length(select_state)==1 & outcome[1]=="UCI" & type=="new_cases"){
    titles <-paste("Casos incidentes en unidad de cuidados intensivos (UCI) en ", select_state, ".", sep = "")
  }
  
  type_ <- switch(type,
                  "cum_cases" = "acumulados",
                  "new_cases" = "incidentes")
  
  
  if (length(select_state) > 1){
    gg_outcome <- df_outcome %>% #DF to be used
      filter(var_resultado==outcome & entidad %in% select_state)%>% 
      ggplot(mapping = aes(x = date, #X variable
                           y = get(type), #Y variable
                           color = entidad, #Each line to be plotted will have a different color
                           label = end_label, #Only the last point of each line will have a lable
                           group = entidad2, 
      )) +
      geom_line(size = .8)+ #Geom line so the data points are connected
      guides(linetype = FALSE)+ 
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_text(check_overlap = TRUE,
                size = 4.5) +
      #geom_text_repel(nudge_x = 1, # This is to avoid labels to overlap
      #               segment.color = NA,
      #              xlim = NA)+
      guides(color = FALSE) + 
      scale_color_manual(values = colors_list) + #Specify that we will be using the colors from our vector
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         trans = select_trans,
                         breaks = number_ticks(5)) +
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
                                       size = 10, 
                                       family = ""),
            axis.text.y = element_text(size = 12,
                                       family = ""),
            legend.text = element_text(size = 12,
                                       family = "")) +
      if (save_plot == TRUE) {
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), 
                      states_list, ".jpeg"), 
               width = 14, height = 6)}}
  
  else if (length(select_state) == 1){
    gg_outcome <- df_outcome %>% #DF to be used
      filter(var_resultado==outcome & entidad == select_state)%>% 
      ggplot(mapping = aes(x = date, #X variable
                           y = get(type), #Y variable
                           color = entidad, #Each line to be plotted will have a different color
                           label = end_label, #Only the last point of each line will have a lable
                           group = entidad2, 
      )) +
      geom_line(size = 1)+ #Geom line so the data points are connected
      guides(linetype = FALSE)+ 
      geom_vline(xintercept = as.numeric(as.Date("2020-03-23")), #Vertical line indicating the begining of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_vline(xintercept = as.numeric(as.Date("2020-05-31")), #Vertical line indicating the ending of social distancing
                 color = "gray20",
                 linetype = "dashed",
                 size = 0.25) +
      geom_text(check_overlap = TRUE,
                size = 4.5) +
      #geom_text_repel(nudge_x = 1, # This is to avoid labels to overlap
      #               segment.color = NA,
      #              xlim = NA)+
      guides(color = FALSE) + 
      scale_color_manual(values = colors_list) + #Specify that we will be using the colors from our vector
      scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                         trans = select_trans,
                         breaks = number_ticks(5)) +
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
                                       size = 10, 
                                       family = ""),
            axis.text.y = element_text(size = 12,
                                       family = ""),
            legend.text = element_text(size = 14,
                                       family = "")) +
      if (save_plot == TRUE) {
        ggsave(paste0("figs/", n_time_stamp, "/", prefix, "_", prefix_type, "_", paste(outcome ,collapse="-"), 
                      states_list, ".jpeg"), 
               width = 14, height = 6)}}
  
  if(print_plot == T){
    print(gg_outcome)
  }
  if(return_plot == TRUE){
    return(gg_outcome)  
  }
}