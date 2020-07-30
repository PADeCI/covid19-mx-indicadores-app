#****************************************************************# 
# Purpose: Plot Outcome and Doubling Times                       #
#                                                                #
# Created: June 09, 2020 / 23:46                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' Function to plot outcome(s) for selected states 
#' 
#' \code{doubling_times} plots one outcome of interest and relative doubling times
#'
#' @param data Dataframe to be used
#' @param select_state A scalar or vector of states for plotting; More than one state should be selected
#' @param outcome A scalar or vector of covid19-related outcomes for plotting
#' @param print_plot Logical. Prints plot if TRUE
#' @param save_plot Logical. Saves plot if TRUE
#' @param return_plot Logical. Returns plot if TRUE
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
#' @export
doubling_times <- function(select_state = "Ciudad de México",
                           data = "df_covid_ssa_state",
                           outcome = c("Confirmados", "Hospitalizados", 
                                       "Intubados", "Muertes", "Pruebas", 
                                       "Síntomas", "UCI"), 
                           print_plot = TRUE,
                           save_plot = FALSE,
                           return_plot = TRUE){
  
  outcome      <- match.arg(outcome, several.ok = FALSE)
  #select_state <- match.arg(select_state, several.ok = TRUE)
  data         <- match.arg(data, several.ok = FALSE)
  
  # *****************************************************************************
  #### 01_Preliminar functions ####
  # *****************************************************************************  
  
  ## "Not in" operator: This function will allow us to exclude irrelevant series from the graph
  "%nin%" <- function(x, y) {
    return( !(x %in% y) )
  }
  
  as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} #To be sure all the elements that will be plotted are numeric type
  
  n_time_stamp <- df_covid_ssa_state$time_stamp[1]
  
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
  
  colors_list <- c("gray20", "gray40", "gray60", "gray80")
  for (i in 1:34) {
    if (v_states[i] %in% select_state){
      colors_list <- append(colors_list, v_states_colors[i])
    }
  }
  
  if(outcome == "Confirmados" & length(select_state)==1){
    titles <- paste0("Casos confirmados acumulados de COVID-19 y tiempos de duplicación en ", select_state)
  } else if (outcome == "Hospitalizados" & length(select_state)==1){
    titles <- paste0("Hospitalizaciones acumuladas por COVID-19 y tiempos de duplicación en ", select_state)
  }  else if (outcome == "Intubados" & length(select_state)==1){
    titles <- paste0("Pacientes acumulados intubados por COVID-19 y tiempos de duplicación en ", select_state)
  }  else if (outcome == "Muertes" & length(select_state)==1){
    titles <- paste0("Muertes acumuladas por COVID-19 y tiempos de duplicación en ", select_state)
  }  else if (outcome == "Pruebas" & length(select_state)==1){
    titles <-  paste0("Pruebas acumuladas de COVID-19 y tiempos de duplicación en ", select_state)
  }  else if (outcome == "Síntomas" & length(select_state)==1){
    titles <- paste0("Casos sintomáticos acumulados de COVID-19 y tiempos de duplicación en ", select_state)
  } else if (outcome == "UCI" & length(select_state)==1){
    titles <- paste0("Casos acumulados en Unidad de Cuidados Intensivos (UCI) por COVID-19 y tiempos de duplicación en ", select_state)
  } else if (outcome == "Confirmados" & length(select_state)>1){
    titles <- "Casos confirmados acumulados de COVID-19 y tiempos de duplicación para varios estados"
  } else if (outcome == "Hospitalizados" & length(select_state)>1){
    titles <- "Hospitalizaciones acumuladas por COVID-19 y tiempos de duplicación para varios estados"
  } else if (outcome == "Intubados" & length(select_state)>1){
    titles <- "Pacientes acumulados intubados por COVID-19 y tiempos de duplicación para varios estados"
  } else if (outcome == "Muertes" & length(select_state)>1){
    titles <- "Muertes acumuladas por COVID-19 y tiempos de duplicación para varios estados"
  } else if (outcome == "Pruebas" & length(select_state)>1){
    titles <- "Pruebas acumuladas de COVID-19 y tiempos de duplicación para varios estados"
  } else if (outcome == "Síntomas" & length(select_state)>1){
    titles <- "Casos sintomáticos acumulados de COVID-19 y tiempos de duplicación para varios estados"
  } else if (outcome == "UCI" & length(select_state)>1){
    titles <- "Casos acumulados en Unidad de Cuidados Intensivos (UCI) por COVID-19 y tiempos de duplicación para varios estados"
  }
  
  if (outcome == "Confirmados"){
    y_axis <- "Casos confirmados acumulados"
  } else if (outcome == "Hospitalizados"){
    y_axis <- "Hospitalizaciones acumuladas"
  } else if (outcome == "Intubados"){
    y_axis <- "Pacientes intubados acumulados"
  } else if (outcome == "Muertes"){
    y_axis <- "Muertes acumuladas"
  } else if (outcome == "Pruebas"){
    y_axis <- "Pruebas acumuladas"
  } else if (outcome == "Síntomas"){
    y_axis <- "Casos sintomáticos acumulados"
  } else if (outcome == "UCI"){
    y_axis <- "Casos acumulados en UCI"
  } 
  
  
  # *****************************************************************************
  #### 02_Arrange_data: Outcome ####
  # *****************************************************************************
  state <- df_covid_ssa_state
  state <- state[c(4,5,8,10,12,13)] #Keep only relevant variables
  state <- subset(state, var_resultado == outcome) #Keep only relevant observations
  
  
  # *****************************************************************************
  #### 03_Arrange_data: Doubling times ####
  # ***************************************************************************** 
  #Doubling times (4 days)
  x<-2^seq(1, 30, 1) #Create a vector of doubling times.
  re<-rep(NA_character_,3) #This will leave empty spaces as needed for the vector
  cum_cases<-c(1, re, x[1], re, x[2], re, x[3], re, x[4],
               re, x[5], re, x[6], re, x[7], re, x[8], re,
               x[9], re, x[10], re, x[11], re, x[12], re,
               x[13], re, x[14], re, x[15], re, x[16], re,
               x[17]) 
  date<-seq(as.Date(min(state$date)),as.Date(max(state$date)), by = "days") #Vector of dates starting from the first date and finishing in the last
  date<-date[1:length(cum_cases)] #Restrict to the relevant dates
  pais<-rep("México", length(date)) #Vector for the country variable
  entidad<-rep("4 días", length(date)) #Vector for the name of the series
  var_resultado<-rep("Confirmados", length(entidad)) #Vector for the var_resultado variable
  time_cases<-seq(1,length(var_resultado)) #Vector for the time_cases variale
  data_4<-data.frame(pais, entidad, var_resultado, date, cum_cases, time_cases) #Arrange vectors as data frame
  
  data_4<-data_4[complete.cases(data_4),] #Keep only the elements of the DF that are complete (i.e. doubling times)
  data_4$cum_cases<-as.numeric.factor(data_4$cum_cases) #To start from the 10th case (this still needs to be improved)
  data_4$cum_cases[data_4$cum_cases==16]<-10
  data_4$cum_cases[data_4$cum_cases==32]<-20
  data_4$cum_cases[data_4$cum_cases==64]<-40
  data_4$cum_cases[data_4$cum_cases==128]<-80
  data_4$cum_cases[data_4$cum_cases==256]<-160
  data_4$cum_cases[data_4$cum_cases==512]<-320
  data_4$cum_cases[data_4$cum_cases==1024]<-640
  data_4$cum_cases[data_4$cum_cases==2048]<-1280
  data_4$cum_cases[data_4$cum_cases==4096]<-2560
  data_4$cum_cases[data_4$cum_cases==8192]<-5120
  data_4$cum_cases[data_4$cum_cases==16384]<-10240
  data_4$cum_cases[data_4$cum_cases==32768]<-20480
  data_4$cum_cases[data_4$cum_cases==65536]<-40960
  data_4$cum_cases[data_4$cum_cases==131072]<-81920
  state<-rbind(state, data_4) #Append to the original data frame
  
  #Doubling times (2 days)
  x<-2^seq(1, 30, 1)
  re<-rep(NA_character_)
  cum_cases<-c(1, re, x[1], re, x[2], re, x[3], re, x[4],
               re, x[5], re, x[6], re, x[7], re, x[8], re,
               x[9], re, x[10], re, x[11], re, x[12], re,
               x[13], re, x[14], re, x[15], re, x[16], re,
               x[17])
  date<-seq(as.Date(min(state$date)),as.Date(max(state$date)), by = "days")
  date<-date[1:length(cum_cases)]
  pais<-rep("México", length(date))
  entidad<-rep("2 días", length(date))
  var_resultado<-rep("Confirmados", length(entidad))
  time_cases<-seq(1,length(var_resultado))
  data_2<-data.frame(pais, entidad, var_resultado, date, cum_cases, time_cases)
  
  data_2<-data_2[complete.cases(data_2),]
  data_2$cum_cases<-as.numeric.factor(data_2$cum_cases)
  data_2$cum_cases[data_2$cum_cases==16]<-10
  data_2$cum_cases[data_2$cum_cases==32]<-20
  data_2$cum_cases[data_2$cum_cases==64]<-40
  data_2$cum_cases[data_2$cum_cases==128]<-80
  data_2$cum_cases[data_2$cum_cases==256]<-160
  data_2$cum_cases[data_2$cum_cases==512]<-320
  data_2$cum_cases[data_2$cum_cases==1024]<-640
  data_2$cum_cases[data_2$cum_cases==2048]<-1280
  data_2$cum_cases[data_2$cum_cases==4096]<-2560
  data_2$cum_cases[data_2$cum_cases==8192]<-5120
  data_2$cum_cases[data_2$cum_cases==16384]<-10240
  data_2$cum_cases[data_2$cum_cases==32768]<-20480
  data_2$cum_cases[data_2$cum_cases==65536]<-40960
  data_2$cum_cases[data_2$cum_cases==131072]<-81920
  state<-rbind(state, data_2)
  
  
  #Doubling times (5 days)
  x<-2^seq(1, 30, 1)
  re<-rep(NA_character_,4)
  cum_cases<-c(1, re, x[1], re, x[2], re, x[3], re, x[4],
               re, x[5], re, x[6], re, x[7], re, x[8], re,
               x[9], re, x[10], re, x[11], re, x[12], re,
               x[13], re, x[14], re, x[15], re, x[16], re,
               x[17])
  date<-seq(as.Date(min(state$date)),as.Date(max(state$date)), by = "days")
  date<-date[1:length(cum_cases)]
  pais<-rep("México", length(date))
  entidad<-rep("5 días", length(date))
  var_resultado<-rep("Confirmados", length(entidad))
  time_cases<-seq(1,length(var_resultado))
  data_5<-data.frame(pais, entidad, var_resultado, date, cum_cases, time_cases)
  
  data_5<-data_5[complete.cases(data_5),]
  data_5$cum_cases<-as.numeric.factor(data_5$cum_cases)
  data_5$cum_cases[data_5$cum_cases==16]<-10
  data_5$cum_cases[data_5$cum_cases==32]<-20
  data_5$cum_cases[data_5$cum_cases==64]<-40
  data_5$cum_cases[data_5$cum_cases==128]<-80
  data_5$cum_cases[data_5$cum_cases==256]<-160
  data_5$cum_cases[data_5$cum_cases==512]<-320
  data_5$cum_cases[data_5$cum_cases==1024]<-640
  data_5$cum_cases[data_5$cum_cases==2048]<-1280
  data_5$cum_cases[data_5$cum_cases==4096]<-2560
  data_5$cum_cases[data_5$cum_cases==8192]<-5120
  data_5$cum_cases[data_5$cum_cases==16384]<-10240
  data_5$cum_cases[data_5$cum_cases==32768]<-20480
  data_5$cum_cases[data_5$cum_cases==65536]<-40960
  data_5$cum_cases[data_5$cum_cases==131072]<-81920
  state<-rbind(state, data_5)
  
  #Doubling times (7 days)
  x<-2^seq(1, 30, 1)
  re<-rep(NA_character_,6)
  cum_cases<-c(1, re, x[1], re, x[2], re, x[3], re, x[4],
               re, x[5], re, x[6], re, x[7], re, x[8], re,
               x[9], re, x[10], re, x[11], re, x[12], re,
               x[13], re, x[14], re, x[15], re, x[16], re,
               x[17])
  date<-seq(as.Date(min(state$date)),as.Date(max(state$date)), by = "days")
  date<-date[1:length(cum_cases)]
  pais<-rep("México", length(date))
  entidad<-rep("7 días", length(date))
  var_resultado<-rep("Confirmados", length(entidad))
  time_cases<-seq(1,length(var_resultado))
  data_7<-data.frame(pais, entidad, var_resultado, date, cum_cases, time_cases)
  
  data_7<-data_7[complete.cases(data_7),]
  data_7$cum_cases<-as.numeric.factor(data_7$cum_cases)
  data_7$cum_cases[data_7$cum_cases==16]<-10
  data_7$cum_cases[data_7$cum_cases==32]<-20
  data_7$cum_cases[data_7$cum_cases==64]<-40
  data_7$cum_cases[data_7$cum_cases==128]<-80
  data_7$cum_cases[data_7$cum_cases==256]<-160
  data_7$cum_cases[data_7$cum_cases==512]<-320
  data_7$cum_cases[data_7$cum_cases==1024]<-640
  data_7$cum_cases[data_7$cum_cases==2048]<-1280
  data_7$cum_cases[data_7$cum_cases==4096]<-2560
  data_7$cum_cases[data_7$cum_cases==8192]<-5120
  data_7$cum_cases[data_7$cum_cases==16384]<-10240
  data_7$cum_cases[data_7$cum_cases==32768]<-20480
  data_7$cum_cases[data_7$cum_cases==65536]<-40960
  data_7$cum_cases[data_7$cum_cases==131072]<-81920
  state<-rbind(state, data_7)
  
  # *****************************************************************************
  #### 04_Arrange_data: Graph specifics  ####
  # *****************************************************************************  
  
  
  ## States to highlight
  
  #Specify in the following vector all the series that the user wants to plot
  focus_cn <- append(c("2 días", "4 días", "5 días", "7 días"), select_state)
  
  state <- state %>%
    group_by(entidad) %>%
    mutate(last_case = max(time_cases)) #Specifies the only point that will have label in the graph
  state$end_label <- case_when(state$entidad %in% focus_cn & state$time_cases == state$last_case ~ state$entidad) 
  
  state$entidad2 <- paste("01", state$entidad, sep="") #Given that the lines are ploted in an aplphabetically inverse order;
  state$entidad2 <- case_when(state$entidad %in% focus_cn ~ state$entidad, #This will allow us to let the desired lines in front;
                              state$entidad %nin% focus_cn ~ state$entidad2) #And all other lines in the bottom.
  
  
  # *****************************************************************************
  #### 05_Plot  ####
  # *****************************************************************************    
  
  gg_outcome <- state %>% #DF to be used
    filter(cum_cases >= 10 & entidad %in% focus_cn) %>% 
    dplyr::mutate(days_elapsed = date - min(date), #"Create" variable days have elapsed from the 10th case
                  cgroup = case_when(entidad %in% focus_cn ~ entidad,
                                     TRUE ~ "Otros Estados"))%>%  #Groups all other states in a "Otros Estados" category
    ggplot(mapping = aes(x = days_elapsed, #X variable
                         y = cum_cases, #Y variable
                         color = cgroup, #Each line to be plotted will have a different color
                         label = end_label, #Only the last point of each line will have a lable
                         group = entidad2,
                         text = paste( #This is only relevant for the interactive graph
                           entidad, "\n", #This is the taxt that will appear when you move the cursor above a line
                           "Días desde el 10mo caso confirmado: ", days_elapsed, "\n",
                           "Casos acumulados: ", cum_cases,
                           sep = ""
                         ))) +
    geom_line(size = .5)+ #Geom line so the data points are connected
    guides(linetype = FALSE)+ 
    geom_text_repel(nudge_x = 1, # This is to avoid labels to overlap
                    segment.color = NA,
                    xlim = NA)+
    guides(color = FALSE) + 
    scale_color_manual(values = colors_list) + #Specify that we will be using the colors from our vector
    scale_y_continuous(labels = scales::comma_format(accuracy = 1), #Features of the y axis
                       #limits = c(0, max(state$cum_cases)),
                       breaks = 2^seq(4, 19, 1),#This scale needs to be implemented if we are using a log scale
                       trans = "log") + #Transform y scale to ln
    scale_x_continuous(limits = c(NA, NA), #Features for the x axis
                       breaks = seq(0,max(state$time_cases), 5))+ 
    labs(title = titles, #Text options
         hjust = 0,
         x = "Días desde el décimo caso confirmado",
         y = "Casos confirmados acumulados", 
         caption = paste(" Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
                         format(Sys.time(), "%H:%M"), "horas.\n",
                         "Fuente: Elaboración propia con datos abiertos de la Dirección de Epidemiología de",
                         "la Secretaría de Salud, con fecha", n_time_stamp, "."))+
    theme_minimal()+ #This is the basic theme to be used
    theme(plot.title = element_text(face = "bold", 
                                    size = 16,
                                    family = "Open Sans"),
          plot.subtitle = element_text(size = 12,
                                       face = "plain", 
                                       family = "Open Sans"),
          plot.caption = element_text(hjust = 0, 
                                      face = "plain", 
                                      family = "Open Sans",
                                      size = 8,
                                      colour = "#777777"),
          panel.background = element_rect(fill = "white", 
                                          colour = "white", 
                                          size = 0.15, 
                                          linetype = "solid"),
          axis.title.x = (element_text(size = 12,
                                       family = "Open Sans")),
          axis.title.y = (element_text(size =12,
                                       family = "Open Sans")),
          element_line(linetype = "dotted",
                       colour = "gray99",
                       size = .1),
          axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     size = 8, 
                                     family = "Open Sans"),
          axis.text.y = element_text(size = 8,
                                     family = "Open Sans"),
          legend.text = element_text(size = 12,
                                     family = "Open Sans"))+
    if (save_plot == TRUE){
      ggsave(paste0("figs/", n_time_stamp, "/", "11", "_", outcome, "_", 
                    states_list, ".jpeg"), 
             width = 14, height = 6)}
  
  doubling_plot<-ggplotly(gg_outcome, tooltip = "text")%>%
    layout(margin = list(l = 50, r = 50, t = 60, b = 150), ##bottom margin in pixels
           font = "Arial",
           annotations = 
             list(x = 0, y = -0.35, #position of text adjust as needed 
                  text = paste(" Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), 
                               "a las",  format(Sys.time(), "%H:%M"), "horas.\n",
                               "Fuente: Datos de la Dirección de Epidemiología de la", 
                               "Secretaría de Salud con fecha", n_time_stamp, "."),
                  showarrow = F, xref='paper', yref='paper', 
                  align = "left",
                  xanchor='left', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=10, color="gray70", hjust = 0))
    )
  
  if(print_plot == T){
    print(gg_outcome)
  }
  if(return_plot){
    return(doubling_plot)  
  }
}