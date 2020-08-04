#****************************************************************# 
# Purpose: Plot cummulative fatality rate                        #
#                                                                #
# Created: June 24, 2020 / 02:12                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' Function to plot outcome(s) for selected states
#' 
#' \code{cum_fatality} plots the cummulative fatality rate by state
#'
#' @param data Dataframe to be used
#' @param select_state A scalar or vector of states for plotting; More than one state should be selected
#' @param select_date A scalar of the cut date
#' @param print_plot Logical. Prints plot if TRUE
#' @param save_plot Logical. Saves plot if TRUE
#' @param return_plot Logical. Returns plot if TRUE
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
cum_fatality <- function(data = "df_covid_ssa_state",
                         select_state = "Ciudad de México",
                         select_date = "2020-06-25",
                         print_plot = TRUE,
                         save_plot = FALSE,
                         return_plot = TRUE){
  
  data <- match.arg(data, several.ok = FALSE)
  
  # *****************************************************************************
  #### 01_Arrange data: Cummulative case fatality rate ####
  # *****************************************************************************
  
  n_time_stamp <- df_covid_ssa_state$time_stamp[1]
  
  if (select_date > n_time_stamp){
    select_date = n_time_stamp
  }
  
  df_covid_ssa_state<-subset(df_covid_ssa_state, df_covid_ssa_state$entidad %in% select_state)
  
  # Case fatality rate: confirmed cases
  state_confirmed <- subset(df_covid_ssa_state, var_resultado == "Confirmados") #We are focusing on confirmed cases and deaths
  state_confirmed <- state_confirmed %>%
    group_by(entidad) %>%
    mutate(last_case = select_date)
  state_confirmed <- subset(state_confirmed, date == last_case) #Keep only the last entry
  
  # Cummulative positivity rate: deaths
  state_death <- subset(df_covid_ssa_state, var_resultado == "Muertes") #We are focusing on confirmed cases and deaths
  state_death <- state_death %>%
    group_by(entidad) %>%
    mutate(last_case = select_date)
  state_death <- subset(state_death, date == last_case) #Keep only the last entry
  state_death <- state_death[c("entidad", "cum_cases", "last_case")]
  state_death <- state_death %>%
    rename(cum_death = cum_cases, 
           last_test = last_case)
  
  # Case fatality rate
  state_fat <- left_join(state_confirmed, state_death, by = "entidad") 
  state_fat$CFatR <- (state_fat$cum_death/state_fat$cum_cases)*100
  state_fat$alive <- state_fat$cum_cases-state_fat$cum_death
  
  # *****************************************************************************
  #### 02_Arrange data for stacked bars ####
  # *****************************************************************************  
  state_fat <- state_fat[c("entidad", "cum_death", "alive", "CFatR")]
  state_fat_long <- gather(state_fat, type, cases, cum_death:alive, factor_key=TRUE)
  state_fat_long <- state_fat_long[order(state_fat_long$entidad),]
  state_fat_long$types[state_fat_long$type == "cum_death"] <- "Fallecimientos"
  state_fat_long$types[state_fat_long$type == "alive"] <- "Sobrevivientes"
  state_fat_long$types <- fct_rev(state_fat_long$types)
  
  state_fat_long <- subset(state_fat_long, entidad!="Nacional" & entidad!="ZMVM")
  
  state_fat_long$entidad2 <- paste0(state_fat_long$entidad, " ", "(", round(state_fat_long$CFatR, digits = 2), "%)")
  
  state_fat_long$entidad2 <- reorder(state_fat_long$entidad2, state_fat_long$CFatR)
  
  if (length(select_state)==32) {
    multi = 1
  } else {
    multi = 5-log(5+length(select_state))
    if (10 + multi >= 15) {
      multi = 5
    } else {
      multi = multi
    }
  }
  
  # *****************************************************************************
  #### 03_Stacked bars ####
  # ***************************************************************************** 
  gg_outcome <- state_fat_long %>%
    ggplot(aes(fill=types,
               y=cases,
               x=entidad2)) + 
    geom_bar(position="stack",
             stat="identity") +
    coord_flip()+
    scale_fill_manual(values=c("#1dad8e", "#215c8a"))+
    scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                       trans = "identity",
                       breaks = number_ticks(10)) +
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
          axis.text.y = element_text(size = 10+multi,
                                     family = ""),
          legend.text = element_text(size = 12,
                                     family = ""),
          legend.title = element_blank(),
          legend.position = "bottom") +
    labs(title = "Fallecimientos acumulados y tasa de letalidad acumulada de COVID-19 por estado",
         subtitle = paste0("Datos al ", select_date),
         x = "",
         y = "",
         caption = paste(" Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
                         format(Sys.time(), "%H:%M"), "horas.\n",
                         "Fuente: Elaboración propia con datos abiertos de la Dirección de Epidemiología de la Secretaría de Salud, con fecha", n_time_stamp, ".")) +
    if (save_plot == TRUE) {
      ggsave(paste0("figs/", n_time_stamp, "/", "08", "_", "cum_fatality", ".jpeg"), 
             width = 14, height = 6)}
  
  if(print_plot == T){
    print(gg_outcome)
  }
  if(return_plot){
    return(gg_outcome)  
  }
}