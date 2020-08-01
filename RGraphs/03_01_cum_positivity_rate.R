#****************************************************************# 
# Purpose: Plot positive and negative tests and positivity rate  #
#                                                                #
# Created: June 08, 2020 / 10:12                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' Function to plot outcome(s) for selected states
#' 
#' \code{pos_neg_test} plots the outcome(s) of interest for states of 
#' interest.
#'
#' @param data Dataframe to be used
#' @param select_state A scalar or vector of states for plotting; More than one state should be selected
#' @param select_date A scalar of the cut date
#' @param type A scalar with type of variable for plotting
#' @param print_plot Logical. Prints plot if TRUE
#' @param save_plot Logical. Saves plot if TRUE
#' @param return_plot Logical. Returns plot if TRUE
#' @param select_trans Identity or log
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
#' @export
pos_neg_test <- function(data = "df_covid_ssa_state",
                         select_state = "Ciudad de México",
                         select_date = "2020-06-25",
                         type = "cum_cases",
                         select_trans = c("identity", "log"),
                         print_plot = TRUE,
                         save_plot = FALSE,
                         return_plot = TRUE){
  
  type         <- match.arg(type, several.ok = FALSE)
  select_trans <- match.arg(select_trans, several.ok = FALSE)
  data         <- match.arg(data, several.ok = FALSE)
  
  
  # *****************************************************************************
  #### 01_Arrange data: Cummulative positivity rate ####
  # *****************************************************************************
  
  n_time_stamp <- df_covid_ssa_state$time_stamp[1]
  
  if (select_date > n_time_stamp){
    select_date = n_time_stamp
  }
  
  df_covid_ssa_state<-subset(df_covid_ssa_state, df_covid_ssa_state$entidad %in% select_state)
  
  # Cummulative positivity rate: confirmed cases
  state_confirmed <- subset(df_covid_ssa_state, var_resultado == "Confirmados") #We are focusing on confirmed cases and deaths
  state_confirmed <- state_confirmed %>%
    group_by(entidad) %>%
    mutate(last_case = select_date)
  state_confirmed <- subset(state_confirmed, date == last_case) #Keep only the last entry
  
  # Cummulative positivity rate: tests
  state_test <- subset(df_covid_ssa_state, var_resultado == "Pruebas") #We are focusing on confirmed cases and deaths
  state_test <- state_test %>%
    group_by(entidad) %>%
    mutate(last_case = select_date)
  state_test <- subset(state_test, date == last_case) #Keep only the last entry
  state_test <- state_test[c("entidad", "cum_cases", "last_case")]
  state_test <- state_test %>%
    rename(cum_test = cum_cases, 
           last_test = last_case)
  
  # Cummulative Positivity rate
  state_pos <- left_join(state_confirmed, state_test, by = "entidad") #Counties not available are counties with no confirmed cases
  state_pos$TPosA <- (state_pos$cum_cases/state_pos$cum_test)*100
  state_pos$cum_neg <- state_pos$cum_test-state_pos$cum_cases
  
  # *****************************************************************************
  #### 02_Arrange data for stacked bars ####
  # *****************************************************************************  
  state_pos <- state_pos[c("entidad", "cum_cases", "cum_neg", "TPosA")]
  state_pos_long <- gather(state_pos, type, cases, cum_cases:cum_neg, factor_key=TRUE)
  state_pos_long <- state_pos_long[order(state_pos_long$entidad),]
  state_pos_long$types[state_pos_long$type == "cum_cases"] <- "Pruebas positivas"
  state_pos_long$types[state_pos_long$type == "cum_neg"] <- "Pruebas negativas"
  state_pos_long$types <- fct_rev(state_pos_long$types)
  
  state_pos_long <- subset(state_pos_long, entidad!="Nacional" & entidad!="ZMVM")
  
  state_pos_long$entidad2 <- paste0(state_pos_long$entidad, " ", "(", round(state_pos_long$TPosA, digits = 2), "%)")
  
  state_pos_long$entidad2 <- reorder(state_pos_long$entidad2, state_pos_long$TPosA)
  
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
  gg_outcome <- state_pos_long %>%
    ggplot(aes(fill=types,
               y=cases,
               x=entidad2)) + 
    geom_bar(position="stack",
             stat="identity") +
    coord_flip()+
    scale_fill_manual(values=c("#F4E42D", "#2C878A"))+
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
    labs(title = "Pruebas realizadas y tasa de positividad acumulada de COVID-19 por estado",
         subtitle = paste0("Datos al ", select_date),
         x = "",
         y = "",
         caption = paste(" Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
                         format(Sys.time(), "%H:%M"), "horas.\n",
                         "Fuente: Elaboración propia con datos abiertos de la Dirección de Epidemiología de la Secretaría de Salud, con fecha", n_time_stamp, ".")) +
    if (save_plot == TRUE) {
      ggsave(paste0("figs/", n_time_stamp, "/", "07", "_", "pos_rate", ".jpeg"), 
             width = 14, height = 6)}
  
  if(print_plot == T){
    print(gg_outcome)
  }
  if(return_plot){
    return(gg_outcome)  
  }
}