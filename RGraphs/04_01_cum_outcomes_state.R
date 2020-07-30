#****************************************************************# 
# Purpose: Table cummulative variables for each state            #
#                                                                #
# Created: June 10, 2020 / 14:13                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' \code{cum_outcomes_state} Creates a table with the cummulative outcomes
#'
#' @param data Dataframe to be used
#' @param select_date A scalar for the cut date
#' @param type A scalar with type of variable for plotting
#' @param save_table Logical. If TRUE, saves table.
#' @param return_table Logical. If TRUE, returns table.
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
#' @export
#' 
cum_outcomes_state <- function(data = "df_covid_ssa_state",
                               select_date = "2020-07-21",
                               type = c("cum_cases", "new_cases"),
                               save_table = FALSE,
                               return_table = TRUE){
  
  type <- match.arg(type, several.ok = FALSE)
  
  # *****************************************************************************
  #### 01_Preliminar arguments ####
  # *****************************************************************************
  
  n_time_stamp <- df_covid_ssa_state$time_stamp[1]
  
  export_formattable <- function(f, file, width = "100%", height = NULL, 
                                 background = "white", delay = 0.2)
  {
    w <- as.htmlwidget(f, width = width, height = height)
    path <- html_print(w, background = background, viewer = NULL)
    url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
    webshot(url,
            file = file,
            selector = ".formattable_widget",
            delay = delay)
  }
  
  # *****************************************************************************
  #### 02_Arrange data: Cummulative variables ####
  # *****************************************************************************
  state <-  df_covid_ssa_state
  
  if (type == "cum_cases") {
    types = "cum_cases"
  } else if (type == "new_cases") {
    types = "new_cases"
  }
  
  # Confirmed cases
  confirmed <- subset(state, var_resultado == "Confirmados") #We are focusing on confirmed cases
  confirmed <- confirmed %>%
    group_by(entidad) %>%
    rename(Confirmados = types) %>% 
    mutate(last_case = select_date)
  confirmed <- subset(confirmed, date == last_case) #Keep only the last entry
  confirmed <- confirmed[c("entidad", "Confirmados")]
  
  # Hospitalizations
  hospi <- subset(state, var_resultado == "Hospitalizados")
  hospi <- hospi  %>%
    group_by(entidad) %>%
    rename(Hospitalizados = types) %>%
    mutate(last_case = select_date)
  hospi <- subset(hospi, date == last_case)
  hospi <- hospi[c("entidad", "Hospitalizados")]
  
  # Intubados
  intu <- subset(state, var_resultado == "Intubados")
  intu <- intu  %>%
    group_by(entidad) %>%
    rename(Intubados = types) %>%
    mutate(last_case = select_date)
  intu <- subset(intu, date == last_case)
  intu <- intu[c("entidad", "Intubados")] 
  
  # Deaths
  death <- subset(state, var_resultado == "Muertes")
  death <- death  %>%
    group_by(entidad) %>%
    rename(Muertes = types) %>%
    mutate(last_case = select_date)
  death <- subset(death, date == last_case)
  death <- death[c("entidad", "Muertes")] 
  
  # Tests
  test <- subset(state, var_resultado == "Pruebas") #We are focusing on tests
  test <- test %>%
    group_by(entidad) %>%
    rename(Pruebas = types) %>%
    mutate(last_case = select_date)
  test <- subset(test, date == last_case) #Keep only the last entry
  test$entidad[test$entidad=='México'] <- 'Nacional'
  test <- test[c("entidad", "Pruebas")]
  
  # Symptoms
  sinto <- subset(state, var_resultado == "Síntomas") #We are focusing on tests
  sinto <- sinto %>%
    group_by(entidad) %>%
    rename(Síntomas = types) %>%
    mutate(last_case = select_date)
  sinto <- subset(sinto, date == last_case) #Keep only the last entry
  sinto <- sinto[c("entidad", "Síntomas")]
  
  # UCI
  uci <- subset(state, var_resultado == "UCI") #We are focusing on tests
  uci <- uci %>%
    group_by(entidad) %>%
    rename(UCI = types) %>%
    mutate(last_case = select_date)
  uci <- subset(uci, date == last_case) #Keep only the last entry
  uci <- uci[c("entidad", "UCI")]
  
  
  # Cummulative Positivity rate
  tposa <- left_join(confirmed, test, by = "entidad") 
  tposa$TPosA <- (tposa$Confirmados/tposa$Pruebas)*100
  tposa <- tposa[c("entidad", "TPosA")]
  
  # Incidence rate
  inci <- subset(state, var_resultado == "Confirmados") #We are focusing on confirmed cases
  inci <- inci %>%
    group_by(entidad) %>%
    rename(Confirmados = types) %>% 
    mutate(last_case = select_date)
  inci <- subset(inci, date == last_case) #Keep only the last entry
  inci <- inci[c("entidad", "Confirmados", "population")]
  inci$TIA <- (inci$Confirmados/inci$population)*100000
  inci <- inci[c("entidad", "TIA")]
  
  # Cummulative test rate
  trate <- subset(state, var_resultado == "Pruebas") #We are focusing on tests
  trate <- trate %>%
    group_by(entidad) %>%
    rename(Pruebas = types) %>%
    mutate(last_case = select_date)
  trate <- subset(trate, date == last_case) #Keep only the last entry
  trate <- trate[c("entidad", "Pruebas", "population")]
  trate$TPA <- (trate$Pruebas/trate$population)*100000
  trate$entidad[trate$entidad=='México'] <- 'Nacional'
  trate <- trate[c("entidad", "TPA")]
  
  # Case fatality rate
  tfatality <- left_join(confirmed, death, by = "entidad")
  tfatality$TLetal <- (tfatality$Muertes/tfatality$Confirmados)*100 
  tfatality <- tfatality[c("entidad", "TLetal")]
  
  
  #Merge all outcomes
  outcomes <- left_join(confirmed, death, by = "entidad")
  outcomes <- left_join(outcomes, hospi, by = "entidad")
  outcomes <- left_join(outcomes, intu, by = "entidad")
  outcomes <- left_join(outcomes, sinto, by = "entidad")
  outcomes <- left_join(outcomes, test, by = "entidad")
  outcomes <- left_join(outcomes, uci, by = "entidad")
  outcomes <- left_join(outcomes, inci, by = "entidad")
  outcomes <- left_join(outcomes, trate, by = "entidad")
  outcomes <- left_join(outcomes, tposa, by = "entidad")
  outcomes <- left_join(outcomes, tfatality, by = "entidad")
  
  outcomes <- outcomes %>%
    rename(Entidad =  entidad,
           Sintomáticos = Síntomas)
  
  outcomes$TIA <- round(outcomes$TIA, digits=1)
  outcomes$TPosA <- round(outcomes$TPosA, digits=1)
  outcomes$TPA <- round(outcomes$TPA, digits=1)
  outcomes$TLetal <- round(outcomes$TLetal, digits=1)
  
  # *****************************************************************************
  #### 03_Create table ####
  # ***************************************************************************** 
  table_state <- formattable(outcomes, 
                             align = c("r",rep("c", 7), rep("r", 4)),
                             list(`Entidad` = formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold")), 
                                  `TIA` = color_bar("#F991CC"), 
                                  `TPosA` = color_bar("#E2AFDE"), 
                                  `TPA` = color_bar("#D3C2CE"), 
                                  `TLetal` = color_bar("#D3D2C7")))
  
  if (save_table == TRUE){
    export_formattable(table_state, paste0("figs/", n_time_stamp, "/", "09", "_", "cum_outcomes", ".png"))
  }
  
  if(return_table){
    return(table_state)  
  }
  
}
