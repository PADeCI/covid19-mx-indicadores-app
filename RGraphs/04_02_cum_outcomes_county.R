#****************************************************************# 
# Purpose: Table cummulative variables for each county of a      #
#          specific state                                        #
#                                                                #
# Created: June 11, 2020 / 02:13                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' \code{cum_outcomes_county} Creates a table with the cummulative outcomes for a specific state by county
#'
#' @param data Dataframe to be used
#' @param select_date A scalar for the cut date
#' @param type A scalar with type of variable for plotting
#' @param select_state Chooses the state to use
#' @param save_table Logical. If TRUE, saves table.
#' @param return_table Logical. If TRUE, returns table.
#' @return 
#' A list with a ggplot object for outcome(s) and state(s) of interest.
#' @export
#' 
cum_outcomes_county <- function(data = "df_covid_ssa_county",
                                select_date = "2020-07-21",
                                type = c("cum_cases", "new_cases"),
                                select_state = "Ciudad de México",
                                save_table = FALSE,
                                return_table = TRUE){
  
  type         <- match.arg(type, several.ok = FALSE)
  
  # *****************************************************************************
  #### 01_Preliminar arguments ####
  # *****************************************************************************
  
  n_time_stamp <- df_covid_ssa_county$time_stamp[1]
  
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
  
  # *****************************************************************************
  #### 02_Arrange data: Cummulative variables ####
  # *****************************************************************************
  county <-  subset(df_covid_ssa_county, entidad == select_state & municipio != "No Especificado")
  
  if (type == "cum_cases") {
    types = "cum_cases"
  } else if (type == "new_cases") {
    types = "new_cases"
  }
  
  # Confirmed cases
  confirmed <- subset(county, var_resultado == "Confirmados") #We are focusing on confirmed cases
  confirmed <- confirmed %>%
    group_by(municipio) %>%
    rename(Confirmados = types) %>% 
    mutate(last_case = select_date)
  confirmed <- subset(confirmed, date == last_case) #Keep only the last entry
  confirmed <- confirmed[c("municipio", "Confirmados")]
  
  # Hospitalizations
  hospi <- subset(county, var_resultado == "Hospitalizados")
  hospi <- hospi  %>%
    group_by(municipio) %>%
    rename(Hospitalizados = types) %>%
    mutate(last_case = select_date)
  hospi <- subset(hospi, date == last_case)
  hospi <- hospi[c("municipio", "Hospitalizados")]
  
  # Intubados
  intu <- subset(county, var_resultado == "Intubados")
  intu <- intu  %>%
    group_by(municipio) %>%
    rename(Intubados = types) %>%
    mutate(last_case = select_date)
  intu <- subset(intu, date == last_case)
  intu <- intu[c("municipio", "Intubados")] 
  
  # Deaths
  death <- subset(county, var_resultado == "Muertes")
  death <- death  %>%
    group_by(municipio) %>%
    rename(Muertes = types) %>%
    mutate(last_case = select_date)
  death <- subset(death, date == last_case)
  death <- death[c("municipio", "Muertes")] 
  
  # Tests
  test <- subset(county, var_resultado == "Pruebas") #We are focusing on tests
  test <- test %>%
    group_by(municipio) %>%
    rename(Pruebas = types) %>%
    mutate(last_case = select_date)
  test <- subset(test, date == last_case) #Keep only the last entry
  test <- test[c("municipio", "Pruebas")]
  
  # Symptoms
  sinto <- subset(county, var_resultado == "Síntomas") #We are focusing on tests
  sinto <- sinto %>%
    group_by(municipio) %>%
    rename(Síntomas = types) %>%
    mutate(last_case = select_date)
  sinto <- subset(sinto, date == last_case) #Keep only the last entry
  sinto <- sinto[c("municipio", "Síntomas")]
  
  # UCI
  uci <- subset(county, var_resultado == "UCI") #We are focusing on tests
  uci <- uci %>%
    group_by(municipio) %>%
    rename(UCI = types) %>%
    mutate(last_case = select_date)
  uci <- subset(uci, date == last_case) #Keep only the last entry
  uci <- uci[c("municipio", "UCI")]
  
  
  # Cummulative Positivity rate
  tposa <- left_join(confirmed, test, by = "municipio") 
  tposa$TPosA <- (tposa$Confirmados/tposa$Pruebas)*100
  tposa <- tposa[c("municipio", "TPosA")]
  
  # Incidence rate
  inci <- subset(county, var_resultado == "Confirmados") #We are focusing on confirmed cases
  inci <- inci %>%
    group_by(municipio) %>%
    rename(Confirmados = types) %>% 
    mutate(last_case = select_date)
  inci <- subset(inci, date == last_case) #Keep only the last entry
  inci <- inci[c("municipio", "Confirmados", "population")]
  inci$TIA <- (inci$Confirmados/inci$population)*100000
  inci <- inci[c("municipio", "TIA")]
  
  # Cummulative test rate
  trate <- subset(county, var_resultado == "Pruebas") #We are focusing on tests
  trate <- trate %>%
    group_by(municipio) %>%
    rename(Pruebas = types) %>%
    mutate(last_case = select_date)
  trate <- subset(trate, date == last_case) #Keep only the last entry
  trate <- trate[c("municipio", "Pruebas", "population")]
  trate$TPA <- (trate$Pruebas/trate$population)*100000
  trate <- trate[c("municipio", "TPA")]
  
  # Case fatality rate
  tfatality <- left_join(confirmed, death, by = "municipio")
  tfatality$TLetal <- (tfatality$Muertes/tfatality$Confirmados)*100 
  tfatality <- tfatality[c("municipio", "TLetal")]
  
  
  #Merge all outcomes
  outcomes <- left_join(confirmed, death, by = "municipio")
  outcomes <- left_join(outcomes, hospi, by = "municipio")
  outcomes <- left_join(outcomes, intu, by = "municipio")
  outcomes <- left_join(outcomes, sinto, by = "municipio")
  outcomes <- left_join(outcomes, test, by = "municipio")
  outcomes <- left_join(outcomes, uci, by = "municipio")
  outcomes <- left_join(outcomes, inci, by = "municipio")
  outcomes <- left_join(outcomes, trate, by = "municipio")
  outcomes <- left_join(outcomes, tposa, by = "municipio")
  outcomes <- left_join(outcomes, tfatality, by = "municipio")
  
  outcomes <- outcomes %>%
    rename(Municipio =  municipio,
           Sintomáticos = Síntomas)
  
  outcomes$TIA <- round(outcomes$TIA, digits=1)
  outcomes$TPosA <- round(outcomes$TPosA, digits=1)
  outcomes$TPA <- round(outcomes$TPA, digits=1)
  outcomes$TLetal <- round(outcomes$TLetal, digits=1)
  
  # *****************************************************************************
  #### 03_Create table ####
  # ***************************************************************************** 
  table_county <- formattable(outcomes, 
                              align = c("r",rep("c", 7), rep("r", 4)),
                              list(`Municipio` = formatter("span", style = ~ formattable::style(color = "grey", font.weight = "bold")), 
                                   `TIA` = color_bar("#F991CC"), 
                                   `TPosA` = color_bar("#E2AFDE"), 
                                   `TPA` = color_bar("#D3C2CE"), 
                                   `TLetal` = color_bar("#D3D2C7")))
  
  if (save_table == TRUE){
    export_formattable(table_county, paste0("figs/", n_time_stamp, "/", "10", "_", "cum_outcomes", states_list, ".png"))
  }
  
  if(return_table){
    return(table_county)  
  }
}