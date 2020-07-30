#****************************************************************# 
# Purpose: Map VR Cum/Inc by state                               #
#                                                                #
# Created: June 06, 2020 / 20:12                                 #
# Depends on:                                                    #
#   Author: Manuel Cardona                                       # 
#   e-mail: mcardona@poverty-action.org                          #
#                                                                #
#****************************************************************# 

#' Function to plot outcome(s) for selected states
#' 
#' \code{map_outcome} maps the outcome(s) of interest for states of 
#' interest.
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
map_outcome <- function(select_state = "Ciudad de México",
                        data = "df_covid_ssa_county",
                        outcome = c("TPosA", "TIA", "TPA", "TLetal"), 
                        print_plot = TRUE,
                        save_plot = FALSE,
                        return_plot = TRUE){
  
  outcome      <- match.arg(outcome, several.ok = FALSE)
  data         <- match.arg(data, several.ok = FALSE)
  
  # *****************************************************************************
  #### 02_Load data ####
  # *****************************************************************************
  
  #Shape files
  mexico_map <- read_sf("data/SHP/municipalities/mex_admbnda_adm2_govmex.shp")
  
  n_time_stamp <- df_covid_ssa_county$time_stamp[1]
  
  # *****************************************************************************
  #### 03_Arrange data: Cummulative variables ####
  # *****************************************************************************
  county <-  df_covid_ssa_county
  county$county_id <- case_when(nchar(county$county_id)==4 ~ paste("0", county$county_id, sep=""), 
                                nchar(county$county_id)==5 ~ paste("", county$county_id, sep=""))
  
  # Confirmed cases
  county_confirmed <- subset(county, var_resultado == "Confirmados") #We are focusing on confirmed cases
  county_confirmed <- county_confirmed %>%
    group_by(county_id) %>%
    rename(Confirmados = cum_cases) %>% 
    mutate(last_case = max(date))
  county_confirmed <- subset(county_confirmed, date == last_case) #Keep only the last entry
  county_confirmed <- county_confirmed[c("county_id", "Confirmados")]
  
  # Hospitalizations
  county_hospi <- subset(county, var_resultado == "Hospitalizados")
  county_hospi <- county_hospi  %>%
    group_by(county_id) %>%
    rename(Hospitalizados = cum_cases) %>%
    mutate(last_case = max(date))
  county_hospi <- subset(county_hospi, date == last_case)
  county_hospi <- county_hospi[c("county_id", "Hospitalizados")]
  
  # Intubados
  county_intu <- subset(county, var_resultado == "Intubados")
  county_intu <- county_intu  %>%
    group_by(county_id) %>%
    rename(Intubados = cum_cases) %>%
    mutate(last_case = max(date))
  county_intu <- subset(county_intu, date == last_case)
  county_intu <- county_intu[c("county_id", "Intubados")] 
  
  # Deaths
  county_death <- subset(county, var_resultado == "Muertes")
  county_death <- county_death  %>%
    group_by(county_id) %>%
    rename(Muertes = cum_cases) %>%
    mutate(last_case = max(date))
  county_death <- subset(county_death, date == last_case)
  county_death <- county_death[c("county_id", "Muertes")] 
  
  # Tests
  county_test <- subset(county, var_resultado == "Pruebas") #We are focusing on tests
  county_test <- county_test %>%
    group_by(county_id) %>%
    rename(Pruebas = cum_cases) %>%
    mutate(last_case = max(date))
  county_test <- subset(county_test, date == last_case) #Keep only the last entry
  county_test <- county_test[c("county_id", "municipio", "Pruebas")]
  
  # Symptoms
  county_sinto <- subset(county, var_resultado == "Síntomas") #We are focusing on tests
  county_sinto <- county_sinto %>%
    group_by(county_id) %>%
    rename(Síntomas = cum_cases) %>%
    mutate(last_case = max(date))
  county_sinto <- subset(county_sinto, date == last_case) #Keep only the last entry
  county_sinto <- county_sinto[c("county_id", "Síntomas")]
  
  # UCI
  county_uci <- subset(county, var_resultado == "UCI") #We are focusing on tests
  county_uci <- county_uci %>%
    group_by(county_id) %>%
    rename(UCI = cum_cases) %>%
    mutate(last_case = max(date))
  county_uci <- subset(county_uci, date == last_case) #Keep only the last entry
  county_uci <- county_uci[c("county_id", "UCI")]
  
  
  # Cummulative Positivity rate
  county_tposa <- left_join(county_confirmed, county_test, by = "county_id") #Counties not available are counties with no confirmed cases
  county_tposa$TPosA <- (county_tposa$Confirmados/county_tposa$Pruebas)*100
  county_tposa <- county_tposa[c("county_id", "TPosA")]
  
  # Incidence rate
  county_inci <- subset(county, var_resultado == "Confirmados") #We are focusing on confirmed cases
  county_inci <- county_inci %>%
    group_by(county_id) %>%
    rename(Confirmados = cum_cases) %>% 
    mutate(last_case = max(date))
  county_inci <- subset(county_inci, date == last_case) #Keep only the last entry
  county_inci <- county_inci[c("county_id", "Confirmados", "population")]
  county_inci$TIA <- (county_inci$Confirmados/county_inci$population)*100000
  county_inci <- county_inci[c("county_id", "TIA")]
  
  # Cummulative test rate
  county_trate <- subset(county, var_resultado == "Pruebas") #We are focusing on tests
  county_trate <- county_trate %>%
    group_by(county_id) %>%
    rename(Pruebas = cum_cases) %>%
    mutate(last_case = max(date))
  county_trate <- subset(county_trate, date == last_case) #Keep only the last entry
  county_trate <- county_trate[c("county_id", "Pruebas", "population")]
  county_trate$TPA <- (county_trate$Pruebas/county_trate$population)*100000
  county_trate <- county_trate[c("county_id", "TPA")]
  
  # Case fatality rate
  county_fatality <- left_join(county_confirmed, county_death, by = "county_id")
  county_fatality$TLetal <- (county_fatality$Muertes/county_fatality$Confirmados)*100 
  county_fatality <- county_fatality[c("county_id", "TLetal")]
  
  #Merge all outcomes
  county_outcomes <- left_join(county_confirmed, county_death, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_hospi, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_inci, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_intu, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_sinto, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_test, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_tposa, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_trate, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_uci, by = "county_id")
  county_outcomes <- left_join(county_outcomes, county_fatality, by = "county_id")
  
  county_outcomes <- county_outcomes %>%
    rename(ADM2_PCODE = county_id)
  county_outcomes$ADM2_PCODE <- paste("MX", county_outcomes$ADM2_PCODE, sep="")
  
  
  # *****************************************************************************
  #### 04_Arrange data: Coordinates ####
  # *****************************************************************************
  mexico_map <- mexico_map %>%
    left_join(county_outcomes, by ="ADM2_PCODE")
  
  # *****************************************************************************
  #### 05_Map features  ####
  # ***************************************************************************** 
  
  if (outcome == "TPosA"){
    titles <- paste0("Tasa de positividad acumulada en ", select_state)
    labels <- "Tasa de positividad acumulada (%)"
  } else if (outcome == "TIA"){
    titles <- paste0("Tasa de incidencia acumulada en ", select_state)
    labels <- "Tasa de incidencia acumulada (/100mil habs)"
  } else if (outcome == "TPA"){
    titles <- paste0("Tasa de pruebas acumuladas en ", select_state)
    labels <- "Tasa de pruebas acumulada (/100mil habs)"
  } else if (outcome == "TLetal"){
    titles <- paste0("Tasa de letalidad acumulada en ", select_state)
    labels <- "Tasa de letalidad acumulada (%)"
  }
  
  # *****************************************************************************
  #### 06_Map:  ####
  # *****************************************************************************
  v_states <- c("Aguascalientes", "Baja California", "Baja California Sur", 
                "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
                "Hidalgo", "Jalisco", "Estado de México", "Michoacán",
                "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
                "Yucatán", "Zacatecas")
  states_names <- c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR",
                    "CAMPECHE", "COAHUILA DE ZARAGOZA", "COLIMA", "CHIAPAS", "CHIHUAHUA",
                    "DISTRITO FEDERAL", "DURANGO", "GUANAJUATO", "GUERRERO",
                    "HIDALGO", "JALISCO", "MEXICO", "MICHOACAN DE OCAMPO",
                    "MORELOS", "NAYARIT", "NUEVO LEON", "OAXACA", "PUEBLA", 
                    "QUERETARO DE ARTEAGA", "QUINTANA ROO", "SAN LUIS POTOSI", "SINALOA",
                    "SONORA", "TABASCO", "TAMAULIPAS", "TLAXCALA", "VERACRUZ DE IGNACIO DE LA LLAVE",
                    "YUCATAN", "ZACATECAS")
  v_states_ab <- c("AGU", "BC", "BCS",
                   "CA", "COA", "COL", "CHP", "CHH",
                   "CDMX", "DUR", "GUA", "GRO", 
                   "HGO", "JAL", "MEX", "MICH",
                   "MOR", "NAY", "NLE", "OAX", "PUE",
                   "QRO", "QROO", "SLP", "SIN",
                   "SON", "TAB", "TAM", "TLA", "VER",
                   "YUC", "ZAC")
  states_list <- c()
  for (i in 1:34) {
    if (v_states[i] %in% select_state){
      states_list <- paste0(states_list, "_", v_states_ab[i])
    }
  }
  state_map <- ""
  for (i in 1:32) {
    if (v_states[i] %in% select_state){
      state_map <- append(state_map, states_names[i])
    }
  }
  state_map <- state_map[-1]
  
  map <- mexico_map %>%
    filter(ADM1_ES %in% state_map)
  
  if (outcome == "TPosA") {
    text_plot=paste(map$municipio, "\n", 
                    "TPosA: ", round(map$TPosA, digits = 2), "\n",
                    sep = "")
  } else if (outcome == "TIA") {
    text_plot=paste(map$municipio, "\n", 
                    "TIA: ", round(map$TIA, digits = 2), "\n",
                    sep = "")    
  } else if (outcome == "TPA") {
    text_plot=paste(map$municipio, "\n", 
                    "TPA: ", round(map$TPA, digits = 2), "\n",
                    sep = "")    
  } else if (outcome == "TLetal") {
    text_plot=paste(map$municipio, "\n", 
                    "TLetal: ", round(map$TLetal, digits = 2), "\n",
                    sep = "")    
  }
  
  
  
  
  
  gg_outcome <- map %>%
    ggplot(mapping = aes(text = text_plot)) + 
    geom_sf(aes(fill = get(outcome)),
            size = 0.5) +
    scale_fill_viridis_b(option = "viridis",
                         alpha = 1, 
                         direction = -1) +
    labs(fill = labels) +
    theme_void() +
    theme(plot.title = element_text(face = "bold",
                                    size = 16,
                                    family = "Open Sans"),
          plot.subtitle = element_text(size = 12,
                                       face = "plain", 
                                       family = "Open Sans"),
          plot.caption = element_text(hjust = 0, 
                                      face = "plain", 
                                      family = "",
                                      size = 8,
                                      colour = "#777777"),
          panel.background = element_rect(fill = "white", 
                                          colour = "white", 
                                          size = 0.15, 
                                          linetype = "solid"),
          legend.text = element_text(size = 8,
                                     family = ""),
          legend.position = "bottom")+
    labs(title = titles,
         caption = paste(" Elaborado por @PADeCI1 el", format(Sys.Date(), "%d/%m/%Y"), "a las",  
                         format(Sys.time(), "%H:%M"), "horas.\n",
                         "Fuente: Elaboraci?n propia con datos abiertos de la Direcci?n de Epidemiolog?a de la Secretar?a de Salud, con fecha", n_time_stamp, "."))+
    if (save_plot == TRUE) {
      ggsave(paste0("figs/", n_time_stamp, "/", "12", "_", outcome, "_", 
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