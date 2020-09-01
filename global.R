##************************************************************************
## Script Name: Global environment
## Purpose:  Load the main objects used for the shiny app       
## 
##
## Created: 2020-07-30                               
## Authors: Mariana Fernández 
##          
## GitHub: marianafdz465  
##
##
##************************************************************************

# Libraries ---------------------------------------------------------------

#For graphs
library(httr)
library(dampack)
library(devtools)
library(dplyr)
library(forcats)
library(formattable)
library(ggplot2)
library(ggrepel)
library(haven)
library(htmltools)
library(knitr)
library(lubridate)
library(magrittr)
library(paletteer)
library(plotly)
library(prismatic)
library(sf)
library(shiny)
library(spData)
library(stats)
library(tidyr)
library(webshot)
library(repmis)

# For ShinyApp
library("shinythemes")
library("shinydashboard")
library("shinycssloaders")
library("shinymaterial")
library("shinyWidgets")
library("reactable")
library("sparkline")
library("shinydashboardPlus")

# Load source graphs ------------------------------------------------------

source("RGraphs/01_cum_inc.R", local = TRUE)
source("RGraphs/02_cum_inc_nofacet.R", local = TRUE)
source("RGraphs/03_01_cum_positivity_rate.R", local = TRUE)
source("RGraphs/03_02_cum_fatality_rate.R", local = TRUE)
source("RGraphs/04_01_cum_outcomes_state.R", local = TRUE)
source("RGraphs/04_02_cum_outcomes_county.R", local = TRUE)
source("RGraphs/05_doublingtime_by_state.R", local = TRUE)
source("RGraphs/06_map_outcome.R", local = TRUE)

# Load data set -----------------------------------------------------------

source_data("https://github.com/PADeCI/covid19-mx-data/blob/master/data/state/df_covid_ssa_state.Rdata?raw=true")
source_data("https://github.com/PADeCI/covid19-mx-data/blob/master/data/county/df_covid_ssa_county.Rdata?raw=true")

general_data <- df_covid_ssa_state


# Vectors for data --------------------------------------------------------

# Vector with names for all states with ZMVM
v_states_zmvm <- c("Aguascalientes", "Baja California", "Baja California Sur", 
              "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
              "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
              "Hidalgo", "Jalisco", "Estado de México", "Michoacán",
              "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
              "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
              "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
              "Yucatán", "Zacatecas", "Nacional", "ZMVM")

# Vector with names for all states without ZMV
v_states <- c("Aguascalientes", "Baja California", "Baja California Sur", 
                   "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                   "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
                   "Hidalgo", "Jalisco", "Estado de México", "Michoacán",
                   "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                   "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
                   "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
                   "Yucatán", "Zacatecas")

# Vector with output names 
v_outcome <-  c("Confirmados", "Hospitalizados", "Intubado", 
                "Muertes", "Pruebas", "Síntomas", "UCI")
# Vector with output names just for 01_cum_inc

v_outcome_cum <-  c("Confirmados", "Hospitalizados", "Intubado", 
                    "Muertes", "Pruebas", "Síntomas", "UCI", "TPos", "TI", "TP", "TLetal")


#Vector type cases
v_type <- c("cum_cases", "new_cases")

#Vector of scales
v_scale <- c("identity", "log")
v_scaleAux <- c("fixed", "free", "free_y", "free_x")

#Vector of rates
v_rate <-  c("TPosA", "TIA", "TPA", "TLetal")

# Label for date update --------------------------------------------------------

n_time_stamp <- df_covid_ssa_state$time_stamp[1]
n_time_stamp_county <- df_covid_ssa_county$time_stamp[1]


# Series ------------------------------------------------------------------

# Confirmed cases
confirmed <- subset(general_data, var_resultado == "Confirmados") 
confirmed <- confirmed %>%
  group_by(entidad) %>%
  rename(Confirmados = cum_cases) %>% 
  mutate(last_case = max(date))
confirmed <- subset(confirmed, date == last_case) #Keep only the last entry
confirmed <- confirmed[c("entidad", "Confirmados")]

#Confirmados Nacional
confirmedNacional <- filter(confirmed, 
                            entidad == "Nacional")
confirmedNacional <- confirmedNacional$Confirmados 

confirmadosVector <- dplyr::pull(confirmed, "Confirmados")

#Muertes
death <- subset(general_data, var_resultado == "Muertes")
death <- death  %>%
  group_by(entidad) %>%
  rename(Muertes = cum_cases) %>%
  mutate(last_case = max(date))
death <- subset(death, date == last_case)
death <- death[c("entidad", "Muertes")] 

#Muertes Nacional
deathNacional <- filter(death, 
                        entidad == "Nacional")
deathNacional <- deathNacional$Muertes

defuncionesVector <- dplyr::pull(death, "Muertes")

#Hospitalizados
hospi <- subset(general_data, var_resultado == "Hospitalizados")
hospi <- hospi  %>%
  group_by(entidad) %>%
  rename(Hospitalizados = cum_cases) %>%
  mutate(last_case = max(date))
hospi <- subset(hospi, date == last_case)
hospi <- hospi[c("entidad", "Hospitalizados")]

#Hospitalizados Nacional
hospiNacional <- filter(hospi, 
                        entidad == "Nacional")
hospiNacional <- hospiNacional$Hospitalizados

hospitalizadosVector <- dplyr::pull(hospi, "Hospitalizados")

#UCI
uci <- subset(general_data, var_resultado == "UCI") #We are focusing on tests
uci <- uci %>%
  group_by(entidad) %>%
  rename(UCI = cum_cases) %>%
  mutate(last_case = max(date))
uci <- subset(uci, date == last_case) #Keep only the last entry
uci <- uci[c("entidad", "UCI")]

#Uci Nacional
uciNacional <- filter(uci, 
                      entidad == "Nacional")
uciNacional <- uciNacional$UCI


uciVector <- dplyr::pull(uci, "UCI")

# Intubados
intu <- subset(general_data, var_resultado == "Intubados")
intu <- intu  %>%
  group_by(entidad) %>%
  rename(Intubados = cum_cases) %>%
  mutate(last_case = max(date))
intu <- subset(intu, date == last_case)
intu <- intu[c("entidad", "Intubados")] 

#Intubados Nacional
intuNacional <- filter(intu, 
                       entidad == "Nacional")
intuNacional <- intuNacional$Intubados

intubadosVector <- dplyr::pull(intu, "Intubados")

#Pruebas 
test <- subset(general_data, var_resultado == "Pruebas") #We are focusing on tests
test <- test %>%
  group_by(entidad) %>%
  rename(Pruebas = cum_cases) %>%
  mutate(last_case = max(date))
test <- subset(test, date == last_case) #Keep only the last entry
test <- test[c("entidad", "Pruebas")]

#Pruebas Nacional
testNacional <- filter(test, 
                       entidad == "México")
testNacional <- testNacional$Pruebas

pruebasVector <- dplyr::pull(test, "Pruebas")

# 
# map_outcome(select_state = "Aguascalientes",
#             data = "df_covid_ssa_county",
#             outcome = "TPosA",
#             print_plot = TRUE,
#             save_plot = FALSE,
#             return_plot = TRUE)

# doubling_times(select_state = c("Nacional", "ZMVM", "Ciudad de México",
#                                 "Aguascalientes", "Hidalgo", "Baja California"),
#                #data = "df_covid_ssa_state",
#                outcome = "Muertes",
#                print_plot = TRUE,
#                save_plot = FALSE,
#                return_plot = TRUE)


