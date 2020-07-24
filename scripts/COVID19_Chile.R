install.packages("RCurl")
library(RCurl)
library(tidyverse)
library(lubridate)
library(readxl)
library(here)


#===========================================
#             COVID19 data
#==========================================

# Source:
#    Ministery of Science: http://www.minciencia.gob.cl/covid19
#    Github account: https://github.com/MinCiencia/Datos-COVID19

### Confirmed COVID19  cases by council

urlcovid <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto6/bulk/data.csv"

cases<- read_csv(url(urlcovid)) %>%
  arrange(`Region ID`, Comuna, Fecha)

#select
cases2 <- cases %>%
  group_by(Comuna, month_year = month(Fecha)) %>%
  filter(Fecha == max(Fecha)) %>% # to keep the rows with the latest day of the month (ommit this line to have all dates)
  mutate(total_cases = as.numeric(`Casos Confirmados`),
         rate = as.numeric(Tasa)) %>%
  rename(population = Poblacion,
         date = Fecha,
         region_code = `Region ID`,
         region_name = Region,
         county_code = `Provincia ID`,
         county_name = Provincia,
         council_code = `Comuna ID`,
         council_name = Comuna)

# new confirmed cases by month

cases2<- cases2 %>% 
  group_by(council_name) %>% 
  mutate(new_cases = total_cases - lag(total_cases, default = total_cases[1])) %>%
  ungroup()



### COVID19 deaths

urldeaths<- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto38/CasosFallecidosPorComuna.csv"

C19_death<- read_csv(url(urldeaths)) %>%
  rename(population = Poblacion,
         region_code = `Codigo region`,
         region_name = Region,
         council_code = `Codigo comuna`,
         council_name = Comuna)


C19_death <- C19_death %>%
  pivot_longer(-c(1:5), names_to = "date", values_to = "counts") %>%
  group_by(council_name, month_year = month(date)) %>%
  filter(date == max(date))  # to keep the rows with the latest day of the month

# Deaths by month, new cases are the new cases added to the previous month
C19_death<- C19_death %>% 
  group_by(council_name) %>% 
  mutate(new_cases = counts - lag(counts, 
                                  default = counts[1])) %>%
  ungroup()




### all death since 2010 (daily deaths)

urlalldeaths <- "https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto32/Defunciones.csv"

all_deaths <- read_csv(url(urlalldeaths))


#============================
# Population data
#===========================

# Deprivation

# Source: Ministerio de Desarrollo Social y familia (Social Security Ministery)
# website: http://observatorio.ministeriodesarrollosocial.gob.cl/indicadores/datos_pobreza_comunal.php

#Multidimensional deprivation (sort of SIMD)

url <- "http://observatorio.ministeriodesarrollosocial.gob.cl/documentos/PLANILLA_Estimaciones_comunales_tasa_pobreza_por_ingresos_multidimensional_2017.xlsx"

download.file(url,
              destfile = "data/deprivation.xlsx",
              mode = "wb")

deprivation <- read_excel("data/deprivation.xlsx",
                          sheet = 2,  range = "A4:E348", 
                          col_names = c("council_code", "region_name", "council_name", 
                                        "total_depriv.", "prop. depriv."))

View(deprivation)


### population by age

# Source: Census 2017: http://www.censo2017.cl/descargue-aqui-resultados-de-comunas/

urlage <- "http://www.censo2017.cl/wp-content/uploads/2017/12/Cantidad-de-Personas-por-Sexo-y-Edad.xlsx"

download.file(urlage,
              destfile = "data/pop_age.xlsx",
              mode = "wb")

pop_age <- read_excel("data/pop_age.xlsx",
                      sheet = 2,  range = "C26:L7636", 
                      col_names = c("region_name", "region_code", "county_name", "county_code", "council_name", 
                                    "council_code.", "age_group", "male", "female", "total"))
pop_age <- pop_age %>%
  filter(!str_detect(age_group, "Total")) # delete rows that summarise total population by council

View(pop_age)
