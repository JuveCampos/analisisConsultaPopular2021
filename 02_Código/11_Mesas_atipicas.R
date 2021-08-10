# Análisis datos consulta: 
options(scipen = 999)
# Librerias: 
library(tidyverse)
library(sf)
library(leaflet)

# Datos globales ----
datos_globales <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                             "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                             skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric))

# Secciones: 
shape_secciones <- readRDS("01_Datos/Datos Geograficos/shape.rds") %>% 
  mutate(entidad = as.numeric(entidad))

# Mesas atípicas: 
atipicas <- datos_globales %>% 
  mutate(pp = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP)) 

# Numero de mesas arriba de 40%
atipicas %>% 
  filter(pp >= 40) %>% 
  nrow()

# Le juntamos los datos municipales: 
atipicas <- left_join(atipicas %>% mutate(SECCION_SEDE = as.numeric(SECCION_SEDE)), 
                      shape_secciones %>% as_tibble() %>% select(entidad, seccion, nom_mun), 
                      by = c("ID_ENTIDAD"="entidad", 
                             "SECCION_SEDE" = "seccion"
                      )) %>% 
  arrange(-pp)

# Mesas arriba del 90% de participacion: 
mesas_mas_90 <- atipicas %>% 
  filter(pp >= 90)

# Guardamos
mesas_mas_90 %>% 
  openxlsx::write.xlsx("03_Resultados/tablas/mas_90s.xlsx")

# Mesas con mas nulos que SI
mas_nulos <- atipicas %>% 
  filter(NULOS > OPINION_SI)

# Guardamos
mas_nulos %>% 
  openxlsx::write.xlsx("03_Resultados/tablas/mas_nulos.xlsx")

# Mesas con más noes: 
mas_no <- atipicas %>% 
  filter(OPINION_NO > OPINION_SI)

# Guardamos
mas_no %>% 
  openxlsx::write.xlsx("03_Resultados/tablas/mas_noes.xlsx")

