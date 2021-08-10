
options(scipen = 999)
# Librerias: 
library(tidyverse)
library(sf)
library(leaflet)

# Datos Sección -----
datos_seccion <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                            "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                            skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE)) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  group_by(ID_ENTIDAD, ENTIDAD, SECCION_SEDE) %>% 
  summarise(TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T), 
            LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
            OPINION_SI = sum(OPINION_SI, na.rm = T), 
            OPINION_NO = sum(OPINION_NO, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pp = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP))

# Secciones: 
shape_secciones <- readRDS("01_Datos/Datos Geograficos/shape.rds") %>% 
  mutate(entidad = as.numeric(entidad))

# Datos municipio ----
bd_map <- left_join(shape_secciones, datos_seccion, 
                    by = c("seccion" = "SECCION_SEDE", 
                           "entidad" = "ID_ENTIDAD")) %>% 
  filter(!is.na(ENTIDAD))

datos_municipio <- bd_map %>% 
  as_tibble() %>% 
  group_by(CVE_INE,nom_mun, ENTIDAD) %>% 
  summarise(TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T), 
            LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
            OPINION_SI = sum(OPINION_SI, na.rm = T), 
            OPINION_NO = sum(OPINION_NO, na.rm = T)) %>% 
  filter(!is.na(ENTIDAD)) %>% 
  mutate(pp = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP))

# datos_municipio <- datos_municipio %>% 
#   mutate(quintil_lista_nominal = case_when(between(LISTA_NOMINAL_MRCP, quintiles_pob["0%"], quintiles_pob["20%"]) ~ str_c("Grupo 1: Menos votantes\n ", "[", quintiles_pob["0%"], "-", quintiles_pob["20%"], "]" ), 
#                                            between(LISTA_NOMINAL_MRCP, quintiles_pob["20%"], quintiles_pob["40%"]) ~ str_c("Grupo 2: \n", "[", quintiles_pob["20%"], "-", quintiles_pob["40%"], "]" ), 
#                                            between(LISTA_NOMINAL_MRCP, quintiles_pob["40%"], quintiles_pob["60%"]) ~ str_c("Grupo 3: \n", "[", quintiles_pob["40%"], "-", quintiles_pob["60%"], "]" ), 
#                                            between(LISTA_NOMINAL_MRCP, quintiles_pob["60%"], quintiles_pob["80%"]) ~ str_c("Grupo 4: \n", "[", quintiles_pob["60%"], "-", quintiles_pob["80%"], "]" ),  
#                                            between(LISTA_NOMINAL_MRCP, quintiles_pob["80%"], quintiles_pob["100%"]) ~ str_c("Grupo 5: Más votantes\n ", "[", quintiles_pob["80%"], "-", quintiles_pob["100%"], "]" ), 
#   ))

# Municipios: 
shape_muni <- readRDS("01_Datos/Datos Geograficos/mapa_muni.rds") %>% 
  select(entidad, municipio, CVE_INE)

# Municipios por participación: ----
mapx <- left_join(shape_muni, 
                  datos_municipio)

# Mapa por quintiles de participación: 
quintiles_part <- quantile(na.omit(mapx$pp), seq(0,1,by = 0.2))

mapx <- mapx %>% 
  mutate(quintil_part = case_when(between(pp, quintiles_part["0%"], quintiles_part["20%"]) ~ "1, Menor participación [0%-3.3%]", 
                                  between(pp, quintiles_part["20%"], quintiles_part["40%"]) ~ "2 [3.3%-5.4%]", 
                                  between(pp, quintiles_part["40%"], quintiles_part["60%"]) ~ "3 [5.4%-7.6%]", 
                                  between(pp, quintiles_part["60%"], quintiles_part["80%"]) ~ "4 [7.6%-10.9%]", 
                                  between(pp, quintiles_part["80%"], quintiles_part["100%"]) ~ "5, Mayor participación [10.9%-78.7%]"))

pal_quint <- colorFactor(domain = mapx$quintil_part, 
                         palette = "YlOrRd")

leaflet(mapx) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = pal_quint(mapx$quintil_part), 
              color = "black", 
              weight = 0, 
              # popup = popup,
              # label = label,
              fillOpacity = 1) %>% 
  addLegend(title = "Grupos municipales por participación",
            opacity = 1,
            labFormat = labelFormat(prefix = "Grupo "),
            pal = pal_quint, values = mapx$quintil_part, position = "bottomright")
