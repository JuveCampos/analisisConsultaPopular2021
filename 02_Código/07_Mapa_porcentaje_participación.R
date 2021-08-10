
# Opciones: ----
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)
library(ggtext)

# Datos:
datos <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
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
# Por su peso, esta bd esta disponible en este enlace: 
# https://drive.google.com/drive/folders/1BuinRpztvvra7Bo_TF0LIjudIFL33Hd6?usp=sharing
# Una vez descargado, colocale en la carpeta 01_Datos/Datos Geograficos/ para que funcione todo. 
shape <- readRDS("01_Datos/Datos Geograficos/shape.rds") %>% 
  mutate(entidad = as.numeric(entidad))

# Municipios: 
shape_muni <- readRDS("01_Datos/Datos Geograficos/mapa_muni.rds") %>% 
  select(entidad, municipio, CVE_INE)

# Estados: 
shape_ent <- read_sf("01_Datos/Datos Geograficos/entidades.geojson") %>% 
  mutate(NOMBRE = str_replace_all(NOMBRE, c("YUCATAN" = "YUCATÁN", 
                                            "SAN LUIS POTOSI" = "SAN LUIS POTOSÍ", 
                                            "QUERETARO" = "QUERÉTARO",
                                            "NUEVO LEON" =  "NUEVO LEÓN", 
                                            "MICHOACAN" = "MICHOACÁN", 
                                            "MEXICO"="MÉXICO")))

# Mergeamos: 
bd_map <- left_join(shape, datos, 
                    by = c("seccion" = "SECCION_SEDE", 
                           "entidad" = "ID_ENTIDAD"))

datos_mapa <- bd_map %>% 
  as_tibble() %>% 
  group_by(CVE_INE,nom_mun, ENTIDAD) %>% 
  summarise(TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T), 
            LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
            OPINION_SI = sum(OPINION_SI, na.rm = T), 
            OPINION_NO = sum(OPINION_NO, na.rm = T)) %>% 
  filter(!is.na(ENTIDAD)) %>% 
  mutate(pp = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP))


mapx <- left_join(shape_muni, datos_mapa)

# Paleta de colores: 
pal_colores <- colorNumeric(palette = c("white", "orange", "red"), 
                            domain = c(0:80))

# Mapa: 
mp <- leaflet(mapx) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = pal_colores(mapx$pp), 
              color = "black", 
              weight = 0, 
              fillOpacity = 1) %>% 
  addPolygons(data = shape_ent, 
              fill = NA,
              opacity = 1,
              weight = 2, 
              color = "black") %>% 
  addLegend(title = "Porcentaje de<br>participación<br><b style = 'color:brown;'>Consulta<br>Popular 2021</b>",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1,
            pal = pal_colores, values = mapx$pp, position = "bottomright")

# Imprimimos mapa
mp
