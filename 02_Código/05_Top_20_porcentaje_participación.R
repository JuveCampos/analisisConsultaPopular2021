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

part <- mapx %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  arrange(-LISTA_NOMINAL_MRCP) 

dx <- mapx %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  arrange(-pp) %>% 
  head(20) 

dx %>% 
  ggplot(aes(x = reorder(str_c("<b>", nom_mun, "</b>", "<br>", str_to_title(ENTIDAD)), pp),
             y = pp)) + 
  geom_col(fill = pal_colores(dx$pp), alpha = 0.5) + 
  geom_hline(yintercept = 40, linetype = 2, color = "blue", size = 1, alpha = 0.5) +
  geom_text(aes(label = str_c(round(pp, 2), "%")), 
            fontface = "bold", 
            color = pal_colores(dx$pp),
            family = "Poppins", 
            size = 4, hjust = -0.1, 
            show.legend = FALSE) +
  coord_flip() + 
  labs(x = "", y = "% Participación", title = "Top 20 municipios con mayor porcentaje de participación<br>en la Consulta Popular 2021", 
       caption = "<b>Fuente: </b> INE. Base de datos de resultados de la consulta popular.<br>Color de barra igual que el utilizado para el municipio en el mapa de participación.") +
  scale_y_continuous(expand = expansion(c(0,0.2)), 
                     breaks = c(0,20,40,60,80), 
                     label = scales::comma_format(suffix = "%")) + 
  theme(plot.title = element_markdown(size = 20,
                                      color = "brown",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 13),
        plot.caption = element_markdown(color = "gray20"),
        text = element_text(family = "Poppins", color = "black"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_markdown(family = "Poppins",
                                       color = "black",
                                       size = 9),
        axis.text.x = element_text(family = "Poppins",
                                   color = "black",
                                   face = "bold",
                                   size = 9),
        axis.line.x = element_line(color = "black")) + 
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5, 
                             nrow = 1))

