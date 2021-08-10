
# Opciones: ----
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)
library(ggtext)

# Datos ---

# Tema: 
tema_juve <- theme(plot.title = element_markdown(size = 20,
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
                   axis.text = element_text(family = "Poppins",
                                            color = "black",
                                            face = "bold",
                                            size = 9),
                   axis.line.x = element_line(color = "black"))

# Catalogo entidades 
cat_ent <- tibble::tribble(
  ~cve_ent,                  ~ent,
  "00",            "Nacional",
  "01",      "Aguascalientes",
  "02",     "Baja California",
  "03", "Baja California Sur",
  "04",            "Campeche",
  "05",            "Coahuila",
  "06",              "Colima",
  "07",             "Chiapas",
  "08",           "Chihuahua",
  "09",    "Ciudad de México",
  "10",             "Durango",
  "11",          "Guanajuato",
  "12",            "Guerrero",
  "13",             "Hidalgo",
  "14",             "Jalisco",
  "15",              "México",
  "16",           "Michoacán",
  "17",             "Morelos",
  "18",             "Nayarit",
  "19",          "Nuevo León",
  "20",              "Oaxaca",
  "21",              "Puebla",
  "22",           "Querétaro",
  "23",        "Quintana Roo",
  "24",     "San Luis Potosí",
  "25",             "Sinaloa",
  "26",              "Sonora",
  "27",             "Tabasco",
  "28",          "Tamaulipas",
  "29",            "Tlaxcala",
  "30",            "Veracruz",
  "31",             "Yucatán",
  "32",           "Zacatecas") %>% 
  mutate(cve_ent = as.numeric(cve_ent))

datos <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                    "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                    skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE)) %>% 
  left_join(cat_ent, by = c("ID_ENTIDAD" = "cve_ent"))

datos_ent <- datos %>% 
  group_by(ID_ENTIDAD, ent) %>% 
  summarise(
    LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
    TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T),
    participacion = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP)) 

shape_ent <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

mapa_entidad <- left_join(shape_ent %>% mutate(CVE_EDO = as.numeric(CVE_EDO)),
                          datos_ent, 
                          by = c("CVE_EDO" = "ID_ENTIDAD"))

# Mapa en ggplot: 
mapa_entidad %>% 
  ggplot(aes(fill = participacion)) +
  geom_sf() + 
  labs(title = "Porcentaje de participación por Entidad Federativa", 
       subtitle = "Encuesta Popular, 01 de Agosto 2021",
       fill = "Porcentaje de Participación (%)",
       caption = "Fuente: INE. Cómputo de resultados de la Consulta Popular 2021.") +
  scale_fill_gradientn(breaks = seq(from = 3, to = 13, by = 1),
                       limits = c(3, 13),
                       labels = scales::dollar_format(prefix = "",
                                                      suffix = "%"),
                       colors = c("white", 
                                  rgb(225,158,149, maxColorValue = 255),
                                  "brown")
  ) +
  tema_juve +
  theme(legend.position = "bottom", 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.line.x = element_blank()) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5, 
                               barwidth = 25,
                               barheight = 0.5,
                               nrow = 1))

