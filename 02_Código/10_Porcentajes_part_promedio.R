# Librerias:
library(tidyverse)
library(sf)
library(leaflet)
library(ggtext)

# Datos:
datos <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                    "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                    skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE))

# Secciones: 
shape <- readRDS("01_Datos/Datos Geograficos/shape.rds") %>% 
  mutate(entidad = as.numeric(entidad))

# Municipios: 
shape_muni <- readRDS("01_Datos/Datos Geograficos/mapa_muni.rds") %>% 
  select(entidad, municipio, CVE_INE)

# Estados: 
shape_ent <- read_sf("01_Datos/Datos Geograficos/entidades.geojson")

# Mergeamos: 
bd_map <- left_join(shape, datos, 
                    by = c("seccion" = "SECCION_SEDE", 
                           "entidad" = "ID_ENTIDAD"))

# Generamos variable de participación: 
bd_map <- bd_map %>% 
  mutate(pp = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP))

# Promedio de participación por secciones: 
promedio <- mean(bd_map$pp, na.rm = T)

# Ordenamos por participación de mayor a menor
bd_map <- bd_map %>% 
  arrange(-pp)

bd_map %>% 
  filter(!is.na(ENTIDAD)) %>% 
  group_by(ENTIDAD) %>% 
  mutate(mediana = median(pp, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(ENTIDAD, mediana), y = pp)) + 
  geom_jitter(alpha = 0.1, color = "brown") + 
  geom_boxplot(color = "black", 
               outlier.alpha = 0, size = 1) + 
  geom_hline(yintercept = 40, color = "blue", size = 1.5, linetype = 2) + 
  coord_flip() + 
  labs(title = "Porcentajes de participación promedio<br>por Mesa Receptora y Entidad", 
       subtitle = "Datos de la consulta popular del 01 de Agosto, 2021", 
       caption = "<b>Cada punto representa la participación promedio de una mesa receptora</b><br><b>Fuente de los datos: </b> INE. Base de datos de resultados de la consulta popular.", 
       x = "", y = "") + 
  scale_y_continuous(label = scales::comma_format(suffix = "%"), 
                     breaks = seq(0,100, by = 10)) + 
  theme(plot.title = element_markdown(size = 20,
                                      color = "brown",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 13),
        plot.caption = element_markdown(color = "gray20"),
        text = element_text(family = "Poppins", color = "black"),
        # plot.background = element_rect(fill = "#242840"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y  = element_blank(),
        panel.grid.major.x = element_line(color = "gray80"),
        axis.text = element_text(family = "Poppins",
                                 color = "black",
                                 face = "bold",
                                 size = 9),
        axis.line.x = element_line(color = "black"))
