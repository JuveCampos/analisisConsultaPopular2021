# Opciones: ----
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)
library(ggtext)
library(waffle)

# Datos ----
datos <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                    "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                    skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE))

datos %>% 
  summarise(LN = sum(LISTA_NOMINAL_MRCP)) %>% 
  summarise(LN_10000 = LN/10000)

# Gráfica 1. Waffle de participación: ----

# Datos: (Provenientes del Informe del INE, redondeados)
df <- data.frame(grupo = c("No votó", "Votó Sí", "Votó No", "Nulos"),
                 valor = c(9289, 695, 11, 5))

# Grafica: 
ggplot(df, aes(fill = grupo, values = valor)) +
  geom_waffle(n_rows = 100, size = 0.33, colour = "white") +
  scale_fill_manual(name = NULL,
                    values = c("blue", "orange", "purple", "brown"),
                    labels = c("No votó (93%)", "Votó Si (93%)", "Votó No ()", "Nulos ()")) +
  coord_equal() +
  theme_void() + 
  labs(title = "Proporción del padrón electoral en la Consulta Popular", 
       subtitle = "Si el padrón electoral estuviera conformado por 10,000 unidades,estas se verían así: ", 
       caption = "Fuente: INE. Cómputo de resultados de la Consulta Popular 2021.\nhttps://computos.cp2021.ine.mx/votos-distrito/mapa") + 
  theme(legend.position = "bottom", 
        plot.title = element_markdown(size = 20,
                                      color = "brown",
                                      face = "bold"),
        plot.subtitle = element_markdown(size = 13),
        plot.caption = element_text(color = "gray20"),
        text = element_text(family = "Poppins", color = "black"))

