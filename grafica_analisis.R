# Mi análisis de los datos de la consulta popular 2021. 
# Jorge Juvenal Campos Ferreira 

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

# 02 - Gráfica de participación estatal: 


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

cat_partidos <- tibble::tribble(
  ~NO_ENT,               ~Estado,                      ~Gobernador, ~Cuenta.de.Twitter, ~Fecha.de.salida, ~Partido, ~Gobierno.del.estado,
  1L,      "Aguascalientes",                  "Martín Orozco", "@MartinOrozcoAgs",            2022L,    "PAN",            "@GobAgs",
  5L,            "Coahuila",                "Miguel Riquelme",        "@mrikelme",            2024L,    "PRI",                   NA,
  7L,             "Chiapas",               "Rutilio Escandón", "@RutilioEscandon",            2021L, "MORENA",                   NA,
  9L,    "Ciudad de México",        "Claudia Sheinbaum Pardo",    "@Claudiashein",            2023L, "MORENA",                   NA,
  10L,             "Durango",             "José R. Aispuro T.",  "@AispuroDurango",            2021L,    "PAN",                   NA,
  11L,          "Guanajuato", "Diego Sinhue Rodrúguez Vallejo",     "@diegosinhue",            2024L,    "PAN",                   NA,
  13L,             "Hidalgo",                     "Omar Fayad",       "@omarfayad",            2022L,    "PRI",                   NA,
  14L,             "Jalisco",                 "Enrique Alfaro",  "@EnriqueAlfaroR",            2024L,     "MC",   "@GobiernoJalisco",
  15L,              "México",               "Alfredo del Mazo",  "@alfredodelmazo",            2023L,    "PRI",                   NA,
  17L,             "Morelos",              "Cuauhtemoc Blanco",   "@cuauhtemocb10",            2024L,    "PES",   "@GobiernoMorelos",
  20L,              "Oaxaca",                "Alejandro Murat",  "@alejandromurat",            2022L,    "PRI",                   NA,
  21L,              "Puebla",                 "Miguel Barbosa",      "@MBarbosaMX",            2024L, "MORENA",                   NA,
  23L,        "Quintana Roo",                 "Carlos Joaquín",   "@CarlosJoaquin",            2022L,    "PAN",                   NA,
  27L,             "Tabasco",           "Adán Augusto López H",    "@adan_augusto",            2024L, "MORENA",                   NA,
  28L,          "Tamaulipas",       "Francisco Cabeza de Vaca",  "@fgcabezadevaca",            2022L,    "PAN",                   NA,
  30L,            "Veracruz",              "Cuitláhuac García",    "@CuitlahuacGJ",            2024L, "MORENA",                   NA,
  31L,             "Yucatán",                  "Mauricio Vila",         "@MauVila",            2024L,    "PAN",                   NA,
  2L,     "Baja California",               "Marina del Pilar",   "MarinadelPilar",               NA, "MORENA",                   NA,
  3L, "Baja California Sur",           "Víctor Manuel Castro",  "VictorCastroCos",               NA, "MORENA",                   NA,
  8L,           "Chihuahua",           "María Eugenia Campos",     "MaruCampos_G",               NA,    "PAN",                   NA,
  6L,              "Colima",                 "Indira Vicaíno",  "indira_vizcaino",               NA, "MORENA",                   NA,
  16L,           "Michoacán",        "Alfredo Ramírez Bedolla",        "ARBedolla",               NA, "MORENA",                   NA,
  18L,             "Nayarit",           "Miguel Ángel Navarro",  "MiguelANavarroQ",               NA, "MORENA",                   NA,
  19L,          "Nuevo León",        "Samuel García Sepúlveda",   "samuel_garcias",               NA,     "MC",                   NA,
  22L,           "Querétaro",         "Mauricio Kuri González",           "makugo",               NA,    "PAN",                   NA,
  24L,     "San Luis Potosí",               "Ricardo Gallardo",           "RCG_Mx",               NA,   "PVEM",                   NA,
  25L,             "Sinaloa",               "Rubén Rocha Moya",       "rochamoya_",               NA, "MORENA (2021)",                   NA,
  26L,              "Sonora",                 "Alfonso Durazo",    "AlfonsoDurazo",               NA, "MORENA (2021)",                   NA,
  29L,            "Tlaxcala",                 "Lorena Cuéllar",    "LorenaCuellar",               NA, "MORENA (2021)",                   NA,
  32L,           "Zacatecas",            "David Monreal Ávila",    "DavidMonrealA",               NA, "MORENA (2021)",                   NA,
  12L,            "Guerrero",          "Evelyn Salgado Pineda",  "SoyEvelynSalgad",               NA, "MORENA (2021)",                   NA,
  04L,            "Campeche",                 "Layda Sansores",    "LaydaSansores",               NA, "MORENA (2021)",                   NA
)

datos <- datos %>% 
  left_join(cat_ent, by = c("ID_ENTIDAD" = "cve_ent"))

datos %>% 
  group_by(ent) %>% 
  left_join(cat_partidos %>% select(Estado, Partido), by = c("ent" = "Estado")) %>% 
  summarise(
    LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
    TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T),
    participacion = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP), 
    Partido = first(Partido)) %>% 
  mutate(Partido = factor(Partido, levels = c("PAN", "PRI", "MORENA", "MORENA (2021)", "MC", "PES", "PVEM"))) %>% 
  ggplot(aes(x = reorder(ent, participacion), y = participacion)) + 
  geom_col(aes(fill = Partido), alpha = 0.8) + 
  geom_hline(yintercept = 40, linetype = 2, 
             color = "blue", size = 1) + 
  geom_text(aes(label = str_c(round(participacion, 2), "%"), 
                color = Partido), 
            fontface = "bold", family = "Poppins", size = 4, hjust = -0.1, 
            show.legend = FALSE) +
  coord_flip() + 
  labs(x = "", y = "(%) Participación",
       title = "Porcentaje de Participación por Entidad Federativa", 
       subtitle = "Datos de la consulta popular del 01 de Agosto, 2021", 
       caption = "<b>Fuente: </b> INE. Base de datos de resultados de la consulta popular." ,
       fill = "Partido del Gobernador (o Gobernador Electo)") + 
  scale_y_continuous(limits = c(0,40), label = scales::comma_format(suffix = "%"), 
                     expand = expansion(c(0,0.1))) + 
  scale_fill_manual(values = c("blue", "red", "brown", "#c9674f", "orange", "purple", "#6DB556")) + 
  scale_color_manual(values = c("blue", "red", "brown", "#c9674f", "orange", "purple", "#6DB556")) +
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
        axis.text = element_text(family = "Poppins",
                                 color = "black",
                                 face = "bold",
                                 size = 9),
        axis.line.x = element_line(color = "black")) + 
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5, 
                             nrow = 1))

# Mapa Estatal: 
shape <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
  mutate(ENTIDAD = str_replace_all(ENTIDAD, c("Coahuila de Zaragoza" = "Coahuila",
                                              "Ciudad de México" = "Ciudad De México",
                                              # "Ciudad de México" = "Ciudad de México",
                                              "Michoacan de Ocampo" = "Michoacán",
                                              "Querétaro de Arteaga" = "Querétaro",
                                              "San Luís Potosi" = "San Luis Potosí",
                                              "Veracruz de Ignacio de La Llave" = "Veracruz"))) %>% 
  mutate(ID_ENTIDAD = as.numeric(CVE_EDO)) %>% 
  select(ID_ENTIDAD, ENTIDAD)





# Gráfica de numeros absolutos 
bd <- datos %>% 
  group_by(ent) %>% 
  left_join(cat_partidos %>% 
              select(Estado, Partido), by = c("ent" = "Estado")) %>% 
  summarise(
    LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
    TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T),
    participacion = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP), 
    Partido = first(Partido)) %>% 
  mutate(Aportacion_total = 100*(TOTAL_OPINIONES/sum(TOTAL_OPINIONES))) %>% 
  mutate(Partido = factor(Partido, levels = c("PAN", "PRI", "MORENA", "MC", "PES", "PVEM")))  

bd %>% 
  ggplot(aes(x = reorder(ent, TOTAL_OPINIONES), 
             y = TOTAL_OPINIONES)) + 
  geom_col(fill = "brown", alpha = 0.5) + 
  geom_hline(yintercept = 40, linetype = 2, color = "blue") + 
  geom_text(aes(label = str_c(prettyNum(round(TOTAL_OPINIONES, 2), 
                                        big.mark = ","),
                              " (", round(Aportacion_total, 1),
                              "%)")), 
  fontface = "bold", family = "Poppins", size = 4, hjust = -0.1) +
  coord_flip() + 
  labs(x = "", y = "", title = "Total de Opiniones Emitidas", 
       subtitle = "Datos de la consulta popular del 01 de Agosto, 2021", 
       caption = "Datos de aportación al total entre paréntesis" ) + 
  scale_y_continuous(label = scales::comma_format(suffix = ""),
    expand = expansion(c(0,0.3))) +
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
        axis.text = element_text(family = "Poppins",
                                 color = "black",
                                 face = "bold",
                                 size = 9),
        axis.line.x = element_line(color = "black")) + 
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5, 
                             nrow = 1))


