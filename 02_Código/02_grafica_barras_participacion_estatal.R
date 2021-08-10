# Opciones: ----
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(ggtext)

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

# Datos ----
datos <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                    "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                    skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE))

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
