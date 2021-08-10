# Análisis datos consulta: 
# Jorge Juvenal Campos Ferreira

options(scipen = 999)
# Librerias: 
library(tidyverse)
library(sf)
library(leaflet)

# Datos globales ----
datos_globales <- read_delim("20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                            "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                            skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric))

# Datos Sección -----
datos_seccion <- read_delim("20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
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
shape_secciones <- readRDS("shape.rds") %>% 
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

# Municipios: 
shape_muni <- readRDS("mapa_muni.rds") %>% 
  select(entidad, municipio, CVE_INE)

# Datos Entidad ----
datos_entidad <- read_delim("20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                 "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                 skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE)) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  group_by(ID_ENTIDAD, ENTIDAD) %>% 
  summarise(TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T), 
            LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
            OPINION_SI = sum(OPINION_SI, na.rm = T), 
            OPINION_NO = sum(OPINION_NO, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Porcentaje_participacion = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP), 
         ENTIDAD = str_to_title(ENTIDAD)) 

# Estados: 
shape_ent <- read_sf("entidades.geojson") %>% 
  mutate(NOMBRE = str_replace_all(NOMBRE, c("YUCATAN" = "YUCATÁN", 
                                            "SAN LUIS POTOSI" = "SAN LUIS POTOSÍ", 
                                            "QUERETARO" = "QUERÉTARO",
                                            "NUEVO LEON" =  "NUEVO LEÓN", 
                                            "MICHOACAN" = "MICHOACÁN", 
                                            "MEXICO"="MÉXICO")))

# RESULTADOS GLOBALES: ----

# Estadísticos sumarios:  
datos_globales %>% 
  summarise(LISTA_NOMINAL_MRCP = sum(LISTA_NOMINAL_MRCP, na.rm = T), 
            TOTAL_OPINIONES = sum(TOTAL_OPINIONES, na.rm = T), 
            OPINION_SI = sum(OPINION_SI, na.rm = T), 
            OPINION_NO = sum(OPINION_NO, na.rm = T)) %>% 
  mutate(pctje_si = 100*(OPINION_SI/TOTAL_OPINIONES), 
         pctje_no = 100*(OPINION_NO/TOTAL_OPINIONES), 
         pctje_participacion = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP)
         ) %>% 
  t()

datos_globales %>% 
  group_by(OBSERVACIONES) %>% 
  count() %>% 
  arrange(-n) 



# Mesas atípicas: 
atipicas <- datos_globales %>% 
  mutate(pp = 100*(TOTAL_OPINIONES/LISTA_NOMINAL_MRCP)) 

atipicas %>% 
  filter(pp >= 40) %>% 
  nrow()

mean(atipicas$pp)

atipicas <- left_join(atipicas %>% mutate(SECCION_SEDE = as.numeric(SECCION_SEDE)), 
          shape_secciones %>% as_tibble() %>% select(entidad, seccion, nom_mun), 
          by = c("ID_ENTIDAD"="entidad", 
                 "SECCION_SEDE" = "seccion"
                 )) %>% 
  arrange(-pp)
  
# Mesas arriba del 90% de participacion: 
mesas_mas_90 <- atipicas %>% 
  filter(pp >= 90)

# Mesas con mas nulos que SI
mas_nulos <- atipicas %>% 
  filter(NULOS > OPINION_SI)

# Mesas con más noes: 
mas_no <- atipicas %>% 
  filter(OPINION_NO > OPINION_SI)

# RESULTADOS A NIVEL ENTIDAD ----
datos_entidad

# RESULTADOS MUNICIPALES: ---- 

# Municipios más votadores: 
datos_municipio %>% names()

# Arriba del 40%
arriba_40 <- datos_municipio %>% 
  filter(pp >= 40) %>% 
  arrange(-pp) %>% 
  select(-quintil_lista_nominal)

# Menos votos
datos_municipio %>% 
  arrange(TOTAL_OPINIONES) %>% 
  head(6) %>% 
  select(nom_mun, ENTIDAD, TOTAL_OPINIONES, LISTA_NOMINAL_MRCP)

# Mas votos
datos_municipio %>% 
  arrange(-TOTAL_OPINIONES) %>% 
  head(6) %>% 
  select(nom_mun, ENTIDAD, TOTAL_OPINIONES, LISTA_NOMINAL_MRCP)

# Densidad de votos 
datos_municipio %>% 
  ggplot(aes(x = TOTAL_OPINIONES)) + 
  geom_density()

# Total Acumulado de votos: ---- 
sum_acum <- datos_municipio %>% 
  ungroup() %>% 
  arrange(-TOTAL_OPINIONES) %>% 
  mutate(part_acum = cumsum(TOTAL_OPINIONES), 
         porcentaje = 100*(part_acum/6663208), 
         rank = rank(-TOTAL_OPINIONES))

# Municipios con mayor porcentaje de participación: 
part <- datos_municipio %>% 
  ungroup() %>% 
  arrange(-pp)

# Municipios cpn mayor porcentaje de votos por el NO
muni_no <- datos_municipio %>% 
  mutate(pctje_no = 100*(OPINION_NO/LISTA_NOMINAL_MRCP)) %>% 
  select(nom_mun, ENTIDAD, pctje_no) %>% 
  arrange(-pctje_no)

# ggplot(muni_no, aes(x = pctje_no)) +
#   geom_density()

# Relación población/Participación: 
# Mas de 100,000 habitantes
part_10mil <- datos_municipio %>% 
  filter(LISTA_NOMINAL_MRCP >= 100000) %>% 
  ungroup() %>% 
  arrange(-pp)

quintiles_pob <- quantile(datos_municipio$LISTA_NOMINAL_MRCP, 
                          seq(0, 1, by = 0.2))

datos_municipio <- datos_municipio %>% 
  mutate(quintil_lista_nominal = case_when(between(LISTA_NOMINAL_MRCP, quintiles_pob["0%"], quintiles_pob["20%"]) ~ str_c("Grupo 1: Menos votantes\n ", "[", quintiles_pob["0%"], "-", quintiles_pob["20%"], "]" ), 
                                           between(LISTA_NOMINAL_MRCP, quintiles_pob["20%"], quintiles_pob["40%"]) ~ str_c("Grupo 2: \n", "[", quintiles_pob["20%"], "-", quintiles_pob["40%"], "]" ), 
                                           between(LISTA_NOMINAL_MRCP, quintiles_pob["40%"], quintiles_pob["60%"]) ~ str_c("Grupo 3: \n", "[", quintiles_pob["40%"], "-", quintiles_pob["60%"], "]" ), 
                                           between(LISTA_NOMINAL_MRCP, quintiles_pob["60%"], quintiles_pob["80%"]) ~ str_c("Grupo 4: \n", "[", quintiles_pob["60%"], "-", quintiles_pob["80%"], "]" ),  
                                           between(LISTA_NOMINAL_MRCP, quintiles_pob["80%"], quintiles_pob["100%"]) ~ str_c("Grupo 5: Más votantes\n ", "[", quintiles_pob["80%"], "-", quintiles_pob["100%"], "]" ), 
                                           ))

medianas_part <- datos_municipio %>% 
  group_by(quintil_lista_nominal) %>% 
  summarise(mediana = median(pp))

plt <- datos_municipio %>% 
  ggplot(aes(x = factor(quintil_lista_nominal), y = pp, label = nom_mun)) + 
  geom_jitter() + 
  geom_boxplot(outlier.alpha = 0, color = "red") + 
  geom_label(data = medianas_part, aes(x = factor(quintil_lista_nominal), 
                                       y = mediana, 
                                       label = str_c(round(mediana, 1), "%")
                                       ), 
             color = "red") + 
  coord_flip() + 
  scale_y_continuous(label = scales::label_comma(suffix = "%")) + 
  labs(x = "", y = "Porcentaje de participación", 
       title = "Porcentaje de participación por quintiles de votantes en Lista Nominal", 
       subtitle = "Datos de la consulta popular del 01 de Agosto, 2021", 
       caption = "Fuente: INE. Cómputo de resultados de la Consulta Popular 2021.<br>Numero dentro de la boxplot es la mediana del porcentaje de participación por grupo."
       ) +
  tema_juve

plt

# Municipios por participación: ----
mapx <- left_join(shape_muni, 
          datos_municipio)

# Paleta de colores: 
pal_colores <- colorNumeric(palette = c("white", "orange", "red"), 
                            domain = c(0:80))

label <- str_c(mapx$nom_mun, ", ", str_to_title(mapx$ENTIDAD))
popup <- str_c("<b>Municipio: </b>",  mapx$nom_mun, ", ", str_to_title(mapx$ENTIDAD), "<br>",
               "<b>Porcentaje de participación: </b>", round(mapx$pp, 2), "%", "<br>",
               "<b>Lista Nominal: </b>", prettyNum(round(mapx$LISTA_NOMINAL_MRCP, 2),big.mark = ","), "<br>",
               "<b>SI: </b>", prettyNum(mapx$OPINION_SI, big.mark = ","), " votos <br>",
               "<b>NO: </b>", prettyNum(mapx$OPINION_NO, big.mark = ","), " votos <br>")

# Mapa: 
mp <- leaflet(mapx) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(fillColor = pal_colores(mapx$pp), 
              color = "black", 
              weight = 0, 
              popup = popup,
              label = label,
              fillOpacity = 1) %>% 
  addPolygons(data = shape_ent, 
              fill = NA, 
              weight = 3, 
              color = "black") %>% 
  addLegend(title = "Porcentaje de<br>participación<br>Consulta",
            labFormat = labelFormat(suffix = "%"),
            pal = pal_colores, values = mapx$pp, position = "bottomright")

mp

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
              popup = popup,
              label = label,
              fillOpacity = 1) %>% 
  # addPolygons(data = shape_ent, 
  #             fill = NA, 
  #             weight = 3, 
  #             color = "white") %>% 
  addLegend(title = "Grupos municipales por participación",
            opacity = 1,
            labFormat = labelFormat(prefix = "Grupo "),
            pal = pal_quint, values = mapx$quintil_part, position = "bottomright")


# Resultados a nivel sección: 

mediana <- datos_globales %>% 
  group_by(ENTIDAD) %>% 
  summarise(mediana = median(LISTA_NOMINAL_MRCP)) %>% 
  arrange(-mediana) 

datos_globales %>% 
  mutate(ENTIDAD = factor(ENTIDAD, levels = rev(mediana$ENTIDAD))) %>%  
  ggplot(aes(x = ENTIDAD, y = LISTA_NOMINAL_MRCP)) + 
  geom_jitter(alpha = 0.1,
              color = "brown") + 
  geom_boxplot(outlier.alpha = 0) + 
  geom_label(data = mediana, 
             color = "brown",
             size = 3,
             aes(x = ENTIDAD, 
                 y = mediana,
                 label = prettyNum(round(mediana, 2), big.mark = ","))) +
  coord_flip() +
  labs(x = "", y = "", 
       title = "Distribución de las Listas Nominales por Mesa Receptora y Entidad", 
       subtitle = "Datos de la consulta popular del 01 de Agosto, 2021", 
       caption = "Textos dentro de las gráficas de caja corresponden a las medianas por entidad.") +
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
  



