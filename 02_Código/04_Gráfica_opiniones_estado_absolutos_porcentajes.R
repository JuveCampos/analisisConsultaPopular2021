# Opciones: ----
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(ggtext)

# Datos ----
datos <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                    "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                    skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric)) %>%
  mutate(SECCION_SEDE = as.numeric(SECCION_SEDE))

datos <- datos %>% 
  left_join(cat_ent, by = c("ID_ENTIDAD" = "cve_ent"))

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
