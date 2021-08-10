# Opciones: ----
options(scipen = 999)

# Librerias ----
library(tidyverse)
library(ggtext)
library(cowplot)

# Grafica 2, mesas por Entidad

# Datos globales ----
datos_globales <- read_delim("01_Datos/20210802-2130_INE-CONSULTA-POPULAR-2021/20210802-2130_COMPUTOS-INE-CP2021.csv", 
                             "|", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "WINDOWS-1252"),
                             skip = 5) %>% 
  mutate(across(OPINION_SI:LISTA_NOMINAL_MRCP, as.numeric))

# Mesas X Entidad
mesas_entidad <- datos_globales %>% 
  group_by(ENTIDAD) %>% 
  summarise(n = n(), 
            LN_Entidad = sum(LISTA_NOMINAL_MRCP, na.rm = T)) %>% 
  arrange(-n) %>% 
  mutate(mesas_10000electores = n/(LN_Entidad/10000))

mesas_entidad$ENTIDAD <- factor(mesas_entidad$ENTIDAD, 
                                levels = rev(c(mesas_entidad$ENTIDAD)))

no_mesas_plot <- mesas_entidad %>% 
  ggplot(aes(x = ENTIDAD, y = n)) + 
  geom_col(fill = "brown") + 
  geom_text(aes(label = prettyNum(round(n,1), big.mark = ",")), 
            fontface = "bold", 
            family = "Poppins",
            color = "black",
            hjust = -0.2) +
  coord_flip() +
  labs(x = "", 
       y = "Mesas Receptoras") +
  scale_y_continuous(limits = c(0,8500)) +
  tema_juve
no_mesas_plot

mesas_10000 <- mesas_entidad %>% 
  ggplot(aes(x = ENTIDAD, y = mesas_10000electores)) + 
  geom_col(fill = "brown") + 
  geom_text(aes(label = round(mesas_10000electores,1)), 
            fontface = "bold", 
            family = "Poppins",
            color = "white",
            hjust = 1.5) + 
  coord_flip() +
  labs(x = "",
       y = "Mesas Receptoras/10,000 votantes @ LN") +
  tema_juve 
mesas_10000

# Gráfica:   
plot_grid(no_mesas_plot, mesas_10000,
          labels = c('', ''))

