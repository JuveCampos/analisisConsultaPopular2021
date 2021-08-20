
# Analisis de la Consulta Popular 2021

Mi análisis sobre los datos de la consulta popular 2021, publicado en el [blog de datos de Nexos](https://datos.nexos.com.mx/la-participacion-electoral-en-la-consulta-popular/) el 12 de Agosto del 2021. 

![](https://i1.wp.com/datos.nexos.com.mx/wp-content/uploads/2021/08/consulta-pb.jpg?w=700&ssl=1)

**Ilustración:** Patricio Betteo

## Notas de reproducibilidad: 

Para reproducir el análisis, se recomienda tener instalado R y RStudio en la computadora.

Igualmente, se recomienda tener instaladas las librerías siguientes: 

* `sf`,
* `tidyverse`,
* `scales`,
* `ggtext`, 
* `leaflet`, 
* `waffle`

Para instalarlas, se puede utilizar la siguiente función: 

```
# Instalar paquetes
install.packages(c("sf", "tidyverse","scales","ggtext", "leaflet", "waffle"))
```

Igualmente, para descargar los datos a nivel sección, seguir las instrucciones que vienen en el script que los requiere. 

## Visualizaciones: 

A continuación vienen las visualizaciones que se incluyeron en el artículo: 

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/01_waffle_participación.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/02_participacion_estatal.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/03_mapa_participacion.png)

![](https://github.com/JuveCampos/analisisConsultaPopular2021/blob/main/03_Resultados/imagenes/04_opiniones_entidad.png)

![](https://github.com/JuveCampos/analisisConsultaPopular2021/blob/main/03_Resultados/imagenes/05_top_20_municipios_participacion.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/06_top_20_participaciones.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/07_mapa_porcentaje_participacion.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/08_mapa_porcentaje_participacion_quintiles.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/09_mesas_por_entidad.png)

![](https://raw.githubusercontent.com/JuveCampos/analisisConsultaPopular2021/main/03_Resultados/imagenes/10_porcentajes_part_promedio_mesa.png)


