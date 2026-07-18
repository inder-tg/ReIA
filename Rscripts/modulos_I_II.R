############################################################
# Curso CADI - Tecnológico de Monterrey
# Introducción al análisis de datos en R con IA
# Módulos 1 & 2
# Dataset: CalCOFI (datos_calcofi.csv)
# Autor: Inder Rafael Tecuapetla Gómez
############################################################

# ----------------------------------------------------------
# 0. Preparación del entorno
# ----------------------------------------------------------
# Instalar paquetes (solo una vez)
# install.packages("tidyverse")

library(here)
library(readr)
library(tidyverse)
library(patchwork)

# ----------------------------------------------------------
# 1. Lectura de datos
# ----------------------------------------------------------
# Leemos el dataset depurado de CalCOFI
calcofi <- read_csv( here( "data", "datos_calcofi.csv" ) )

# Exploración inicial
spec(calcofi)
glimpse(calcofi)   # vista rápida de variables
View(calcofi) # vista rápida de variables (como un excel)

summary(calcofi)   # medidas básicas

# ----------------------------------------------------------
# 2. Surfing con dplyr: selección y filtrado
# ----------------------------------------------------------
# Seleccionar algunas variables clave
calcofi_subset <- calcofi %>%
  select(Year, Quarter, Depth_zone, Temperature = mean_T_degC,
         Salinity = mean_Salnty, Oxygen = mean_O2ml_L,
         Nitrates = mean_NO3uM, Phosphates = mean_PO4uM,
         Chlorophyll = mean_ChlorA, Distance, Dist_cat)

# Ver primeras filas
head(calcofi_subset)

# -------------------------------------
# 3. Exploración de la variable Quarter con tidyverse y ggplot
# -------------------------------------

# La variable **Quarter** indica el trimestre del año en que se realizó el muestreo.  
# Esto nos permite analizar la **variabilidad estacional** en las condiciones del océano.  
# Por ejemplo, podemos observar cómo cambia la temperatura promedio del agua a lo largo del año.

# Calcular temperatura promedio por trimestre
temp_quarter <- calcofi_subset %>%
  group_by(Quarter) # %>%
summarise(media_T = mean(Temperature, na.rm = TRUE))

# Gráfico de barras
ggplot(temp_quarter, aes(x = Quarter, y = media_T, fill = Quarter)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Temperatura promedio por trimestre",
       x = "Trimestre del año",
       y = "Temperatura (°C)") +
  theme_minimal()

# Posible interpretación:
#   
# 1. Este gráfico muestra cómo la temperatura del agua varía a lo largo del año.
# 
# 2. Los trimestres con temperaturas más bajas suelen asociarse a surgencias (afloramiento de aguas frías y ricas en nutrientes).
# 
# 3. Los trimestres con temperaturas más altas reflejan condiciones superficiales más cálidas, que pueden estar vinculadas a fenómenos como El Niño.
# 
# 4. Esta variabilidad estacional es clave para entender la productividad marina y los riesgos ecológicos.

# --------------------------------------------------
# Mostrar boxplots de la Temperatura por trimestre
# --------------------------------------------------

## Distribución de la temperatura por trimestre

# La variable **Quarter** indica el trimestre del año en que se realizó el muestreo.  
# Al analizar la temperatura por trimestre, podemos observar la **variabilidad estacional** del océano.  
# Los boxplots muestran la distribución completa de los valores, y los puntos rojos indican la media en cada trimestre.

# Boxplot con medias añadidas
ggplot(calcofi_subset, aes(x = Quarter, y = Temperature, fill = Quarter)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(title = "Distribución de la temperatura por trimestre",
       x = "Trimestre del año",
       y = "Temperatura (°C)") +
  theme_minimal()

calcofi_subset <- calcofi_subset %>%
  mutate(Quarter_factor = factor(Quarter))

# Boxplot con medias añadidas
ggplot(calcofi_subset, aes(x = Quarter_factor, y = Temperature, fill = Quarter_factor)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(title = "Distribución de la temperatura por trimestre",
       x = "Trimestre del año",
       y = "Temperatura (°C)") +
  theme_minimal()

## Posible interpretación:
# 1. Los boxplots muestran la dispersión y los valores extremos de la temperatura en cada trimestre.
# 
# 2. Los puntos rojos (media) permiten comparar rápidamente los niveles promedio entre trimestres.
# 
# 3. Diferencias claras entre trimestres sugieren patrones estacionales o influencia de fenómenos como El Niño.
# 
# 4. Este tipo de visualización es más rico que un gráfico de barras porque muestra tanto la tendencia central como la variabilidad.


## Comparación de histogramas y boxplots

# Para entender mejor qué es un **boxplot**, comparemos dos gráficos de la misma variable:  
#   la **temperatura del agua en el primer trimestre (Quarter = 1)**.
# 
# - El **histograma** muestra la forma de la distribución (frecuencia de valores).  
# - El **boxplot** resume la dispersión, la mediana y los valores extremos.  
# - Juntos, ofrecen una visión más completa de los datos.

# Filtrar datos del primer trimestre
temp_Q1 <- calcofi_subset %>%
  filter(Quarter == 1)

# Gráfico side-by-side
par(mfrow = c(1, 2))  # dos paneles en una fila

# Histograma
hist(temp_Q1$Temperature,
     col = "lightblue", border = "white",
     main = "Histograma de Temperatura (Q1)",
     xlab = "Temperatura (°C)")

# Boxplot con media añadida
boxplot(temp_Q1$Temperature,
        col = "lightgreen",
        main = "Boxplot de Temperatura (Q1)",
        ylab = "Temperatura (°C)")
points(mean(temp_Q1$Temperature, na.rm = TRUE),
       col = "red", pch = 20)  # añadir la media en rojo

# Misma idea de arriba pero usando datos de temperatura del segundo trimestre
# y gráficos con un enfoque más moderno

# Filtrar datos del primer trimestre
temp_Q2 <- calcofi_subset %>%
  filter(Quarter == 2)

# Histograma
p1 <- ggplot(temp_Q2, aes(x = Temperature)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Histograma de Temperatura (Q2)",
       x = "Temperatura (°C)", y = "Frecuencia") +
  theme_minimal()

# Boxplot con media añadida (nota: x fijo = "Q1")
p2 <- ggplot(temp_Q2, aes(x = "Q2", y = Temperature)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6) +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(title = "Boxplot de Temperatura (Q2)",
       y = "Temperatura (°C)", x = "") +
  theme_minimal()

# Combinar ambos gráficos lado a lado (patchwork hace el trabajo rudo)
p1 + p2

# ----------------------------------------------------------
# 4. Surfing con tidyverse: resúmenes temporales
# ----------------------------------------------------------

# 4.1 Conteo de registros por año
print(n = Inf,
      calcofi_subset %>%
        group_by(Year) %>%
        summarise(n_registros = n())
)

# # 2. Series de tiempo: oxígeno promedio anual
# oxigeno_anual <- calcofi_subset %>%
#   group_by(Year) %>%
#   summarise(media_O2 = mean(Oxygen, na.rm = TRUE),
#             sd_O2 = sd(Oxygen, na.rm = TRUE))

# 4.2 Series de tiempo: temperatura promedio anual
temperatura_anual <- calcofi_subset %>%
  group_by(Year) %>%
  summarise(media_T = mean(Temperature, na.rm = TRUE),
            sd_T = sd(Temperature, na.rm = TRUE))

# 4.3 Visualización: temperatura a lo largo del tiempo
ggplot(temperatura_anual, aes(x = Year, y = media_T)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Temperatura promedio por año",
       x = "Año", y = "°C") +
  theme_minimal()


# 3. Visualización: oxígeno y temperatura a lo largo del tiempo
# ggplot(oxigeno_anual, aes(x = Year, y = media_O2)) +
#   geom_line(color = "blue") +
#   geom_point() +
#   labs(title = "Oxígeno disuelto promedio por año",
#        x = "Año", y = "ml/L") +
#   theme_minimal()

# 5A. Comparación por trimestre (Quarter)
calcofi_subset %>%
  group_by(Year, Quarter, Depth_zone) %>%
  summarise(media_T = mean(Temperature, na.rm = TRUE),
            media_O2 = mean(Oxygen, na.rm = TRUE))

# 5B. Comparación por trimestre (Quarter) y zona de profundidad (Depth_zone)
calcofi_subset %>%
  group_by(Year, Quarter, Depth_zone) %>%
  summarise(media_T = mean(Temperature, na.rm = TRUE),
            media_O2 = mean(Oxygen, na.rm = TRUE))

# # ----------------------------------------------------------
# # 4. Resúmenes con tidyverse
# # ----------------------------------------------------------
# 
# # Media y desviación estándar de oxígeno por zona de profundidad
# calcofi_subset %>%
#   group_by(Depth_zone) %>%
#   summarise(media_O2 = mean(Oxygen, na.rm = TRUE),
#             sd_O2 = sd(Oxygen, na.rm = TRUE))
# 
# # Comparación de temperatura por trimestre
# calcofi_subset %>%
#   group_by(Quarter) %>%
#   summarise(media_T = mean(Temperature, na.rm = TRUE))

# ----------------------------------------------------------
# 5. Visualizaciones con ggplot2
# ----------------------------------------------------------

# Calcular medias y desviaciones por año y zona de profundidad
temp_year_profundidad <- calcofi_subset %>%
  group_by(Year, Depth_zone) %>%
  summarise(
    media_T = mean(Temperature, na.rm = TRUE),
    sd_T = sd(Temperature, na.rm = TRUE),
    .groups = "drop"
  )

# Plot con bandas de confianza
ggplot(temp_year_profundidad, aes(x = Year, y = media_T, color = Depth_zone, fill = Depth_zone)) +
  geom_line(size = 1) +
  labs(title = "Temperatura promedio anual por zona de profundidad",
       x = "Año", y = "Temperatura (°C)") +
  theme_minimal()

# Qué se puede aprender de estos plots
# Tendencias de largo plazo: si la temperatura promedio sube o baja en ciertas zonas de profundidad entre 1993 y 2021.
# 
# Variabilidad interanual: la banda de confianza (±1 desviación estándar) muestra qué tan estable o variable es la temperatura en cada zona.
# 
# Comparación entre zonas: las diferencias entre “Very shallow”, “Shallow”, “Intermediate” y “Deep” pueden revelar procesos como surgencias, mezcla vertical o influencia de fenómenos climáticos.
# 
# Eventos anómalos: años con desviaciones grandes pueden estar asociados a fenómenos como El Niño/La Niña o cambios oceanográficos regionales.

# Filtrar solo una zona de profundidad
temp_intermediate <- temp_year_profundidad %>%
  filter(Depth_zone == "Intermediate")

# Plot de una sola serie con ribbon
ggplot(temp_intermediate, aes(x = Year, y = media_T)) +
  geom_line(color = "red", size = 1) +
  geom_ribbon(aes(ymin = media_T - sd_T, ymax = media_T + sd_T),
              fill = "red", alpha = 0.3) +
  labs(title = "Temperatura promedio anual (Intermediate)",
       x = "Año", y = "Temperatura (°C)") +
  theme_minimal()

temp_shallow <- temp_year_profundidad %>%
  filter(Depth_zone == "Shallow") # %>%

# Plot de una sola serie con ribbon
ggplot(temp_shallow, aes(x = Year, y = media_T)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = media_T - sd_T, ymax = media_T + sd_T),
              fill = "green", alpha = 0.3) +
  labs(title = "Temperatura promedio anual (Shallow)",
       x = "Año", y = "Temperatura (°C)") +
  theme_minimal()

temp_veryShallow <- temp_year_profundidad %>%
  filter(Depth_zone == "Very shallow") # %>%

# Plot de una sola serie con ribbon
ggplot(temp_veryShallow, aes(x = Year, y = media_T)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = media_T - sd_T, ymax = media_T + sd_T),
              fill = "lightblue", alpha = 0.3) +
  labs(title = "Temperatura promedio anual (Very shallow)",
       x = "Año", y = "Temperatura (°C)") +
  theme_minimal()

# Calcular medias y desviaciones por año, trimestre y zona de profundidad
temp_year_profundidad_sinDeep <- temp_year_profundidad %>%
  filter( Depth_zone != "Deep" )

ggplot(temp_year_profundidad_sinDeep,
       aes(x = Year, y = media_T, color = Depth_zone, fill = Depth_zone, group = Depth_zone)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = media_T - sd_T/5, ymax = media_T + sd_T/5),
              alpha = 0.1, color = NA) +
  labs(title = "Temperatura promedio anual por zona (sin Deep)",
       x = "Año", y = "Temperatura (°C)") +
  theme_minimal()


ggplot(temp_year_profundidad_sinDeep,
       aes(x = Year, y = media_T, color = Depth_zone, fill = Depth_zone, group = Depth_zone)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = media_T - sd_T, ymax = media_T + sd_T),
              alpha = 0.1, color = NA) +
  labs(title = "Temperatura promedio anual por zona (sin Deep)",
       x = "Año", y = "Temperatura (°C)") +
  theme_minimal()

# Ahora bien, aunque es un efecto puramente visual, sí puede inspirar una reflexión ecológica interesante:
#   
# 🌊 Superposición de procesos: en el océano, las capas de agua no son compartimentos aislados. Las condiciones de temperatura y oxígeno en distintas profundidades se influyen mutuamente. La mezcla de colores puede verse como una metáfora de esa interacción.
# 
# 🔄 Variabilidad y solapamiento: las bandas representan la variabilidad interanual. Cuando se traslapan, sugieren que las fluctuaciones de una zona pueden alcanzar los rangos de otra, lo cual es ecológicamente relevante: indica que los límites entre capas no son rígidos.
# 
# 🌐 Interpretación pedagógica: este “artefacto” es una oportunidad de discutir cómo los datos y las visualizaciones pueden sugerir dinámicas ecológicas —aunque el color extra no sea un dato real, sí refleja la idea de que los sistemas naturales son complejos y entrelazados.

# ----

# ----------------------------------------------------------
# 5. Interpretación guiada
# ----------------------------------------------------------
# Preguntas para discusión:
# - ¿Qué variables muestran mayor variabilidad?
# - ¿Qué patrones sugieren estrés ambiental (ej. hipoxia)?
# - ¿Qué diferencias se observan entre zonas costeras y oceánicas?
# - ¿Qué variables podrían ser más relevantes para biodiversidad o pesca?
#
# A partir de este surfing, los profesores deben definir
# un conjunto de 5–7 variables de interés para los siguientes módulos.
############################################################


