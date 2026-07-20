############################################################
# Curso CADI - Tecnológico de Monterrey
# ============================================================
# Módulo 4: Inferencia estadística aplicada
# ============================================================
# Dataset: CalCOFI (datos_calcofi_corregido.csv)
# Autor: Inder Rafael Tecuapetla Gómez
############################################################

library(dplyr)
library(ggplot2)
library(moments)

# ----------------------------------------------------------
# 0. Lectura de datos
# ----------------------------------------------------------
# Leemos el dataset depurado de CalCOFI (corregido)
calcofi <- read_csv( here( "data", "datos_calcofi_corregido.csv" ) )

# ----------------------------------------------------------
# 1. Selección de datos: Clorofila?
# ----------------------------------------------------------

# La clorofila-a en los océanos es el pigmento verde esencial para 
# la fotosíntesis del fitoplancton (microalgas), indicando la salud y 
# productividad del ecosistema marino.

# Silicato disuelto Es esencial para la formación de frústulas de diatomeas 
# y otros organismos silíceos. Se utiliza como indicador de productividad 
# fitoplanctónica, afloramiento y procesos del ciclo del silicio en sistemas 
# oceánicos y costeros; sin embargo, su concentración debe interpretarse en 
# conjunto con clorofila, nitratos y fosfatos para evaluar el balance de nutrientes 
# y el estado del sistema. Por ejemplo, Si03 alto, con Clorofila-a baja significa 
# que hay nutriente disponible pero sin respuesta biológica, puede ser que sea por 
# limitación de otro factor.

calcofi_factor <- calcofi %>%
  mutate(Quarter = factor(Quarter, levels = c(1,2,3,4),
                          labels = c("Q1","Q2","Q3","Q4")))

ggplot(calcofi_factor, aes(x = Quarter, y = mean_ChlorA, fill = Dist_cat)) +
  geom_boxplot() +
  labs(title = "Variabilidad estacional de la clorofila",
       x = "Trimestre", y = "Clorofila (mg/m3)",
       fill = "Categoría de distancia") +
  theme_minimal()

ggplot(calcofi_factor, aes(x = Quarter, y = mean_SiO3uM, fill = Dist_cat)) +
  geom_boxplot() +
  labs(title = "Variabilidad estacional del silicato disuelto",
       x = "Trimestre", y = "Silicato disuelto (µmol/L)",
       fill = "Categoría de distancia") +
  theme_minimal()

# Clorofila y silicato en Q2 
chl_Q2 <- calcofi %>%
  filter(Quarter == 2) %>%
  pull(mean_ChlorA)

SiO3_Q2 <- calcofi %>%
  filter(Quarter == 2) %>%
  pull(mean_SiO3uM)

# ----------------------------------------------------------
# 2. Estadísticos descriptivos: 
# ----------------------------------------------------------

mean_chl <- mean(chl_Q2, na.rm = TRUE)
sd_chl   <- sd(chl_Q2, na.rm = TRUE)
mad_chl  <- mad(chl_Q2, na.rm = TRUE)
n_chl    <- sum(!is.na(chl_Q2))

mean_chl; sd_chl; mad_chl; n_chl

ggplot( data.frame(chl_Q2), aes(x = chl_Q2) ) +
  geom_histogram( binwidth = 0.5, fill = "steelblue", color = "white",
                  alpha = 0.7) +
  labs( title = "Distribución de Clorofila en el segundo trimestre",
        x = "Clorofila (µg/L)", y = "Frecuencia" ) +
  theme_minimal()

ggplot( data.frame(calcofi$mean_ChlorA), aes(x = calcofi$mean_ChlorA) ) +
  geom_histogram( binwidth = 0.5, fill = "steelblue", color = "white",
                  alpha = 0.7) +
  labs( title = "Distribución de Clorofila",
        x = "Clorofila (µg/L)", y = "Frecuencia" ) +
  theme_minimal()

# ChlorA No es simétrica

mean_SiO3 <- mean(SiO3_Q2, na.rm = TRUE)
sd_SiO3   <- sd(SiO3_Q2, na.rm = TRUE)
mad_SiO3 <- mad(SiO3_Q2, na.rm = TRUE)
n_SiO3    <- sum(!is.na(SiO3_Q2))

ggplot( data.frame(SiO3_Q2), aes(x = SiO3_Q2) ) +
  geom_histogram( binwidth = 0.5, fill = "steelblue", color = "white",
                  alpha = 0.7) +
  labs( title = "Distribución de Silicato disuelto en el segundo trimestre",
        x = "Silicato disuelto (µmol/L)", y = "Frecuencia" ) +
  theme_minimal()

ggplot( data.frame(calcofi$mean_SiO3uM), aes(x = calcofi$mean_SiO3uM) ) +
  geom_histogram( binwidth = 0.5, fill = "steelblue", color = "white",
                  alpha = 0.7) +
  labs( title = "Distribución de Silicato disuelto",
        x = "Silicato disuelto (µmol/L)", y = "Frecuencia" ) +
  theme_minimal()

# SiO3 es bimodal

# ----------------------------------------------------------
# 3. Chequeo de supuestos: Normalidad
# ----------------------------------------------------------

num_vars <- calcofi %>% select(where(is.numeric)) %>%
  select(-Cst_Cnt, -Bottom_D, -Distance, -Lat_Dec, -Lon_Dec, -Year, -Quarter)

shape_stats <- data.frame(
  variable = names(num_vars),
  skewness = sapply(num_vars, function(x) skewness(x, na.rm = TRUE)),
  kurtosis = sapply(num_vars, function(x) kurtosis(x, na.rm = TRUE))
)

# Ordenar por cercanía a normalidad
shape_stats <- shape_stats %>%
  mutate(dist_to_normal = abs(skewness) + abs(kurtosis - 3)) %>%
  arrange(dist_to_normal)

print(shape_stats)

# La variable "más normal" es Temperatura seguida del Silicato disuelto

# ----------------------------------------------------------
# 4. Intervalo de confianza para la media (95%) con
# Temp y SiO3 como ejemplos
# ----------------------------------------------------------

mean_Temp <- mean(calcofi$mean_T_degC, na.rm = TRUE)
ci <- t.test(calcofi$mean_T_degC)$conf.int

ggplot(data.frame(calcofi$mean_T_degC), aes(x = calcofi$mean_T_degC)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = mean_Temp, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ci[1], color = "darkgreen", linetype = "dotted") +
  geom_vline(xintercept = ci[2], color = "darkgreen", linetype = "dotted") +
  labs(title = "Distribución de Temperatura con IC 95%",
       x = "Temperatura (°C)", y = "Frecuencia") +
  theme_minimal()

Temp_Q2 <- calcofi %>%
  filter(Quarter == 2) %>%
  pull(mean_T_degC)

mean_Temp <- mean(Temp_Q2, na.rm = TRUE)
ci <- t.test(Temp_Q2)$conf.int

ggplot(data.frame(Temp_Q2), aes(x = Temp_Q2)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = mean_Temp, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ci[1], color = "darkgreen", linetype = "dotted") +
  geom_vline(xintercept = ci[2], color = "darkgreen", linetype = "dotted") +
  labs(title = "Distribución de Temperatura en Quarter 2 con IC 95%",
       x = "Temperatura (°C)", y = "Frecuencia") +
  theme_minimal()

