
############################################################
# Curso CADI - Tecnológico de Monterrey
# ============================================================
# Módulo 5: Modelo de regresión lineal simple y múltiple
# ============================================================
# Dataset: CalCOFI (datos_calcofi_corregido.csv)
# Autor: Inder Rafael Tecuapetla Gómez
############################################################

library(dplyr)
library(readr)
library(ggplot2)
library(GGally)   # para ggpairs
library(corrplot) # para matrices de correlación
library(ggcorrplot)
library(here)
library(patchwork)

# ----------------------------------------------------------
# 0. Lectura de datos
# ----------------------------------------------------------
# Leemos el dataset depurado de CalCOFI (corregido)
calcofi <- read_csv( here( "data", "datos_calcofi_corrected.csv" ) )

PO4 <- calcofi %>%
  select(mean_PO4uM, mean_T_degC, mean_O2ml_L, mean_O2Sat,
         mean_NO3uM, mean_SiO3uM, Quarter, Dist_cat,
         Depth_zone) %>%
  rename(
    PO4 = mean_PO4uM,
    Temperature = mean_T_degC,
    O2 = mean_O2ml_L,
    O2Sat = mean_O2Sat,
    NO3 = mean_NO3uM,
    SiO3 = mean_SiO3uM
  )

# Usamos GGally::ggpairs para ver relaciones bivariadas
PO4_subset <- PO4 %>%
  select(-Quarter, -Dist_cat, -Depth_zone)

cor_PO4 <- cor(PO4_subset, use = "complete.obs")

ggcorrplot(cor_PO4, hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3,
           colors = c("red", "white", "blue"),
           title = "Matriz de correlación (con interés en PO4)")

ggplot(PO4, aes(x = PO4, y = NO3, color = Dist_cat)) +
  geom_point(alpha = 0.6) +
  # geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relación entre fosfato y nitrato",
       x = "Fosfato (µg/L)", y = "Nitrato (µmol/L)",
       color = "Zona") +
  theme_minimal()

# ---- Condiciona por Quarter

PO4_subset_Q1 <- PO4 %>%
  filter( Quarter == 1 )

ggplot(PO4_subset_Q1, aes(x = PO4, y = NO3, color = Dist_cat)) +
  geom_point(alpha = 0.6) +
  # geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Relación entre fosfato y nitrato (Q1)",
       x = "Fosfato (µg/L)", y = "Nitrato (µmol/L)",
       color = "Zona") +
  theme_minimal()

# --- Condiciona por Dist_cat

PO4_subset_Q1_ocean <- PO4_subset_Q1 %>%
  filter( Dist_cat == "Oceánica" )

ggplot(PO4_subset_Q1_ocean, 
       aes(x = PO4, y = NO3, color = Dist_cat )) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relación entre fosfato y nitrato (Q1 y Oceánica)",
       x = "Fosfato (µg/L)", y = "Nitrato (µmol/L)",
       color = "Zona") +
  theme_minimal()

# --- Ajuste de modelo lineal simple: NO3 ~ PO4 (Q1+Oceánica))
modelo <- lm(NO3 ~ PO4, data = PO4_subset_Q1_ocean)

# Resumen del modelo: coeficientes, t-test sobre la pendiente, R²
summary(modelo)

# --- Análisis diagnóstico del modelo
# 1. Residuos vs valores ajustados (linealidad y homocedasticidad)
plot(modelo$fitted.values, modelo$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados")
abline(h = 0, lty = 2, col = "red")

# 2. Histograma de residuos (normalidad aproximada)
hist(modelo$residuals, breaks = 20,
     main = "Histograma de residuos",
     xlab = "Residuos", col = "lightblue")

# 3. QQ plot (normalidad de residuos)
qqnorm(modelo$residuals, main = "QQ plot de residuos")
qqline(modelo$residuals, col = "red")

# 4. Cooks distance (influencia de observaciones)

cooks <- cooks.distance(modelo)

# Visualización
plot(cooks, 
     ylab = "Cook's distance", 
     xlab = "Observación", 
     main = "Diagnóstico de influencia (Cook's distance)")
abline(h = 4/length(cooks), col = "red", lty = 2)  # umbral práctico

# Identificar observaciones influyentes
which(cooks > 4/length(cooks))

length( which(cooks > 4/length(cooks)) )

# --- Condiciona por Depth_zone

ggplot(PO4_subset_Q1_ocean, 
       aes(x = PO4, y = NO3, fill = Dist_cat, color = Depth_zone )) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relación entre fosfato y nitrato (Q1, Oceanica, Depth Zone)",
       x = "Fosfato (µg/L)", y = "Nitrato (µmol/L)",
       color = "Zona") +
  theme_minimal()

# --- Depth_zone = Deep
PO4_subset_Q1_ocean_deep <- PO4_subset_Q1_ocean %>%
  filter( Depth_zone == "Deep" )

ggplot(PO4_subset_Q1_ocean_deep, 
       aes(x = PO4, y = NO3, color = Dist_cat, fill = Depth_zone )) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relación entre fosfato y nitrato (Q1, Oceanica, Deep)",
       x = "Fosfato (µg/L)", y = "Nitrato (µmol/L)",
       color = "Zona") +
  theme_minimal()

# --- Ajuste modelo lineal
modelo <- lm(NO3 ~ PO4, data = PO4_subset_Q1_ocean_deep)

# Resumen del modelo: coeficientes, t-test sobre la pendiente, R²
summary(modelo)

# 1. Residuos vs valores ajustados (linealidad y homocedasticidad)
plot(modelo$fitted.values, modelo$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados")
abline(h = 0, lty = 2, col = "red")

# 2. Histograma de residuos (normalidad aproximada)
hist(modelo$residuals, breaks = 20,
     main = "Histograma de residuos",
     xlab = "Residuos", col = "lightblue")

# 3. QQ plot (normalidad de residuos)
qqnorm(modelo$residuals, main = "QQ plot de residuos")
qqline(modelo$residuals, col = "red")

# 4. Cooks distance (influencia de observaciones)
cooks <- cooks.distance(modelo)

plot(cooks, 
     ylab = "Cook's distance", 
     xlab = "Observación", 
     main = "Diagnóstico de influencia (Cook's distance)")
abline(h = 4/length(cooks), col = "red", lty = 2)  # umbral práctico

# Identificar observaciones influyentes
length(which(cooks > 4/length(cooks)))

# plot(modelo)

# --- Trans ( en progreso, si har tiempo finalizar)

PO4_subset_Q1_trans <- PO4_subset_Q1 %>%
  filter( Dist_cat == "Transición" )

ggplot(PO4_subset_Q1_trans, 
       aes(x = PO4, y = NO3, color = Dist_cat )) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Relación entre fosfato y nitrato",
       x = "Fosfato (µg/L)", y = "Nitrato (µmol/L)",
       color = "Zona") +
  theme_minimal()

modelo <- lm(NO3 ~ PO4, data = PO4_subset_Q1_trans)

# Resumen del modelo: coeficientes, t-test sobre la pendiente, R²
summary(modelo)

# Intervalo de confianza para la pendiente (95%)
confint(modelo, level = 0.95)


# ---- Regrrsion multiple


# Paquetes necesarios
library(car)       # para VIF
library(lmtest)    # para pruebas adicionales
library(ggplot2)

# Ajustar un modelo múltiple (ejemplo con CalCOFI)
modelo <- lm(mean_T_degC ~ mean_O2ml_L + mean_Salnty + mean_ChlorA, data = calcofi)

# --- 1. Resumen del modelo: incluye R^2 y estadístico F global ---
summary(modelo)

# 1. Residuos vs valores ajustados (linealidad y homocedasticidad)
plot(modelo, which = 1)

# 2. QQ plot de residuos (normalidad)
plot(modelo, which = 2)

# 3. Escala-Localización (Spread vs Fitted, homocedasticidad)
plot(modelo, which = 3)

# 4. Residuos vs leverage (influencia, Cook's distance)
plot(modelo, which = 5)

# --- Diagnóstico de multicolinealidad ---
library(car)
vif(modelo)
# --- 4. Gráficos de diagnóstico uno por uno ---

# Residuos vs valores ajustados (linealidad y homocedasticidad)
plot(modelo$fitted.values, modelo$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados")
abline(h = 0, lty = 2, col = "red")

# QQ plot (normalidad de residuos)
qqnorm(modelo$residuals, main = "QQ Plot de residuos")
qqline(modelo$residuals, col = "blue")

# Histograma de residuos
hist(modelo$residuals, breaks = 20,
     main = "Histograma de residuos",
     xlab = "Residuos")

# Influencia: gráfico de leverage vs residuos estandarizados
plot(modelo, which = 5)  # gráfico de influencia (Cook's distance)

# Multicolinealidad visual: pares de predictores
pairs(calcofi[, c("mean_O2ml_L", "mean_Salnty", "mean_ChlorA")],
      main = "Relaciones entre predictores (CalCOFI)")


