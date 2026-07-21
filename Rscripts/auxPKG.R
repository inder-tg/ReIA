
# -----------------------------------------------------------------------------
#
# Elaborado por Inder Tecuapetla, May 31, 2023
#
# Modificado Julio 20, 2026
#
# Instalación de paquetes/bibliotecas a utilizar en este módulo
# 
# Hecho para SELPER/CEOS Working Group Chapter D Training Group  
#
# -----------------------------------------------------------------------------

neededPackages <- c("here", "readr", "tidyr", "tidyverse",
                    "patchwork", "leaflet", "dplyr", "ggplot2",
                    "GGally", "corrplot", "ggcorrplot", "moments")

packagesToInstall <- setdiff(neededPackages, rownames(installed.packages()))

if( length(packagesToInstall) > 0 ){
  for( i in 1:length(packagesToInstall) ){
    message("Installing package: ", packagesToInstall[i], "\n")
    install.packages(packagesToInstall[i], dependencies = TRUE)
  }
} 
