#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Configuración del Proyecto %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este  script se realiza la configuración inicial del proyecto. Aquí se cargan las librerías que serán utilizadas a lo largo
# del proyecto, se establecen las rutas de acceso para las bases de datos, scripts adicionales así como las rutas  para 
# almacenar los resultados del proyecto.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\tConfiguración del Proyecto' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#--------------------------------------------------- Librerias --------------------------------------------------------------

#~~~~~~~~~~ Función de instalación y carga  de librerias:

cargar_libreria <- function(nombre_libreria) {
  if (!requireNamespace(nombre_libreria, quietly = TRUE)) {
    install.packages(nombre_libreria, dependencies = TRUE)
    suppressPackageStartupMessages( library(nombre_libreria, character.only = TRUE) )
    message(paste("Se cargo la libreria:", nombre_libreria, "\n"))
  } else {
    suppressPackageStartupMessages( library(nombre_libreria, character.only = TRUE) )
    message(paste("Se cargo la libreria:", nombre_libreria, "\n"))
  }
}

#~~~~~~~~~~ Carga de Librerias:

cargar_libreria("lubridate")
cargar_libreria("data.table")
cargar_libreria("readxl")
cargar_libreria("rlang")
cargar_libreria("openxlsx")
cargar_libreria("fpp3")
cargar_libreria("tidyverse")
cargar_libreria("stringr")
cargar_libreria("expss")
cargar_libreria("readr")
cargar_libreria("caret")
cargar_libreria("pROC")
cargar_libreria("ggcorrplot")
cargar_libreria("car")
cargar_libreria("lattice")
cargar_libreria("caret")
cargar_libreria("ggplot2")
cargar_libreria("cluster")
cargar_libreria("DataExplorer")
cargar_libreria("tidyverse")
cargar_libreria("scales")
cargar_libreria("scorecard")
cargar_libreria("rio")
cargar_libreria("DT")
#--------------------------------------------------- Direcciones --------------------------------------------------------------

# Dirección del Proyecto:
dir.p <- getwd()
# Dirección de la  carpeta de Resultados:
dir.r <- paste0(dir.p, "/Resultados")
# Dirección de la  carpeta de Scripts Adicionales:
dir.s <- paste0(dir.p, "/Scripts Adicionales")

rm(list = "cargar_libreria")
gc()

