#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Ejecutar Proyecto %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Este script se encarga de ejecutar todos los scripts del proyecto en el orden establecido. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#--------------------------------------------------------- Configuración del Proyecto --------------------------------------------------------------------------------

# Se cargan las librerías y se establecen las direcciones base para todo el proyecto.

source( '000_Configuracion_Proyecto.R')

#--------------------------------------------------------- Lectura de la Información  -------------------------------------------------------------------------------

# Se lee la informacion de la institucion y  se calculan ciertos estadísticos para las variables.
source( '100_lectura_y_estadisticos.R' )

# Se identifican variables candidatas para el modelo predictivo y posterior procesamiento.
source( '101_Analisis_Variables_Inicial.R' )

#------------------------------------------------------- Procesamiento de la Información  -------------------------------------------------------------------------------

# Se realiza ingenieria de variables para la información 
source( '200_tratamiento_informacion.R' )

# Se realiza ingenieria de variables con árboles de decisión (este script aun no es parte oficial del proyecto, no ejecutar por source) 
# source( '200_tratamiento_informacion.R' )

#----------------------------------------------------------------- Modelos  -------------------------------------------------------------------------------

# Se obtiene el modelo bajo la filosofía TTC, así como las tablas de performance del Buró y del Modelo TTC, junto con sus métricas de rendimiento
source('300_modelo_TTC.R')

# Se obtiene el modelo bajo la filosofía PIT, así como las tablas de performance del Buró y del Modelo PIT, junto con sus métricas de rendimiento
source('301_modelo_PIT.R')

