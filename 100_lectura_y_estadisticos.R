# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Lectura y Estadísticos de la Institución %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script, se leerá y consolidará la información de las variables de la Institución, la cual se almacena en un
# archivo RData, InfoConsolidada.RData, en la subcarpeta RData de la carpeta BDD. Además, se calculan las medidas 
# de tendencia central y dispersión para cada variable, realizando un breve análisis exploratorio de datos (EDA).
# Los resultados se guardan en un archivo Excel, Descriptivos_Institucion_Inicial.xlsx, en la carpeta de Resultados.

# También se particiono la base es base de modelamiento y validación.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\tLectura y Consolidación de la Información de la Institución' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#-------------------------------------------------- Direcciones y Funciones --------------------------------------------------------

# Dirección de la data de la Institución
dir.b <- paste0(dir.p, "/BDD/Institucion")

# Se fija la dirección hacia la carpeta con las bases de datos de la Institución
setwd(dir.b)
list.files()

#~~~~~~~~~~~~~~~~~~~ Función de reemplazo de NA's por columna
reemplazo_col = function(dt, vars, valor){ 
  na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
  for (i in vars)
    eval(parse(text=paste0("dt[,`",i,"`:=na.replace(`",i,"`)]")))
}

#---------------------------------------------------------- Carga de la Data --------------------------------------------------------

#datos<-setDT(rio::import("OperComportamientoAU.csv"))
datos <- read_csv("OperComportamientoAU.csv")
class(datos)
datos<-as.data.table(datos)

#---------------------------------------------------- Selección de Información Válida --------------------------------------------------------

# Excluimos a los individuos clasificados como Indeterminados
datos<-datos[Var_Objetivo!="Indeterminado"]

# Denotamos a los individuos Buenos con (0) y a los Malos con (1) en la variable VarDep
datos[, VarDep := ifelse(Var_Objetivo == "Bueno", 0, 1)]

# Se identificaron problemas en las estadísticas (promedio, mínimo y máximo) para cada variable,
# ya que para cada variable su suma no contiene valores ausentes (NA), a diferencia de estas tres.
# Con el objetivo de evitar posibles sesgos en los resultados, se optó por omitir las variables
# asociadas con estas estadísticas (min, max, mean).

Colstodelate<-c(grep(colnames(datos), pattern = "min", value = TRUE),
                #grep(colnames(datos), pattern = "max", value = TRUE),
                grep(colnames(datos), pattern = "mean", value = TRUE))
datos[, (Colstodelate) := NULL]

#------------------------------------------------------- Generación de Estadisticos ------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~ Función de estadísticos:

fun <- function(x, rm=TRUE){
  if(class(x)[1] %in% c("numeric", "integer")){
    res <- c(tipo=class(x),
             perdidos =100*mean(is.na(x)),
             nulos = 100*mean(x==0, na.rm = rm),
             minimo = min(x, na.rm = rm),
             p1 = quantile(x, probs=0.01, na.rm=rm),
             p2 = quantile(x, probs=0.02, na.rm=rm),
             p5 = quantile(x, probs=0.05, na.rm=rm),
             p10 = quantile(x, probs=0.1, na.rm=rm),
             p25 = quantile(x, probs=0.25, na.rm=rm),
             mediana = median(x, na.rm = rm),
             p75 = quantile(x, probs=0.75, na.rm=rm),
             p90 = quantile(x, probs=0.9, na.rm=rm),
             p95 = quantile(x, probs=0.95, na.rm=rm),
             p98 = quantile(x, probs=0.98, na.rm=rm),
             p99 = quantile(x, probs=0.99, na.rm=rm),
             maximo = max(x, na.rm = rm),
             media = mean(x, na.rm = rm),
             sd = sd(x, na.rm = rm)
    )
  } else {
    res <- c(tipo=class(x),perdidos = 100*mean(is.na(x)),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  }
  return(res)
}

# Se calculan los estadisticos usando la función fun:
res <- do.call(cbind, lapply(datos, fun))
res <- as.data.table(res)
t_res <- t(res)
colnames(t_res) <- c("tipo variable","perdidos", "nulos", "minimo", "p1", "p2", "p5", "p10", "p25", "mediana", "p75", "p90", "p95", "p98", "p99",  "maximo", "media", "sd")
t_res <- as.data.table(t_res)
t_res[, Variable := colnames(res)]
setcolorder(t_res, c(ncol(t_res), 1:(ncol(t_res)-1)))

# Se guardan los estadísticos en un Excel:
setwd(dir.r)
write.xlsx(list("Descriptivos" = t_res), file = "Descriptivos_Institucion_Inicial.xlsx")

rm(list=setdiff(ls(),c("datos","dir.p","dir.r","dir.s","reemplazo_col")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)

#--------------------------------------------------------------- Imputación ------------------------------------------------------

# A continucación se van a imputar ciertas variables asociadas a Deuda


# grep me da las variables que contienen en su nombre "Abonos", con TRUE obtengo los nombres de las columnas, con false obtengo 
# los indices de las columnas
grep(colnames(datos), pattern = "Abonos", value = TRUE)
grep(colnames(datos), pattern = "Abonos", value = FALSE)

# Guardamos los nombres de las variables
nombres_variables<-data.frame(nombres=colnames(datos))

# Para las columnas de Abonos, con grep obtenemos todas la de este tipo,  se las imputa por 0, es decir esos clientes en esa fecha 
# no tienen abonos y lo mismo para las variables derivadas de abonos. Un análisis similar se hizo para las demás variables

reemplazo_col(datos, grep(colnames(datos), pattern = "Abonos", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "Atraso", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "CarteraCastigada", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "Cuotas", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "Morosidad", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "SaldoPonderado", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "SaldoTotal", value = TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "Judicial",value= TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "NoDevengaIntereses",value= TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "PorVencer",value= TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "ValorVencido",value= TRUE), 0)
reemplazo_col(datos, grep(colnames(datos), pattern = "VencidoPonderado",value= TRUE), 0)

#--------------------------------------------------------- Base de Modelamiento/Validación ----------------------------------------------------------------

set.seed(12345)

# Se selecciona aleatoriamente el 70% de los registros de la base
marca <- sample(1:nrow(datos), size=floor(0.7*nrow(datos)), replace = FALSE)
datos[, ModVal := 1:nrow(datos)]

# Asignamos aleatoriamente registros a la base de modelamiento (0) o de validación (1).
datos[, ModVal := ifelse(ModVal %in% marca, 0,1)]

# Tablas de frecuencias
datos[, .N, by=ModVal]

#----------------------------------------------------------- Almacenamiento de la Data ------------------------------------------------------------

# Se guarda la data generada
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoConsolidada.RData")

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)
