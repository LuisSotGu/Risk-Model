# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Análisis de Variables Incial %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se realiza el análisis de variables (inicial) para el modelamiento futuro. Se extrae la información de la 
# base de modelamiento, y se aplican pruebas específicas para identificar las variables más informativas para predecir la
# variable dependiente, las cuales son candidatas a entrar al modelo. Los resultados se guardan en un archivo Excel, 
# Analisis_Variables_Inicial.xlsx, en la carpeta de Resultados.

# Además, se calculan las medidas  de tendencia central y dispersión para las variables validas, realizando un breve 
# análisis exploratorio de datos (EDA). Los resultados se guardan en un archivo Excel, Descriptivos_Institucion_Variables_Validas.xlsx,
# en la carpeta de Resultados.

# Donde, la información obtenida nos permitira tener una idea de como realizar la ingeniería de variables.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\tAnálisis de Variables Inicial' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#---------------------------------------------------- Lectura de la base consolidada  ------------------------------------------------------------

patron <- "\\InfoConsolidada\\b"
archivos_filtrados <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)
load(archivos_filtrados)

# Filtramos la data de modelamiento (Esta sera la información a modelar)
mod <- datos[ModVal == 0]
dim(mod)
datos[,table(ModVal)]

#---------------------------------------------------- Identificación Variables Validas ------------------------------------------------------------

# Fijamos la dirección de la carpeta en donde se encuentra el script con las funciones auxiliares para el análisis de variables
setwd(dir.s)
source("Tidy_data.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Identificación de variables con alto porcentaje de NA's

porc <- sort(sapply(mod, porcNA), decreasing = TRUE)
PorcentajeNA <- data.frame(names(porc), as.numeric(porc))
colnames(PorcentajeNA) <- c("Var", "Porc")

# Almacenamos las variables validas
dvars <- setdiff(colnames(mod), names(porc)[porc > 0.2])
PorcentajeNA
dvars
# Numero de variables validas
length(dvars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Identificación de variables constantes

# Nos quedamos unicamente con las variables no constantes:
dvars <- dvars[!unname(unlist(sapply(mod[,dvars,with=FALSE], constante)))]
# Numero de variables validas
length(dvars)
rm(list = c("PorcentajeNA", "porc", "porcNA", "constante"))
length(dvars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Selección de Variables Validas

# Nos quedamos con las variables validas de la base de modelamiento ( variables con un porcentaje de NA menor al 20% y que no sean constantes)
mod <- mod[, dvars, with = FALSE]

# Seleccionamos las variables numericas
vnum <- colnames(mod)[unname(sapply(mod, class)) %in% c("numeric", "integer")]
dnum <- mod[, vnum, with=FALSE]

# Seleccionamos las variables categoricas
vcat <- colnames(mod)[unname(sapply(mod, class)) %in% c("character", "logical")]
dcat <- mod[, vcat, with=FALSE]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ejecucion de KS sobre variables numericas

KS <- sapply(seq_along(dnum), function(i){TestKS(dnum[[i]], mod$VarDep)}) # Revisar variable dependiente
dKS <- data.frame(colnames(dnum), KS); dKS <- dKS[order(dKS$KS, decreasing = TRUE),]
colnames(dKS) <- c("Variable", "KS"); rownames(dKS) <- NULL
dKS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~ Ejecucion de VI sobre variables categóricas

VI <- sort(sapply(dcat, TestVI, y=mod$VarDep), decreasing = T) # Revisar variable dependiente
dVI <- data.frame(names(VI), VI)
colnames(dVI) <- c("Variable", "VI"); rownames(dVI) <- NULL
dVI

# Se guardan los resultados del test de KS y VI en un Excel:
setwd(dir.r)
write.xlsx(list("KS_Var" = dKS, "VI" = dVI), file = "Analisis_Variables_Inicial.xlsx")

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
res <- do.call(cbind, lapply(mod, fun))
res <- as.data.table(res)
t_res <- t(res)
colnames(t_res) <- c("tipo variable","perdidos", "nulos", "minimo", "p1", "p2", "p5", "p10", "p25", "mediana", "p75", "p90", "p95", "p98", "p99",  "maximo", "media", "sd")
t_res <- as.data.table(t_res)
t_res[, Variable := colnames(res)]
setcolorder(t_res, c(ncol(t_res), 1:(ncol(t_res)-1)))

# Se guardan los estadísticos en un Excel:
setwd(dir.r)
write.xlsx(list("Descriptivos" = t_res), file = "Descriptivos_Institucion_Variables_Validas.xlsx")

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)
