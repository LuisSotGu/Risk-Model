# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tratamiento Variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se realizará el tratamiento de la información de la base de datos usando árboles de decisión para la recategorización
# de variables.

# ¡¡¡¡¡¡Aviso importante!!!!!

# Este script no se acabo

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\tTratamiento Variables' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#---------------------------------------------------- Lectura de la base consolidada  ------------------------------------------------------------

patron <- "\\InfoConsolidada\\b"
archivos_filtrados <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)
load(archivos_filtrados)


#------------------------------------------------------------- Ratios  ------------------------------------------------------------

auxNombres  <- c("Abonos",
                 "AtrasoMaximoOperacion",
                 "CarteraCastigada",
                 "NumCuotasPagadas/Plazo",
                 "NumCuotasPagadasPunt/Plazo",
                 "NumCuotasPagadasPunt",
                 "NumCuotasPagadas",
                 "NumCuotasPendientes/Plazo",
                 "NumCuotasPendientes",
                 "NumCuotasVencidas/Plazo",
                 "NumCuotasVencidas",
                 "NumDiasMorosidad",
                 "NumTotalCuotasAtraso/Plazo",
                 "NumTotalCuotasAtraso",
                 "SaldoPonderado/ValorOperacion",
                 "SaldoPonderado",
                 "SaldoTotal/ValorOperacion",
                 "SaldoTotal",
                 "ValorDemandaJudicial",
                 "ValorNoDevengaIntereses",
                 "ValorPorVencer/ValorOperacion",
                 "ValorPorVencer",
                 "ValorVencido/ValorOperacion",
                 "ValorVencido",
                 "VencidoPonderado")

#Extraigo los nombres 
#i<-1
for (i in 1:length(auxNombres)) { assign(paste0("Msum36_",auxNombres[i]),grep(paste0("^",auxNombres[i],"_\\d{2}Msum$"), names(datos), value = TRUE)) }
# Extrae la informacion unicamente de los ultimos 24 mese para cada Variable
for (i in 1:length(auxNombres)) { assign(paste0("Msum24_",auxNombres[i]),grep(paste0("^",auxNombres[i],"_(0[1-9]|1[0-9]|2[0-4])Msum$"), names(datos), value = TRUE)) }
for (i in 1:length(auxNombres)) { assign(paste0("Msum12_",auxNombres[i]),grep(paste0("^",auxNombres[i],"_(0[1-9]|1[0-2])Msum$"), names(datos), value = TRUE)) }
for (i in 1:length(auxNombres)) { assign(paste0("Msum06_",auxNombres[i]),grep(paste0("^",auxNombres[i],"_(0[1-6])Msum$"), names(datos), value = TRUE)) }
for (i in 1:length(auxNombres)) { assign(paste0("Msum03_",auxNombres[i]),grep(paste0("^",auxNombres[i],"_(0[1-3])Msum$"), names(datos), value = TRUE)) }

#Visualizo las columnas por nombres
datos[, ..Msum36_Abonos, with = FALSE]
datos[, ..Msum24_Abonos, with = FALSE]
datos[, ..Msum12_Abonos, with = FALSE]
datos[, ..Msum06_Abonos, with = FALSE]
# COn esto veo la informacion de los ultimos 3 meses 
datos[, ..Msum03_Abonos, with = FALSE]

#Creo colmunas: media de 3, 6, 12, 24 y 36 M
for (i in 1:length(auxNombres)) { datos[, paste0("MeanMsum36_",auxNombres[i]) := rowMeans(.SD, na.rm = TRUE), .SDcols = get(paste0("Msum36_",auxNombres[i]))] }
for (i in 1:length(auxNombres)) { datos[, paste0("MeanMsum24_",auxNombres[i]) := rowMeans(.SD, na.rm = TRUE), .SDcols = get(paste0("Msum24_",auxNombres[i]))] }
for (i in 1:length(auxNombres)) { datos[, paste0("MeanMsum12_",auxNombres[i]) := rowMeans(.SD, na.rm = TRUE), .SDcols = get(paste0("Msum12_",auxNombres[i]))] }
for (i in 1:length(auxNombres)) { datos[, paste0("MeanMsum06_",auxNombres[i]) := rowMeans(.SD, na.rm = TRUE), .SDcols = get(paste0("Msum06_",auxNombres[i]))] }
for (i in 1:length(auxNombres)) { datos[, paste0("MeanMsum03_",auxNombres[i]) := rowMeans(.SD, na.rm = TRUE), .SDcols = get(paste0("Msum03_",auxNombres[i]))] }

datos[,MeanMsum03_Abonos]
datos[,MeanMsum06_Abonos]
datos[,MeanMsum12_Abonos]
datos[,MeanMsum24_Abonos]
datos[,MeanMsum36_Abonos]

#Ratios
for (i in 1:length(auxNombres)) { datos[, paste0("Ratio03_06",auxNombres[i]) := ifelse(is.na(get(paste0("MeanMsum03_",auxNombres[i]))/get(paste0("MeanMsum06_",auxNombres[i]))),0,get(paste0("MeanMsum03_",auxNombres[i]))/get(paste0("MeanMsum06_",auxNombres[i]))) ] }
for (i in 1:length(auxNombres)) { datos[, paste0("Ratio03_12",auxNombres[i]):= ifelse(is.na(get(paste0("MeanMsum03_",auxNombres[i]))/get(paste0("MeanMsum12_",auxNombres[i]))),0,get(paste0("MeanMsum03_",auxNombres[i]))/get(paste0("MeanMsum12_",auxNombres[i]))) ] }
for (i in 1:length(auxNombres)) { datos[, paste0("Ratio06_12",auxNombres[i]):= ifelse(is.na(get(paste0("MeanMsum06_",auxNombres[i]))/get(paste0("MeanMsum12_",auxNombres[i]))),0,get(paste0("MeanMsum06_",auxNombres[i]))/get(paste0("MeanMsum12_",auxNombres[i]))) ] }
for (i in 1:length(auxNombres)) { datos[, paste0("Ratio12_24",auxNombres[i]):= ifelse(is.na(get(paste0("MeanMsum12_",auxNombres[i]))/get(paste0("MeanMsum24_",auxNombres[i]))),0,get(paste0("MeanMsum12_",auxNombres[i]))/get(paste0("MeanMsum24_",auxNombres[i]))) ] }
for (i in 1:length(auxNombres)) { datos[, paste0("Ratio12_36",auxNombres[i]):= ifelse(is.na(get(paste0("MeanMsum12_",auxNombres[i]))/get(paste0("MeanMsum36_",auxNombres[i]))),0,get(paste0("MeanMsum12_",auxNombres[i]))/get(paste0("MeanMsum36_",auxNombres[i]))) ] }

#---------------------------------------------------- Identificación Variables Validas ------------------------------------------------------------

# Fijamos la dirección de la carpeta en donde se encuentra el script con las funciones auxiliares para el análisis de variables
setwd(dir.s)
source("Tidy_data.R")

# Filtramos la data de modelamiento (Esta sera la información a modelar)
mod <- datos[ModVal == 0]
dim(mod)
datos[,table(ModVal)]

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
dKS <- as.data.table(dKS)
dKS |> print(150)

dKS$Categoria <- NA
dKS[,Categoria:=ifelse(is.na(str_extract(Variable, "(?<=_\\d{2})[^_]+$")),Variable,str_extract(Variable, "(?<=_\\d{2})[^_]+$"))]
dKS |> print(125)
dKS[order(Categoria)] |> print(125)
dKS<-dKS[order(Categoria)] 


dKS1 <- dKS[, .SD[which.max(KS)], by = Categoria]
dKS1[,.(Variable,KS)][order(-KS)]
dKS1[KS>=0.454,.(Variable,KS)][order(-KS)]

vnum_KS <- unlist(dKS1[KS>=0.454]$Variable)
vnum_KS


#---------------------------------------------------- Categorización de Variables ------------------------------------------------------------

# Se hace en base a árboles de decisión
set.seed(123)
fc_tree <- datos|> woebin(y='VarDep', positive = 1 , method = 'tree',bin_num_limit = 2) 
fc_tree$Edad
woebin_plot(fc_tree$Edad)

dVI2 <- fc_tree |> map_df(function(x) pluck(x,10,1)) |> pivot_longer(cols=everything(), names_to = 'Variable', values_to = 'VI') |> arrange(desc(VI))
dVI2 <- as.data.table(dVI2)
dVI2

#Variables categoricas que ingresan al modelo
dVI2[VI>=0.23 & VI<=1]

dKS1[KS>=0.454,.(Variable,KS)][order(-KS)]

rio::export(dVI2,"bdd/TestVI.xlsx")
info2BIN <- datos|> woebin_ply(fc_tree,to='bin')
info2BIN
fc_tree$Edad$posprob
fc_tree$Edad$total_iv

info2woe <- datos |> woebin_ply(fc_tree,to='woe')
info2woe$Abonos_16Msum_woe

df <- data.frame(edad = c(25, 40, 60, 30, 50),
                 grupo = c("A", "B", "C", "A", "B"))

# Utilizar case_when para clasificar la edad en categorías
df <- df %>%
  mutate(categoria = case_when(
    edad < 30 ~ "Joven",
    edad >= 30 & edad < 50 ~ "Adulto",
    edad >= 50 ~ "Mayor",
    TRUE ~ "Desconocido"  # Esto es como un "else" por defecto
  ))

df<-setDT(df)
class(df
      )
df[,CATE:=case_when(
  edad < 30 ~ "Joven",
  edad >= 30 & edad < 50 ~ "Adulto",
  edad >= 50 ~ "Mayor",
  TRUE ~ "Desconocido"  # Esto es como un "else" por defecto
)]

fc_tree$Ratio03_06Abonos

