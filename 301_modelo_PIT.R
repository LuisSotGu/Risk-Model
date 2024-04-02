# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Modelo PIT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Este script está diseñado para analizar y comparar el rendimiento de dos modelos: uno proporcionado por el buró de crédito 
# y otro creado bajo la filosofía (PIT). Se utiliza una regresión logística para modelar el problema y evalúa cómo se 
# desempeñan ambos modelos en dos conjuntos de datos diferentes: uno para entrenar el modelo y otro para validar sus resultados.

# Los resultados se guardan en un archivo de Excel llamado "Tablas_Performance_PIT.xlsx" en la carpeta de resultados. 
# Además, de las tablas performance se encuentran métricas como KS, ROC y GINI para cada modelo-base.

# También se obtienen los grupos de Score para esta filosofía.

# ¡¡¡¡¡¡Aviso importante!!!!!

# No es necesario ejecutar este script de forma independiente. Se puede ejecutar desde el script 001_Ejecutar_Proyecto.R 
# para asegurar una ejecución ordenada y completa del proyecto.

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\t\t\t\t\tModelo PIT' )
message( paste( rep( '-', 100 ), collapse = '' ) )

#------------------------------------------------------- Direcciones y Funciones --------------------------------------------------------

# Saca los deciles del Score y categoriza cada intervalo
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

#---------------------------------------------------- Lectura de la base Modelamiento ------------------------------------------------------------

patron <- "\\InfoModelamiento\\b"
archivos_filtrados <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)
load(archivos_filtrados)

#---------------------------------------------------- Graficas: Género  ------------------------------------------------------
datos[,table(Genero,useNA = "always")]
100*datos[,prop.table(table(Genero,useNA = "always"))]
datos[,prop.table(table(Genero,VarDep),1)]
datos[,prbm_genero_agrupada:=ifelse(Genero=="M",0.1502617,0.1835693)]
datos[,table(Genero,prbm_genero_agrupada,useNA = "always")]

tabla_resultados <- datos |>
  group_by(TipoCredito) |>
  summarise(
    Proporcion_A = n() / nrow(datos),  # Proporción según columna A
    Porcentaje_B = sum(VarDep == "1") / n()  # Porcentaje según CategoriaX en columna B
  )

# Crea el gráfico de barras para las frecuencias
grafico_barras <- ggplot(tabla_resultados, aes(x = TipoCredito, y = Proporcion_A)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  labs(title = "Frecuencia por Género",
       x = "Género",
       y = "Frecuencia")+theme_minimal()

# Crea el gráfico de líneas para los porcentajes de malos
grafico_lineas <- grafico_barras +
  geom_line(aes(y = Porcentaje_B * 3), color = "red", size = 1, group = 1) +
  geom_point(aes(y = Porcentaje_B * 3), color = "red", size = 3, shape = 19)+
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Porcentaje Malos"))

# Muestra el gráfico combinado
print(grafico_lineas)

#---------------------------------------------------- Base de Modelamiento TTC ------------------------------------------------------------

# Filtramos la base de Modelamiento
mod <- datos[ModVal == 0 ]
dim(mod)
mod[,prop.table(table(VarDep))]

#----------------------------------------------- Lectura Plantilla Tablas Performance -------------------------------------------------------

#~~~~~~~~~~~~ Lectura de las tablas performance:

patron <- "\\Plantilla_Tablas_Performance\\b"
# Direccion del excel con la plantilla de las tablas performance:
dir.tpf <- list.files(dir.p, pattern = patron, full.names = TRUE, recursive = TRUE)

# Se carga la plantilla
plantilla<- loadWorkbook(dir.tpf)

#--------------------------------------------- Tablas Performance: Buro-Base-Total -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
res_Buro <- data.table(Var=datos$VarDep, Score=datos$ScoreRiesgo)
res_Buro$Rango <- rango_score(res_Buro$Score)
# Informacion a poner en el excel
rangos<-res_Buro[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_Buro[,table(Rango, Var)]# Frecuencias de buenos y malos por Rango de Score
frecuencias
100*res_Buro[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score

#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Buro-Base-Total",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Buro-Base-Total",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Buro-Base-Total",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Buro-Base-Total",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango

#--------------------------------------------- Tablas Performance: Buro-Base-Modelizacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
res_mod_Buro <- data.table(Var=mod$VarDep, Score=mod$ScoreRiesgo)
res_mod_Buro$Rango <- rango_score(res_mod_Buro$Score)
# Informacion a poner en el excel
rangos<-res_mod_Buro[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_mod_Buro[,table(Rango, Var)]# Frecuencias de buenos y malos por Rango de Score
frecuencias
100*res_mod_Buro[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score

#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Buro-Base-Modelizacion",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Buro-Base-Modelizacion",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Buro-Base-Modelizacion",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Buro-Base-Modelizacion",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango

#--------------------------------------------- Tablas Performance: Buro-Base-Validacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de validacion
val <- datos[ModVal == 1]
res_val_Buro <- data.table(Var=val$VarDep, Score=val$ScoreRiesgo)
res_val_Buro$Rango <- rango_score(res_val_Buro$Score)
# Informacion a poner en el excel
rangos<-res_val_Buro[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_val_Buro[,table(Rango, Var)]# Frecuencias de buenos y malos por Rango de Score
frecuencias
100*res_val_Buro[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score

#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Buro-Base-Validacion",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Buro-Base-Validacion",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Buro-Base-Validacion",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Buro-Base-Validacion",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango

#---------------------------------------------------- Regresion Logistica -------------------------------------------------------

#~~~~~~~~~~~~ Se escogen las variables del modelo

variables_modelo<-c("prbm_cuotasvencidas_1M","prbm_diasmorosidad_3M","prbm_domicilio_agrupada","prbm_tipovivienda","Marca_Garantia_PAG",
                    "r_endeudamiento"
                    ,"prbm_estadocivil_agrupada","prbm_tipocredito","Marca_reestructuracion")
numvariable<-1

#~~~~~~~~~~~~ Se crea la fórmula del modelo
formula<-"VarDep~"

for(variable in variables_modelo){
  if(numvariable==1){
    formula<-paste0(formula," ",variable," ")
  }else{
    formula<-paste0(formula,"+"," ",variable," ")
  }
  numvariable<-numvariable+1
}

# Formula del modelo
message( paste( rep( '~', 100 ), collapse = '' ) )
message( '\t\t\t\t\tFormula del Modelo' )
message(formula)
message( paste( rep( '~', 100 ), collapse = '' ) )

#~~~~~~~~~~~~ Ajuste regresión logistica (data modelamiento)
modelo <- glm(formula = as.formula(formula), family = binomial("logit"), data = mod)
summary(modelo)

#---------------------------------------------------- Analisis de Correlación -------------------------------------------------------

#~~~~~~~~~~~~ Matriz de Correlacion:
res <- cor(setDT(mod)[,variables_modelo, with=FALSE])
res

#~~~~~~~~~~~~ Graficos de la matriz de correlación
ggcorrplot(res, hc.order = TRUE, type = "upper",method = "circle",lab = TRUE)

ggcorrplot(res, hc.order = TRUE, type = "upper",method = "circle")

ggcorrplot(res, hc.order = TRUE, type = "upper",
           outline.col = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"),
           lab=TRUE)

ggcorrplot(res, hc.order = TRUE, type = "lower",outline.col = "white",
           lab = TRUE)

#~~~~~~~~~~~~ Análisis espectral de la matriz de autocorrelación
# Condición de número de la matriz de correlación
sqrt(max(eigen(res)$values)/min(eigen(res)$values))
min(eigen(res)$values)
max(eigen(res)$values)

# Como el indice de condicionamiento  es  menor a 10 se descarta problemas de multicolinealidad

#---------------------------------------------------- GVIF -------------------------------------------------------
vif(modelo)
#---------------------------------------------------- Score Modelo -------------------------------------------------------

# Calculo del Score para todos los individuos ( bases de modelamiento y validacion)

#~~~~~~~~~~~~ Coeficientes del Modelo 
coeficientes_modelo<-modelo$coefficients

#~~~~~~~~~~~~ Fórmula ajustada del modelo
formula_modelo<-paste0(coeficientes_modelo[1])
numvar<-1

for(variable in variables_modelo){#variable<-variables_modelo[1]
  if(coeficientes_modelo[numvar+1]>0){
    formula_modelo<-paste0(formula_modelo," + ",coeficientes_modelo[numvar+1],"*",variable," ") 
  }else{
    formula_modelo<-paste0(formula_modelo," ",coeficientes_modelo[numvar+1],"*",variable," ")
  }
  numvar<-numvar+1
}

message( paste( rep( '~', 100 ), collapse = '' ) )
message( '\t\t\t\t\tModelo Ajustado' )
message(paste0("VarDep~ ",formula_modelo))
message( paste( rep( '~', 100 ), collapse = '' ) )

#~~~~~~~~~~~~ Scores dados por el modelo
eval(parse(text=paste0("datos[, Y:=",formula_modelo,"]")))
datos[, SCORE_PIT := ceiling(1000/(1+exp(Y)))]# Probabilidad de ser bueno 

#La regresion logistica da las probabilidades de ser malo porque hemos considerado 1 a ellos

#--------------------------------------------- Tablas Performance: Modelo-Base-Modelizacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de modelizacion
mod <- datos[ModVal == 0]
res_mod <- data.table(Var=mod$VarDep, Score=mod$SCORE_PIT)
res_mod$Rango <- rango_score(res_mod$Score)
# Informacion a poner en el excel
rangos<-res_mod[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_mod[,table(Rango, Var)]# Frecuencias de buenos y malos por Rango de Score
frecuencias
100*res_mod[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score

#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Modelo-Base-Modelizacion",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango

#--------------------------------------------- Tablas Performance: Modelo-Base-Validacion -----------------------------------------------

#~~~~~~~~~~~~ Obtenemos la Información:
# Base de validacion
val <- datos[ModVal == 1]
res_val <- data.table(Var=val$VarDep, Score=val$SCORE_PIT)
res_val$Rango <- rango_score(res_val$Score)
# Informacion a poner en el excel
rangos<-res_val[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]# Intervalos Score
rangos
frecuencias<-res_val[,table(Rango, Var)]# Frecuencias de buenos y malos por Rango de Score
frecuencias
100*res_val[,prop.table(table(Rango, Var),1)]# Tasa de buenos y malos por Rango de Score


#~~~~~~~~~~~~ Cargamos la información a la plantilla:
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(rangos$Min),startCol = 1,startRow = 3)# Cargamos los minimos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(rangos$Max),startCol = 2,startRow = 3)# Cargamos los maximos de cada rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(frecuencias[1:10]),startCol =4,startRow = 3)# Cargamos los buenos por rango
writeData(plantilla, sheet = "Modelo-Base-Validacion",x=as.numeric(frecuencias[11:20]),startCol =5,startRow = 3)# Cargamos los malos por rango

#--------------------------------------------- Almacenamiento de Resultados -----------------------------------------------

# Guardamos las tablas performances
saveWorkbook(plantilla,paste(dir.r,"Tablas_Performance_PIT.xlsx",sep="/"),overwrite = TRUE)

# Se guarda la data generada paa este modelo
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoPIT.RData")

#-----------------------------------------------------------  Graficas Modelo ---------------------------------------------------------------

#~~~~~~~~~~~~ Grafica Curva ROC del modelo

objroc1 <- roc(res_mod$Var, res_mod$Score, auc=T, ci=T)
objroc2 <- roc(res_val$Var, res_val$Score, auc=T, ci=T)
plot(objroc1, col="blue", xlab="1 - Especificidad", ylab="Sensibilidad", main="Comparación curvas ROC", legacy.axes = TRUE)
plot(objroc2, col="red", add=TRUE)
legend("bottomright", legend=c("Modelamiento", "Validación"), col=c("blue", "red"), lwd=1.4)

#-----------------------------------------------------------  Matriz Confusion ---------------------------------------------------------------

#~~~~~~~~~~~~ Modelamiento:
objroc1 <- roc(res_mod$Var, res_mod$Score, auc=T, ci=T)
cutoff <- coords(objroc1, "best", best.method = "youden")$threshold
cutoff<-cutoff/1000
y_predicho=(modelo$fitted.values>=1-cutoff)*1
y_predicho=as.factor(y_predicho)
y=as.factor(mod$VarDep)
length(y_predicho)
confusionMatrix(y_predicho,y)

#~~~~~~~~~~~~ Validacion:
objroc1 <- roc(res_mod$Var, res_mod$Score, auc=T, ci=T)
cutoff <- coords(objroc1, "best", best.method = "youden")$threshold
cutoff<-cutoff/1000
y_predicho<-(1-val$SCORE_PIT/1000>=1-cutoff)*1
y_predicho<-as.factor(y_predicho)
y=as.factor(val$VarDep)
length(y_predicho)
length(y)
confusionMatrix(y_predicho,y)


#--------------------------------------------------------- Grupos de Score  ------------------------------------------------------------

fc_tree <- mod|> woebin(y='VarDep', x='SCORE_PIT',positive = 1 , method = 'tree',bin_num_limit = 20)
woebin_plot(fc_tree$SCORE_PIT)

mod_f<-mod|>filter(SCORE_PIT>=820)

fc_tree <- mod_f|> woebin(y='VarDep', x='SCORE_PIT',positive = 1 , method = 'tree',bin_num_limit = 20)
woebin_plot(fc_tree$SCORE_PIT)

mod_f<-mod|>filter(SCORE_PIT<820)

fc_tree <- mod_f|> woebin(y='VarDep', x='SCORE_PIT',positive = 1 , method = 'tree',bin_num_limit = 3)
woebin_plot(fc_tree$SCORE_PIT)

datos[,Grupos_PIT:=case_when(SCORE_PIT<540~"E",
                             SCORE_PIT>=540 & SCORE_PIT<820~"D",
                             SCORE_PIT>=820 & SCORE_PIT<910~"C",
                             SCORE_PIT>=910 & SCORE_PIT<960~"B",
                             TRUE~"A")]


grupos<-datos|>dplyr::select(Fecha,ModVal,VarDep,Grupos_PIT)
#grupos<-grupos|>as_tsibble(index=Fecha,key=c(Grupos_TTC,ModVal))

grupos<-grupos|>group_by(Fecha,Grupos_PIT)|>mutate(Num_individuos=n())
grupos<-as.data.frame(grupos)

grupos<-grupos|>group_by(Fecha,Grupos_PIT)|>filter(VarDep==1)|>mutate(Num_malos=n())
grupos<-as.data.frame(grupos)

grupos<-as.data.table(grupos)

grupos<-grupos|>group_by(Fecha,Grupos_PIT)|>summarise(Num_individuos=mean(Num_individuos),
                                                      Num_malos=mean(Num_malos))

grupos<-as.data.table(grupos)

grupos<-grupos|>mutate(tasa_malos=Num_malos/Num_individuos)

grupos|>group_by(Grupos_PIT)|>summarise(tasa_media_malos=mean(tasa_malos))

grupos|>group_by(Grupos_PIT)|>summarise(Num_individuos=sum(Num_individuos),
                                        Num_malos=sum(Num_malos))



grupos<-grupos|>as_tsibble(index=Fecha,key=c(Grupos_PIT))

grupos|>autoplot(tasa_malos)

ggplot(grupos, aes(x = Fecha, y = tasa_malos, color = Grupos_PIT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 1)) +
  labs(title = "Tasa de malos ajustada ",
       x = "Tiempo",
       y = "Tasa de malos") +
  theme(
    plot.title = element_text(size = 10),  # Tamaño del título
    axis.title.x = element_text(size = 8),  # Tamaño del título del eje x
    axis.title.y = element_text(size = 8), # Tamaño del título del eje y
    legend.text = element_text(size =8 ),
    legend.title = element_text(size = 8) 
  )

print(grafico)

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

setwd(dir.p)

