# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tratamiento Variables %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# En este script se realizará la ingeniería de  variables a corde a los resultados de la base de modelización (mod), además 
# se guarda la data generada en un archivo RData, InfoModelamiento.RData, en la subcarpeta RData de la carpeta BDD.

# La recategorización de variables se realizó teniendo en cuenta la representatividad y el porcentaje de malos pagadores en las 
# categorías resultantes. Una estrategia más sólida para la categorización de variables es el uso de árboles de decisión, 
# que se encuentra implementado en la biblioteca scorecard, como se muestra en el script 201_ingenieria_variables.R de este proyecto. 
# Es importante señalar que este script no forma parte del proyecto final. Además, este método también está implementado
# en otro proyecto de Credit Scoring disponible en el siguiente enlace: https://github.com/RENE-2312/TIC-RENE-2023B 
# en el script 200_tratamiento_identificacion_variables.R.

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
mod <- datos |> filter(ModVal==0)

#---------------------------------------------------- Provincias  ------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Información respecto a Provincias Agrupadas: (Categoria Base:Pichincha)

# Se agruparon a las provincias de domicilio respecto a su representatividad así como por su tasa de malos. El análisis es 
# similar al  caso anterior.

# Marca_Domicilio_Agrupada. Si ProvinciaDomicilio es Pichincha (1) , Guayas (2), STD o Chimborazo (3), Otras Provincias (4)

datos[,Marca_Domicilio_Agrupada:=ifelse(ProvinciaDomicilio=="Pichincha",1,
                                        ifelse(ProvinciaDomicilio=="Guayas",2,
                                               ifelse(ProvinciaDomicilio %in%c ("Santo Domingo De Los Tsachilas","Chimborazo"),3,4)))]
mod <- datos |> filter(ModVal==0)

mod[,table(Marca_Domicilio_Agrupada,useNA = "always")]

# Observamos la probabilidad de ser buen/mal pagador dado la provincia agrupada a la que pertenecen
mod[,prop.table(table(Marca_Domicilio_Agrupada,VarDep,useNA = "always"),1)]

# Creacion de la Variable, prbm_domicilio_agrupada, probabilidad de ser mal pagador dada la provincia agrupada
# (Categoria Base:Pichincha):
datos[,prbm_domicilio_agrupada:=ifelse(Marca_Domicilio_Agrupada==4 | is.na(Marca_Domicilio_Agrupada),0.2626087,
                                       ifelse(Marca_Domicilio_Agrupada==3,0.1886544,
                                              ifelse(Marca_Domicilio_Agrupada==2,0.5090090,0.1385184)))]

# Marca_Vivienda_Agrupada. Si TipoVivienda es N (1), P o S (2), Otros Tipos (3)
datos[!is.na(TipoVivienda),Marca_Vivienda_Agrupada:=ifelse(TipoVivienda=="N",1,
                                                           ifelse(TipoVivienda %in% c("P","S"),2,3))]
mod <- datos |> filter(ModVal==0)
mod[,table(Marca_Vivienda_Agrupada,useNA ="always")]
100*mod[,prop.table(table(Marca_Vivienda_Agrupada,useNA ="always"))]


# Observamos la probabilidad de ser buen/mal pagador dado el tipo de vivienda
mod[,prop.table(table(Marca_Vivienda_Agrupada,VarDep,useNA = "always"),1)]

# Creacion de la Variable,prbm_tipovivienda, probabilidad de ser mal pagador dado el tipo de vivienda
# (Categoria Base:1):
datos[,prbm_tipovivienda:=ifelse(is.na(Marca_Vivienda_Agrupada)| Marca_Vivienda_Agrupada==3,0.23345492,
                                 ifelse(Marca_Vivienda_Agrupada==2,0.18255578,0.09992561))]


#---------------------------------------------------- Ratios  ------------------------------------------------------

datos[, r_endeudamiento := ifelse(TotalPatrimonio> 0, TotalPasivo/TotalPatrimonio, 0)]

#---------------------------------------------------- Tipo Garantia  ------------------------------------------------------

datos[,table(TipoGarantia,useNA = "always")]
100*datos[,prop.table(table(TipoGarantia,useNA = "always"))]
datos[,prop.table(table(TipoGarantia,VarDep),1)]
datos[,Marca_Garantia_PAG:=ifelse(TipoGarantia=="PAG",1,0)]

#----------------------------------------------------- Estado Civil  ------------------------------------------------------

mod[,table(EstadoCivil,useNA = "always")]
100*mod[,prop.table(table(EstadoCivil,useNA = "always"))]
mod[,prop.table(table(EstadoCivil,VarDep),1)]
datos[,EstadoCivil_Agrupada:=ifelse(EstadoCivil%in%c ("C","V"),1,
                                    ifelse(EstadoCivil=="S",2,3))]
mod <- datos |> filter(ModVal==0)
mod[,table(EstadoCivil_Agrupada,useNA = "always")]

# Observamos la probabilidad de ser buen/mal pagador dado el estado civil agrupado a la que pertenecen
mod[,prop.table(table(EstadoCivil_Agrupada,VarDep,useNA = "always"),1)]
# Creacion de la Variable, prbm_estadocivil_agrupada, probabilidad de ser mal pagador dado el estado civil agrupado
# (Categoria Base:Casado-Viudo):
datos[,prbm_estadocivil_agrupada:=ifelse(EstadoCivil_Agrupada==3 | is.na(EstadoCivil_Agrupada),0.1935271,
                                         ifelse(EstadoCivil_Agrupada==2,0.2360976,0.1260904))]

#------------------------------------------------------------------ Tipo Credito -----------------------------------------------------------------

datos[,Marca_credito_MS:=ifelse(TipoCredito=="MS",1,0)]
mod <- datos |> filter(ModVal==0)
mod[,prop.table(table(Marca_credito_MS,VarDep),1)]
datos[,prbm_tipocredito:=ifelse(Marca_credito_MS==1,0.2749922,0.1202118)]

#------------------------------------------------------------------ Reestructuracion -----------------------------------------------------------------

datos[,Marca_reestructuracion:=ifelse(Reestructuracion=="SI",1,0)]

#------------------------------------------------------------------ Reefinanciamiento-----------------------------------------------------------------

datos[,Marca_refinanciamiento:=ifelse(Refinanciamiento=="SI",1,0)]

#------------------------------------------------------------------ NumCuotasVencidas  -----------------------------------------------------------------

#~~~~~~~~~~~~~~~ 1 Mes
datos[,table(NumCuotasVencidas_01Msum,useNA = "always")]
datos[,Marca_NumCuotasVencidas_1M:=ifelse(NumCuotasVencidas_01Msum>0,1,0)]
mod <- datos |> filter(ModVal==0)
mod[,table(Marca_NumCuotasVencidas_1M,useNA = "always")]

mod[,table(Marca_NumCuotasVencidas_1M,VarDep)]
mod[,prop.table(table(Marca_NumCuotasVencidas_1M,VarDep),1)]
# Base (Tienes más de una cuota vencida)
datos[,prbm_cuotasvencidas_1M:=ifelse(Marca_NumCuotasVencidas_1M==0,0.03727927,0.29409433)]

#~~~~~~~~~~~~~~~ 3 Meses
datos[, Total_DiasMorosidad_3M := NumDiasMorosidad_01Msum+NumDiasMorosidad_02Msum+NumDiasMorosidad_03Msum]

datos[,Marca_DiasMorosidad_3M:=ifelse(Total_DiasMorosidad_3M>0,1,0)]
datos[,table(Marca_DiasMorosidad_3M)]
mod <- datos |> filter(ModVal==0)
mod[,prop.table(table(Marca_DiasMorosidad_3M,VarDep),1)]
datos[,prbm_diasmorosidad_3M:=ifelse(Marca_DiasMorosidad_3M==1,0.44050828,0.07262649)]

#----------------------------------------------------------- Fechas -------------------------------------------------------------------------------------------------

Fechas<-c(rep(1,1376),rep(2,1376),rep(3,1376),rep(4,1376),rep(5,1376),rep(6,1376),rep(7,1376),rep(8,1376),rep(9,1376),rep(10,1378))
datos[, Fecha:=Fechas]

# Se guarda la data generada
setwd(paste(dir.p,"BDD","RData",sep="/"))
save(list = c("datos"), file = "InfoModelamiento.RData")

rm(list=setdiff(ls(),c("dir.p","dir.r","dir.s")))
gc()

# Se restaura la dirección de trabajo a la carpeta principal del proyecto
setwd(dir.p)
