#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tidy data %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Este script contiene funciones auxiliares para el desarrollo del trabajo. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#~~~~~~~~~~~~~~~~~~ Porcentaje de NA
porcNA <- function(x){
  porc <- mean(is.na(x))
  return(porc) 
}

#~~~~~~~~~~~~~~~~~~ Identificación de variables constantes (numéricas o no)
constante <- function(x){
  if(class(x)=="numeric"){
    cte <- min(x, na.rm = TRUE)==max(x, na.rm = TRUE)
  } else {
    tc <- prop.table(table(x))>=0.90
    cte <- any(tc)
  }
  return(cte) 
}


#~~~~~~~~~~~~~~~~~~ Test KS (Kolmogorov-Smirnov de dos muestras)
# Esta función realiza el Test KS (Kolmogorov-Smirnov de dos muestras) para verificar si las distribuciones de las variables son similares
# entre los grupos de buenos y malos.
TestKS <- function(x, y){
        if(class(x)!="character"){
                vars <- data.frame(y,x)
                vars_e <- subset(vars,subset=vars[,1]==1)
                vars_f <- subset(vars,subset=vars[,1]==0)
                ks <- suppressWarnings(ks.test(vars_e[,2],vars_f[,2],alternative="two.sided"))
                ks <- round(as.numeric(ks$statistic),4)
        } else{
                ks <- 0
        }
        return(ks)
}

#~~~~~~~~~~~~~~~~~~ Valor de Información (IV)
# Calcula el Valor de Información (IV) de una variable categórica con respecto a la variable de buenos y malos.

TestVI <- function(x,y){
        if(class(x)=="character"){
                tc <- table(y,x)
                f1 <- tc[1,]
                f2 <- tc[2,]
                aux1 <- ifelse(f1/sum(f1)==0,0.001,ifelse(f1/sum(f1)==1,0.999, f1/sum(f1)))
                aux2 <- ifelse(f2/sum(f2)==0,0.001,ifelse(f2/sum(f2)==1,0.999, f2/sum(f2)))
                wof <- log(aux2/aux1)
                wof <- ifelse(wof==-Inf,0,wof)
                VI <-   sum(((f2/sum(f2))-(f1/sum(f1)))*wof)
        }else{
                VI <- 0
        }
        return(VI)
}

#~~~~~~~~~~~~~~~~~~ Correlacion superior a un valor dado 
DVarCorr <- function(data, corr.max = 0.75){
        COR.AUX <- cor(data)
        pos <- which(((abs(COR.AUX)>=corr.max) & (row(COR.AUX) < col(COR.AUX))), arr.ind=T)
        if(nrow(pos)>0){
                col_elim <- numeric(nrow(pos))
                for(i in seq(1:nrow(pos))){
                        aux_col_elim <- c(pos[i,1],pos[i,2])
                        if (!any(col_elim %in% aux_col_elim)){
                                col_elim [i] <- pos[i,which.max(c(pos[i,1],
                                                                  pos[i,2]))]
                        }
                }
                if(length(col_elim)>0){
                        col_elim <- unique(col_elim[col_elim>0])
                        vars <- names(data)[-(col_elim)]
                        data <- data.frame(data[,-(col_elim)])
                        colnames(data) <- vars
                }
        }
        return(data)
}

#~~~~~~~~~~~~~~~~~~ Diferencia de fechas en meses
num_anios <- function(end_date, start_date) {
        ed <- as.POSIXlt(end_date)
        sd <- as.POSIXlt(start_date)
        anio <- floor((12 * (ed$year - sd$year) + (ed$mon - sd$mon))/12)
        return(anio)
}

num_meses <- function(end_date, start_date){
        ed <- as.POSIXlt(end_date)
        sd <- as.POSIXlt(start_date)
        mes <- floor(12 * (ed$year - sd$year) + (ed$mon - sd$mon))
        return(mes)
}

#~~~~~~~~~~~~~~~~~~ Función de reemplazo NA's
reemplazo_col = function(dt, vars, valor){ 
        na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
        for (i in vars)
                eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}

#~~~~~~~~~~~~~~~~~~ Función encera el codigo provincia, canton, parroquia
cod2dig <- function(vector){
        vector <- ifelse(str_length(vector)==1, paste0('0', vector), vector)
        return(vector)
}