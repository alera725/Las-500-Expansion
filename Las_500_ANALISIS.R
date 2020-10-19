suppressMessages(library(ggplot2))
suppressMessages(library(plotly))
suppressMessages(library(Quandl))
suppressMessages(library(TTR))
suppressMessages(library(PortfolioAnalytics))
suppressMessages(library(quantmod))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(readxl))
suppressMessages(library(plotly))
suppressMessages(library(NormalLaplace))
suppressMessages(library(ExtDist))
suppressMessages(library(MASS))
suppressMessages(library(DiscreteLaplace))
suppressMessages(library(extraDistr))
suppressMessages(library(smoothmest))

require(reshape2)

# Aqui ponemos la ubicacion en donde se encuentran los archivos de Excel y este programa
# Deben de estar en la misma carpeta 

setwd("~/Documents/ALEJANDRO RAMOS GUTIERREZ/ITESO/6TO SEMESTRE/BECARIA")

#-------------------------------------- IMPORTAR DATOS, ACOMODO DE DATOS Y CALCULO DE TASAS ------------------------------------------

# -- Leemos la base de datos de las ventas de las Empresas
Datos <- readxl::read_excel("Pruebar.xlsx", sheet = 1)
Datos <- as.data.frame(Datos)

# -- Eliminamos los 0`s de la base de "Datos" y los convertimos en NA`s

for (i in 5:length(Datos)){
  
  index <- which(Datos[,i]==0)
  Datos[index,i] <- NA
  
}

# -- Eliminamos columnas irrelevantes Propiedad, Naics y Registros 
Datos_anual <- Datos[,-c(2,3,4)]

# -- En PNR guardamos las variables de Propiedad, Naics y Registros de cada una de las empresas 
dim_pr<-dim(Datos)
PNR<-data.frame(Datos)
PNR<-PNR[,-c(5:dim_pr[2])]
PNR<-PNR[,-1]
Companies_names<-as.character(unlist(Datos[,1])) # Guardamos el nombre de las compa??ias 
PNRTrans<-as.data.frame(r1=names(PNR), t(PNR)) # Aqui se transpone PNR para facilitar su manipulacion
colnames(PNRTrans)<-Companies_names

# -- Transponemos el DataFrame para facilitar su manejo dentro del entorno de R
Ventas <- data.frame(Datos[,-1])
Ventas <- as.data.frame(r1=names(Ventas), t(Ventas))
colnames(Ventas)<-as.character(unlist(Datos[,1]))

# -- Eliminamos REGISTROS NAICS Y PROPIEDAD DEL DATAFRAME VENTAS
Ventas <- Ventas[-1,]
Ventas <- Ventas[-1,]
Ventas <- Ventas[-1,]

rownames(Ventas) <- seq(1978,2016,1) # El nombre de los renglones seran los a??os de analisis

# -- Normalizamos las Ventas
Ventas_Normalizadas<-data.frame(Ventas)
colnames(Ventas_Normalizadas) <- colnames(Ventas)
dim_ventas <- dim(Ventas)

for (i in 1:dim_ventas[2]){
  if (sum(is.na(Ventas[,i]))>0){
    vec <- na.omit(Ventas[,i])
    mean <- mean(vec)
    sd <- sd(vec)
    for(j in 1:nrow(Ventas_Normalizadas)){
      
      Ventas_Normalizadas[j,i]<-(Ventas_Normalizadas[j,i]-mean)/sd
      
    }
  }else{
    mean <- mean(Ventas[,i])
    sd <- sd(Ventas[,i])
    for(j in 1:dim_ventas[1]){
      
      Ventas_Normalizadas[j,i]<-(Ventas_Normalizadas[j,i]-mean)/sd
      
    }
  }
}

# Aqui TRATAREMOS DE GRAFICAR TODAS LAS Ventas Normalizadas con Pltoly
# Create data
my_y=Ventas_Normalizadas[,1]
my_x=as.numeric(rownames(Ventas_Normalizadas))

# Let's do a first plot
p<-plot_ly(y=my_y, x=my_x , type="scatter", mode="markers+lines",  name = paste(colnames(Ventas_Normalizadas)[1],sep=" "), evaluate = TRUE) %>%
  layout(
    title = "Mexican Companies Normalized Sales (1978-2016)",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Normalized Sales"))

# Add 9 trace to this graphic with a loop!
for(i in 2:ncol(Ventas_Normalizadas)){
  
  my_y=Ventas_Normalizadas[,i]
  p<-add_trace(p, y=my_y, type="scatter", mode="markers+lines", name = paste(colnames(Ventas_Normalizadas)[i],sep=" "), evaluate = TRUE)
  
}

p

# -- Calcularemos las tasas de crecimiento de ventas de cada empresa
Tasas_ventas <- data.frame(Datos)
Tasas_ventas <- subset(Tasas_ventas, select = -c(PROPIEDAD, NAICS, REGISTROS)) #Eliminamos columnas que no se requieren 

dimension_pruebar <- dim(Tasas_ventas)

for (j in 1:dimension_pruebar[1]){
  for (i in 3:dimension_pruebar[2]){
    
    Tasas_ventas[j,i-1]<-(Tasas_ventas[j,i]/Tasas_ventas[j,i-1])-1 #Si no redondeamos a 2 decimales se ve la grafica bonita
  
  }
}

Tasas_ventas <- subset(Tasas_ventas, select = -c(X2016)) #Eliminamos columnas que no se requieren 

# Aqui TRATAREMOS DE GRAFICAR TODAS LAS TASAS DE CRECIMIENTO EN VENTAS con Pltoly
# Create data
Tasas_ventas_t <- as.data.frame(t(Tasas_ventas))
Tasas_ventas_t <- Tasas_ventas_t[-1,]
colnames(Tasas_ventas_t) <- colnames(Ventas_Normalizadas)
rownames(Tasas_ventas_t) <- rownames(Ventas_Normalizadas)[1:nrow(Ventas_Normalizadas)-1]
my_y = apply(as.matrix(Tasas_ventas_t[,1]), 2, as.numeric)
my_y = my_y[,1]
my_x = as.numeric(rownames(Tasas_ventas_t))

# Let's do a first plot
pt<-plot_ly(y=my_y, x=my_x , type="scatter", mode="lines",  name = paste(colnames(Tasas_ventas_t)[1],sep=" "), evaluate = TRUE) %>%
  layout(
    title = "Mexican Companies Growth Rates (1978-2016)",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Growth Rates"))

# Add 9 trace to this graphic with a loop!
for(i in 2:ncol(Tasas_ventas_t)){
  
  my_y = apply(as.matrix(Tasas_ventas_t[,i]), 2, as.numeric)
  my_y = my_y[,1]
  pt<-add_trace(pt, y=my_y, type="scatter", mode="lines", name = paste(colnames(Tasas_ventas_t)[i],sep=" "), evaluate = TRUE)
  
}

pt

# -- Ahora calularemos el promedio de las tasas de crecimiento de cada a??o 

# -- Reacomodamos transponiendo el DataFrame de Tasas para mejor manejo
Tasas_ventas <- data.frame(r1=names(Tasas_ventas), t(Tasas_ventas))
colnames(Tasas_ventas)<-as.character(unlist(Tasas_ventas[1,]))
Tasas_ventas = Tasas_ventas[-1, ]
Tasas_ventas = Tasas_ventas[,-1]
rownames(Tasas_ventas)<-seq(from=1978, to=2015, by=1)
Tasas_promedio_anual <- data.frame("Periodo" = rownames(Tasas_ventas), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor_Tasa"=NA, "Empresa_Mayor_Tasa"=NA, "Menor_Tasa"=NA,  "Empresa_Menor_Tasa"=NA)

for (i in 1:nrow(Tasas_ventas)){
  Tasa_anual <- apply(as.matrix(Tasas_ventas[i,]), 2, as.numeric)
  Tasa_anual <- na.omit(Tasa_anual)
  Nombres <- colnames(Tasas_ventas)
  Nombres <- Nombres[-which(is.na(Tasas_ventas[i,1:length(Tasas_ventas[i,])]))]
  Tasas_promedio_anual$Promedio[i] <- mean(Tasa_anual)
  Tasas_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_anual)
  Tasas_promedio_anual$Mayor_Tasa[i] <- max(Tasa_anual)
  Tasas_promedio_anual$Empresa_Mayor_Tasa[i] <- Nombres[which(Tasa_anual==max(Tasa_anual))]
  Tasas_promedio_anual$Menor_Tasa[i] <- min(Tasa_anual)
  Tasas_promedio_anual$Empresa_Menor_Tasa[i] <- Nombres[which(Tasa_anual==min(Tasa_anual))]
}


# -- Leemos archivos de parametrsos secuandarios de Excel 
Empleo <- readxl::read_excel("Empleo_r.xlsx", sheet = 1)
Empleo <- as.data.frame(Empleo)
Empleo_anual <- data.frame(t(Empleo))
Empleo_anual <- Empleo_anual[-1,]
colnames(Empleo_anual) <- colnames(Tasas_ventas)

Utilidad_Operacion <- readxl::read_excel("Utilidad_Operacion_r.xlsx", sheet = 1)
Utilidad_Operacion <- as.data.frame(Utilidad_Operacion)
Utilidad_Operacion_anual <- data.frame(t(Utilidad_Operacion))
Utilidad_Operacion_anual <- Utilidad_Operacion_anual[-1,]
colnames(Utilidad_Operacion_anual) <- colnames(Tasas_ventas)

Utilidad_neta <- readxl::read_excel("Utilidad_neta_r.xlsx", sheet = 1)
Utilidad_neta <- as.data.frame(Utilidad_neta)
Utilidad_neta_anual <- data.frame(t(Utilidad_neta))
Utilidad_neta_anual <- Utilidad_neta_anual[-1,]
colnames(Utilidad_neta_anual) <- colnames(Tasas_ventas)

Activos <- readxl::read_excel("Activo_r.xlsx", sheet = 1)
Activos <- as.data.frame(Activos)
Activos_anual <- data.frame(t(Activos))
Activos_anual <- Activos_anual[-1,]
colnames(Activos_anual) <- colnames(Tasas_ventas)

Pasivos <- readxl::read_excel("Pasivo_r.xlsx", sheet = 1)
Pasivos <- as.data.frame(Pasivos)
Pasivos_anual <- data.frame(t(Pasivos))
Pasivos_anual <- Pasivos_anual[-1,]
colnames(Pasivos_anual) <- colnames(Tasas_ventas)

Patrimonio <- readxl::read_excel("Patrimonio_r.xlsx", sheet = 1)
Patrimonio <- as.data.frame(Patrimonio)
Patrimonio_anual <- data.frame(t(Patrimonio))
Patrimonio_anual <- Patrimonio_anual[-1,]
colnames(Patrimonio_anual) <- colnames(Tasas_ventas)

# -- Ahora calularemos el promedio por periodo de cada uno de los parametros 

# -- Empleo 
Empleo_promedio_anual <- data.frame("Periodo" = rownames(Empleo_anual), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor"=NA, "Empresa_Mayor"=NA, "Menor"=NA,  "Empresa_Menor"=NA)

for (i in 1:nrow(Empleo_anual)){
  Tasa_empleo_anual <- apply(as.matrix(Empleo_anual[i,]), 2, as.numeric)
  Tasa_empleo_anual <- na.omit(Tasa_empleo_anual)
  Nombres <- colnames(Empleo_anual)
  Nombres <- Nombres[-which(is.na(Empleo_anual[i,1:length(Empleo_anual[i,])]))]
  Empleo_promedio_anual$Promedio[i] <- mean(Tasa_empleo_anual)
  Empleo_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_empleo_anual)
  Empleo_promedio_anual$Mayor[i] <- max(Tasa_empleo_anual)
  Empleo_promedio_anual$Empresa_Mayor[i] <- Nombres[which(Tasa_empleo_anual==max(Tasa_empleo_anual))]
  Empleo_promedio_anual$Menor[i] <- min(Tasa_empleo_anual)
  Empleo_promedio_anual$Empresa_Menor[i] <- Nombres[which(Tasa_empleo_anual==min(Tasa_empleo_anual))]
}

# -- Utilidad Operacion 
Utilidad_Operacion_promedio_anual <- data.frame("Periodo" = rownames(Utilidad_Operacion_anual), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor"=NA, "Empresa_Mayor"=NA, "Menor"=NA,  "Empresa_Menor"=NA)

for (i in 1:nrow(Utilidad_Operacion_anual)){
  Tasa_UO_anual <- apply(as.matrix(Utilidad_Operacion_anual[i,]), 2, as.numeric)
  Tasa_UO_anual <- na.omit(Tasa_UO_anual)
  Nombres <- colnames(Utilidad_Operacion_anual)
  Nombres <- Nombres[-which(is.na(Utilidad_Operacion_anual[i,1:length(Utilidad_Operacion_anual[i,])]))]
  Utilidad_Operacion_promedio_anual$Promedio[i] <- mean(Tasa_UO_anual)
  Utilidad_Operacion_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_UO_anual)
  Utilidad_Operacion_promedio_anual$Mayor[i] <- max(Tasa_UO_anual)
  Utilidad_Operacion_promedio_anual$Empresa_Mayor[i] <- Nombres[which(Tasa_UO_anual==max(Tasa_UO_anual))]
  Utilidad_Operacion_promedio_anual$Menor[i] <- min(Tasa_UO_anual)
  Utilidad_Operacion_promedio_anual$Empresa_Menor[i] <- Nombres[which(Tasa_UO_anual==min(Tasa_UO_anual))]
}

# -- Utilidad Neta 
Utilidad_Neta_promedio_anual <- data.frame("Periodo" = rownames(Utilidad_neta_anual), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor"=NA, "Empresa_Mayor"=NA, "Menor"=NA,  "Empresa_Menor"=NA)

for (i in 1:nrow(Utilidad_neta_anual)){
  Tasa_UN_anual <- apply(as.matrix(Utilidad_neta_anual[i,]), 2, as.numeric)
  Tasa_UN_anual <- na.omit(Tasa_UN_anual)
  Nombres <- colnames(Utilidad_neta_anual)
  Nombres <- Nombres[-which(is.na(Utilidad_neta_anual[i,1:length(Utilidad_neta_anual[i,])]))]
  Utilidad_Neta_promedio_anual$Promedio[i] <- mean(Tasa_UN_anual)
  Utilidad_Neta_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_UN_anual)
  Utilidad_Neta_promedio_anual$Mayor[i] <- max(Tasa_UN_anual)
  Utilidad_Neta_promedio_anual$Empresa_Mayor[i] <- Nombres[which(Tasa_UN_anual==max(Tasa_UN_anual))]
  Utilidad_Neta_promedio_anual$Menor[i] <- min(Tasa_UN_anual)
  Utilidad_Neta_promedio_anual$Empresa_Menor[i] <- Nombres[which(Tasa_UN_anual==min(Tasa_UN_anual))]
}

# -- Activos 
Activos_promedio_anual <- data.frame("Periodo" = rownames(Activos_anual), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor"=NA, "Empresa_Mayor"=NA, "Menor"=NA,  "Empresa_Menor"=NA)

for (i in 1:nrow(Activos_anual)){
  Tasa_A_anual <- apply(as.matrix(Activos_anual[i,]), 2, as.numeric)
  Tasa_A_anual <- na.omit(Tasa_A_anual)
  Nombres <- colnames(Activos_anual)
  Nombres <- Nombres[-which(is.na(Activos_anual[i,1:length(Activos_anual[i,])]))]
  Activos_promedio_anual$Promedio[i] <- mean(Tasa_A_anual)
  Activos_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_A_anual)
  Activos_promedio_anual$Mayor[i] <- max(Tasa_A_anual)
  Activos_promedio_anual$Empresa_Mayor[i] <- Nombres[which(Tasa_A_anual==max(Tasa_A_anual))]
  Activos_promedio_anual$Menor[i] <- min(Tasa_A_anual)
  Activos_promedio_anual$Empresa_Menor[i] <- Nombres[which(Tasa_A_anual==min(Tasa_A_anual))]
}

# -- Pasivos 
Pasivos_promedio_anual <- data.frame("Periodo" = rownames(Pasivos_anual), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor"=NA, "Empresa_Mayor"=NA, "Menor"=NA,  "Empresa_Menor"=NA)

for (i in 1:nrow(Pasivos_anual)){
  Tasa_P_anual <- apply(as.matrix(Pasivos_anual[i,]), 2, as.numeric)
  Tasa_P_anual <- na.omit(Tasa_P_anual)
  Nombres <- colnames(Pasivos_anual)
  Nombres <- Nombres[-which(is.na(Pasivos_anual[i,1:length(Pasivos_anual[i,])]))]
  Pasivos_promedio_anual$Promedio[i] <- mean(Tasa_P_anual)
  Pasivos_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_P_anual)
  Pasivos_promedio_anual$Mayor[i] <- max(Tasa_P_anual)
  Pasivos_promedio_anual$Empresa_Mayor[i] <- Nombres[which(Tasa_P_anual==max(Tasa_P_anual))]
  Pasivos_promedio_anual$Menor[i] <- min(Tasa_P_anual)
  Pasivos_promedio_anual$Empresa_Menor[i] <- Nombres[which(Tasa_P_anual==min(Tasa_P_anual))]
}

# -- Patrimonio 
Patrimonio_promedio_anual <- data.frame("Periodo" = rownames(Pasivos_anual), "Promedio" = NA, "Desviacion_estandar"=NA, "Mayor"=NA, "Empresa_Mayor"=NA, "Menor"=NA,  "Empresa_Menor"=NA)

for (i in 1:nrow(Pasivos_anual)){
  Tasa_Pat_anual <- apply(as.matrix(Pasivos_anual[i,]), 2, as.numeric)
  Tasa_Pat_anual <- na.omit(Tasa_Pat_anual)
  Nombres <- colnames(Pasivos_anual)
  Nombres <- Nombres[-which(is.na(Pasivos_anual[i,1:length(Pasivos_anual[i,])]))]
  Patrimonio_promedio_anual$Promedio[i] <- mean(Tasa_Pat_anual)
  Patrimonio_promedio_anual$Desviacion_estandar[i] <- sd(Tasa_Pat_anual)
  Patrimonio_promedio_anual$Mayor[i] <- max(Tasa_Pat_anual)
  Patrimonio_promedio_anual$Empresa_Mayor[i] <- Nombres[which(Tasa_Pat_anual==max(Tasa_Pat_anual))]
  Patrimonio_promedio_anual$Menor[i] <- min(Tasa_Pat_anual)
  Patrimonio_promedio_anual$Empresa_Menor[i] <- Nombres[which(Tasa_Pat_anual==min(Tasa_Pat_anual))]
}

# -- Realizamos las tasas de crecimiento para los parametros secundarios de las empresas
Tasas_empleo <- data.frame(Empleo)
Tasas_utilidad_operacion <- data.frame(Utilidad_Operacion)
Tasas_utilidad_neta <- data.frame(Utilidad_neta)
Tasas_activo <- data.frame(Activos)
Tasas_pasivos <- data.frame(Pasivos)
Tasas_patrimonio <- data.frame(Patrimonio)

# -- Calculamos las tasas de los parametros secundarios 
dimension_pruebar <- dim(Tasas_empleo)

for (j in 3:dimension_pruebar[2]){
  for (i in 1:dimension_pruebar[1]){
    
    Tasas_empleo[i,j-1]<-round((Tasas_empleo[i,j]/Tasas_empleo[i,j-1])-1,2) 
    Tasas_utilidad_operacion[i,j-1]<-round((Tasas_utilidad_operacion[i,j]/Tasas_utilidad_operacion[i,j-1])-1,2)
    Tasas_utilidad_neta[i,j-1]<-round((Tasas_utilidad_neta[i,j]/Tasas_utilidad_neta[i,j-1])-1,2) 
    Tasas_activo[i,j-1]<-round((Tasas_activo[i,j]/Tasas_activo[i,j-1])-1,2)
    Tasas_pasivos[i,j-1]<-round((Tasas_pasivos[i,j]/Tasas_pasivos[i,j-1])-1,2) 
    Tasas_patrimonio[i,j-1]<-round((Tasas_patrimonio[i,j]/Tasas_patrimonio[i,j-1])-1,2) 
    
  }
}

# -- Reacomodamos los DataFrames transponiendolos para hacer mas facil su manejo en r
Tasas_empleo <- data.frame(r1=names(Tasas_empleo), t(Tasas_empleo))
Tasas_empleo = Tasas_empleo[-1,]
Tasas_empleo = Tasas_empleo[,-1]
colnames(Tasas_empleo)<-colnames(Tasas_ventas)
rownames(Tasas_empleo)<-seq(from=2006, to=2016, by=1)
Tasas_empleo = Tasas_empleo[-dim(Tasas_empleo)[1],]

Tasas_utilidad_operacion <- data.frame(r1=names(Tasas_utilidad_operacion), t(Tasas_utilidad_operacion))
Tasas_utilidad_operacion = Tasas_utilidad_operacion[-1,]
Tasas_utilidad_operacion = Tasas_utilidad_operacion[,-1]
colnames(Tasas_utilidad_operacion)<-colnames(Tasas_ventas)
rownames(Tasas_utilidad_operacion)<-seq(from=2006, to=2016, by=1)
Tasas_utilidad_operacion = Tasas_utilidad_operacion[-dim(Tasas_utilidad_operacion)[1],]

Tasas_utilidad_neta <- data.frame(r1=names(Tasas_utilidad_neta), t(Tasas_utilidad_neta))
Tasas_utilidad_neta = Tasas_utilidad_neta[-1,]
Tasas_utilidad_neta = Tasas_utilidad_neta[,-1]
colnames(Tasas_utilidad_neta)<-colnames(Tasas_ventas)
rownames(Tasas_utilidad_neta)<-seq(from=2006, to=2016, by=1)
Tasas_utilidad_neta = Tasas_utilidad_neta[-dim(Tasas_utilidad_neta)[1],]

Tasas_activo <- data.frame(r1=names(Tasas_activo), t(Tasas_activo))
Tasas_activo = Tasas_activo[-1,]
Tasas_activo = Tasas_activo[,-1]
colnames(Tasas_activo)<-colnames(Tasas_ventas)
rownames(Tasas_activo)<-seq(from=2006, to=2016, by=1)
Tasas_activo = Tasas_activo[-dim(Tasas_activo)[1],]

Tasas_pasivos <- data.frame(r1=names(Tasas_pasivos), t(Tasas_pasivos))
Tasas_pasivos = Tasas_pasivos[-1,]
Tasas_pasivos = Tasas_pasivos[,-1]
colnames(Tasas_pasivos)<-colnames(Tasas_ventas)
rownames(Tasas_pasivos)<-seq(from=2006, to=2016, by=1)
Tasas_pasivos = Tasas_pasivos[-dim(Tasas_pasivos)[1],]

Tasas_patrimonio <- data.frame(r1=names(Tasas_patrimonio), t(Tasas_patrimonio))
Tasas_patrimonio = Tasas_patrimonio[-1,]
Tasas_patrimonio = Tasas_patrimonio[,-1]
colnames(Tasas_patrimonio)<-colnames(Tasas_ventas)
rownames(Tasas_patrimonio)<-seq(from=2006, to=2016, by=1)
Tasas_patrimonio = Tasas_patrimonio[-dim(Tasas_patrimonio)[1],]

# ------------------------------------ ANALISIS DE LAS EMPRESAS -------------------------------------

# -- Aqui obtenemos las estadisticas descriptivas principales de las ventas y los parametros secundarios de las empresas que nosotros indiquemos  

# Aqui comparamos Tasas de Crecimiento en Ventas VS Tasas de Crecimiento en Empleo por empresa
Tasas_ventas_comparacion <- Tasas_ventas[which(rownames(Tasas_ventas)==first(rownames(Tasas_empleo))):nrow(Tasas_ventas),]
Tasas_ventas_comparacion <- Tasas_ventas_comparacion[sapply(Tasas_ventas_comparacion, function(Tasas_ventas_comparacion) !any(is.na(Tasas_ventas_comparacion)))] 
nombres_ventas <- colnames(Tasas_ventas_comparacion)

Tasas_empleo_comparacion <- Tasas_empleo
Tasas_empleo_comparacion <- Tasas_empleo_comparacion[sapply(Tasas_empleo_comparacion, function(Tasas_empleo_comparacion) !any(is.na(Tasas_empleo_comparacion)))] 
nombres_empleo <- colnames(Tasas_empleo_comparacion)

nombres_completos <- nombres_ventas[nombres_ventas %in% nombres_empleo]

# -- Modificamos los dataframes de las tasas de ventas y empleo una vez que sabemos cuales son las empresas que tienen los datos completos en ambos dataframe`s
Tasas_ventas_comparacion <- Tasas_ventas_comparacion[,nombres_completos]
Tasas_empleo_comparacion <- Tasas_empleo_comparacion[,nombres_completos]

# -- Calculamos la correlacion entre las ventas y el empleo
A <- as.matrix(Tasas_ventas_comparacion)
B <- as.matrix(Tasas_empleo_comparacion)
Correlation_data <- data.frame("Empresa"=nombres_completos, "Correlacion Ventas/Empleo"=NA)
correlacion <- 0

for (i in 1:ncol(A)){
  AA <- apply(as.matrix(A[,i]), 2, as.numeric)
  BB <- apply(as.matrix(B[,i]), 2, as.numeric)
  correlacion[i] <- cor(AA[,1], BB[,1])
  Correlation_data$Correlacion.Ventas.Empleo[i] <- correlacion[i]
}

Empresas_correlaciones_ordenado <- Correlation_data[order(-Correlation_data[,2], Correlation_data[,1]), ]

Datos_correlacion <- data.frame("Informacion"=c("Correlacion_promedio","Desviacion_estandar","Maxima_correlacion", "Empresa_maxima_correlacion", "Minima_correlacion", "Empresa_minima_correlacion", "50%_empresas_arriba_de"),"Dato"=NA)
Maxima_correlacion <- max(Correlation_data$Correlacion.Ventas.Empleo)
Empresa_maxima_correlacion <- as.character(Correlation_data$Empresa[which(Correlation_data$Correlacion.Ventas.Empleo==Maxima_correlacion)]) 
Minima_correlacion <- min(Correlation_data$Correlacion.Ventas.Empleo)
Empresa_minima_correlacion <- as.character(Correlation_data$Empresa[which(Correlation_data$Correlacion.Ventas.Empleo==Minima_correlacion)])
Correlacion_promedio <- mean(Correlation_data$Correlacion.Ventas.Empleo)
Desviacion_estandar <- sd(Correlation_data$Correlacion.Ventas.Empleo)

#Empresas_correlaciones_ordenado[1:nrow(Empresas_correlaciones_ordenado)/2,]
#Empresas_correlaciones_ordenado[1:round(nrow(Empresas_correlaciones_ordenado)*.80,0),]
# Aqui se muestra que la mitad de las empresas tienen correlacion arriba de .36
# Y que el 80% de las empresas estan con correlacion por arriba de .02

Datos_correlacion$Dato[1] <- Correlacion_promedio
Datos_correlacion$Dato[2] <- Desviacion_estandar
Datos_correlacion$Dato[3] <- Maxima_correlacion
Datos_correlacion$Dato[4] <- Empresa_maxima_correlacion
Datos_correlacion$Dato[5] <- Minima_correlacion
Datos_correlacion$Dato[6] <- Empresa_minima_correlacion
Datos_correlacion$Dato[7] <- .36

# -- Aqui graficaremos las Ventas vs Empleo de cada empresa para hacer mas intuitiva la relacion de estas dos variables 
 
plt_v_vs_e <- list()

for (i in 1:ncol(Tasas_empleo_comparacion)){ 

  t_empleo = apply(as.matrix(Tasas_empleo_comparacion[,i]), 2, as.numeric)
  t_empleo = t_empleo[,1]
  t_ventas = apply(as.matrix(Tasas_ventas_comparacion[,i]), 2, as.numeric)
  t_ventas = t_ventas[,1]
  x <- as.numeric(rownames(Tasas_empleo_comparacion))
  
  grafica <- plot_ly(x = x, y = t_empleo, type = "scatter", name = paste("Empleo", sep=''), mode = 'markers+lines', evaluate = TRUE) %>% 
    add_trace(y = t_ventas, type = "scatter", name = paste("Ventas", sep=''), mode = "markers+lines") %>% 
    layout(title = paste("Empleo VS Ventas: ", colnames(Tasas_ventas_comparacion)[i], '  (Correlacion: ', round(Correlation_data$Correlacion.Ventas.Empleo[i],2),')',sep=''),
           xaxis = list(title = "Periodo"),
           yaxis = list(title = "Tasa crecimiento"))
  plt_v_vs_e[[i]] <- grafica
  
}

plt_v_vs_e

# Algunas empresas con correlaciones positivas 
#plt_v_vs_e[[5]] 
#plt_v_vs_e[[10]]
#plt_v_vs_e[[20]]
#plt_v_vs_e[[21]]

# Algunas empresas con correlaciones negativas
#plt_v_vs_e[[27]]
#plt_v_vs_e[[28]]

# -- Ahora calcularemos cuantos pesos($) genera cada empleado por Empresa de acuerdo a sus activos y empleados 
Activos_comparacion <- Activos_anual
Activos_comparacion <- Activos_comparacion[sapply(Activos_comparacion, function(Activos_comparacion) !any(is.na(Activos_comparacion)))] 
nombres_activos <- colnames(Activos_comparacion)

Empleo_comparacion <- Empleo_anual
Empleo_comparacion <- Empleo_comparacion[sapply(Empleo_comparacion, function(Empleo_comparacion) !any(is.na(Empleo_comparacion)))] 
nombres_empleo_a <- colnames(Empleo_comparacion)

nombres_completos_ea <- nombres_activos[nombres_activos %in% nombres_empleo_a]

# -- Modificamos los dataframes de las tasas de ventas y empleo una vez que sabemos cuales son las empresas que tienen los datos completos en ambos dataframe`s
Activos_comparacion <- Activos_comparacion[,nombres_completos_ea]
Empleo_comparacion <- Empleo_comparacion[,nombres_completos_ea]

# -- Calculamos los pesos que genera un empleado de las empresas 
A <- as.matrix(Activos_comparacion)
B <- as.matrix(Empleo_comparacion)
Profit_per_employment_data <- data.frame("Empresa"=nombres_completos_ea, '2006'=NA, "2007"=NA, "2008"=NA, "2009"=NA, "2010"=NA, "2011"=NA, "2012"=NA, "2013"=NA, "2014"=NA, "2015"=NA, "2016"=NA, "Promedio"=NA)
Genera <- 0

#cuanto genera un empleado en dinero de cada empresa por a??o (2006-2016)

for (i in 1:ncol(A)){
  AA <- apply(as.matrix(A[,i]), 2, as.numeric)
  BB <- apply(as.matrix(B[,i]), 2, as.numeric)
  Genera <- AA[,1]/BB[,1]
  Profit_per_employment_data[i,2:(length(Profit_per_employment_data)-1)] <- Genera
  Profit_per_employment_data[i,length(Profit_per_employment_data)] <- mean(Genera)
}

Empresas_empleo_genera_ordenado <- Profit_per_employment_data[order(-Profit_per_employment_data[,length(Profit_per_employment_data)], Profit_per_employment_data[,1]), ]

Datos_empleado_genera <- data.frame("Informacion"=c("Promedio","Desviacion_estandar","Maximo", "Empresa_maximo", "Minimo", "Empresa_minimo", "50%_empresas_arriba_de"),"Dato"=NA)
Maximo <- max(Profit_per_employment_data$Promedio)
Empresa_maximo <- as.character(Profit_per_employment_data$Empresa[which(Profit_per_employment_data$Promedio==Maximo)])
Minimo <- min(Profit_per_employment_data$Promedio)
Empresa_minimo <- as.character(Profit_per_employment_data$Empresa[which(Profit_per_employment_data$Promedio==Minimo)])
Promedio <- mean(Profit_per_employment_data$Promedio)
Desviacion_estandar <- sd(Profit_per_employment_data$Promedio)

#View(Empresas_empleo_genera_ordenado[1:round(nrow(Empresas_empleo_genera_ordenado)/2,0),])
#View(Empresas_empleo_genera_ordenado[1:round(nrow(Empresas_empleo_genera_ordenado)*.80,0),])
# los 2 codigos comentados de arriba muestran  que la mitad de las empresas, un empleado genera mas de 2.82
# Y que el 80% de las empresas, un empleado genera mas de 1.44 

Datos_empleado_genera$Dato[1] <- Promedio
Datos_empleado_genera$Dato[2] <- Desviacion_estandar
Datos_empleado_genera$Dato[3] <- Maximo
Datos_empleado_genera$Dato[4] <- Empresa_maximo
Datos_empleado_genera$Dato[5] <- Minimo
Datos_empleado_genera$Dato[6] <- Empresa_minimo
Datos_empleado_genera$Dato[7] <- 2.82

# Aqui se redondean los valores a dos decimales 

Profit_per_employment_data[,2:length(Profit_per_employment_data)] <- round(Profit_per_employment_data[,2:length(Profit_per_employment_data)],2)

Profit_per_employment_data_round <- Empresas_empleo_genera_ordenado
Profit_per_employment_data_round[,2:length(Empresas_empleo_genera_ordenado)] <- round(Empresas_empleo_genera_ordenado[,2:length(Empresas_empleo_genera_ordenado)],2)



# -- Ahora calcularemos cuantos pesos($) o utilidad neta genera cada empleado por Empresa de acuerdo a su utilidad neta y empleados 
Utilidad_Neta_comparacion <- Utilidad_neta_anual
Utilidad_Neta_comparacion <- Utilidad_Neta_comparacion[sapply(Utilidad_Neta_comparacion, function(Utilidad_Neta_comparacion) !any(is.na(Utilidad_Neta_comparacion)))] 
nombres_ut_neta <- colnames(Utilidad_Neta_comparacion)

Empleo_comparacion_ut_neta <- Empleo_anual
Empleo_comparacion_ut_neta <- Empleo_comparacion_ut_neta[sapply(Empleo_comparacion_ut_neta, function(Empleo_comparacion_ut_neta) !any(is.na(Empleo_comparacion_ut_neta)))] 
nombres_empleo_a <- colnames(Empleo_comparacion_ut_neta)

nombres_completos_eutn <- nombres_ut_neta[nombres_ut_neta %in% nombres_empleo_a]

# -- Modificamos los dataframes de las tasas de ventas y empleo una vez que sabemos cuales son las empresas que tienen los datos completos en ambos dataframe`s
Utilidad_Neta_comparacion <- Utilidad_Neta_comparacion[,nombres_completos_eutn]
Empleo_comparacion_ut_neta <- Empleo_comparacion_ut_neta[,nombres_completos_eutn]

# -- Calculamos los pesos que genera un empleado de las empresas 
A <- as.matrix(Utilidad_Neta_comparacion)
B <- as.matrix(Empleo_comparacion_ut_neta)
Profit_per_employment_data_utneta_empleo <- data.frame("Empresa"=nombres_completos_eutn, "2006"=NA, "2007"=NA, "2008"=NA, "2009"=NA, "2010"=NA, "2011"=NA, "2012"=NA, "2013"=NA, "2014"=NA, "2015"=NA, "2016"=NA, "Promedio"=NA)
Genera <- 0

#cuanto genera un empleado en dinero de cada empresa por a??o (2006-2016)

for (i in 1:ncol(A)){
  AA <- apply(as.matrix(A[,i]), 2, as.numeric)
  BB <- apply(as.matrix(B[,i]), 2, as.numeric)
  Genera <- AA[,1]/BB[,1]
  Profit_per_employment_data_utneta_empleo[i,2:(length(Profit_per_employment_data_utneta_empleo)-1)] <- Genera
  Profit_per_employment_data_utneta_empleo[i,length(Profit_per_employment_data_utneta_empleo)] <- mean(Genera)
}

Empresas_empleo_genera_ordenado_ut_neta <- Profit_per_employment_data_utneta_empleo[order(-Profit_per_employment_data_utneta_empleo[,length(Profit_per_employment_data_utneta_empleo)], Profit_per_employment_data_utneta_empleo[,1]), ]

Datos_empleado_genera_ut_neta <- data.frame("Informacion"=c("Promedio","Desviacion_estandar","Maximo", "Empresa_maximo", "Minimo", "Empresa_minimo", "50%_empresas_arriba_de"),"Dato"=NA)
Maximo <- max(Profit_per_employment_data_utneta_empleo$Promedio)
Empresa_maximo <- as.character(Profit_per_employment_data_utneta_empleo$Empresa[which(Profit_per_employment_data_utneta_empleo$Promedio==Maximo)])
Minimo <- min(Profit_per_employment_data_utneta_empleo$Promedio)
Empresa_minimo <- as.character(Profit_per_employment_data_utneta_empleo$Empresa[which(Profit_per_employment_data_utneta_empleo$Promedio==Minimo)])
Promedio <- mean(Profit_per_employment_data_utneta_empleo$Promedio)
Desviacion_estandar <- sd(Profit_per_employment_data_utneta_empleo$Promedio)

#View(Empresas_empleo_genera_ordenado_ut_neta[1:round(nrow(Empresas_empleo_genera_ordenado_ut_neta)/2,0),])
#View(Empresas_empleo_genera_ordenado_ut_neta[1:round(nrow(Empresas_empleo_genera_ordenado_ut_neta)*.80,0),])
# los 2 codigos comentados de arriba muestran que la mitad de las empresas, un empleado genera mas de .12
# Y que el 80% de las empresas, un empleado genera mas de .0345

Datos_empleado_genera_ut_neta$Dato[1] <- Promedio
Datos_empleado_genera_ut_neta$Dato[2] <- Desviacion_estandar
Datos_empleado_genera_ut_neta$Dato[3] <- Maximo
Datos_empleado_genera_ut_neta$Dato[4] <- Empresa_maximo
Datos_empleado_genera_ut_neta$Dato[5] <- Minimo
Datos_empleado_genera_ut_neta$Dato[6] <- Empresa_minimo
Datos_empleado_genera_ut_neta$Dato[7] <- .12

# Aqui se redondean los valores a dos decimales 

Profit_per_employment_data_utneta_empleo[,2:length(Profit_per_employment_data_utneta_empleo)] <- round(Profit_per_employment_data_utneta_empleo[,2:length(Profit_per_employment_data_utneta_empleo)],2)

Profit_per_employment_data_utneta_empleo_round <- Empresas_empleo_genera_ordenado_ut_neta
Profit_per_employment_data_utneta_empleo_round[,2:length(Empresas_empleo_genera_ordenado_ut_neta)] <- round(Empresas_empleo_genera_ordenado_ut_neta[,2:length(Empresas_empleo_genera_ordenado_ut_neta)],2)


# -- Ahora calcularemos cuantas ventas genera cada empleado por Empresa de acuerdo a sus Ventas y Empleo 
Ventas_comparacion <- Ventas[rownames(Empleo_anual),]
Ventas_comparacion <- Ventas_comparacion[sapply(Ventas_comparacion, function(Ventas_comparacion) !any(is.na(Ventas_comparacion)))] 
nombres_ut_neta <- colnames(Ventas_comparacion)

Empleo_comparacion_ventas <- Empleo_anual
Empleo_comparacion_ventas <- Empleo_comparacion_ventas[sapply(Empleo_comparacion_ventas, function(Empleo_comparacion_ventas) !any(is.na(Empleo_comparacion_ventas)))] 
nombres_empleo_a <- colnames(Empleo_comparacion_ventas)

nombres_completos_eventas <- nombres_ut_neta[nombres_ut_neta %in% nombres_empleo_a]

# -- Modificamos los dataframes de las tasas de ventas y empleo una vez que sabemos cuales son las empresas que tienen los datos completos en ambos dataframe`s
Ventas_comparacion <- Ventas_comparacion[,nombres_completos_eventas]
Empleo_comparacion_ventas <- Empleo_comparacion_ventas[,nombres_completos_eventas]

# -- Calculamos los pesos que genera un empleado de las empresas 
A <- as.matrix(Ventas_comparacion)
B <- as.matrix(Empleo_comparacion_ventas)
Profit_per_employment_data_ventas_empleo <- data.frame("Empresa"=nombres_completos_eventas, "2006"=NA, "2007"=NA, "2008"=NA, "2009"=NA, "2010"=NA, "2011"=NA, "2012"=NA, "2013"=NA, "2014"=NA, "2015"=NA, "2016"=NA, "Promedio"=NA)
Genera <- 0

#cuanto genera un empleado en dinero de cada empresa por a??o (2006-2016)

for (i in 1:ncol(A)){
  AA <- apply(as.matrix(A[,i]), 2, as.numeric)
  BB <- apply(as.matrix(B[,i]), 2, as.numeric)
  Genera <- AA[,1]/BB[,1]
  Profit_per_employment_data_ventas_empleo[i,2:(length(Profit_per_employment_data_ventas_empleo)-1)] <- Genera
  Profit_per_employment_data_ventas_empleo[i,length(Profit_per_employment_data_ventas_empleo)] <- mean(Genera)
}

Empresas_empleo_genera_ordenado_ventas <- Profit_per_employment_data_ventas_empleo[order(-Profit_per_employment_data_ventas_empleo[,length(Profit_per_employment_data_ventas_empleo)], Profit_per_employment_data_ventas_empleo[,1]), ]

Datos_empleado_genera_ventas <- data.frame("Informacion"=c("Promedio","Desviacion_estandar","Maximo", "Empresa_maximo", "Minimo", "Empresa_minimo", "50%_empresas_arriba_de"),"Dato"=NA)
Maximo <- max(Profit_per_employment_data_ventas_empleo$Promedio)
Empresa_maximo <- as.character(Profit_per_employment_data_ventas_empleo$Empresa[which(Profit_per_employment_data_ventas_empleo$Promedio==Maximo)])
Minimo <- min(Profit_per_employment_data_ventas_empleo$Promedio)
Empresa_minimo <- as.character(Profit_per_employment_data_ventas_empleo$Empresa[which(Profit_per_employment_data_ventas_empleo$Promedio==Minimo)])
Promedio <- mean(Profit_per_employment_data_ventas_empleo$Promedio)
Desviacion_estandar <- sd(Profit_per_employment_data_ventas_empleo$Promedio)

#View(Empresas_empleo_genera_ordenado_ventas[1:round(nrow(Empresas_empleo_genera_ordenado_ventas)/2,0),])
#View(Empresas_empleo_genera_ordenado_ventas[1:round(nrow(Empresas_empleo_genera_ordenado_ventas)*.80,0),])
# Aqui se muestra que la mitad de las empresas, un empleado genera mas de 177
# Y que el 80% de las empresas, un empleado genera mas de 106

Datos_empleado_genera_ventas$Dato[1] <- Promedio
Datos_empleado_genera_ventas$Dato[2] <- Desviacion_estandar
Datos_empleado_genera_ventas$Dato[3] <- Maximo
Datos_empleado_genera_ventas$Dato[4] <- Empresa_maximo
Datos_empleado_genera_ventas$Dato[5] <- Minimo
Datos_empleado_genera_ventas$Dato[6] <- Empresa_minimo
Datos_empleado_genera_ventas$Dato[7] <- 177

# Aqui se redondean los valores a dos decimales 

Profit_per_employment_data_ventas_empleo[,2:length(Profit_per_employment_data_ventas_empleo)] <- round(Profit_per_employment_data_ventas_empleo[,2:length(Profit_per_employment_data_ventas_empleo)],2)

Profit_per_employment_data_ventas_empleo_round <- Empresas_empleo_genera_ordenado_ventas
Profit_per_employment_data_ventas_empleo_round[,2:length(Empresas_empleo_genera_ordenado_ventas)] <- round(Empresas_empleo_genera_ordenado_ventas[,2:length(Empresas_empleo_genera_ordenado_ventas)],2)

# -- Estadistica Descriptiva de los Datos 

# -- En este caso son por empresa
Estadisticas_Ventas <- summary(Ventas[,1:ncol(Ventas)])
# Estos tienen error investigar porque tal vez porque son pocos datos por empresa
Estadisticas_Empleo <- summary(Empleo_anual[,1:ncol(Empleo_anual)])
Estadisticas_Utilidad_operacion <- summary(Utilidad_Operacion_anual[,1:ncol(Utilidad_Operacion_anual)])
Estadisticas_Utilidad_neta <- summary(Utilidad_neta_anual[,1:ncol(Utilidad_neta_anual)])
Estadisticas_Activos <- summary(Activos_anual[,1:ncol(Activos_anual)])
Estadisticas_Pasivos <- summary(Pasivos_anual[,1:ncol(Pasivos_anual)])
Estadisticas_Patrimonio <- summary(Patrimonio_anual[,1:ncol(Patrimonio_anual)])


# -- En este caso son por a??o 
Estadisticas_Ventas_anual <- summary(Datos_anual[,2:ncol(Datos_anual)])
Estadisticas_Empleo_anual <- summary(Empleo[,2:ncol(Empleo)])
Estadisticas_Utilidad_operacion_anual <- summary(Utilidad_Operacion[,2:ncol(Utilidad_Operacion)])
Estadisticas_Utilidad_neta_anual <- summary(Utilidad_neta[,2:ncol(Utilidad_neta)])
Estadisticas_Activos_anual <- summary(Activos[,2:ncol(Activos)])
Estadisticas_Pasivos_anual <- summary(Pasivos[,2:ncol(Pasivos)])
Estadisticas_Patrimonio_anual <- summary(Patrimonio[,2:ncol(Patrimonio)])

# -- En este caso son por Propiedad por periodo
Propiedad <- unique(Datos$PROPIEDAD)
Estadisticas_Propiedad_1 <- summary(Datos_anual[which(Datos$PROPIEDAD==Propiedad[3]),])
Estadisticas_Propiedad_3 <- summary(Datos_anual[which(Datos$PROPIEDAD==Propiedad[1]),])
Estadisticas_Propiedad_4 <- summary(Datos_anual[which(Datos$PROPIEDAD==Propiedad[2]),])
Estadisticas_Propiedad_5 <- summary(Datos_anual[which(Datos$PROPIEDAD==Propiedad[4]),])

# -- En este caso son por Naics por periodo
Naics <- sort(unique(Datos$NAICS))
Estadisticas_Naics_11 <- summary(Datos_anual[which(Datos$NAICS==Naics[1]),])
Estadisticas_Naics_21 <- summary(Datos_anual[which(Datos$NAICS==Naics[2]),])
Estadisticas_Naics_22 <- summary(Datos_anual[which(Datos$NAICS==Naics[3]),])
Estadisticas_Naics_23 <- summary(Datos_anual[which(Datos$NAICS==Naics[4]),])
Estadisticas_Naics_31 <- summary(Datos_anual[which(Datos$NAICS==Naics[5]),])
Estadisticas_Naics_32 <- summary(Datos_anual[which(Datos$NAICS==Naics[6]),])
Estadisticas_Naics_33 <- summary(Datos_anual[which(Datos$NAICS==Naics[7]),])
Estadisticas_Naics_41 <- summary(Datos_anual[which(Datos$NAICS==Naics[8]),])
Estadisticas_Naics_42 <- summary(Datos_anual[which(Datos$NAICS==Naics[9]),])
Estadisticas_Naics_44 <- summary(Datos_anual[which(Datos$NAICS==Naics[10]),])
Estadisticas_Naics_45 <- summary(Datos_anual[which(Datos$NAICS==Naics[11]),])
Estadisticas_Naics_46 <- summary(Datos_anual[which(Datos$NAICS==Naics[12]),])
Estadisticas_Naics_48 <- summary(Datos_anual[which(Datos$NAICS==Naics[13]),])
Estadisticas_Naics_49 <- summary(Datos_anual[which(Datos$NAICS==Naics[14]),])
Estadisticas_Naics_51 <- summary(Datos_anual[which(Datos$NAICS==Naics[15]),])
Estadisticas_Naics_52 <- summary(Datos_anual[which(Datos$NAICS==Naics[16]),])
Estadisticas_Naics_53 <- summary(Datos_anual[which(Datos$NAICS==Naics[17]),])
Estadisticas_Naics_54 <- summary(Datos_anual[which(Datos$NAICS==Naics[18]),])
Estadisticas_Naics_56 <- summary(Datos_anual[which(Datos$NAICS==Naics[19]),])
Estadisticas_Naics_62 <- summary(Datos_anual[which(Datos$NAICS==Naics[20]),])
Estadisticas_Naics_71 <- summary(Datos_anual[which(Datos$NAICS==Naics[21]),])
Estadisticas_Naics_72 <- summary(Datos_anual[which(Datos$NAICS==Naics[22]),])
Estadisticas_Naics_81 <- summary(Datos_anual[which(Datos$NAICS==Naics[23]),])
Estadisticas_Naics_99 <- summary(Datos_anual[which(Datos$NAICS==Naics[24]),])


# -- En este caso son por Registro 
Registro <- sort(unique(Datos$REGISTROS))
Estadisticas_Registro_2 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[1]),])
Estadisticas_Registro_3 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[2]),])
Estadisticas_Registro_4 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[3]),])
Estadisticas_Registro_5 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[4]),])
Estadisticas_Registro_5 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[5]),])
Estadisticas_Registro_6 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[6]),])
Estadisticas_Registro_7 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[7]),])
Estadisticas_Registro_8 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[8]),])
Estadisticas_Registro_9 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[9]),])
Estadisticas_Registro_10 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[10]),])
Estadisticas_Registro_11 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[11]),])
Estadisticas_Registro_12 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[12]),])
Estadisticas_Registro_13 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[13]),])
Estadisticas_Registro_14 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[14]),])
Estadisticas_Registro_15 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[15]),])
Estadisticas_Registro_16 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[16]),])
Estadisticas_Registro_17 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[17]),])
Estadisticas_Registro_18 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[18]),])
Estadisticas_Registro_19 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[19]),])
Estadisticas_Registro_20 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[20]),])
Estadisticas_Registro_21 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[21]),])
Estadisticas_Registro_22 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[22]),])
Estadisticas_Registro_23 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[23]),])
Estadisticas_Registro_24 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[24]),])
Estadisticas_Registro_25 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[25]),])
Estadisticas_Registro_26 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[26]),])
Estadisticas_Registro_27 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[27]),])
Estadisticas_Registro_28 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[28]),])
Estadisticas_Registro_29 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[29]),])
Estadisticas_Registro_30 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[30]),])
Estadisticas_Registro_31 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[31]),])
Estadisticas_Registro_32 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[32]),])
Estadisticas_Registro_33 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[33]),])
Estadisticas_Registro_34 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[34]),])
Estadisticas_Registro_35 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[35]),])
Estadisticas_Registro_36 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[36]),])
Estadisticas_Registro_37 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[37]),])
Estadisticas_Registro_38 <- summary(Datos_anual[which(Datos$REGISTROS==Registro[38]),])


# -- En esta parte vemos como se distribuyen los datos de las ventas por a??o 
plt <- list()

for (i in 1:nrow(Tasas_ventas)){ 
  
  y <- apply(as.matrix(Tasas_ventas[i,]), 2, as.numeric)
  x <- na.omit(y)
  fit <- density(x, n = length(x))

  grafica <- plot_ly(x = x, type = "histogram", name = "Histogram", evaluate = TRUE) %>% 
                add_trace(x = fit$x, y = fit$y, type = "scatter", mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 

                layout(title = paste("Distribucion de las Ventas en: ", rownames(Tasas_ventas)[i], sep=''),
                        xaxis = list(title = "Tasa crecimiento"),
                        yaxis = list(title = "Frecuencia"),
                        yaxis2 = list(overlaying = "y", side = "right"))
  plt[[i]] <- grafica
  
}

plt

# -- En esta parte graficaremos las ventas (Normalizadas) de todas las empresas durante 1978-2016
# -- Primeramente tomaremos las empresas que cuentan con todos los datos de ventas de 1978-2016
Ventas_completas <- Ventas_Normalizadas
Ventas_completas <- Ventas_completas[sapply(Ventas_completas, function(Ventas_completas) !any(is.na(Ventas_completas)))] 

df <- Ventas_completas
df$Periodo <- as.numeric(rownames(df))
df <- melt(df ,  id.vars = 'Periodo', variable.name = 'series')
# plot on same grid, each series colored differently -- 
# good if the series have same scale
ggplot(df, aes(Periodo,value)) + ggtitle("Ventas Normalizadas (1978-2016) \n Empresas con datos completos") +  xlab("Periodo") + ylab("Ventas") + geom_line(aes(colour = series))
# or plot on different plots si no estan normalizados los datos 
ggplot(df, aes(Periodo,value)) + geom_line() + facet_grid(series ~ .)


# -- Ahora veremos de todas las ventas, en que periodos se tienen menos NAs, para trabajar con esa base de datos para graficarlas y ver si es viable separarlas por categorias 
nas <- 0
nas_col <- 0
total <- sum(is.na(Ventas))

for (i in 1:nrow(Ventas)){
  
  nas[i] <- sum(is.na(Ventas[i,]))
    
}

for (i in 1:ncol(Ventas)){
  
  nas_col[i] <- sum(is.na(Ventas[,i]))
  
}

# Data Frame para ver cuantos Nas tenemos en cada a??o 
Missing_Data <- data.frame("Periodo"=rownames(Ventas), "NAs"=nas)

# Ordenamos de menor a mayor y viceversa para ver cuantos datos les faltan a cada una de las empresas 
nas_col_ordenado <- sort(nas_col, decreasing = TRUE) 
nas_col_ordenado_menos_a_mas <- sort(nas_col, decreasing = FALSE)

# Empresas que tienen mas Nas y menos Nas (el valor que tomamos por default es 20 ya que es la mitad de los datos que se tienen que son en total 39 periodos de 1978-2016)
Empresas_mas_nas <- colnames(Ventas)[which(nas_col>20)]
Empresas_menos_nas <- colnames(Ventas)[which(nas_col<20)]

# Armamos la base de datos con las empresas que menos tienen NAs 
# Cabe mencionar que el lapso de tiempo elegido fue de la mitad hacia adelante, es decir de 1997-2016, ya que de aqu?? en adelante se tienen mas datos disponibles que en la primera mitad, ademas de que los datos mas recientes suponen un mejor entendimiento del presente 
Data_base_less_nas_ventas <- Ventas[20:39,Empresas_menos_nas]
Data_base_less_nas_ventas <- Data_base_less_nas_ventas[sapply(Data_base_less_nas_ventas, function(Data_base_less_nas_ventas) !any(is.na(Data_base_less_nas_ventas)))] 
Data_base_less_nas_ventas_normalizadas <- Ventas_Normalizadas[20:39,Empresas_menos_nas]
Data_base_less_nas_ventas_normalizadas <- Data_base_less_nas_ventas_normalizadas[sapply(Data_base_less_nas_ventas_normalizadas, function(Data_base_less_nas_ventas_normalizadas) !any(is.na(Data_base_less_nas_ventas_normalizadas)))] 

Tasas_base_less_nas_ventas_normalizadas <- Tasas_ventas[20:38,Empresas_menos_nas]
Tasas_base_less_nas_ventas_normalizadas <- Tasas_base_less_nas_ventas_normalizadas[sapply(Tasas_base_less_nas_ventas_normalizadas, function(Tasas_base_less_nas_ventas_normalizadas) !any(is.na(Tasas_base_less_nas_ventas_normalizadas)))] 

#GRAFICAMOS VENTAS NORMALIZADAS

#--NOTA: HAY QUE SEPARARLAS POR PROPIEDAD, NAICS PARA QUE SE PUEDAN GRAFICAR BIEN Y TENER MAS ANALISIS VISUAL

#Primeramente creamos una lista de dataframes Naics y Propiedad, ya que de Registro no tiene sentido hacerlo porque son 48 en total y registros tenemos 38 

Datos_completo <- Tasas_base_less_nas_ventas_normalizadas
Datos_completo <- rbind(PNRTrans['NAICS', colnames(Tasas_base_less_nas_ventas_normalizadas)], PNRTrans['PROPIEDAD',colnames(Tasas_base_less_nas_ventas_normalizadas)],Datos_completo)

# Comenzamos con las NAIC`s

NAICS_DF_list <- list()
NAICS_numbers <- sort(unique(as.numeric(Datos_completo['NAICS',])))

for (i in 1:length(NAICS_numbers)){
  
  Names <- colnames(Tasas_base_less_nas_ventas_normalizadas)[which(Datos_completo['NAICS',]==NAICS_numbers[i])]
  Naic_df_interno <- as.data.frame(Tasas_base_less_nas_ventas_normalizadas[,Names])
  colnames(Naic_df_interno) <- Names
  
  NAICS_DF_list[[i]] <- Naic_df_interno
  
}


# Seguimos con las Propiedades

PROPIEDAD_DF_list <- list()
PROPIEDAD_numbers <- sort(unique(as.numeric(Datos_completo['PROPIEDAD',])))

for (i in 1:length(PROPIEDAD_numbers)){
  
  Names <- colnames(Tasas_base_less_nas_ventas_normalizadas)[which(Datos_completo['PROPIEDAD',]==PROPIEDAD_numbers[i])]
  Propiedad_df_interno <- as.data.frame(Tasas_base_less_nas_ventas_normalizadas[,Names])
  colnames(Propiedad_df_interno) <- Names
  
  PROPIEDAD_DF_list[[i]] <- Propiedad_df_interno
  
}

  
# GRAFICAMOS Tasas de crecimiento de acuerdo a NAICs

# Let's do a first plot
plt_naics <- list() 
for(k in 1:length(NAICS_numbers)){
  
    my_y = t(apply(as.matrix(NAICS_DF_list[[k]][,1]), 2, as.numeric))
    my_y <- my_y[1,]
    my_x = as.array(as.numeric(rownames(Datos_completo)[3:length(Datos_completo[,1])])) 
    
    if(ncol(NAICS_DF_list[[k]])<=1){
          pp<-plot_ly(y=my_y, x=my_x , type="scatter", mode="markers+lines",  name = paste(colnames(NAICS_DF_list[[k]])[1],sep=" ")) %>%
            layout(
              title = paste(colnames(NAICS_DF_list[[k]])," Growth Rates - ", "NAIC: ", NAICS_numbers[k]),
              xaxis = list(title = "Date"),
              yaxis = list(title = "Growth Rates"))
    }else{
      
      pp<-plot_ly(y=my_y, x=my_x , type="scatter", mode="markers+lines",  name = paste(colnames(NAICS_DF_list[[k]])[1],sep=" ")) %>%
        layout(
          title = paste("Mexican Companies Growth Rates - ", "NAIC: ", NAICS_numbers[k]),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Growth Rates"))
          loops <- length(colnames(NAICS_DF_list[[k]]))
        
          if(loops>1){
            for(i in 2:loops){ 
              
              my_y = t(apply(as.matrix(NAICS_DF_list[[k]][,i]), 2, as.numeric))
              my_y <- my_y[1,]
              pp<-add_trace(pp, y=my_y, type="scatter", mode="markers+lines", name = paste(colnames(NAICS_DF_list[[k]])[i],sep=" "), evaluate = TRUE)
              
            }
          }
    }#Cierre else 
    
    plt_naics[[k]] <- pp

}

plt_naics

# GRAFICAMOS Tasas de crecimiento de acuerdo a Propiedad

# Let's do a first plot
plt_propiedad <- list() 
for(k in 1:length(PROPIEDAD_numbers)){
  
  my_y = t(apply(as.matrix(PROPIEDAD_DF_list[[k]][,1]), 2, as.numeric))
  my_y <- my_y[1,]
  my_x = as.array(as.numeric(rownames(Datos_completo)[3:length(Datos_completo[,1])])) 
  
  if(ncol(PROPIEDAD_DF_list[[k]])<=1){
    pp<-plot_ly(y=my_y, x=my_x , type="scatter", mode="markers+lines",  name = paste(colnames(PROPIEDAD_DF_list[[k]])[1],sep=" ")) %>%
      layout(
        title = paste(colnames(PROPIEDAD_DF_list[[k]])," Growth Rates - ", "PROPIEDAD: ", PROPIEDAD_numbers[k]),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Growth Rates"))
  }else{
    
    pp<-plot_ly(y=my_y, x=my_x , type="scatter", mode="markers+lines",  name = paste(colnames(PROPIEDAD_DF_list[[k]])[1],sep=" ")) %>%
      layout(
        title = paste("Mexican Companies Growth Rates - ", "PROPIEDAD: ", PROPIEDAD_numbers[k]),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Growth Rates"))
    loops <- length(colnames(PROPIEDAD_DF_list[[k]]))
    
    if(loops>1){
      for(i in 2:loops){ 
        
        my_y = t(apply(as.matrix(PROPIEDAD_DF_list[[k]][,i]), 2, as.numeric))
        my_y <- my_y[1,]
        pp<-add_trace(pp, y=my_y, type="scatter", mode="markers+lines", name = paste(colnames(PROPIEDAD_DF_list[[k]])[i],sep=" "), evaluate = TRUE)
        
      }
    }
  }#Cierre else 
  
  plt_propiedad[[k]] <- pp
  
}

plt_propiedad

# Aqui utilizamos ggplot para graficar tasas de crecimiento DE TODAS LAS EMPRESAS (con Datos de 1997-2016)
my_y = apply(as.matrix(Tasas_base_less_nas_ventas_normalizadas), 2, as.numeric)
my_y <- my_y[1:nrow(Tasas_base_less_nas_ventas_normalizadas),]
df <- as.data.frame(my_y)
df$Periodo <- as.numeric(rownames(Datos_completo)[3:length(Datos_completo[,1])])
df <- melt(df ,  id.vars = 'Periodo', variable.name = 'series')
# plot on same grid, each series colored differently -- 
# good if the series have same scale
plot_ggplot_tasas <- ggplot(df, aes(Periodo,value)) + ggtitle(paste("Tasas de Crecimiento en Ventas (1997-2015) \n Empresas con datos completos", sep='')) +  xlab("Periodo") + ylab("Tasa crecimiento en Ventas") + geom_line(aes(colour = series))
plot_ggplot_tasas + scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")


# Aqui utilizamos ggplot para graficar ventas normalizadas DE TODAS LAS EMPRESAS
my_y = apply(as.matrix(Data_base_less_nas_ventas_normalizadas), 2, as.numeric)
my_y <- my_y[1:nrow(Data_base_less_nas_ventas_normalizadas),]
df <- as.data.frame(my_y)
df$Periodo <- as.numeric(rownames(Data_base_less_nas_ventas_normalizadas))
df <- melt(df ,  id.vars = 'Periodo', variable.name = 'series')
# plot on same grid, each series colored differently -- 
# good if the series have same scale
plot_ggplot_ventas_norm <- ggplot(df, aes(Periodo,value)) + ggtitle(paste("Ventas Normalizadas (1997-2015) \n Empresas con datos completos", sep='')) +  xlab("Periodo") + ylab("Ventas Normalizadas") + geom_line(aes(colour = series))
plot_ggplot_ventas_norm + scale_fill_continuous(guide = guide_legend()) +
  theme(legend.position="bottom")


# Aqui Ventas Normalizadas con Pltoly
# Create data
my_y=Data_base_less_nas_ventas_normalizadas[,1]
my_x=as.numeric(rownames(Data_base_less_nas_ventas_normalizadas))

# Let's do a first plot
p_1997_2016<-plot_ly(y=my_y, x=my_x , type="scatter", mode="markers+lines",  name = paste(colnames(Data_base_less_nas_ventas_normalizadas)[1],sep=" "), evaluate = TRUE) %>%
  layout(
    title = "Mexican Companies Normalized Sales",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Normalized Sales"))

# Add 9 trace to this graphic with a loop!
for(i in 2:ncol(Data_base_less_nas_ventas_normalizadas)){
  
  my_y=Data_base_less_nas_ventas_normalizadas[,i]
  p_1997_2016<-add_trace(p_1997_2016, y=my_y, type="scatter", mode="markers+lines", name = paste(colnames(Data_base_less_nas_ventas_normalizadas)[i],sep=" "), evaluate = TRUE)
  
}

p_1997_2016










  







