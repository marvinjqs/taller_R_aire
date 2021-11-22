#################################
# AUTOMATIZACION DE LA DESCARGA DE DATOS HIDROMETEOROLOGICOS DE SENAMHI
# CURSO: ANALISIS DE DATOS DE CALIDAD DE AIRE CON R Y RSTUDIO
# Marvin J. Quispe Sedano
# Email: marvinjqs@gmail.com
#################################

#---------------------------------------------------------
# Para limpiar la consola:
# TeclaS :  Ctrl + L

# Para limpiar el workspace:
rm(list = ls())

###############
#  Paquetes   #
###############

library(ggplot2)
library(RSelenium)
library(DataExplorer)
library(tidyr)

# FIJAR EL DIRECTORIO DE TRABAJO

setwd("D:/PROYECTOS-R/taller_R_aire/script/CLASE3") 

#######################
#  DESCARGA DE DATOS  #
#######################

# DEFINIR UNA FUNCION PARA LA DESCARGA DE DATOS

library(RSelenium)

binman::list_versions("chromedriver")

download_senamhi_data <- function(url_list) {
  
  # NUMERO ALEATORIO DE PUERTO 
  port <- as.integer(runif(1, min = 5000, max = 6000))
  
  # EJECUTAMOS EL DRIVER DE GOOGLE CHROME 
  rD <- rsDriver(port = port, browser = "chrome", 
                 chromever = "96.0.4664.35")
  
  remDrv <- rD$client
  
  for (url in url_list){
  
  # INGRESAR AL URL
  remDrv$navigate(url)
  
  # ENCONTRAR EL BOTON DE DESCARGA 
  down_button <- remDrv$findElement(using = "id", "export2")
  down_button$clickElement()
  
  }
  
  # CERRAR LA SESION ACTUAL
  remDrv$close()
  rD$server$stop()
  rm(rD, remDrv)
  gc()

}

# EJECUTAR LA FUNCION PARA DESCARGAR TODOS LOS MESES DE UN AÃ‘O

list_url <- list()

for (i in 1:9) {
  
  list_url[i] = paste("https://www.senamhi.gob.pe//mapas/mapa-estaciones-2/_dato_esta_tipo02.php?estaciones=112181&CBOFiltro=20200",
               i, "&t_e=M&estado=AUTOMATICA&cod_old=&cate_esta=EMA&alt=117", sep = "")
  
 }

for (i in 10:12) {
  
  list_url[i] = paste("https://www.senamhi.gob.pe//mapas/mapa-estaciones-2/_dato_esta_tipo02.php?estaciones=112181&CBOFiltro=2020",
               i, "&t_e=M&estado=AUTOMATICA&cod_old=&cate_esta=EMA&alt=117", sep = "")
  
}

download_senamhi_data(list_url)


###################
#  IMPORTAR DATOS #
###################

# IMPORTAR LOS ARCHIVOS CSV DE UNA CARPETA 

wd_path <- "D:/PROYECTOS-R/taller_R_aire/script/CLASE3"
myfiles <- list.files(path=wd_path, pattern="*.csv", full.names=TRUE)

df_list <- list()

for (i in 1:length(myfiles)){
  
  df_list[[i]] <- read.table(myfiles[i], sep = ",", header = T ,
                           skip = 10 , stringsAsFactors = F, 
                           na.strings = "S/D")
}

# CONCATENAR LOS DATAFRAMES DE LA LISTA

df <- Reduce(function(...) merge(... , all=TRUE), df_list)

# MODIFICAR LA COLUMNA DE LA FECHA

df$date = as.POSIXct(paste(df[,1], df[,2], sep = " "), 
                        format = "%Y/%m/%d %H:%M")

df <- df[,-c(1,2)]
df <- df[, c(6,1:5)]

# MODIFICAR LOS NOMBRES DE CADA COLUMNA

colnames(df) <- c("date", "TEMP", "PP", "HUM", "DIR_V", "VEL_V")

# COMPLETAR LAS FECHAS FALTANTES CON NA

library(tidyr)

df <- df %>%
  complete(date = seq(min(date), max(date), by = "1 hour"),
           fill = list(VALOR = NA))

# GUARDAR BASE DE DATOS 

write.csv(df, "data-meteo-campodm.csv", row.names = F)

####################################
#  ANALISIS EXPLORATORIO DE DATOS  #
####################################

library(DataExplorer)

# VISUALIZAR LA ESCTRUCTURA DE NUESTRA DATA
plot_str(df)

# RESUMEN DE NUESTROS DATOS
introduce(df)
plot_intro(df)

# VALORES FALTANTES O MISSING
plot_missing(df)
profile_missing(df)

# HISTOGRAMA 
dev.new()
plot_histogram(df)

# QQPLOT
plot_qq(df)

# ANALISIS DE CORRELACIÓN
plot_correlation(na.omit(df))

# BOXPLOT
ggplot(stack(df), aes(x = ind, y = values)) +
  geom_boxplot()

ggplot(df, aes(x = date, y = HUM)) +
  geom_boxplot()

# SCATTER PLOT
library(reshape2)
df_sp <- melt(df[,c(1,2,4)], id.vars = 'date', variable.name = 'Variables')

ggplot(df_sp, aes(date,value)) + 
  geom_line() + 
  geom_smooth() +
  facet_grid(Variables ~ .) 

ggplot(df, aes(date,PP)) + 
  geom_line() + 
  geom_smooth()  
                                                    
######################
#  LIMPIEZA DE DATOS #
######################      

summary(df)

# ELIMINAR LAS FILAS QUE CONTIENEN VALORES NA
df_complete <- na.omit(df)
df_complete <- df[complete.cases(df),]

# COMPLETAR VALORES NA MEDIANTE IMPUTACION MULTIPLE
library(mice)

df_mice <- mice(df,m=5,maxit=10,meth='cart',seed=500)
summary(df_mice)
df_complete <- complete(df_mice,1)
df <- df_complete

# ELIMINAR VALORES OUTLIERS UNIVARIABLES MEDIANTE BOXPLOT
boxplot.stats(df$VEL_V)$out
out_VEL_V <- boxplot.stats(df$VEL_V)$out
df <- df[!(df$VEL_V %in% out_VEL_V),]


###################################
#  ANALISIS ESTADISTICO DE DATOS  #
###################################  

# Obtener los promedios, maximos, minimos, etc

df_2 <- df

df_2$date <- seq(as.POSIXct("2019-12-31 23:00", format = "%Y-%m-%d %H:%M"), 
                 as.POSIXct("2020-12-31 22:00", format = "%Y-%m-%d %H:%M"), 
                 by = "1 hour") 

library(data.table)

data_day_mean <- setDT(df_2)[,lapply(.SD, function(x) if(length(na.omit(x)) >= 18)
  (mean(x, na.rm = T)) else NA_real_) ,
  by = .(day = format(date, '%d/%m/%Y'))]

data_day_sum <- setDT(df_2)[,lapply(.SD, function(x) if(length(na.omit(x)) >= 18)
  (sum(x)) else NA_real_) ,
  by = .(day = format(date, '%d/%m/%Y'))]


data_month_mean <- setDT(df_2)[,lapply(.SD, function(x) if(length(na.omit(x)) >= 23)
  (mean(x, na.rm = T)) else NA_real_) ,
  by = .(month = format(date, '%m/%Y'))]


# ETC

############################
#  VISUALIZACION DE DATOS  #
############################  

library(openair)

# GRAFICAMOS LAS SERIES DE TIEMPO 
timePlot(df, pollutant = c("TEMP", "PP", "HUM"),
         name.pol = c("TEMPERATURA", "PRECIPITACION", "HUMEDAD"), 
         smooth = T,
         xlab = "Tiempo", ylab = "Valores", 
         main = "DATOS HIDROMETEOROLÓGICOS DE LA ESTACION CAMPO DE MARTE - 2020",
         avg.time = "1 hour")

# GRAFICAMOS LA CORRELACION ENTRE VARIABLES
pairs(df,
      lower.panel = panel.smooth,
      upper.panel = NULL,
      col = "skyblue3")

###########################

colnames(df)[5] <- "wd"
colnames(df)[6] <- "ws"

# GRAFICAMOS LAS ROSAS DE VIENTO
windRose(df)
windRose(df, type = "season")
windRose(df, type = "week")

# GRAFICAMOS LAS SERIES DE TIEMPO CON LOS DATOS DE VIENTO INCLUIDOS
timePlot(df, pollutant = c("HUM"), 
         windflow = list(scale = 0.1, lwd = 2, col = "darkcyan"), 
         lwd = 3, group = FALSE, 
         xlab = "Tiempo", ylab = "Valores",
         cols = c("red", "blue"),
         main = "DATOS HIDROMETEOROLÓGICOS DE LA ESTACION VON HUMBOLDT - 2020",
         avg.time = "1 month")

# GRAFICAMOS LA VARIACION TEMPORAL DE LA TEMPERATURA
timeVariation(df, pollutant = c("TEMP","HUM"))

# GRAFICAMOS UN CALENDARIO PARA EVALUAR CADA VARIABLE
calendarPlot(df, pollutant = "TEMP", year = 2020)

calendarPlot(df, pollutant = "PP", year = 2020, cols = "Blues")

calendarPlot(df, pollutant = "HUM", year = 2020, annotate = "ws")

calendarPlot(df, pollutant = "TEMP", year = 2020,
             breaks = c(-20, 0, 10, 20, 25, 100),
             labels = c("Muy frio", "Frio", "Templado", "Calido", "Muy calido"),
             cols = "increment", statistic = "max")

                                                       
              