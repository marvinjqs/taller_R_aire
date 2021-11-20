#################################
# AUTOMATIZACION Y PROCESAMIENTO DE DATOS DE CALIDAD DE AIRE DEL SENAMHI
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
library(httr)
library(DataExplorer)

#######################
#  DESCARGA DE DATOS  #
#######################

r <- GET("https://www.senamhi.gob.pe/site/sea/www/site/sea/graficas/dato_hora.php?estacion=112194&cont=N_PM25&f1=01/01/2020&f2=31/12/2020")
s <- content(r, "text")

# REALIZAMOS LOS CORTES PARA QUEDARNOS SOLO CON LOS DATOS

s_split1 <- strsplit(s, 'radius: 0,')[[1]][3]
s_split2 <- strsplit(s_split1, "\\[")[[1]][2]
s_split3 <- strsplit(s_split2, ',]')[[1]][1]

# REALIZAMOS LOS CORTES PARA QUEDARNOS CON LAS FECHAS

s2_split1 <- strsplit(s, 'xAxis: \\{')[[1]][2]
s2_split2 <- strsplit(s2_split1, "\\[")[[1]][2]
s2_split3 <- strsplit(s2_split2, ',]')[[1]][1]

# OBTENER UN DATAFRAME CON LOS VALORES DE CONCENTRACION

values_df <- data.frame()
values_o <- strsplit(s_split3, ",")

for (i in 1:length(values_o[[1]])){
  values_df[i,1] <- as.numeric(values_o[[1]][i])
}

# OBTENER UN DATAFRAME PARA LAS FECHAS

date_df <- data.frame()
date_o <- strsplit(s2_split3, ",")

for (i in 1:length(date_o[[1]])){
  date_1 <- as.character(date_o[[1]][i])
  date_2 <- substr(date_1, 2, 16 )
  date_df[i,1] <- date_2
}

# CONCATENAR LOS DATAFRAMES

df <- data.frame(date_df[,1], values_df[,1])
names(df) <- c("date", "PM25")

# ASIGNAR EL FORMATO DE FECHA
df$date <- as.POSIXct(as.character(df$date), format = "%d/%m/%Y%H:%M")
write.csv(df, "air-data-campodm.csv", row.names = F)

####################################
#  ANALISIS EXPLORATORIO DE DATOS  #
####################################

# VISUALIZAR LA ESCTRUCTURA DE NUESTRA DATA
plot_str(df)

# RESUMEN DE NUESTROS DATOS
introduce(df)
plot_intro(df)

# VALORES FALTANTES O MISSING
plot_missing(df)
profile_missing(df)

# HISTOGRAMA 
plot_histogram(df)

# QQPLOT
plot_qq(df)

# TIME PLOT
timePlot(df, pollutant = "PM25",
         ref.y = list(h = 40, lty = 5),
         avg.time = "1 day")

# BOXPLOT
ggplot(df, aes(y = PM25)) +
  geom_boxplot()

# SCATTER PLOT
ggplot(df, aes(x=date, y=PM25)) + 
  geom_line() +
  geom_smooth(method = "loess")
  
 


