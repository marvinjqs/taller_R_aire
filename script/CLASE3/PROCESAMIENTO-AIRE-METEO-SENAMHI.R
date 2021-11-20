#################################
# PROCESAMIENTO DE DATOS DE CALIDAD DE AIRE Y METEOROLOGICOS
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

###############
#  Paquetes   #
###############

library(ggplot2)
library(httr)
library(DataExplorer)

# CONFIGURAR EL DIRECTORIO DE TRABAJO

path_wd <- readClipboard()
                   
setwd(path_wd)

#######################
#  iMPORTACION DE DATOS  #
#######################

# IMPORTAR BASES DE DATO

df1 <- read.table("air-data-campodm.csv", header = T, 
                  stringsAsFactors = F, sep = ",")

df2 <- read.table("data-meteo-campodm.csv", header = T, 
                  stringsAsFactors = F, sep = ",")

# MERGE A LOS DF
df <- merge(df1, df2, by = "date")

# CONVERTIR FECHAS
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S")

# COMPLETAR LAS FECHAS FALTANTES CON NA
library(tidyr)
df <- df %>%
  complete(date = seq(min(date), max(date), by = "1 hour"),
           fill = list(VALOR = NA))


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
library(openair)
timePlot(df, pollutant = "PM25",
         ref.y = list(h = 50, lty = 5),
         avg.time = "1 day")

# BOXPLOT
ggplot(df, aes(y = PM25)) +
  geom_boxplot()

# SCATTER PLOT
ggplot(df, aes(x=date, y=PM25)) + 
  geom_line() +
  geom_smooth(method = "loess")



#coef(model)
#model <- lm(mpg~disp+hp+wt, data)

library(caret)
library(klaR)

library(caret)

preproc1 <- preProcess(df, method = "range")
norm1 <- predict(preproc1, df)

df <- norm1
smp_size <- floor(0.8 * nrow(df))

set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]


# MODELO DE REGRESION LINEAL MULTIVARIABLE
model <- lm(HUM~TEMP+VEL_V, data=train)
summary(model)$r.squared

sqrt(mean(model$residuals^2))

