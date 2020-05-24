####### Instalaci?n de paquete
install.packages("tidyverse")

####### Importaci?n de librer?a
library(tidyverse)

####### Definimos directorio de trabajo
setwd("~/sistemas/statistics/practices/prac05")
getwd()

####### Cargar la base de datos
data <- read_csv("hour.csv")

View(data)

####### Calcular la media, mediana y moda

####### Media o promedio
lapply(data, mean)

####### Mediana
lapply(data, median)

####### Moda
md <- lapply(data, table)
lapply(md, max)


####### Calcular la temperatura (temp) promedio por d?a de semana (weekday)
data %>%
  group_by(weekday) %>%
  summarise(media_temp = mean(temp))

####### Crear un histograma
data %>%
  ggplot(aes(x=temp)) +
  geom_histogram(bins = 30)






