## FRECUENCIAS  Y MEDIDAD DE TENDENCIA CENTRAL, DISPERSION, POSICION Y FORMA

## definir directorio de trabajo

setwd("~/sistemas/estadistica/clase03")
getwd()

data <- read.csv("base.csv")
View(data)
data
str(data)


tabcol <- table(data$color);
tabcol
library(tidyverse)
## cuale son los 3 colores de carros mas vendidos
data %>%
   count(color)%>%
  top_n(3, n) %>%
  arrange(desc(n))

##PREGUNTA 2. de que año son los carros mas vendidos

# covertir año a factor

data$year <- as.factor(data$year)
tabyear <- table(data$year)

 plot <- data %>%
  count(year) %>%
  top_n(3,n)

# crear un bar plot para presentar la respuesta a la pregunta 2
  barplot(tabcol, main = "colores")
  barplot(tabyear, main = "años", col = rainbow(9))
  
  
#  data %>%
 #   count(year) %>%
  #  top_n(3, n) %>%
   # ggplot(aes((x=year, y = n)))+
#geom_bar(stat = "identity") +
  

## determinar el porcentaje de ventas de carros segun transmision
    
    
## para crear una frecuencia relativa debe generar una tabla de distribucion de frecuencia
    
    ## crear tabla de distribucion de frecuencia
    
tabcambio <- table(data$transmi)
tabcambio
# determinar la frecuencia relativo 0<1
frecrel <- prop.table(tabcambio)        
frecrel          

# obteniendo el porcentaje

porcentaje <- prop.table(tabcambio) * 100
porcentaje

# mejoorar el reporte a la pregunta y agrupar

data %>%
  group_by(Tipo = transmi) %>%
  summarise(ventas = n()) %>%
  mutate("%" = round((ventas / sum(ventas) * 100), 3))

## generar un reporte pie chart
labels <- c("automatico", "manual")
labels <- paste(labels," %", sep = "")

pie(frecrel, 
    labels = labels, 
    col = rainbow(length(labels)), 
    main="Porcentaje de ventas por tipo transmision")


data %>%
  group_by(transmi) %>%
  summarise(ventas=n())%>%
  mutate("%" = round ((ventas/sum(ventas)*100),2))%>%
  ggplot(aes(x="", y = transmi, fill = transmi)) + 
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  geom_text(aes(label = `%`), hjust= .5, vjust = -1.5, color ="white", fontface="bold")+
  labs(title="Porcentaje vendido de cars con transmisión automatica")+
  xlab("")+
  ylab("Transmisión")


## obtener porcentaje de ventas por modelo

install.packages("janitor")
#library(janitor)

# obtener tabla de distribcion de frecuencia con tabyl
tabyl(data$modelo, sort=T)
tabyl

#data %>%
#  group_by(Modelo = modelo) %>%
#  summarise(ventas=n())%>%
#  mutate("%" = round ((ventas/sum(ventas)*100),2))%>%

  
#install.packages("epiDisplay")


# tab1(data$modelo, sort.group = "decresing", cum.percent = TRUE, main = "Grafico")
  
  ### lista el total de carros  vendido por año y por tipo de transmision
  ## ->>>>>tabla de contigencia por que tiene mas de una variable a evaluar
  
## test chi cuadrado -> hay o no relacion entre las variables

tab <- table(data$year, data$transmi)
tab  

## 9. reportar resumen estadistico  chi-cuadrado

summary(tab)

## generando grafico de la pregunta 8 

barplot(tab, beside = T, legend.text = rownames(tab), ylab = "Frecuencia Absoluta")

