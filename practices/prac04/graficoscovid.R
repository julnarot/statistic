
#GENERANDO REPORTE CON VARIABLES CUALITATIVAS COVID 19

## CARGAR LIBRERIAS
library(ggplot2)
library(scales)
## install.packages("plotly") # para agregar tooltips
??plotly
library(plotly)
library(dplyr)
install.packages("plotly")


#library(tidyverse)

## creamos los diferentes vectores


dep <- c('LIMA','CALLAO','LAMBAYEQUE','PIURA')
pcr <- c(9762, 926, 779, 200)
pr <- c(10286, 2007, 1035, 760)

data <- data.frame(dep, pcr,pr)
View(data)

fig <- plot_ly(data, x=~dep, y=~pcr, type = 'bar', text=pcr, textposition = 'auto', name= 'PCR', marker = list(color = 'rgb(49, 130, 189)'))
fig <- fig %>% add_trace(y=pr, textposition = 'auto',  name= 'PR', marker = list(color = 'rgb(204, 204, 204)'))
fig <- fig %>% layout(xaxis = list(title = '', tickangle = -45),
                     text = pcr, textposition = 'auto',
                     yaxis = list(title = ""),
                     margin = list(b=100),
                     barmode = 'group') %>% layout (title = "Reporte del PCR(+) Y PCR(-) por departamentos COVID-19")
fig



#generando reporte descrptivo pcr

fig1 <- plot_ly(x=pcr, y=reorder(dep, pcr),
                type='bar', orientation = 'h',
                text=pcr, textposition = 'auto',
                marker = list(color = 'rgba(50, 171,96,1.0)', line = list(color='rgba(50,171)', width = 1)
                              )
                )

fig1