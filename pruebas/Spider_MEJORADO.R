library(ggplot2)
library(Hmisc)
library(plotly)
library(gridExtra)
library(ggthemes)
library(tidyverse)
library(gapminder)
library(RColorBrewer)
library(scales)
library(hrbrthemes)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(leaflet)
library(shinythemes)
library(shinymanager)
library(tidyr)
library(rintrojs)
library(shinyBS)
library(shinyjs)
library(shinydashboardPlus)
library(shinyAce)
library(styler)
library(shinyEffects)
library(shinyjqui)
library(viridis)  
library(lubridate) 
library(ggExtra)
library(d3treeR)
library(dplyr)
library(htmlwidgets)
library(treemap)
library(remotes)
library(chron)
library(makeR)
library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(leaflet)
library(RCurl)
library(fmsb)

dir_in = 'C:/BDA II/DATATHON/Datathon-Datamasters/codigo'
file1_in <- 'consumo_cor.csv'
file2_in <- 'precios.csv'
file3_in <- 'coordenadas_ccaa.csv'

cor <- read.csv(file.path(dir_in, file3_in), sep=',',  header = TRUE)
consumo <- read.csv(file.path(dir_in, file1_in), sep=',',  header = TRUE,fileEncoding = "UTF-8")
precios <- read.csv(file.path(dir_in, file2_in), sep=',',  header = TRUE,fileEncoding = "UTF-8")

consumo$date = as.Date(consumo$date)


#################################################################################
unique(consumo$Producto)
consumo=consumo%>%
  filter(Producto!="T.HORTALIZAS FRESCAS" & Producto!="T.FRUTAS FRESCAS" & Producto!="TOTAL PATATAS" & Producto!="OTR.HORTALIZAS/VERD." & Producto!="VERD./HORT. IV GAMA")
consumo$year<-year(consumo$date)
consumo<-consumo%>%
  filter(CCAA!="Total Nacional")
unique(consumo$CCAA)


#mi func
funcion_spider<-function(ccaa){
  df <- consumo %>% filter(year == 2020) %>% 
    select(year, Producto, CCAA, Valor..miles.de... ) %>% group_by(Producto,CCAA) %>%
    summarise(Valor..miles.de... = mean(Valor..miles.de...)) %>% filter(CCAA == ccaa) %>% 
    arrange(desc(Valor..miles.de...)) %>%  head(4) %>% spread(Producto, Valor..miles.de...)
  table_spider <- rbind(apply(X= df[,-1] , MARGIN=1, FUN=max),rep(0,times=4), df[,-1])
  df
  nombres<-colnames(df)
  nombres
  
  df2 <- consumo %>% filter(year == 2019) %>% 
    select(year, Producto, CCAA, Valor..miles.de... ) %>% group_by(Producto,CCAA) %>%
    summarise(Valor..miles.de... = mean(Valor..miles.de...)) %>% filter(CCAA == ccaa) %>% 
    arrange(desc(Valor..miles.de...))%>%filter(Producto%in%nombres)%>% spread(Producto, Valor..miles.de...)
  
  table_spider <- rbind(apply(X= df[,-1] , MARGIN=1, FUN=max),rep(0,times=4), df[,-1], df2[,-1])
  coul <- brewer.pal(3, "BuPu")
  colors_border <- coul
  colors_in <- alpha(coul,0.3)
  p<-radarchart(table_spider, pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) ) , plwd=1 , 
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                vlcex=0.8 , lwd = 1, title=ccaa)
}

library(RColorBrewer)
library(scales)

unique(consumo$CCAA)

#TOP DEL 2020 (AQUÍ IRÍA LA COLUMNA)
funcion_spider("Andalucia")

#TOP DEL 2019 
#mi func
funcion_spider2<-function(ccaa){
  df <- consumo %>% filter(year == 2019) %>% 
    select(year, Producto, CCAA, Valor..miles.de... ) %>% group_by(Producto,CCAA) %>%
    summarise(Valor..miles.de... = mean(Valor..miles.de...)) %>% filter(CCAA == ccaa) %>% 
    arrange(desc(Valor..miles.de...)) %>%  head(4) %>% spread(Producto, Valor..miles.de...)
  table_spider <- rbind(apply(X= df[,-1] , MARGIN=1, FUN=max),rep(0,times=4), df[,-1])
  df
  nombres<-colnames(df)
  nombres
  
  df2 <- consumo %>% filter(year == 2020) %>% 
    select(year, Producto, CCAA, Valor..miles.de... ) %>% group_by(Producto,CCAA) %>%
    summarise(Valor..miles.de... = mean(Valor..miles.de...)) %>% filter(CCAA == ccaa) %>% 
    arrange(desc(Valor..miles.de...))%>%filter(Producto%in%nombres)%>% spread(Producto, Valor..miles.de...)
  
  table_spider <- rbind(apply(X= df[,-1] , MARGIN=1, FUN=max),rep(0,times=4), df[,-1], df2[,-1])
  coul <- brewer.pal(3, "BuPu")
  colors_border <- coul
  colors_in <- alpha(coul,0.3)
  p<-radarchart(table_spider, pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=c( rgb(0.8,0.2,0.5,0.4), rgb(0.2,0.5,0.5,0.4) ) , plwd=1 , 
                cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
                vlcex=0.8 , lwd = 1, title=ccaa)
}

#(AQUÍ IRÍA LA COLUMNA)
funcion_spider2("Andalucia")



#LEYENDA (ES OTRO PLOT QUE SE PUEDE COLOCAR DONDE QUERAMOS PARA QUE SEA COMUN)
#pcol="rgb(0.2,0.5,0.5,0.9)" , pfcol=rgb(0.2,0.5,0.5,0.2)
names <- c('2020', '2019')
clrs <- c('blue', 'maroon2')
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", title="Year", legend = names, lty=1, lwd=2, cex=1.25,
       bty='n', col = clrs)

