#Librerias
library(readxl)
library(ggplot2)
library(maps)
#install.packages("rgeos")
library(rgeos)
library(maptools)
#install.packages("maptools")
library(ggmap)
library(geosphere)
library(dplyr)
library(gganimate)

#Datos
file1_in <- 'comercio_exterior_spread.csv'
file2_in<- "Paises.xlsx"

data <- read.csv(file1_in, sep=',',  header = TRUE)
data_cities <- read_excel(file2_in, sheet=1)
class(data)
class(data_cities)

head(data)
head(data_cities)
unique(data$REPORTER)
unique(data_cities$Country)

#guardar latitud y longitud de Espa?a
coordenadas<-data_cities[data_cities$Country=="Spain",]
head(coordenadas)
latitud<-"40?25'N"
longitud<-"03?45'W"

#eliminar las innecesarias
data<-data[!data$REPORTER=="Spain (incl. Canary Islands 'XB' from 1997)",]
data<-data[!data$REPORTER=="European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)",]
data<-data[!data$?..PERIOD=="Jan.-Dec. 2018",]
data<-data[!data$?..PERIOD=="Jan.-Dec. 2019",]
data<-data[!data$?..PERIOD=="Jan.-Dec. 2020",]

#cambiar nombre de columnas
data$REPORTER<-as.character(data$REPORTER)
data$REPORTER[data$REPORTER == "Belgium (incl. Luxembourg 'LU' -> 1998)"] <- 'Belgium'
data$REPORTER[data$REPORTER == "Czechia"] <- "Czech Republic"
data$REPORTER[data$REPORTER == "France (incl. Saint Barthélemy 'BL' -> 2012| incl. French Guiana 'GF', Guadeloupe 'GP', Martinique 'MQ', Réunion 'RE' from 1997| incl. Mayotte 'YT' from 2014)"] <- 'France'
data$REPORTER[data$REPORTER == "Germany (incl. German Democratic Republic 'DD' from 1991)"] <- 'Germany'
data$REPORTER[data$REPORTER == "Ireland (Eire)"] <- 'Ireland'
data$REPORTER[data$REPORTER == "Italy (incl. San Marino 'SM' -> 1993)"] <- 'Italy'

#JOIN
joineado <- data %>% left_join(data_cities, by = c("REPORTER"="Country"))
joineado2<-joineado %>% group_by(REPORTER, PARTNER, ?..PERIOD, Latitude, Longitude) %>% summarise(conteo=n())
joineado2$lat2<-latitud
joineado2$long2<-longitud

unique(joineado2$REPORTER)
#View(joineado2)
head(joineado2)

#renombrar columnas
colnames(joineado2)<-c("REPORTER","PARTNER","Fecha","lat1","lon1","conteo","lat2","lon2")


#pasar latitudes y longitudes a + t - (quitando N y S)
data_plot<-joineado2

coordenadas_bien <- function(columna) {
  chd = substr(columna, 3, 3)[1]
  chm = substr(columna, 6, 6)[1]
  cd = char2dms(columna,chd=chd,chm=chm)
  cd = as.numeric(cd)
  return(cd)
}

data_plot$lat1<-coordenadas_bien(data_plot$lat1)
data_plot$lon1<-coordenadas_bien(data_plot$lon1)
data_plot$lat2<-coordenadas_bien(data_plot$lat2)
data_plot$lon2<-coordenadas_bien(data_plot$lon2)

# mapa --------------------------------------------------------------------
library(maptools)
library(ggmap)
library(geosphere)

#fondo mapa
worldmap <- map_data("world")#, region = europeanUnion)
wrld<-c(geom_polygon(aes(long,lat,group=group), 
                     size = 0.1, 
                     colour= "#969b9c",#969b9c", 
                     fill="#969b9c", alpha=0.8, data=worldmap))

ggplot() + 
  wrld + 
  theme(panel.background =   
        element_rect(fill='#eeeeee',colour='#eeeeee'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  coord_cartesian(xlim = c(-25,45), ylim = c(32,70))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


#meter datos
#install.packages("plyr")
library(plyr)

fortify.SpatialLinesDataFrame <- function(model, data, ...){
  ldply(model@lines, fortify)
}

#calcular rutas para cada fila
routes <- gcIntermediate(data_plot[,c('lon1', 'lat1')], data_plot[,c('lon2', 'lat2')], 15, breakAtDateLine = F, addStartEnd = T, sp=TRUE)

# fortify to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)

# merge to form circles
routes_count <- data.frame('count'=data_plot$conteo, 'id'=1:nrow(data_plot), 'Countries'=data_plot$REPORTER)
greatcircles <- merge(fortifiedroutes, routes_count, all.x=T, by='id')

#colores simples
colors_new <- c("tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3","tomato3")


#PLOTEAR
plot <- 
  ggplot() +
  wrld +
  geom_line(aes(long,lat,group=id, color=Countries), alpha=0.3, 
            size=0.01, data= greatcircles) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_colour_manual(values=colors_new)+
  theme(panel.background =  element_rect(fill='#c6dee4', 
                                         colour='#c6dee4'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(-20,35), ylim = c(35,70))+
  theme(legend.position = "none")

plot

#animar
library(gganimate)

anim <- plot + 
  transition_reveal(greatcircles$order)
anim

anim_save("europe.gif")

#LO CONTRARIO EXPORTACIONES
# mapa --------------------------------------------------------------------
library(maptools)
library(ggmap)
library(geosphere)

#fondo mapa
worldmap <- map_data("world")#, region = europeanUnion)
wrld<-c(geom_polygon(aes(long,lat,group=group), 
                     size = 0.1, 
                     colour= "#969b9c",#969b9c", 
                     fill="#969b9c", alpha=0.8, data=worldmap))

ggplot() + 
  wrld + 
  theme(panel.background =   
          element_rect(fill='#00001C',colour='#00001C'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) + 
  coord_cartesian(xlim = c(-25,45), ylim = c(32,70))+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())


#meter datos
#install.packages("plyr")
library(plyr)

fortify.SpatialLinesDataFrame <- function(model, data, ...){
  ldply(model@lines, fortify)
}

#calcular rutas para cada fila
routes <- gcIntermediate(data_plot[,c('lon2', 'lat2')], data_plot[,c('lon1', 'lat1')], 15, breakAtDateLine = F, addStartEnd = T, sp=TRUE)

# fortify to dataframe
fortifiedroutes <- fortify.SpatialLinesDataFrame(routes)

# merge to form circles
routes_count <- data.frame('count'=data_plot$conteo, 'id'=1:nrow(data_plot), 'Countries'=data_plot$PARTNER)
greatcircles <- merge(fortifiedroutes, routes_count, all.x=T, by='id')

#colores simples
colors_new <- c("slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4","slateblue4")
#PLOTEAR
plot2 <- 
  ggplot() +
  wrld +
  geom_line(aes(long,lat,group=id, color=Countries), alpha=0.3, 
            size=0.01, data= greatcircles) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())+
  scale_colour_manual(values=colors_new)+
  theme(panel.background =  element_rect(fill='#c6dadd', 
                                         colour='#c6dadd'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  coord_cartesian(xlim = c(-20,35), ylim = c(35,70))+
  theme(legend.position = "none")

plot2

#animar
library(gganimate)

anim2 <- plot2 + 
  transition_reveal(greatcircles$order)
anim2

anim_save("europe2.gif")

# mapa tipo balance -------------------------------------------------------
head(data)

library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)

data_balance<-data[!data$QUANTITY_IN_100KG==":",]
data_balance<-data_balance[!data_balance$VALUE_IN_EUROS==":",]

#corregimos
#data_balance<-data_balance[!data_balance$?..PERIOD=="Dec. 2020",]
data_balance$A?o<-str_sub(data_balance$?..PERIOD,-4,-1)
data_balance$A?o<-as.character(data_balance$A?o)
data_balance$?..PERIOD<-as.character(data_balance$?..PERIOD)
data_balance$VALUE_IN_EUROS<-as.numeric(data_balance$VALUE_IN_EUROS)

unique(data_balance$A?o)
unique(data_balance$?..PERIOD)


#separar importaciones y exportaciones totales
data_balance_import<-data_balance[data_balance$FLOW=="IMPORT",]
data_balance_export<-data_balance[data_balance$FLOW=="EXPORT",]

#se coge el valor pero se podria coger la cantidad (QUANTITY)
data_balance_import<-aggregate(data_balance_import$VALUE_IN_EUROS, by=list(Category=data_balance_import$?..PERIOD), FUN=sum)
data_balance_export<-aggregate(data_balance_export$VALUE_IN_EUROS, by=list(Category=data_balance_export$?..PERIOD), FUN=sum)
data_balance_import$Tipo<-"Importacion"
data_balance_export$Tipo<-"Exportacion"

head(data_balance_import)
head(data_balance_export)

#names(data_balance_export)[names(data_balance_export) == "x"] <- "x2"
#names(data_balance_export)[names(data_balance_export) == "Category"] <- "Category2"

data_diferencia<-rbind(data_balance_import, data_balance_export)
head(data_diferencia)

#data_diferencia<-cbind(data_balance_import, data_balance_export)
#data_diferencia$Diferencia<-data_diferencia$x2-data_diferencia$x
#data_diferencia
#diferencia<-data_diferencia$Diferencia

data_total<-data_diferencia
data_total$Category<-gsub('\\.', '', data_total$Category)
data_total$Category<-gsub( " ", "", data_total$Category) 
data_total$Category<-tolower(data_total$Category)
data_total$Category<-paste0("1",data_total$Category)
unique(data_total$Category)
head(data_total)

colnames(data_total)<-c("Fecha", "Valor", "Tipo")
head(data_total)

#pasar a formato fecha
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
data_total$Fecha<-as.Date(data_total$Fecha, "%d%b%Y")
Sys.setlocale("LC_TIME", lct)
data_total$Fecha<-as.character(data_total$Fecha)
unique(data_total$Fecha)
class(data_total$Fecha)
dim(data_total)
is.na(data_total)

data_total$Fecha<-as.factor(data_total$Fecha)
data_total$Fecha<-as.Date(data_total$Fecha)
class(data_total$Fecha)


#graficar
library(plotly)
options(scipen=10000)

addUnits <- function(n) {
  labels <- ifelse(n < 1e9, paste0(round(n/1e6), 'M'))
  return(labels)
}

head(data_total)

# g<- ggplot(datos, aes(x = Fecha, y = Importacion, fill = Tipo))+
#   geom_bar(stat = "identity")+
#   scale_y_continuous(limits=c(-37000000, 37000000), labels = addUnits)+
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
#   scale_x_date(breaks="3 month", date_labels = "%b%d%y")+
#   geom_line(aes(x = Fecha, y = diferencia_biderbi),size = 1, color="red", group = 1)
# 
# ggplotly(g)

ggplot(data_total, aes(x=Fecha, y=Valor, fill=Tipo))+
  geom_bar(position=position_dodge(), stat="identity")+
  scale_y_continuous(limits=c(0, 37000000), labels = addUnits)+
  scale_x_date(breaks="3 month", date_labels = "%b%y")+
  scale_fill_manual("Tipo", values = c("Exportacion" = rgb(0.8,0.2,0.5,0.4), "Importacion" = rgb(0.2,0.5,0.5,0.4)))
