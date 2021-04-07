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
data <- read.csv(file.path(dir_in, file1_in), sep=',')
data_cities <- read_excel(file.path(dir_in, file2_in), sheet=1)

#eliminar las innecesarias
data<-data[!data$REPORTER=="Spain (incl. Canary Islands 'XB' from 1997)",]
data<-data[!data$REPORTER=="European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)",]
data<-data[!data$ï..PERIOD=="Jan.-Dec. 2018",]
data<-data[!data$ï..PERIOD=="Jan.-Dec. 2019",]
data<-data[!data$ï..PERIOD=="Jan.-Dec. 2020",]

#cambiar nombre de columnas
data$REPORTER<-as.character(data$REPORTER)
data$REPORTER[data$REPORTER == "Belgium (incl. Luxembourg 'LU' -> 1998)"] <- 'Belgium'
data$REPORTER[data$REPORTER == "Czechia"] <- "Czech Republic"
data$REPORTER[data$REPORTER == "France (incl. Saint Barthélemy 'BL' -> 2012| incl. French Guiana 'GF', Guadeloupe 'GP', Martinique 'MQ', Réunion 'RE' from 1997| incl. Mayotte 'YT' from 2014)"] <- 'France'
data$REPORTER[data$REPORTER == "Germany (incl. German Democratic Republic 'DD' from 1991)"] <- 'Germany'
data$REPORTER[data$REPORTER == "Ireland (Eire)"] <- 'Ireland'
data$REPORTER[data$REPORTER == "Italy (incl. San Marino 'SM' -> 1993)"] <- 'Italy'

#grafico
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(lubridate)
library(stringr)

data_balance<-data[!data$QUANTITY_IN_100KG==":",]
data_balance<-data_balance[!data_balance$VALUE_IN_EUROS==":",]

#corregimos

data_balance$Anio<-str_sub(data_balance$ï..PERIOD,-4,-1)
data_balance$Anio<-as.character(data_balance$A?o)
data_balance$ï..PERIOD<-as.character(data_balance$ï..PERIOD)
data_balance$VALUE_IN_EUROS<-as.numeric(data_balance$VALUE_IN_EUROS)


#AQUI DEPENDERÍA DEL INPUT (PAIS)
#data_balance<-data_balance%>%
#  filter(REPORTER=="Austria")

#separar importaciones y exportaciones totales
data_balance_import<-data_balance[data_balance$FLOW=="IMPORT",]
data_balance_export<-data_balance[data_balance$FLOW=="EXPORT",]

#AQUI DEPENDE DE OTRO INPUT se coge el valor pero se podria coger la cantidad (QUANTITY) 
#data_balance_import<-aggregate(data_balance_import$VALUE_IN_EUROS, by=list(Category=data_balance_import$ï..PERIOD), FUN=sum)
#data_balance_export<-aggregate(data_balance_export$VALUE_IN_EUROS, by=list(Category=data_balance_export$ï..PERIOD), FUN=sum)

data_balance_import$Tipo<-"Importacion"
data_balance_export$Tipo<-"Exportacion"

data_diferencia<-rbind(data_balance_import, data_balance_export)

data_total<-data_diferencia
data_total$Category<-gsub('\\.', '', data_total$Category)
data_total$Category<-gsub( " ", "", data_total$Category) 
data_total$Category<-tolower(data_total$Category)
data_total$Category<-paste0("1",data_total$Category)
colnames(data_total)<-c("Fecha", "Valor", "Tipo")

#pasar a formato fecha
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
data_total$Fecha<-as.Date(data_total$Fecha, "%d%b%Y")
Sys.setlocale("LC_TIME", lct)
data_total$Fecha<-as.character(data_total$Fecha)
data_total$Fecha<-as.factor(data_total$Fecha)
data_total$Fecha<-as.Date(data_total$Fecha)

#graficar
library(plotly)
options(scipen=10000)

addUnits <- function(n) {
  labels <- ifelse(n < 1e9, paste0(round(n/1e6), 'M'))
  return(labels)
}


g<-ggplot(data_total, aes(x=Fecha, y=Valor, fill=Tipo))+
  geom_bar(position=position_dodge(), stat="identity")+
 # scale_y_continuous(limits=c(0, 37000000), labels = addUnits)+
  scale_x_date(breaks="3 month", date_labels = "%b%y")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  scale_fill_manual("Tipo", values = c("Exportacion" = rgb(0.2,0.5,0.5,0.4), "Importacion" = rgb(0.8,0.2,0.5,0.4)))
ggplotly(g)
