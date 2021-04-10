# LIBRERIAS ---------------------------------------------------------------
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
library(shinyAce)
library(styler)
library(shinyEffects)
library(shinyjqui)
library(viridis)  
library(lubridate) 
library(ggExtra)
library(dplyr)
library(htmlwidgets)
library(treemap)
library(remotes)
library(fmsb)
library(chron)
library(tmap)
library(sf)
library(stringi)
library(readxl)
library(tidyselect)
options(scipen = 999)


# Funciones a utilizar ----------------------------------------------------
addUnits <- function(n) {
  labels <- ifelse(n < 1e9, paste0(round(n/1e6), 'M'))
  return(labels)
}
# CARGA DE DATOS ----------------------------------------------------------
dir_in <- 'datos/datos_desarrollo'
file1_in <- 'consumo_final.csv'
file2_in <- 'precios.csv'
file3_in <- 'data_total.csv'
file4_in <- 'predicciones_consumo.csv'


precio <- read.csv(file.path(dir_in, file2_in), sep=',',  header = TRUE,fileEncoding = "UTF-8", stringsAsFactors = FALSE)
consumo <- read.csv(file.path(dir_in, file1_in), sep=';',  header = TRUE,fileEncoding = "UTF-8", stringsAsFactors = FALSE)
consumo_pred <- read.csv(file.path(dir_in, file4_in), sep = ';', stringsAsFactors = F, fileEncoding = 'UTF-8')
data_total <- read.csv(file.path(dir_in, file3_in), sep=',',  header = TRUE,fileEncoding = "UTF-8", stringsAsFactors = FALSE)
consumo_pred[consumo_pred$ccaa == 'Cataluna', 'ccaa'] <- 'Cataluña'

data_total = data_total %>% 
  select(-X)
colnames(consumo)
colnames(consumo)[4] <-'valor'
colnames(consumo)[3] <- 'volumen'
colnames(consumo)[5] <-'precio'
colnames(consumo_pred)[7] <-'valor_predicted'
colnames(consumo_pred)[4] <- 'volumen_predicted'
colnames(consumo_pred)[10] <-'precio_predicted'

# Asignacion de clases ----------------------------------------------------
# Precio
precio <- precio %>% select(-X)
precio$inicio <- as.Date(precio$inicio)
precio$fin <- as.Date(precio$fin)

# Consumo
head(consumo)
unique(consumo$ccaa)

consumo$ccaa[consumo$ccaa== "Andalucia"  ] <- 'Andalucía'
consumo$ccaa[consumo$ccaa== "Aragon"  ] <- 'Aragón'
consumo$ccaa[consumo$ccaa== "Castilla la mancha"  ] <- 'Castilla la Mancha'
consumo$ccaa[consumo$ccaa== "Castilla leon"  ] <- 'Castilla y León'
consumo$ccaa[consumo$ccaa== "La rioja"  ] <- 'La Rioja'
consumo$ccaa[consumo$ccaa== "Madrid"  ] <- 'Comunidad de Madrid'
consumo$ccaa[consumo$ccaa== "Pais vasco"  ] <- 'País Vasco'

head(consumo_pred)
unique(consumo_pred$ccaa)

consumo_pred$ccaa[consumo_pred$ccaa== "Andalucia"  ] <- 'Andalucía'
consumo_pred$ccaa[consumo_pred$ccaa== "Aragon"  ] <- 'Aragón'
consumo_pred$ccaa[consumo_pred$ccaa== "Castilla la mancha"  ] <- 'Castilla la Mancha'
consumo_pred$ccaa[consumo_pred$ccaa== "Castilla leon"  ] <- 'Castilla y León'
consumo_pred$ccaa[consumo_pred$ccaa== "La rioja"  ] <- 'La Rioja'
consumo_pred$ccaa[consumo_pred$ccaa== "Madrid"  ] <- 'Comunidad de Madrid'
consumo_pred$ccaa[consumo_pred$ccaa== "Pais vasco"  ] <- 'País Vasco'

#consumo$date <- strptime(as.character(consumo$date), "%d/%m/%Y")
consumo$date <- as.Date(consumo$date)
#consumo_pred$X  <- strptime(as.character(consumo_pred$X), "%d/%m/%Y")
consumo_pred$X <- as.Date(consumo_pred$X)
consumo <- consumo %>% filter(producto != 'Chirimoya')

#Creacion de tablas
consumo_resta <- consumo %>% 
  inner_join(consumo_pred, by = c('date' = 'X', 'ccaa', 'producto')) %>% 
  select(date, ccaa, producto, volumen, volumen_predicted,
         valor, valor_predicted, precio, 
         precio_predicted, Olas) %>% 
  mutate(volumen_resta = volumen - volumen_predicted,
         valor_resta = valor - valor_predicted,
         precio_resta = precio - precio_predicted) %>% 
  select(date, ccaa, producto, volumen_resta, valor_resta, 
         precio_resta, Olas)

unique(consumo$ccaa)

consumo_resta <- consumo_resta %>% 
  filter(ccaa != 'Nacional', date != '2020-02-01')

#Importaciones exportaciones
data_total$Fecha <- as.Date(data_total$Fecha)
consumo_resta_dos_epocas <- consumo_resta %>% group_by(producto, ccaa) %>% summarise(volumen_resta = mean(volumen_resta),
                                                                                     valor_resta = mean(valor_resta),
                                                                                     precio_resta = mean(precio_resta))
# TEMPORADA ---------------------------------------------------------------
temp <- consumo %>% dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date))

temp1819 <- temp %>% filter(date < '2020-03-01') 
temp1819$fecha_filtro_inicio <- ymd(paste(temp1819$year, '-',
                                          temp1819$Fecha_inicio, '-',
                                          '01'))
temp1819$fecha_filtro_fin <- ymd(paste(temp1819$year, '-',
                                       temp1819$Fecha_fin, '-',
                                       '01'))
a_sumar <- which(temp1819$Fecha_fin < temp1819$Fecha_inicio)
year(temp1819$fecha_filtro_fin)[a_sumar] <- year(temp1819$fecha_filtro_fin)[a_sumar] + 1
a_restar <- which((temp1819$Fecha_fin < temp1819$Fecha_inicio) & 
                    (month(temp1819$date) == temp1819$Fecha_fin))
year(temp1819$fecha_filtro_inicio)[a_restar] <- year(temp1819$fecha_filtro_inicio)[a_restar] - 1
year(temp1819$fecha_filtro_fin)[a_restar] <- year(temp1819$fecha_filtro_fin)[a_restar] - 1
temp1819 <- temp1819 %>% 
  filter(date >= fecha_filtro_inicio, date <= fecha_filtro_fin) %>% 
  group_by(ccaa, producto) %>% 
  summarize (volumen = mean(volumen), valor = mean(valor), precio = mean(precio))

temp2020 <- temp %>% filter(date >= '2020-03-01') %>%
  filter(month >= Fecha_inicio) %>%  
  filter(month <= Fecha_fin) %>% 
  group_by(ccaa, producto) %>% 
  summarize (volumen = mean(volumen),valor = mean(valor), precio = mean(precio))

temp_comp <- inner_join(temp1819,temp2020, by = c("ccaa", "producto"),suffix = c("_1819", "_2020"))

temp_comp$volumen = ((temp_comp$volumen_2020 - temp_comp$volumen_1819) / temp_comp$volumen_1819)*100
temp_comp$valor = ((temp_comp$valor_2020 - temp_comp$valor_1819) / temp_comp$valor_1819)*100
temp_comp$precio = ((temp_comp$precio_2020 - temp_comp$precio_1819) / temp_comp$precio_1819)*100

temp_comp <- as.data.frame(temp_comp)
productos_en_comun <- unique(temp_comp$producto)
productos_en_comun <- productos_en_comun[-which(productos_en_comun %in% 
                                                  c('Ciruelas', 'Tomates', 'Uvas'))]
temp1819 <- temp1819 %>% filter(producto %in% productos_en_comun)
temp2020 <- temp2020 %>% filter(producto %in% productos_en_comun)

# MAPAS ----------------------------------------------------------
dir_in_mapas <-  "ccaa_mapita"
file3_in <- 'Comunidades_Autonomas_ETRS89_30N.shp'
autonomias <- read_sf(file.path(dir_in_mapas, file3_in))
municipalities_spain<-st_set_geometry(autonomias,NULL)


#data_spain<- as.data.frame(consumo %>% select(ccaa, precio, producto))
data_spain <- consumo
data_spain$ccaa <- as.character(data_spain$ccaa)

municipalities_spain$Texto <- as.character(municipalities_spain$Texto)
municipalities_spain$Texto <- str_to_title(stri_trans_general(municipalities_spain$Texto,"Latin-ASCII"))

municipalities_spain$Texto[municipalities_spain$Texto== "Principado De Asturias"  ] <- 'Asturias'
municipalities_spain$Texto[municipalities_spain$Texto==  "Islas Baleares"] <- 'Baleares'
municipalities_spain$Texto[municipalities_spain$Texto== "Castilla - La Mancha"] <- 'Castilla la Mancha'
municipalities_spain$Texto[municipalities_spain$Texto== "Castilla Y Leon"] <- 'Castilla y León'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad De Madrid"] <- 'Comunidad de Madrid'
municipalities_spain$Texto[municipalities_spain$Texto== "Region De Murcia"] <- 'Murcia'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad Foral De Navarra"] <- 'Navarra'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad Valenciana"] <- 'Valencia'
municipalities_spain$Texto[municipalities_spain$Texto== "Cataluna"] <- 'Cataluña'
municipalities_spain$Texto[municipalities_spain$Texto== "La Rioja"] <- 'La Rioja'
municipalities_spain$Texto[municipalities_spain$Texto== "Pais Vasco"] <- 'País Vasco'
municipalities_spain$Texto[municipalities_spain$Texto== "Andalucia"] <- 'Andalucía'
municipalities_spain$Texto[municipalities_spain$Texto== "Aragon"] <- 'Aragón'

data_spain<-data_spain[!data_spain$ccaa=="Nacional",]
municipalities_spain<-municipalities_spain[!municipalities_spain$Texto=="Ceuta",]
municipalities_spain<-municipalities_spain[!municipalities_spain$Texto=="Melilla",]


autonomias2<-data.frame(autonomias)
autonomias2<-autonomias2[!autonomias2$Texto=="Ceuta",]
autonomias2<-autonomias2[!autonomias2$Texto=="Melilla",]
autonomias2$Texto <- str_to_title(stri_trans_general(autonomias2$Texto,"Latin-ASCII"))


autonomias2$Texto[autonomias2$Texto== "Principado De Asturias"  ] <- 'Asturias'
autonomias2$Texto[autonomias2$Texto==  "Islas Baleares"] <- 'Baleares'
autonomias2$Texto[autonomias2$Texto== "Castilla - La Mancha"] <- 'Castilla la Mancha'
autonomias2$Texto[autonomias2$Texto== "Castilla Y Leon"] <- 'Castilla y León'
autonomias2$Texto[autonomias2$Texto== "Comunidad De Madrid"] <- 'Comunidad de Madrid'
autonomias2$Texto[autonomias2$Texto== "Region De Murcia"] <- 'Murcia'
autonomias2$Texto[autonomias2$Texto== "Comunidad Foral De Navarra"] <- 'Navarra'
autonomias2$Texto[autonomias2$Texto== "Comunidad Valenciana"] <- 'Valencia'
autonomias2$Texto[autonomias2$Texto== "La Rioja"] <- 'La Rioja'
autonomias2$Texto[autonomias2$Texto== "Cataluna"] <- 'Cataluña'
autonomias2$Texto[autonomias2$Texto== "Andalucia"] <- 'Andalucía'
autonomias2$Texto[autonomias2$Texto== "Aragon"] <- 'Aragón'
autonomias2$Texto[autonomias2$Texto== "Pais Vasco"] <- 'País Vasco'


autonomias3<-as_tibble(autonomias2)
autonomias3<-st_sf(autonomias3)

# Tabla top productos -----------------------------------------------------
unique(consumo$producto)


consumo_top <- consumo %>% filter(producto!="Hortalizas frescas" & producto!="Frutas frescas" & producto!="Patatas" & producto!="Hortalizas" & producto!="Frutas iv gama")
consumo_top <- consumo_top %>% filter(ccaa!="Nacional")
consumo_top$year<-year(consumo_top$date)



# Importaciones y exportaciones -------------------------------------------
file1_in <- 'comercio_exterior_spread.csv'
file2_in<- "Paises.xlsx"
data <- read.csv(file.path(dir_in, file1_in), sep=',')
data_cities <- read_excel(file.path(dir_in, file2_in), sheet=1)

#eliminar las innecesarias
data<-data[!data$REPORTER=="Spain (incl. Canary Islands 'XB' from 1997)",]
data<-data[!data$REPORTER=="European Union - 27 countries (AT, BE, BG, CY, CZ, DE, DK, EE, ES, FI, FR, GR, HR, HU, IE, IT, LT, LU, LV, MT, NL, PL, PT, RO, SE, SI, SK)",]
data<-data[!data$PERIOD=="Jan.-Dec. 2018",]
data<-data[!data$PERIOD=="Jan.-Dec. 2019",]
data<-data[!data$PERIOD=="Jan.-Dec. 2020",]

#cambiar nombre de columnas
data$REPORTER<-as.character(data$REPORTER)
data$REPORTER[data$REPORTER == "Belgium (incl. Luxembourg 'LU' -> 1998)"] <- 'Belgium'
data$REPORTER[data$REPORTER == "Czechia"] <- "Czech Republic"
data$REPORTER <- ifelse(data$REPORTER == unique(data$REPORTER)[10], " France", data$REPORTER)
data$REPORTER[data$REPORTER == "France (incl. Saint BarthÃ©lemy 'BL' -> 2012| incl. French Guiana 'GF', Guadeloupe 'GP', Martinique 'MQ', RÃ©union 'RE' from 1997| incl. Mayotte 'YT' from 2014)"] <- 'France'
data$REPORTER[data$REPORTER == "Germany (incl. German Democratic Republic 'DD' from 1991)"] <- 'Germany'
data$REPORTER[data$REPORTER == "Ireland (Eire)"] <- 'Ireland'
data$REPORTER[data$REPORTER == "Italy (incl. San Marino 'SM' -> 1993)"] <- 'Italy'

data_balance<-data[!data$QUANTITY_IN_100KG==":",]
data_balance$QUANTITY_IN_100KG <- as.numeric(data_balance$QUANTITY_IN_100KG)
data_balance<-data_balance[!data_balance$VALUE_IN_EUROS==":",]
data_balance$Anio<-str_sub(data_balance$PERIOD,-4,-1)
data_balance$Anio<-as.character(data_balance$Anio)
data_balance$REPORTER<-as.factor(data_balance$REPORTER)
data_balance$VALUE_IN_EUROS<-as.numeric(data_balance$VALUE_IN_EUROS)
data_balance <- data_balance %>% select(FLOW,PERIOD, REPORTER, QUANTITY_IN_100KG, VALUE_IN_EUROS)
data_balance <- data_balance %>% group_by(FLOW, PERIOD, REPORTER) %>% summarize(volumen = sum(QUANTITY_IN_100KG, na.rm = TRUE), valor = sum(VALUE_IN_EUROS, na.rm = TRUE))

data_balance$FLOW<- ifelse(data_balance$FLOW == 'EXPORT', " Importación", "Exportación")

colnames(data_balance) <- c('Tipo','fecha','pais','volumen','valor')

data_balance$fecha<-gsub('\\.', '', data_balance$fecha)
data_balance$fecha<-gsub( " ", "", data_balance$fecha) 
data_balance$fecha<-tolower(data_balance$fecha)
data_balance$fecha<-paste0("1",data_balance$fecha)

#pasar a formato fecha
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
data_balance$fecha<-as.Date(data_balance$fecha, "%d%b%Y")
Sys.setlocale("LC_TIME", lct)
data_balance$fecha<-as.character(data_balance$fecha)
data_balance$fecha<-as.factor(data_balance$fecha)
data_balance$fecha<-as.Date(data_balance$fecha)


# CLICK MAPAS -------------------------------------------------------------
data_spain11 <- consumo_resta %>% 
  group_by(ccaa, producto) %>% 
  summarise(mean=mean(volumen_resta, na.rm = TRUE))

data_spain11<-as_tibble(data_spain11)

futuralista <- municipalities_spain %>% 
  left_join(data_spain11, by = c("Texto"="ccaa")) %>% 
  filter(producto == 'Manzanas') %>% 
  group_by(producto, Texto) %>% 
  summarise(mean = mean(mean, na.rm = TRUE))

futuralista<-st_sf(futuralista, st_geometry(autonomias3))

nnc <- futuralista[,-1]
futuralista$idd <- seq(1,dim(futuralista)[1],1)
futuralista<- futuralista[c(5,1,2,3,4)]

nc_names <- nnc %>%
  st_set_geometry(NULL) %>%
  distinct(Texto) %>%
  pull()



# SALUD -------------------------------------------------------------------
file_salud_in <- 'salud.csv'
salud <- read.csv(file.path(dir_in, file_salud_in), sep=';',  header = TRUE,fileEncoding = "UTF-8", stringsAsFactors = FALSE)

salud$Producto <- str_to_title(salud$Producto)
bebidas <- c("Total Vinos","Cervezas","Sidras" ,"T.bebidas Espirituosa")
bebidas_rows <- salud %>% filter(Producto %in% bebidas) %>% group_by(Fecha) %>% summarize(CONSUMO.X.CAPITA = sum(CONSUMO.X.CAPITA), VALOR..Miles.Euros. = sum(VALOR..Miles.Euros.))
bebidas_rows$Producto <- 'Bebidas Alcohólicas'
salud <- salud %>% filter(!(Producto %in% bebidas))
df_salud <- rbind(salud, bebidas_rows)

df_salud$Producto <- ifelse(df_salud$Producto == "T.huevos Kgs" , 'Huevos (Kg)', df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Pescados" , "Total Pescado", df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Frescos" , 'Huevos (Kg)', df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Total Leche Liquida" , 'Leche', df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Boll.past.gallet.cere" , 'Bollería, pastelería, galletas, cereales', df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Total Pastas" , 'Pasta', df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Total Aceite" , 'Aceite', df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto =="Pescados Congelados" , "Pescado Congelado", df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Pescados Frescos" , "Pescado Fresco", df_salud$Producto)
df_salud$Producto <- ifelse(df_salud$Producto == "Leche" , 'Leche (L)', df_salud$Producto)

df_salud$Fecha <- as.Date(df_salud$Fecha, format = '%d/%m/%Y')
salud_frutas <- consumo %>% select(valor, consumo_per_capita, date) %>% group_by(date) %>% 
  summarise(CONSUMO.X.CAPITA = mean(consumo_per_capita), VALOR..Miles.Euros. = mean(valor))
salud_frutas$Producto <- 'Frutas y Hortalizas'
colnames(salud_frutas) <- c('Fecha', "CONSUMO.X.CAPITA","VALOR..Miles.Euros.", 'Producto')
df_salud <- rbind(salud_frutas, df_salud)
df_salud <- as.data.frame(df_salud)

pre_salud <- df_salud %>% filter(Fecha < '2020-03-01') %>% group_by(Producto) %>% 
  summarise(CONSUMO.X.CAPITA = mean(CONSUMO.X.CAPITA), VALOR..Miles.Euros. = mean(VALOR..Miles.Euros.))

post_salud <- df_salud %>% filter(Fecha >= '2020-03-01') %>% group_by(Producto) %>% 
  summarise(CONSUMO.X.CAPITA = mean(CONSUMO.X.CAPITA), VALOR..Miles.Euros. = mean(VALOR..Miles.Euros.))

box_salud <- pre_salud

box_salud$CONSUMO.X.CAPITA <- ((post_salud$CONSUMO.X.CAPITA - pre_salud$CONSUMO.X.CAPITA) / pre_salud$CONSUMO.X.CAPITA) *100
box_salud$VALOR..Miles.Euros. <- ((post_salud$VALOR..Miles.Euros. - pre_salud$VALOR..Miles.Euros.) / pre_salud$VALOR..Miles.Euros.) * 100

# FILTROS -----------------------------------------------------------------
variables31 <- c('Consumo per cápita' = "CONSUMO.X.CAPITA" , 'Ingresos totales' = "VALOR..Miles.Euros." )

pais13 <- unique(data_balance$pais)
variables13 <- c('Volumen de ventas' = 'volumen', 'Ingresos totales'='valor')

variables211 <- c('Consumo per cápita' = 'consumo_per_capita',  'Precio' = "precio")

variables <- c('Consumo per cápita' = 'consumo_per_capita', 'Volumen de venta' = "volumen" , 'Ingresos totales' = "valor", 'Precio' = "precio")
productos <- unique(consumo$producto)
productos <- sort(productos[productos!= 'Frutas frescas'])
ccaa <- sort(unique(consumo$ccaa))

ccaa_top <- sort(unique(consumo_top$ccaa))

data_spain11 <- consumo_resta %>% group_by(ccaa, producto, Olas) %>% summarise(mean=mean(.data[[paste0('volumen', '_resta')]], na.rm = TRUE))

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  
  title = 'UniversityHack 2021 TheDataMasters',
  # SIDEBAR  
  sidebar = dashboardSidebar(
    width = 100,
    #itemns
    sidebarMenu(
      menuItem(text = "Análisis",  tabName = "tema2",icon = icon('info-circle')),
      menuItem(text = "Covid", tabName = "tema1", icon = icon('chart-bar')), 
      menuItem(text = "Salud",  tabName = "tema3",icon = icon('search'))
    )
    
  ),
  
  
  
  # BODY
  body = dashboardBody(
    tags$head(tags$style(HTML('
                                /* Titulo principal */
                                .skin-blue .main-header .logo {background-color: #8FA0B7;}

                                /* Titulo principal active */
                                .skin-blue .main-header .logo:hover {background-color: #8FA0B7;}

                                /* navegador superior */
                                .skin-blue .main-header .navbar {background-color: #8FA0B7;}

                                /* bloque menu */
                                .skin-blue .main-sidebar {background-color: #B7C2D1;}

                                /* bloque menu items */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{background-color: #B7C2D1;}

                                /* Nombre de los items */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #B7C2D1;color: #FFFFFF;}

                                /* Nombre menu items active */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #7F9ED7;}
                                
                                /* boton abrir/cerrar menu activate  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{background-color: #B7C2D1;}

                                /* body */
                                .content-wrapper, .right-side {background-color: #FFFFFF;}'))
    ),
    
    
    #itemns
    tabItems(
      
      # COVID
      tabItem(tabName = "tema1",
              tabBox(width = 15, height = 2,
                     
                     tabPanel("Impacto general",
                              fluidRow(
                                
                                column(5, 
                                       radioGroupButtons('select_variable11', label = 'Seleccione la variable', choices= variables[-1], 
                                                         selected = variables[3])
                                ),
                                column(3,
                                       selectInput('select_ccaa11', label = 'Seleccione la CCAA', choices= ccaa, selected = ccaa[2]),
                                       tags$div(tags$style(HTML(".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))      
                                ),
                                column(3, 
                                       selectInput('select_producto11', label ='Seleccione el producto', choices= productos , selected= productos[4]),
                                       tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                )
                              ),
                              
                              column(6,
                                     fluidRow(
                                       box(
                                         title = "Cuantificando el impacto del virus", width = 12 , solidHeader = TRUE, background = "light-blue",
                                         "¿Y si predecimos lo ocurrido de no haber existido la pandemia?")
                                     ),
                                     fluidRow(
                                       box(plotOutput(outputId = 'grafico_evolucion'), width = 12, solidHeader = TRUE)
                                     ) 
                                     
                              ),
                              column(6,
                                     fluidRow(
                                       box('Mediante la resta de los datos recogidos durante la pandemia y las predicciones realizadas,
                                        se visualiza el aumento/disminución que se ha dado en el precio, volumen o ingresos totales de cada producto 
                                           como consecuencia de la pandemia.', br(), br(),
                                           leafletOutput(outputId = "my_tmap", height = 420), width = 12)
                                     )
                                     
                              )
                     ),
                     
                     
                     tabPanel("Temporada",
                              
                              fluidRow(
                                column(4,
                                       box(
                                         title = 'Impacto en las temporadas', width = 12, background = 'light-blue',
                                         solidHeader = TRUE, '¿Y si visualizamos los datos de la temporada para cada producto?'
                                       )
                                ),
                                column(4, 
                                       radioGroupButtons('select_variable12', label = 'Seleccione la variable', choices= variables[-1],
                                                         selected = variables[4])
                                ),
                                column(2,
                                       selectInput('select_ccaa12', label = 'Seleccione la CCAA', choices= ccaa, selected = ccaa[2]),
                                       tags$div(tags$style(HTML(".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                       
                                ),
                                column(2, selectInput('select_producto12', label ='Seleccione el producto', choices = productos_en_comun, 
                                                      selected= productos[4]),
                                       tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                       
                                )
                                
                              ),
                              
                              fluidRow(
                                column(5,
                                       box(
                                         "Datos: Previos al virus (2018-2019)", 
                                         leafletOutput(outputId = "map12_1819"),width = 12,solidHeader = TRUE)
                                ),
                                column(5,
                                       box(
                                         "Datos: Durante el COVID-19", 
                                         leafletOutput(outputId = "map12_2020"),width = 12,solidHeader = TRUE)
                                ),
                                column(2,
                                       fluidRow(valueBoxOutput('boxtemp', width = 12)),
                                       valueBoxOutput('progressBox', width = 15)
                                )
                                
                              )
                     ),
                     
                     
                     tabPanel("Importaciones y exportaciones",
                              
                              fluidRow(
                                column(6,
                                       box(
                                         title = 'Balance de las importaciones y exportaciones', width = 12, background = 'light-blue',
                                         solidHeader = TRUE, '¿Adónde exporta España sus frutas y hortalizas? Desde dónde provienen la mayoría de los productos que comemos en España?'
                                       )
                                ),
                                column(4,
                                       radioGroupButtons('select_variable13', label = 'Seleccione la variable', choices= variables13, selected = variables13[1])
                                       
                                ),
                                column(2,
                                       selectInput('select_pais13', label = 'País', choices = pais13, selected = pais13[1]),
                                       tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                )
                              ),  
                              
                              fluidRow(
                                column(12,
                                       box(plotOutput('impoexpo13'),  width = 12,solidHeader = TRUE)
                                )
                                
                              )
                              
                     )
                     
              )
              
      ),
      
      #AGROANALISIS
      tabItem(tabName = "tema2",
              tabBox(width = 15, height = 2,
                     tabPanel("Análisis general",
                              tabsetPanel(
                                tabPanel('Productos',
                                         fluidRow(
                                           column(4, 
                                                  radioGroupButtons('select_variable211', label = 'Seleccione la variable', choices= variables211, 
                                                                    selected = variables211[1])
                                           ),
                                           column(3, 
                                                  selectInput('select_producto211', label ='Seleccione el producto', choices= productos, selected= productos[4]),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                           ),
                                           column(4,
                                                  airMonthpickerInput(
                                                    inputId = "select_date211",
                                                    label = "Seleccione el rango temporal",
                                                    language = 'es',
                                                    value = c('2018-01-01', '2020-06-01'),
                                                    range = TRUE,
                                                    minDate = "2018-01-01", maxDate = "2020-06-01"
                                                  )
                                           )
                                           
                                         ),
                                         
                                         fluidRow(
                                           column(6,
                                                  box(
                                                    title = "¿Cómo varían los productos en las diferentes Comunidades Autónomas?", width = 12, solidHeader = TRUE,background = "light-blue",
                                                    "Analicemos cómo varían las características de los productos en cada Comunidad Autónoma"
                                                  )
                                           )
                                         ),
                                         
                                         fluidRow(
                                           column(6,
                                                  leafletOutput(outputId = "mapa211", height = 250)),
                                           column(6,
                                                  fluidRow(valueBoxOutput('precio211', width = 12)),
                                                  fluidRow(valueBoxOutput('nacional211',width = 12))
                                           )
                                         )
                                ),
                                
                                
                                tabPanel('Comparación productos',
                                         fluidRow(
                                           
                                           column(7, 
                                                  radioGroupButtons('select_variable212', label = 'Seleccione la variable', choices= variables, 
                                                                    selected = variables[3])
                                           ),
                                           column(2,
                                                  pickerInput('select_ccaa212', label = 'Seleccione la(s) CCAA', choices= ccaa, selected = ccaa[2],
                                                              multiple = TRUE, pickerOptions(actionsBox = TRUE)),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                                  
                                           ),
                                           
                                           column(3,
                                                  pickerInput('select_producto212', label ='Seleccione el/los producto/s', choices= productos , selected= c(productos[1],productos[2],productos[3],productos[4]), multiple = TRUE, options = 
                                                                pickerOptions(
                                                                  actionsBox = TRUE))
                                                  
                                           )
                                           
                                           
                                         ),
                                         fluidRow(
                                           column(6,
                                                  box(
                                                    title = "Comparemos los productos entre sí", width = 12, height = 120, solidHeader = T, background = "light-blue",
                                                    "Seleccione las Comunidades Autónomas y los productos a comparar. Se pueden comparar varios
                                                    productos y comunidades de manera simultánea.")
                                           ),
                                           column(6,
                                                  box(title = 'Recomendaciones', width = 12, solidHeader = T, tags$div('· No comparar más de 3 Comunidades
                                                      Autónomas simultáneamente.', tags$br(), '· No comparar más de 5 productos simultáneamente.', tags$br(),
                                                                                                                       '· No comparar más de 2 comunidades con 3 productos simultáneamente.'))
                                           )
                                         ),
                                         
                                         fluidRow(
                                           plotOutput(outputId = 'evolucomp212', height = 350)
                                           
                                         )
                                         
                                         
                                ),
                                tabPanel('Económico',
                                         fluidRow(
                                           column(6,
                                                  box(
                                                    title = "Indicadores económicos", width = 12, solidHeader = TRUE,background = "light-blue",
                                                    "Analicemos la relación entre el consumo y el gasto per cápita. Seleccione el producto y la ccaa a estudiar")
                                           ),
                                           column(2, 
                                                  selectInput('select_ccaa213', label = 'Seleccione la CCAA', choices= ccaa, selected = ccaa[2]),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                           ),
                                           column(3,
                                                  selectInput('select_producto213', label ='Seleccione el producto', choices= productos , selected= productos[4]),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                           )
                                           
                                           
                                         ),
                                         fluidRow(
                                           box(plotOutput(outputId = "barraslineas"),width = 12,solidHeader = TRUE)
                                           
                                         )
                                         
                                )
                                
                              )
                              
                     ),
                     
                     
                     tabPanel("Top productos",
                              fluidRow(
                                column(5,
                                       box(
                                         title = "Top Productos", width = 12, solidHeader = TRUE,background = "light-blue",
                                         "Analicemos los productos más consumidos en cada Comunidad Autónoma.")
                                       
                                ),
                                column(3, 
                                       selectInput('select_ccaa22', label = 'Seleccione CCAA', choices= ccaa_top, selected = ccaa[2]),
                                       tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                ),
                                column(4,
                                       numericInput('select_topp22', label = 'Seleccione número de productos (3 o más)', value = 4,
                                                    min = 3, step = 1)
                                )
                                
                              ),
                              fluidRow(
                                column(9,
                                       box('Para poder observar cambios en las tendencias de compras derivadas de la
                                      pandemia, se muestran los productos más vendidos en media de las Comunidades Autónomas
                                      en años anteriores a la pandemia y en el propio periodo de la pandemia.', 
                                           plotOutput(outputId = 'top_productos'), width = 12)
                                ),
                                column(3,
                                       fluidRow(valueBoxOutput('diferencia_producto_top22', width = 12)),
                                       fluidRow(valueBoxOutput('diferencia_top22', width = 12))
                                )
                              )
                              
                     )
                     
                     
              )
              
      ),
      
      # SALUD
      tabItem(tabName = "tema3",
              tabBox(width = 15, height = 2,
                     tabPanel('tema1',
                              fluidRow(
                                column(4,
                                       box(
                                         title = "Diferentes tipos de productos", width = 12, solidHeader = TRUE,background = "light-blue",
                                         "El consumo de frutas y hortalizas ha aumentado durante la pandemia, 
                                         pero, ¿qué ha ocurrido con otro tipo de productos?")
                                ),
                                column(4,
                                       selectInput('select_producto31', 'Seleccione el/los productos', choices = list(
                                         'Frutas y Hortalizas',
                                         Carne = c('Total Carne',"Carne Certificada","Carne Fresca","Carne Congelada","Carne Transformada"),
                                         Pescado = c('Total Pescado',"Pescado Fresco","Pescado Congelado"),
                                         Carbohidratos = c('Arroz','Pasta', 'Legumbres'),
                                         Bebidas = c('Bebidas Refrescantes', 'Bebidas Alcohólicas'),
                                         Bollería = c( "Bollería, pastelería, galletas, cereales", 'Pan'),
                                         Otros = c('Leche (L)', 'Huevos (Kg)', 'Aceite')), 
                                         selectize = TRUE, multiple = TRUE, selected = 'Frutas y Hortalizas')
                                ),
                                column(4, 
                                       radioGroupButtons('select_variable31', label = 'Seleccione la variable', choices= variables31, 
                                                         selected = variables31[1])
                                )
                                
                              ),
                              fluidRow(
                                column(8,
                                       box(
                                         plotOutput(outputId = 'comp31'), width = 12
                                       )
                                ),
                                column(4,
                                       fluidRow(
                                         valueBoxOutput('box31f',width = 12)
                                       ),
                                       fluidRow(
                                         valueBoxOutput('box31', width = 12)
                                       )
                                )
                              )
                              
                              
                     ),
                     tabPanel('tema2'
                              
                     )
              )
      )
      
    ) #tabitems close
    
  ),
  
  
  
  
  ## HEADER
  
  header = dashboardHeader(
    titleWidth = 100,
    title = tagList(
      span(class = 'logo-lg', 'Menu')),
    tags$li(class="dropdown", 
            tags$a(href="https://www.cajamardatalab.com/datathon-cajamar-universityhack-2020/", 
                   icon("web"))),
    tags$li(class="dropdown", 
            tags$a(href="https://www.facebook.com/Datathon/", 
                   icon("facebook"))),
    tags$li(class="dropdown", 
            tags$a(href="https://twitter.com/cajamardatalab?lang=es", 
                   icon("twitter"))),
    tags$li(class="dropdown", 
            tags$a(href="https://www.linkedin.com/company/datathoncamp/about/", 
                   icon("linkedin")))
    
    
  )
  
)


# SERVER ------------------------------------------------------------------


server <- function(input, output) {
  
  
  # COVID
  output$box11 <- renderValueBox({
    
    valor <- consumo_resta %>%  filter(producto == input$select_producto11, 
                                       ccaa == input$select_ccaa11) %>% 
      summarise(mean=mean(.data[[paste0(input$select_variable11, '_resta')]], na.rm = TRUE))
    
    if(valor >= 0){
      valueBox(value = NULL, paste('A raiz de la pandemia, el',input$select_variable11, 'medio de', input$select_producto11, 'ha aumentado ', round(valor,2), 'en', input$select_ccaa11 ), "Progress", icon = NULL,
               color = "navy")
    }else{
      valueBox(value = NULL, paste('A raiz de la pandemia el',input$select_variable11, 'medio de', input$select_producto11, 'ha disminuido ', abs(round(valor,2)), 'en', input$select_ccaa11 ), "Progress", icon = NULL,
               color = "navy")
    }
    
  })
  
  # MAPA 11
  output$my_tmap <- renderLeaflet({
    data_spain11 <- consumo_resta %>% 
      group_by(ccaa, producto) %>% 
      summarise(mean=mean(.data[[paste0(input$select_variable11, '_resta')]], na.rm = TRUE))
    
    data_spain11<-as_tibble(data_spain11)
    
    map_data_spain11 <- municipalities_spain %>% 
      left_join(data_spain11, by = c("Texto"="ccaa")) %>% 
      filter(producto == input$select_producto11) %>% 
      group_by(Codigo, Texto, producto) %>% 
      summarise(mean = mean(mean))
    
    pruebabuena11<-st_sf(map_data_spain11, st_geometry(autonomias3))
    pruebabuena11$variable <- names(which(variables == input$select_variable11))
    pruebabuena11$valor_variable <- paste(as.character(round(pruebabuena11$mean, 2)), 
                                          if (input$select_variable11 == 'precio') '€/kg'
                                          else if (input$select_variable11 == 'volumen') 'miles de kg'
                                          else 'miles de €')
    
    hola <- tm_shape(pruebabuena11,
                     name = paste('Mapa de', input$select_variable11),
                     simplify = 1,
                     line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu", midpoint = 0, 
                  title = paste(names(which(variables == input$select_variable11)),
                                if (input$select_variable11 == 'precio') '(€/kg)'
                                else if (input$select_variable11 == 'volumen') '(miles de kg)'
                                else '(miles de €)'),
                  popup.vars=c("Variable ="="variable", "Valor ="="valor_variable"), id = 'Texto') +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(0,40.4169019,5))+
      tm_layout(legend.format = list(text.separator = 'a'))
    
    tmap_leaflet(hola) %>% addTiles(attribution = 'Fuente: Ministerio de Agricultura, Pesca y Alimentación')
  })
  
  
  #EVOLUCION 11  
  output$grafico_evolucion <- renderPlot({
    
    df_evolucion <- filter(consumo, ccaa == input$select_ccaa11 & 
                             producto == input$select_producto11)
    df_evolucion_pred <- consumo_pred %>% 
      filter(ccaa == input$select_ccaa11 &
               producto == input$select_producto11)
    
    p <- ggplot() +
      geom_line(data = df_evolucion, aes_string('date', input$select_variable11,
                                                color = "'Real'"),size = 1)+
      geom_line(data = df_evolucion_pred, 
                aes_string('X', paste0(input$select_variable11, '_predicted'), 
                           color = "'Predicción'"), size = 1, linetype = 'longdash')+
      labs(y= paste(capitalize(names(which(variables == input$select_variable11))), 
                    if (input$select_variable11 == 'precio') '(€/kg)' 
                    else if (input$select_variable11 == 'volumen') '(miles de kg)'
                    else '(miles de €)'), 
           x = "Fecha",
           title = paste("Evolución del", if (input$select_variable11 == 'valor') 'ingreso total' 
                         else tolower(names(which(variables == input$select_variable11)))),
           subtitle = paste(df_evolucion$producto, 'para', df_evolucion$ccaa),
           caption = "Fuente: Ministerio de Agricultura, Pesca y Alimentación") +
      scale_x_date(date_breaks = "2 month", date_labels = "%Y-%b") +
      scale_color_manual(name = NULL, values = c('Real' = 'steelblue1', 'Predicción' = 'steelblue4'))+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank()) 
    
    rango_min <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
    rango_max<- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]
    
    if (rango_min > rango_max*0.25){
      fill_color <- brewer.pal(3, 'Blues')[2]
      p <- p + annotate(geom = 'label', x = as.Date('2018-03-01'), 
                        y = rango_min - rango_min*0.05, label = 'No empieza en 0',
                        fill = fill_color)
    }
    p
  })
  
  
  # MAPA 12
  output$map12_1819 <- renderLeaflet({
    # 1819
    data_spain12_1819 <- temp1819
    data_spain12_1819 <-data_spain12_1819 %>% group_by(ccaa, producto) %>% 
      summarise(mean=mean(.data[[input$select_variable12]],na.rm = TRUE))
    
    data_spain12_1819 <-as_tibble(data_spain12_1819)
    map_data_spain12_1819 <- municipalities_spain %>% left_join(data_spain12_1819, by = c("Texto"="ccaa")) 
    map_data_spain12_1819 <- filter(map_data_spain12_1819, producto == input$select_producto12)
    max_1819 <- max(map_data_spain12_1819['mean'])
    min_1819 <- min(map_data_spain12_1819['mean'])
    
    # 2020
    data_spain12_2020 <- temp2020
    data_spain12_2020 <-data_spain12_2020 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable12]],na.rm = TRUE))
    data_spain12_2020 <-as_tibble(data_spain12_2020)
    map_data_spain12_2020 <- municipalities_spain %>% left_join(data_spain12_2020, by = c("Texto"="ccaa")) 
    map_data_spain12_2020 <- filter(map_data_spain12_2020, producto == input$select_producto12)
    max_2020 <- max(map_data_spain12_2020['mean'])
    min_2020 <- min(map_data_spain12_2020['mean'])
    
    min <- min(min_1819,min_2020)
    max <- max(max_1819,max_2020)
    
    vector = c(min,max)
    vector = quantile(vector)
    vector = as.vector(vector)
    
    map_data_spain12_1819 <- map_data_spain12_1819 %>% group_by(Codigo, Texto, producto) %>% summarise(mean=mean(mean))
    pruebabuena12_1819<-st_sf(map_data_spain12_1819 , st_geometry(autonomias3))
    pruebabuena12_1819$variable <- names(which(variables == input$select_variable12))
    pruebabuena12_1819$valor_variable <- paste(as.character(round(pruebabuena12_1819$mean, 2)), 
                                               if (input$select_variable12 == 'precio') '€/kg'
                                               else if (input$select_variable12 == 'volumen') 'miles de kg'
                                               else 'miles de €')
    
    mapa12 <- tm_shape(pruebabuena12_1819,
                       name = paste('Mapa de', input$select_variable12),
                       simplify = 1,
                       line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu", breaks = round(vector,2), midpoint = 0,  
                  title = paste(names(which(variables == input$select_variable12)),
                                if (input$select_variable11 == 'precio') '(€/kg)'
                                else if (input$select_variable11 == 'volumen') '(miles de kg)'
                                else '(miles de €)'), 
                  popup.vars=c("Variable ="="variable", "Valor ="="valor_variable"), id = 'Texto') +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3.7056721,39.4169019,5))+
      tm_layout(legend.format = list(text.separator = 'a'))
    
    tmap_leaflet(mapa12) %>% addTiles(attribution = 'Fuente: Ministerio de Agricultura, Pesca y Alimentación')
  })
  
  # MAPA 12
  output$map12_2020 <- renderLeaflet({
    
    # 1819
    data_spain12_1819 <- temp1819
    data_spain12_1819 <-data_spain12_1819 %>% group_by(ccaa, producto) %>% summarise(mean=mean(.data[[input$select_variable12]],na.rm = TRUE))
    data_spain12_1819 <-as_tibble(data_spain12_1819)
    map_data_spain12_1819 <- municipalities_spain %>% left_join(data_spain12_1819, by = c("Texto"="ccaa")) 
    map_data_spain12_1819 <- filter(map_data_spain12_1819, producto == input$select_producto12)
    max_1819 <- max(map_data_spain12_1819['mean'])
    min_1819 <- min(map_data_spain12_1819['mean'])
    
    # 2020
    data_spain12_2020 <- temp2020
    data_spain12_2020 <-data_spain12_2020 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable12]],na.rm = TRUE))
    data_spain12_2020 <-as_tibble(data_spain12_2020)
    map_data_spain12_2020 <- municipalities_spain %>% left_join(data_spain12_2020, by = c("Texto"="ccaa")) 
    map_data_spain12_2020 <- filter(map_data_spain12_2020, producto == input$select_producto12)
    max_2020 <- max(map_data_spain12_2020['mean'])
    min_2020 <- min(map_data_spain12_2020['mean'])
    
    min <- min(min_1819,min_2020)
    max <- max(max_1819,max_2020)
    
    vector = c(min,max)
    vector = quantile(vector)
    vector = as.vector(vector)
    
    map_data_spain12_2020 <- map_data_spain12_2020 %>% group_by(Codigo, Texto, producto) %>% summarise(mean=mean(mean))
    pruebabuena12_2020<-st_sf(map_data_spain12_2020 , st_geometry(autonomias3))
    pruebabuena12_2020$variable <- names(which(variables == input$select_variable12))
    pruebabuena12_2020$valor_variable <- paste(as.character(round(pruebabuena12_2020$mean, 2)), 
                                               if (input$select_variable12 == 'precio') '€/kg'
                                               else if (input$select_variable12 == 'volumen') 'miles de kg'
                                               else 'miles de €')
    mapa12_1 <- tm_shape(pruebabuena12_2020,
                         name = paste('Mapa de', input$select_variable12),
                         simplify = 1,
                         line.center = "midpoint") +
      tm_polygons(col = "mean", breaks = round(vector,2), legend.show = TRUE,  palette = "RdBu", 
                  midpoint = 0, title = paste(names(which(variables == input$select_variable12)),
                                              if (input$select_variable11 == 'precio') '(€/kg)'
                                              else if (input$select_variable11 == 'volumen') '(miles de kg)'
                                              else '(miles de €)'), 
                  popup.vars=c("Variable ="="variable", "Valor ="="valor_variable"), id = 'Texto') +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3.7056721,39.4169019,5))+
      tm_layout(legend.format = list(text.separator = 'a'))
    
    tmap_leaflet(mapa12_1) %>% addTiles(attribution = 'Fuente: Ministerio de Agricultura, Pesca y Alimentación')
    
  })
  
  #Importaciones exportaciones 13
  output$impoexpo13 <- renderPlot({
    
    data13 <- data_balance %>% filter(pais==input$select_pais13)
    
    ggplot(data13, aes_string(x='fecha', y=input$select_variable13, fill='Tipo'))+
      geom_bar(position=position_dodge(), stat="identity")+
      scale_x_date(breaks="3 month", date_labels = "%Y-%b")+
      labs(x = 'Fecha', y = paste(names(which(variables == input$select_variable13)),
                                  if (input$select_variable13 == 'volumen') '(miles de kg)'
                                  else '(miles de €)')) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust=1),
            panel.grid.minor = element_blank())
  })
  # AGROANÁLISIS
  #Tablas top productos
  top_2020 <- reactive({
    consumo_top %>% filter(year == 2020) %>% 
      select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
      summarise(valor = mean(valor)) %>% filter(ccaa == input$select_ccaa22) %>% 
      arrange(desc(valor)) %>%  head(input$select_topp22) %>% spread(producto, valor)
  })
  
  top_2019_20 <- reactive({
    consumo_top %>% filter(year != 2020) %>% 
      select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
      summarise(valor = mean(valor)) %>% filter(ccaa == input$select_ccaa22) %>% 
      arrange(desc(valor))%>%filter(producto %in% colnames(top_2020()))%>% spread(producto, valor)
  })
  
  table_spider_1 <- reactive({
    as.data.frame(rbind(max(top_2020()[,-1], top_2019_20()[,-1], top_2019()[,-1], top_2020_19()[,-1]), 
                        rep(0,times = input$select_topp22), top_2020()[,-1], top_2019_20()[,-1]))
  })
  
  top_2019 <- reactive({
    consumo_top %>% filter(year != 2020) %>% 
      select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
      summarise(valor = mean(valor)) %>% filter(ccaa == input$select_ccaa22) %>% 
      arrange(desc(valor)) %>%  head(input$select_topp22) %>% spread(producto, valor)
  })
  
  top_2020_19 <- reactive({
    consumo_top %>% filter(year == 2020) %>% 
      select(year, producto, ccaa, valor ) %>% group_by(producto,ccaa) %>%
      summarise(valor = mean(valor)) %>% filter(ccaa == input$select_ccaa22) %>% 
      arrange(desc(valor))%>%filter(producto %in% colnames(top_2019()))%>% spread(producto, valor)
  })
  
  table_spider_2 <- reactive({(
    as.data.frame(rbind(max(top_2020()[,-1], top_2019_20()[,-1], top_2019()[,-1], top_2020_19()[,-1]),
                        rep(0,times = 4), top_2020_19()[,-1], top_2019()[,-1])))
  })
  
  # Top productos 
  output$top_productos <- renderPlot({
    table_spider_2 <- table_spider_2()
    table_spider_1 <- table_spider_1()
    rownames(table_spider_1) <- c('Máximo', 'Mínimo', 'Periodo de pandemia', 'Periodo de normalidad')
    rownames(table_spider_2) <- c('Máximo', 'Mínimo', 'Periodo de pandemia', 'Periodo de normalidad')
    names1 <- names(table_spider_1)
    names2 <- names(table_spider_2)
    coincident_columns <- intersect(names1, names2)
    not_coincident_columns <- setdiff(names1, names2)
    
    table_spider_1 <- table_spider_1 %>% 
      select(all_of(coincident_columns), everything())
    
    table_spider_2 <- table_spider_2 %>% 
      select(all_of(coincident_columns), everything())
    colors_border <- rgb(0.2,0.5,0.5,0.9)
    colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
    
    par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
    b <- radarchart(table_spider_2, pcol=colors_border , pfcol=colors_in, 
                    plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                    caxislabels = seq(0, round(max(table_spider_2), 0), 
                                      round(max(table_spider_2)/8, 0)),
                    vlcex = 0.8, title = 'En época de normalidad', seg = 7)
    b
    legend(x='bottomleft', legend = rownames(table_spider_2[-c(1,2),]), bty = "n", pch=20 , 
           col=colors_in , text.col = "azure4", cex=0.9, pt.cex=2)
    
    a1<- radarchart(table_spider_1, pcol=colors_border , pfcol= colors_in,
                    plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                    caxislabels=seq(0, round(max(table_spider_1), 0), 
                                    round(max(table_spider_1)/8, 0)),
                    vlcex=0.8, title = 'En pandemia', seg = 7)
    a1
    legend(x='bottomleft', legend = rownames(table_spider_1[-c(1,2),]), bty = "n", pch=20 , 
           col=colors_in , text.col = "azure4", cex=0.9, pt.cex=2)
    mtext(paste('Top productos en', input$select_ccaa22), outer = T, cex = 1.5)
    
  })
  
  #Caja diferencia_top22
  output$diferencia_top22 <- renderValueBox({
    table_spider_1 <- table_spider_1()
    table_spider_2 <- table_spider_2()
    rownames(table_spider_1) <- c('Máximo', 'Mínimo', 'Pandemia', 'Normalidad')
    rownames(table_spider_2) <- c('Máximo', 'Mínimo', 'Pandemia', 'Normalidad')
    
    names1 <- names(table_spider_1)
    names2 <- names(table_spider_2)
    coincident_columns <- intersect(names1, names2)
    not_coincident_columns <- setdiff(names1, names2)
    
    incremento <- ((table_spider_2['Pandemia', coincident_columns] - 
                      table_spider_2['Normalidad', coincident_columns])/
                     table_spider_2['Normalidad', coincident_columns])*100
    incremento <- round(rowMeans(incremento), 2)
    
    if (incremento >= 0){
      valueBox('Incremento',
               paste('De los productos más populares que coinciden 
                     en ambos periodos', if (length(not_coincident_columns != 0)) paste0('(', paste(coincident_columns, collapse = ', '), ')') 
                     else '(todos)', 'ha habido un incremento del', incremento, '% en las ventas.'), color = 'aqua')
    } else {
      valueBox('Decremento',
               paste('De los productos más populares que coinciden 
                     en ambos periodos', if (length(not_coincident_columns != 0)) paste0('(', paste(coincident_columns, collapse = ', '), ')') 
                     else '(todos)', 'ha habido un decremento del', abs(incremento), '% en las ventas.'), color = 'red')
    }
  })
  
  # Caja diferencia_producto_top22
  output$diferencia_producto_top22 <- renderValueBox({
    names1 <- names(table_spider_1())
    names2 <- names(table_spider_2())
    not_coincident_columns1 <- setdiff(names1, names2)
    not_coincident_columns2 <- setdiff(names2, names1)
    
    if (length(not_coincident_columns1 != 0)) {
      valueBox('Cambios', paste('El producto', paste(tolower(not_coincident_columns2), collapse = ', '), 
                                'ha dejado de estar en el top', input$select_topp22, 'y ha entrado el producto', 
                                paste(tolower(not_coincident_columns1), collapse = ', '))) 
    } else {
      valueBox(tags$div('No ha habido', tags$br(), 'cambios'), subtitle = NULL)
    }
  })
  
  # EVOLUCION + BARRAS 213 ECONOMICO
  output$barraslineas <- renderPlot({             
    
    df213<- filter(consumo, ccaa == input$select_ccaa213 & 
                     producto == input$select_producto213)
    
    ggplot(data = df213, 
           mapping = aes(x = date)) + 
      geom_bar(aes(y = consumo_per_capita, fill = "Consumo"), stat = 'identity') +
      geom_line(aes(y = gasto_per_capita, color = "Gasto"), size = 1) +
      
      labs(x = "Fecha", caption = 'Fuente: Ministerio de Agricultura, Pesca y Alimentación') + 
      scale_y_continuous("Consumo per cápita (kg)", 
                         sec.axis = sec_axis(~.*1, 
                                             name = 'Gasto por capita (euro/kg)')) +
      
      scale_x_date(date_breaks = '4 month', date_labels = '%Y-%b')+
      scale_fill_manual(name = "", values = c("Consumo" = "steelblue1")) +
      scale_color_manual(name = "", values = c("Gasto" = "steelblue4")) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.grid.minor = element_blank())
    
  })
  
  
  # Comparacion de precios 212 
  output$evolucomp212 <- renderPlot({
    
    don<- filter(consumo, ccaa %in% input$select_ccaa212 & 
                   producto %in% input$select_producto212
    )
    
    ggplot(don, aes_string(x='date', y=input$select_variable212, color='producto', linetype='ccaa')) +
      geom_line(size = 0.8) +
      labs(y= paste(capitalize(names(which(variables == input$select_variable212))), 
                    if (input$select_variable212 == 'precio') '(€/kg)' 
                    else if (input$select_variable212 == 'volumen') '(miles de kg)'
                    else if (input$select_variable212 == 'valor') '(miles de €)' 
                    else '(kg)'), 
           x = "Fecha",
           title = capitalize(names(which(variables == input$select_variable212))),
           caption = "Fuente: Ministerio de Agricultura, Pesca y Alimentación",
           color = 'Producto (color)', linetype = 'CCAA (tipo de línea)') +
      scale_x_date(date_breaks = "2 month", date_labels = "%Y-%b") +
      expand_limits(y= 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank()) 
  })
  
  
  
  # MAPA 211: caracteristicas
  output$mapa211 <- renderLeaflet({
    data_spain211 <- filter(data_spain, date >= input$select_date211[1] & date <= input$select_date211[2])
    data_spain211 <-data_spain211 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable211]],na.rm = TRUE))
    data_spain211 <-as_tibble(data_spain211)
    map_data_spain211 <- municipalities_spain %>% left_join(data_spain211, by = c("Texto"="ccaa")) 
    
    map_data_spain211 <- filter(map_data_spain211, producto == input$select_producto211)
    map_data_spain211 <- map_data_spain211 %>% group_by(Codigo, Texto, producto) %>% summarise(mean=mean(mean))
    pruebabuena211<-st_sf(map_data_spain211, st_geometry(autonomias3))
    pruebabuena211$variable <- names(which(variables == input$select_variable211))
    pruebabuena211$valor_variable <- paste(as.character(round(pruebabuena211$mean, 2)), 
                                           if (input$select_variable211 == 'precio') '€/kg'
                                           else 'kg mensuales')
    
    mapa211 <- tm_shape(pruebabuena211,
                        name = paste('Mapa de', input$select_variable211),
                        simplify = 1,
                        line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu", midpoint = 0, 
                  title = paste(names(which(variables == input$select_variable12)),
                                if (input$select_variable11 == 'precio') '(€/kg)'
                                else if (input$select_variable11 == 'volumen') '(miles de kg)'
                                else '(miles de €)'),
                  id = 'Texto', popup.vars=c("Variable ="="variable", "Valor ="="valor_variable")) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-2,40,5)) +
      tm_layout(legend.format = list(text.separator = 'a'))
    tmap_leaflet(mapa211) %>% addTiles(attribution = 'Fuente: Ministerio de Agricultura, Pesca y Alimentación')
  })
  
  # BOX 211: caracteristicas
  output$precio211 <- renderValueBox({
    
    data_spain211 <- filter(data_spain, date >= input$select_date211[1] & date <= input$select_date211[2])
    data_spain211 <-data_spain211 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable211]],na.rm = TRUE))
    maximo <- data_spain211 %>% filter(producto == input$select_producto211) %>% arrange(desc(mean)) %>%  head(1)
    minimo <- data_spain211 %>% filter(producto == input$select_producto211) %>% arrange(mean) %>%  head(1)
    
    valueBox(input$select_producto211, tags$div('· El', if (input$select_variable211 == 'valor') 'ingreso total' 
                                                else tolower(names(which(variables == input$select_variable211))), 
                                                'más alto se ha registrado en', str_to_title(maximo$ccaa), 'siendo este' , 
                                                paste0(round(maximo$mean,2), ' ',
                                                       if (input$select_variable211 == 'precio') '€/kg' 
                                                       else 'kg mensuales', '.'), tags$br(), 
                                                '· El ', if (input$select_variable211 == 'valor') 'ingreso total' 
                                                else tolower(names(which(variables == input$select_variable211))), 
                                                'más bajo se ha registrado en', str_to_title(minimo$ccaa), 'siendo este', 
                                                paste0(round(minimo$mean,2), ' ',
                                                       if (input$select_variable211 == 'precio') '€/kg' 
                                                       else 'kg mensuales', '.')), "Progress", icon = NULL, color = "navy")
  })
  
  
  # box 211 media nacional
  
  output$nacional211 <- renderValueBox({
    
    media_nacional <- filter(consumo, date >= input$select_date211[1] & date <= input$select_date211[2], ccaa == 'Nacional', producto == input$select_producto211)
    media_nacional <- media_nacional  %>% summarise(mean=mean(.data[[input$select_variable211]],na.rm = TRUE))
    
    valueBox(tags$div(if(input$select_variable211 == 'precio') paste(round(media_nacional,2),'€/kg')
                      else paste(round(media_nacional,2),'kg mensuales')),
             paste('Es la media nacional del', tolower(names(which(variables == input$select_variable211))),
                   'para el producto', tolower(input$select_producto211)), "Progress", icon = NULL, color = "aqua")
  })
  
  
  # BOX PROGRESO 12
  output$progressBox <- renderValueBox({
    
    dfinfo <- filter(temp_comp, producto == input$select_producto12,ccaa == input$select_ccaa12)
    dfinfo <- dfinfo %>% select(.data[[input$select_variable12]])
    
    if(dfinfo >= 0){
      valueBox('Impacto',
               paste('El', input$select_variable12, 'de', input$select_producto12 ,'se ha visto aumentado en un', round(dfinfo,2), "%", 'en',input$select_ccaa12), "Progress", icon = NULL,
               color = "light-blue")
    } else {
      valueBox('Progreso',
               paste('El', input$select_variable12, 'de', input$select_producto12 ,'ha disminuido en un', abs(round(dfinfo,2)), '% en',input$select_ccaa12), "Progress", icon = NULL,
               color = "red")
    }
    
  })
  
  # BOX TEMPORADA
  output$boxtemp <- renderValueBox({
    
    boxtemp <- filter(consumo, producto == input$select_producto12) %>% 
      select(Temporada) %>% head(1)
    
    valueBox(value = NULL, paste('La temporada para', input$select_producto12, 'es:', boxtemp), "Progress", icon = NULL,
             color = "aqua")
  })
  
  # SALUD 
  
  # Comparacion de PER CAPITA 31 
  output$comp31 <- renderPlot({
    
    don<- filter(df_salud, Producto %in% input$select_producto31)

    p <- ggplot(don, aes_string(x='Fecha', y = input$select_variable31, color='Producto')) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = ymd('2020-03-01'), linetype = 'dashed') + 
      scale_x_date(date_breaks = "2 month", date_labels = "%Y-%b") +
      labs(y= names(which(variables31 == input$select_variable31)),
           x = "Fecha",
           title = paste('Evolución del', if (input$select_variable31 == "CONSUMO.X.CAPITA") 
             tolower(names(which(variables31 == input$select_variable31))) else 'ingreso total',
                         'para diferentes familias de productos'),
           caption = "Fuente: Ministerio de Agricultura, Pesca y Alimentación",
           color = 'Familia de productos') +
      expand_limits(y= 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank())
    
    rango_max<- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]
    p <- p + annotate(geom = 'label', x = ymd('2019-11-25'), y = rango_max - rango_max*0.05, label = 'Inicio de la pandemia')
    p
  })
  
  
  # salud box frutas y hort.
  output$box31f <- renderValueBox({
    
    boxtemp <- round(filter(box_salud, Producto == 'Frutas y Hortalizas') %>% 
                       select(input$select_variable31), 2)
    
    valueBox(value = names(which(variables31 == input$select_variable31)), 
             if (input$select_variable31 == "CONSUMO.X.CAPITA") paste('El', tolower(names(which(variables31 == input$select_variable31))),
                                                                      'de las frutas y hortalizas ha aumentado un', boxtemp, '% durante
                                                             el periodo de pandemia (marzo de 2020 en adelante') 
             else paste('Los', tolower(names(which(variables31 == input$select_variable31))), 'derivados de las frutas y hortalizas han
               aumentado un', boxtemp, '% durante el periodo de pandemia (marzo de 2020 en adelante)'), 
             "Progress", icon = NULL, color = "aqua")
  })
  
  
  # salud Box
  
  output$box31 <- renderValueBox({
    #boxtemp <- filter(box_salud, Producto != 'Frutas y Hortalizas')
    
    boxtemp <- filter(box_salud, Producto %in% input$select_producto31) %>% 
      select(Producto, input$select_variable31) 
    
    #variables31 <- c('Consumo per cápita' = "CONSUMO.X.CAPITA" , 'Ingresos totales' = "VALOR..Miles.Euros." ) 
    b <- c()
    dim <- 1
    patatas <- input$select_producto31
    
    for(k in 1:dim(boxtemp)[1]){
      valor <- filter(boxtemp, Producto == patatas[k])
      if(valor[,2] >= 0){
        b[dim] <- paste('· El', if (input$select_variable31 == 'CONSUMO.X.CAPITA') paste(tolower(names(which(variables31 == input$select_variable31))), 'de')
                        else 'ingreso total derivado de', tolower(patatas[dim]), 'ha aumentado en un', round(valor[,2], 2), 
                        '% durante el periodo de pandemia (marzo de 2020 en adelante).')
        dim <- dim + 1
      } else {
        b[dim] <- paste('· El', if (input$select_variable31 == 'CONSUMO.X.CAPITA') paste(tolower(names(which(variables31 == input$select_variable31))), 'de')
                        else 'ingreso total derivado de', tolower(patatas[dim]), 'ha disminuido en un', round(abs(valor[, 2]), 2), 
                        '% durante el periodo de pandemia (marzo de 2020 en adelante).')
        dim <- dim + 1
      }
    }
    
    b <- b[-1]
    if (length(b)==0) {
      b2 <- 'Seleccione otro producto'
    } else {
      b2 <- paste(b, collapse = "\n")
    }
    
    valueBox(value = NULL, b2, "Progress", icon = NULL, color = "red")
    
  })
  
}


shinyApp(ui, server)


#ui <- fluidPage(
#  airMonthpickerInput(
#    inputId = "multiple",
#    label = "Select multiple months:",
#    placeholder = "You can pick 5 months",
#    multiple = T, clearButton = TRUE
#  ),
#  verbatimTextOutput("res")
#)
#
#server <- function(input, output, session) {
#  output$res <- renderPrint(input$multiple)
#}
#
#shinyApp(ui, server)

#shinyWidgets::shinyWidgetsGallery()
#shinyWidgets::demoAirDatepicker("months")





