dir_root <- 'D:/datathon_final/'
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
library(shinydashboardPlus)
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
library(chron)
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
consumo_pred[consumo_pred$ccaa == 'Cataluna', 'ccaa'] <- 'Cataluna'

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
consumo$date  <- strptime(as.character(consumo$date), "%d/%m/%Y")
consumo$date <- as.Date(consumo$date )
consumo_pred$X  <- strptime(as.character(consumo_pred$X), "%d/%m/%Y")
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


consumo_resta <- consumo_resta %>% 
  filter(ccaa != 'Nacional')
#Importaciones exportaciones
data_total$Fecha <- as.Date(data_total$Fecha)

consumo_resta_dos_epocas <- consumo_resta %>% group_by(producto, ccaa) %>% summarise(volumen_resta = mean(volumen_resta),
                                                                                 valor_resta = mean(valor_resta),
                                                                                 precio_resta = mean(precio_resta))
consumo_resta_dos_epocas <- as.data.frame(consumo_resta_dos_epocas)
consumo_resta <- as.data.frame(consumo_resta)
consumo_resta_dos_epocas$Olas <- 'Epoca completa'
consumo_resta <- consumo_resta %>% select(-date)
consumo_resta_dos_epocas <- consumo_resta_dos_epocas[c(2,1,3,4,5,6)]


consumo_resta <- rbind(consumo_resta, consumo_resta_dos_epocas)



# TEMPORADA ---------------------------------------------------------------


library(lubridate)
temp <- consumo %>% dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date))
temp <- temp %>% filter(ccaa != 'Nacional')
temp

temp1819 <- temp %>% filter(year == 2018 | year == 2019) %>%  
  filter(month < Fecha_inicio) %>%  
  filter(month > Fecha_fin) %>% group_by(ccaa, producto) %>% 
  summarize (volumen = mean(volumen),valor = mean(valor), precio = mean(precio))

temp2020 <- temp %>% filter(year == 2020) %>%  
  filter(month < Fecha_inicio) %>%  
  filter(month > Fecha_fin) %>% 
  group_by(ccaa, producto) %>% 
  summarize (volumen = mean(volumen),valor = mean(valor), precio = mean(precio))

temp_comp <- inner_join(temp1819,temp2020, by = c("ccaa", "producto"),suffix = c("_1819", "_2020"))

temp_comp$volumen = ((temp_comp$volumen_2020 - temp_comp$volumen_1819) / temp_comp$volumen_1819)*100
temp_comp$valor = ((temp_comp$valor_2020 - temp_comp$valor_1819) / temp_comp$valor_1819)*100
temp_comp$precio = ((temp_comp$precio_2020 - temp_comp$precio_1819) / temp_comp$precio_1819)*100

temp_comp <- as.data.frame(temp_comp)
productos_en_comun <- unique(temp_comp$producto)

temp1819 <- temp1819 %>% filter(producto %in% productos_en_comun)
temp2020 <- temp2020 %>% filter(producto %in% productos_en_comun)

# MAPAS ----------------------------------------------------------

library(tmap)
library(sf)
library(dplyr)
library(stringi)

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
municipalities_spain$Texto[municipalities_spain$Texto== "Castilla - La Mancha"] <- 'Castilla la mancha'
municipalities_spain$Texto[municipalities_spain$Texto== "Castilla Y Leon"] <- 'Castilla leon'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad De Madrid"] <- 'Madrid'
municipalities_spain$Texto[municipalities_spain$Texto== "Region De Murcia"] <- 'Murcia'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad Foral De Navarra"] <- 'Navarra'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad Valenciana"] <- 'Valencia'
municipalities_spain$Texto[municipalities_spain$Texto== "Cataluna"] <- 'Cataluna'
municipalities_spain$Texto[municipalities_spain$Texto== "La Rioja"] <- 'La rioja'
municipalities_spain$Texto[municipalities_spain$Texto== "Pais Vasco"] <- 'Pais vasco'

data_spain<-data_spain[!data_spain$ccaa=="Nacional",]
municipalities_spain<-municipalities_spain[!municipalities_spain$Texto=="Ceuta",]
municipalities_spain<-municipalities_spain[!municipalities_spain$Texto=="Melilla",]


autonomias2<-data.frame(autonomias)
autonomias2<-autonomias2[!autonomias2$Texto=="Ceuta",]
autonomias2<-autonomias2[!autonomias2$Texto=="Melilla",]
autonomias2$Texto <- str_to_title(stri_trans_general(autonomias2$Texto,"Latin-ASCII"))

autonomias2$Texto[autonomias2$Texto== "Principado De Asturias"  ] <- 'Asturias'
autonomias2$Texto[autonomias2$Texto==  "Islas Baleares"] <- 'Baleares'
autonomias2$Texto[autonomias2$Texto== "Castilla - La Mancha"] <- 'Castilla la mancha'
autonomias2$Texto[autonomias2$Texto== "Castilla Y Leon"] <- 'Castilla leon'
autonomias2$Texto[autonomias2$Texto== "Comunidad De Madrid"] <- 'Madrid'
autonomias2$Texto[autonomias2$Texto== "Region De Murcia"] <- 'Murcia'
autonomias2$Texto[autonomias2$Texto== "Comunidad Foral De Navarra"] <- 'Navarra'
autonomias2$Texto[autonomias2$Texto== "Comunidad Valenciana"] <- 'Valencia'
autonomias2$Texto[autonomias2$Texto== "La Rioja"] <- 'La rioja'

autonomias3<-as_tibble(autonomias2)
autonomias3<-st_sf(autonomias3)


# FILTROS -----------------------------------------------------------------
colnames(consumo)


medidas <- c("volumen" , "valor" , "precio")
productos <- unique(consumo$producto)
productos <- productos[productos!= 'Frutas frescas']
epoca11 <- unique(consumo_resta$Olas)
ccaa <- unique(consumo$ccaa)
data_spain11 <- consumo_resta %>% 
  group_by(ccaa, producto, Olas) %>% 
  summarise(mean=mean(.data[[paste0('volumen', '_resta')]], na.rm = TRUE))


# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  
  
  # SIDEBAR  
  sidebar = dashboardSidebar(
    
    #itemns
    sidebarMenu(
      menuItem(text = "Covid", tabName = "tema1", icon = icon('bar-chart-o')), 
      menuItem(text = "Agroanalisis",  tabName = "tema2",icon = icon('info-circle'))
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
                                
                                column(3, 
                                       radioGroupButtons('select_variable11', label = 'Medida', choices= medidas, status = "primary", selected = medidas[3]),
                                       bsTooltip("select_variable11", "Datos en miles de kg",
                                                 "bottom", options = list(container = "body"))
                                ),
                               
                                column(2,
                                       pickerInput('select_epoca11', label = 'Epoca', choices= epoca11, selected = epoca11[1])
                                       
                                ),
                                column(2,
                                       pickerInput('select_ccaa11', label = 'CCAA', choices= ccaa, selected = ccaa[2])
                                       
                                ),
                                column(3, 
                                       pickerInput('select_producto11', label ='Producto', choices= productos , selected= productos[4])
                                )
                              ),
                                
                                fluidRow(
                                  column(5,
                                         h4(paste('¿Impacto  negativo o positivo?'))
                                  ),
                                  column(7,
                                          box(
                                          title = "Cuantificando el impacto del virus", width = 7 , solidHeader = TRUE,background = "light-blue",
                                          "¿Y si predecimos lo ocurrido de no haber existido la pandemia?")
                                  )
   
                              ),
                                
                                
                              fluidRow(
                                box(tmapOutput(outputId = "my_tmap", height = 250),width = 5,solidHeader = TRUE),
                                
                                box(
                                  plotOutput(outputId = "grafico_evolucion", height = 250),width = 7,solidHeader = TRUE)
                                
                              )
                              
                              
                              
                     ),
                     
                     
                     tabPanel("Temporada",
                              
                              fluidRow(
                                column(4, 
                                       radioGroupButtons('select_variable12', label = 'Medida', choices= medidas, status = "primary", selected = medidas[3]),
                                       bsTooltip("select_variable12", "Datos en miles de kg",
                                                 "bottom", options = list(container = "body"))
                                ),
                                column(2,
                                       pickerInput('select_ccaa12', label = 'CCAA', choices= ccaa, selected = ccaa[2])
                                       
                                ),
                                column(3, 
                                       pickerInput('select_producto12', label ='Producto', choices= productos_en_comun , selected= productos[4])
                                ),
                                box(
                                  title = "impacto en las temporadas", width = 3, solidHeader = TRUE,background = "light-blue",
                                  "¿Y si visualizamos los datos de la temporada para cada producto?")
                                
                              ),
                              
                              fluidRow(
                                box(
                                  title = "Antes del virus (2018-2019):", 
                                  tmapOutput(outputId = "map12_1819", height = 250),width = 4,solidHeader = TRUE),
                                box(
                                  title = "El virus ha supuesto cambios:", 
                                  tmapOutput(outputId = "map12_2020", height = 250),width = 4,solidHeader = TRUE),
                                valueBoxOutput('boxtemp'),
                                valueBoxOutput('progressBox')
                              )
                              
                              
                     ),
                     
                     
                     tabPanel("Importaciones y exportaciones",
                              
                              fluidRow(
                                
                                box(plotOutput('impoexpo13'), height = 5,  width = 8,solidHeader = TRUE)
                                
                              )
                              
                     )
                     
              )
              
      ),
      
      #AGROANALISIS
      tabItem(tabName = "tema2",
              tabBox(width = 15, height = 2,
                     tabPanel("Analisis general",
                              tabsetPanel(
                                tabPanel('Productos',
                                         fluidRow(
                                           column(4, 
                                                  radioGroupButtons('select_variable211', label = 'Medida', choices= medidas, status = "primary", selected = medidas[3]),
                                                  bsTooltip("select_variable211", "Datos en miles de kg",
                                                            "bottom", options = list(container = "body"))
                                           ),
                                           column(3, 
                                                  pickerInput('select_producto211', label ='Producto', choices= productos , selected= productos[4])
                                           ),
                                           column(5,
                                                  dateRangeInput('select_date211', 'Rango de fecha', start = min(consumo$date, na.rm = TRUE), end = max(consumo$date, na.rm = TRUE), min = min(consumo$date, na.rm = TRUE),
                                                                 max = max(consumo$date, na.rm = TRUE), startview = "month", weekstart = 0,
                                                                 language = "en", separator = " to ", width = NULL)
                                           ),
                                      
                                           box(
                                             title = "¿Donde triunfa cada producto?", width = 3, solidHeader = TRUE,background = "light-blue",
                                             "Analicemos como varian las caracteristicas de los productos en cada ccaa"
                                           )
                                        ),
                                         
                                         tmapOutput(outputId = "mapa211", height = 250)
                                         
                                ),
                                
                                
                                tabPanel('Comparacion productos',
                                         fluidRow(
                                           column(4, 
                                                  radioGroupButtons('select_variable212', label = 'Medida', choices= medidas, status = "primary", selected = medidas[2]),
                                                  bsTooltip("select_variable212", "Datos en miles de kg",
                                                            "bottom", options = list(container = "body"))
                                           ),
                                           column(2,
                                                  pickerInput('select_ccaa212', label = 'CCAA', choices= ccaa, selected = ccaa[2])
                                                  
                                           ),
                                           
                                           column(3,
                                                  pickerInput('select_producto212', label ='Producto', choices= productos , selected= c(productos[1],productos[2],productos[3],productos[4]), multiple = TRUE, options = 
                                                                pickerOptions(
                                                                  actionsBox = TRUE)),
                                                  bsTooltip("select_producto212", "Seleccione varios productos para compararlos",
                                                            "bottom", options = list(container = "body"), trigger = 'focus')
                                                  
                                           ),
                                           
                                           box(
                                             title = "Comparemos los productos entre si", width = 3, solidHeader = TRUE,background = "light-blue",
                                             "Selecciona los productos y la CCAA")
                                           
                                         ),
                                         fluidRow(
                                           plotOutput(outputId = 'evolucomp212', height = 250)
                                           
                                         )
                                         
                                         
                                ),
                                tabPanel('Economico',
                                         fluidRow(
                                           column(2, 
                                                  pickerInput('select_ccaa213', label = 'CCAA', choices= ccaa, selected = ccaa[2])
                                           ),
                                           column(3,
                                                  pickerInput('select_producto213', label ='Producto', choices= productos , selected= productos[4])
                                           ),
                                           column(4, h1(' ')),
                                           box(
                                             title = "Indicadores economicos", width = 5, solidHeader = TRUE,background = "light-blue",
                                             "Analicemos la relacion entre el consumo y gasto per capita. Selecciona el producto y la ccaa a estudiar")
                                           
                                         ),
                                         fluidRow(
                                           box(plotOutput(outputId = "barraslineas", height = 250),width = 12,solidHeader = TRUE)
                                           
                                         )
                                         
                                )
                                
                              )
                              
                     ),
                     
                     
                     tabPanel("Top productos"
                              
                     )
                     
                     
              )
              
      )
      
    ) #tabitems close
    
  ),
  
  
  
  
  ## HEADER
  
  header = dashboardHeader(
    title = tagList(
      span(class = 'logo-lg','UniversityHack 2021'),
      img(src = 'https://image.shutterstock.com/image-vector/letter-m-logo-mm-initials-260nw-295908632.jpg')),
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
  
  # MAPA 11
  output$my_tmap <- renderTmap({
    data_spain11 <- consumo_resta %>% 
      filter(Olas == input$select_epoca11) %>% 
      group_by(ccaa, producto) %>% 
      summarise(mean=mean(.data[[paste0(input$select_variable11, '_resta')]], na.rm = TRUE))
    
    data_spain11<-as_tibble(data_spain11)
    
    map_data_spain11 <- municipalities_spain %>% 
      left_join(data_spain11, by = c("Texto"="ccaa")) %>% 
      filter(producto == input$select_producto11) %>% 
      group_by(producto, Texto) %>% 
      summarise(mean = mean(mean))
    
    pruebabuena11<-st_sf(map_data_spain11, st_geometry(autonomias3))
    
    tm_shape(pruebabuena11,
             name = paste('Mapa de', input$select_variable11),
             simplify = 1,
             line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu", midpoint = 0,title = input$select_variable11) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3,38,4))
    
    
  })
  
  
  #EVOLUCION 11  
  output$grafico_evolucion <- renderPlot({
    
    df_evolucion <- filter(consumo, ccaa == input$select_ccaa11 & 
                             producto == input$select_producto11)
    df_evolucion_pred <- consumo_pred %>% 
      filter(ccaa == input$select_ccaa11 &
               producto == input$select_producto11)
    
    ggplot() +
      geom_line(data = df_evolucion, aes_string('date', input$select_variable11,
                                                color = "'Real'"),size = 1)+
      geom_line(data = df_evolucion_pred, 
                aes_string('X', paste0(input$select_variable11, '_predicted'), 
                           color = "'Predicho'"), size = 1)+
      labs(y= input$select_variable11, 
           x = "Fecha",
           title = paste("Evolucion del", input$select_variable11),
           subtitle = paste(df_evolucion$producto, 'para', df_evolucion$ccaa),
           caption = "Data source: datathon") +
      scale_x_date(date_breaks = "2 month", date_labels = "%b%y") +
      scale_color_manual(name = 'Dato:', values = c('Real' = 'steelblue1', 'Predicho' = 'steelblue4'))+
      expand_limits(y = 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank()) 
  })
  
  
  # MAPA 12
  output$map12_1819 <- renderTmap({
    # 1819
    data_spain12_1819 <- temp1819
    data_spain12_1819 <-data_spain12_1819 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable12]],na.rm = TRUE))
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
    max <- max(max_2020,max_2020)
    
    vector = c(min,max)
    vector = quantile(vector)
    vector = as.vector(vector)
    
    
    map_data_spain12_1819 <- map_data_spain12_1819 %>% group_by(producto,Texto) %>% summarise(mean=mean(mean))
    pruebabuena12_1819<-st_sf(map_data_spain12_1819 , st_geometry(autonomias3))
    
    tm_shape(pruebabuena12_1819,
             name = paste('Mapa de', input$select_variable12),
             simplify = 1,
             line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu",breaks = round(vector,1),midpoint = 0,title = input$select_variable12) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3,38,4))
    
  })
  
  
  # MAPA 12
  output$map12_2020 <- renderTmap({
    
    # 1819
    data_spain12_1819 <- temp1819
    data_spain12_1819 <-data_spain12_1819 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable12]],na.rm = TRUE))
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
    max <- max(max_2020,max_2020)
    
    vector = c(min,max)
    vector = quantile(vector)
    vector = as.vector(vector)
    
    map_data_spain12_2020 <- map_data_spain12_2020 %>% group_by(producto,Texto) %>% summarise(mean=mean(mean))
    pruebabuena12_2020<-st_sf(map_data_spain12_2020 , st_geometry(autonomias3))
    
    tm_shape(pruebabuena12_2020,
             name = paste('Mapa de', input$select_variable12),
             simplify = 1,
             line.center = "midpoint") +
      tm_polygons(col = "mean", breaks = round(vector,2), legend.show = FALSE,  palette = "RdBu", midpoint = 0,title = input$select_variable12) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3,38,4))
    
  })
  
  
  
  
  #Importaciones exportaciones 13
  output$impoexpo13 <- renderPlot({
    
    ggplot(data = data_total, aes(x = Fecha))+
      geom_bar(stat = 'identity', aes(y = Valor, fill = Tipo))+
      geom_line(aes(y = diferencia_biderbi, color = 'Valor neto'), size = 1, group = 1)+
      scale_y_continuous(limits = c(-40000000, 20000000), labels = addUnits)+
      scale_x_date(date_breaks = '4 month', date_labels = '%Y-%b')+
      scale_fill_manual(values = c('steelblue3', 'tomato3'))+
      scale_color_manual(name = '', values = 'Black')+
      labs(y = 'Valor (kg/euro)')+
      theme_bw()+
      theme(axis.text = element_text(angle = 45, hjust = 1), 
            panel.grid.minor = element_blank()) +
      labs(y = 'Ingresos',
           title = 'Balance de las importaciones y exportaciones',
           subtitle = 'Datos para Espana, sector agroalimentario',
           caption = "Data source: datathon")
  })
  
  
  # AGROANALISIS
  
  
  # EVOLUCION + BARRAS 213
  
  output$barraslineas <- renderPlot({             
    
    df213<- filter(consumo, ccaa == input$select_ccaa213 & 
                     producto == input$select_producto213)
    
    ggplot(data = df213, 
           mapping = aes(x = date)) + 
      geom_bar(aes(y = consumo_per_capita, fill = "Consumo"), stat = 'identity') +
      geom_line(aes(y = gasto_per_capita, color = "Gasto"), size = 1) +
      
      xlab("Fecha") + 
      scale_y_continuous("Consumo per capita (kg)", 
                         sec.axis = sec_axis(~.*1, 
                                             name = 'Gasto por capita (euro/kg)')) +
      
      scale_x_date(date_breaks = '4 month', date_labels = '%Y-%b')+
      scale_fill_manual(name = "", values = c("Consumo" = "steelblue1")) +
      scale_color_manual(name = "", values = c("Gasto" = "steelblue4")) +
      theme_bw() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            panel.grid.minor = element_blank())
    
  })
  
  
  # EVOLUCION 211 comp
  
  output$evolucomp212 <- renderPlot({
    
    don<- filter(consumo, ccaa == input$select_ccaa212 & 
                   producto %in% input$select_producto212
    )
    
    ggplot(don, aes_string(x='date', y=input$select_variable212, group='producto', color='producto')) +
      geom_line() +
      geom_point() +
      labs(y= input$select_variable212, 
           x = "Fecha",
           title = input$select_variable212,
           subtitle = paste(don$producto, 'para', don$ccaa),
           caption = "Data source: datathon") +
      scale_x_date(date_breaks = "2 month", date_labels = "%b%y") +
      expand_limits(y= 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank()) 
  })
  

  # MAPA 211
  output$mapa211 <- renderTmap({
    data_spain211 <- data_spain
    data_spain211 <-data_spain211 %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable211]],na.rm = TRUE))
    data_spain211 <-as_tibble(data_spain211)
    map_data_spain211 <- municipalities_spain %>% left_join(data_spain211, by = c("Texto"="ccaa")) 
    
    map_data_spain211 <- filter(map_data_spain211, producto == input$select_producto211)
    map_data_spain211 <- map_data_spain211 %>% group_by(producto,Texto) %>% summarise(mean=mean(mean))
    pruebabuena211<-st_sf(map_data_spain211, st_geometry(autonomias3))
    
    
    tm_shape(pruebabuena211,
             name = paste('Mapa de', input$select_variable211),
             simplify = 1,
             line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu", midpoint = 0, title = input$select_variable211) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3,40,5))
    
  })
  
  
  # MAPA CALOR 211
  
  output$mapacalor211 <- renderPlot({             
    
    df211_a<- filter(consumo, producto == input$select_producto211,
                     date >= input$select_date211[1] &
                     date <= input$select_date211[2])
    
    ggplot(df211_a, aes(date, ccaa)) + geom_tile(aes(fill = precio))+
      labs( x = "Fecha", y = "Comunidad Autonoma")+
      scale_fill_continuous(name = "Precio medio (kg")+
      scale_x_date(date_breaks='2 month', date_labels="%Y-%b")+
      theme(axis.text.x=element_text(angle=45,  hjust=1))
  })
  
  
  # Box 
  output$progressBox <- renderValueBox({
    
    dfinfo <- filter(temp_comp, producto == input$select_producto12,ccaa == input$select_ccaa12)
    dfinfo <- dfinfo %>% select(.data[[input$select_variable12]])
    
    if(dfinfo >= 0){
      valueBox('Progreso',
               paste('El', input$select_variable12, 'de', input$select_producto12 ,'se ha visto aumentado en un', round(dfinfo,2), "%", 'en',input$select_ccaa12), "Progress", icon = NULL,
               color = "light-blue")
    } else {
      valueBox('Progreso',
               paste('El', input$select_variable12, 'de', input$select_producto12 ,'ha disminuido en un', abs(round(dfinfo,2)), '% en',input$select_ccaa12), "Progress", icon = NULL,
               color = "red")
    }
    
  })
  
  # temp box
  
  output$boxtemp <- renderValueBox({
    
    boxtemp <- filter(consumo, producto == input$select_producto12) %>% 
      select(Temporada) %>% head(1)
    
    valueBox(value = NULL, paste('La temporada para',input$select_producto12, 'es:', boxtemp), "Progress", icon = NULL,
             color = "navy")
  })
  
}


shinyApp(ui, server)


