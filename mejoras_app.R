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


consumo_resta <- consumo_resta %>% 
  filter(ccaa != 'Nacional', date != '2020-02-01')

#Importaciones exportaciones
data_total$Fecha <- as.Date(data_total$Fecha)
consumo_resta_dos_epocas <- consumo_resta %>% group_by(producto, ccaa) %>% summarise(volumen_resta = mean(volumen_resta),
                                                                                     valor_resta = mean(valor_resta),
                                                                                     precio_resta = mean(precio_resta))
# TEMPORADA ---------------------------------------------------------------
temp <- consumo %>% dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date), day = lubridate::day(date))

temp1819 <- temp %>% filter(year == 2018 | year == 2019) %>%  
  filter(month < Fecha_inicio) %>%  
  filter(month > Fecha_fin) %>% group_by(ccaa, producto) %>% 
  summarize (volumen = mean(volumen), valor = mean(valor), precio = mean(precio))

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
productos_en_comun <- productos_en_comun[-which(productos_en_comun %in% 
                            c('Naranjas', 'Pimientos', 'Limones', 'Judias verdes'))]

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
municipalities_spain$Texto[municipalities_spain$Texto== "Castilla - La Mancha"] <- 'Castilla la mancha'
municipalities_spain$Texto[municipalities_spain$Texto== "Castilla Y Leon"] <- 'Castilla leon'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad De Madrid"] <- 'Madrid'
municipalities_spain$Texto[municipalities_spain$Texto== "Region De Murcia"] <- 'Murcia'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad Foral De Navarra"] <- 'Navarra'
municipalities_spain$Texto[municipalities_spain$Texto== "Comunidad Valenciana"] <- 'Valencia'
municipalities_spain$Texto[municipalities_spain$Texto== "Cataluna"] <- 'Cataluña'
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
autonomias2$Texto[autonomias2$Texto== "Cataluna"] <- 'Cataluña'

autonomias3<-as_tibble(autonomias2)
autonomias3<-st_sf(autonomias3)

# Tabla top productos -----------------------------------------------------
unique(consumo$producto)


consumo_top <- consumo %>% filter(producto!="Hortalizas frescas" & producto!="Frutas frescas" & producto!="Patatas" & producto!="Hortalizas" & producto!="Frutas iv gama")
consumo_top <- consumo_top %>% filter(ccaa!="Nacional")
consumo_top$year<-year(consumo_top$date)



# FILTROS -----------------------------------------------------------------
colnames(consumo)

variables <- c('Consumo per cápita' = 'consumo_per_capita', 'Volumen de venta' = "volumen" , 'Ingresos totales' = "valor", 
               'Precio' = "precio")
productos <- unique(consumo$producto)
productos <- productos[productos!= 'Frutas frescas']
epoca11 <- unique(consumo_resta$Olas)
ccaa <- unique(consumo$ccaa)
data_spain11 <- consumo_resta %>% 
  group_by(ccaa, producto, Olas) %>% 
  summarise(mean=mean(.data[[paste0('volumen', '_resta')]], na.rm = TRUE))
ccaa_top <- unique(consumo_top$ccaa)


# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  
  title = 'UniversityHack 2021 TheDataMasters',
  # SIDEBAR  
  sidebar = dashboardSidebar(
    width = 100,
    #itemns
    sidebarMenu(
      menuItem(text = "Covid", tabName = "tema1", icon = icon('chart-bar')), 
      menuItem(text = "Análisis",  tabName = "tema2",icon = icon('info-circle'))
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
                                       box(plotOutput(outputId = 'grafico_evolucion', height = 250), width = 12, solidHeader = TRUE)
                                     ) 
                                     
                              ),
                              column(6,
                                     fluidRow(
                                       box('Mediante la resta de los datos recogidos durante la pandemia y las predicciones realizadas,
                                        se visualiza el aumento/disminución que se ha dado en el precio, volumen o ingresos totales de cada producto en consecuencia de la pandemia.', br(), ' ',
                                           leafletOutput(outputId = "my_tmap", height = 290),width = 12)
                                     )
                                     
                              )
                     ),
                     
                     
                     tabPanel("Temporada",
                              
                              fluidRow(
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
                                       
                                ),
                                column(4,
                                       box(
                                         title = 'Impacto en las temporadas', width = 12, background = 'light-blue',
                                         solidHeader = TRUE, '¿Y si visualizamos los datos de la temporada para cada producto?'
                                       )
                                )
                              ),
                              
                              fluidRow(
                                column(5,
                                       box(
                                         "Datos: Previos al virus (2018-2019)", 
                                         tmapOutput(outputId = "map12_1819"),width = 12,solidHeader = TRUE)
                                ),
                                column(5,
                                       box(
                                         "Datos: Durante el COVID-19", 
                                         tmapOutput(outputId = "map12_2020"),width = 12,solidHeader = TRUE)
                                ),
                                column(2,
                                       fluidRow(valueBoxOutput('boxtemp', width = 12)),
                                       valueBoxOutput('progressBox', width = 15)
                                )
                                
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
                     tabPanel("Análisis general",
                              tabsetPanel(
                                tabPanel('Productos',
                                         fluidRow(
                                           column(4, 
                                                  radioGroupButtons('select_variable211', label = 'Seleccione la variable', 
                                                                    choices= variables[c(1, 4)], selected = variables[4])
                                           ),
                                           column(2, 
                                                  selectInput('select_producto211', label ='Seleccione el producto', choices= productos, selected= productos[4]),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                           ),
                                           column(3,
                                                  dateRangeInput('select_date211', 'Seleccione rango temporal', start = min(consumo$date, na.rm = TRUE), end = max(consumo$date, na.rm = TRUE), min = min(consumo$date, na.rm = TRUE),
                                                                 max = max(consumo$date, na.rm = TRUE), startview = "month", weekstart = 0,
                                                                 language = "es", separator = "hasta", width = NULL)
                                           ),
                                           column(3,
                                                  box(
                                                    title = "¿Dónde triunfa cada producto?", width = 12, solidHeader = TRUE,background = "light-blue",
                                                    "Analicemos cómo varían las características de los productos en cada Comunidad Autónoma"
                                                  )
                                           )
                                         ),
                                         fluidRow(
                                           column(6,
                                                  tmapOutput(outputId = "mapa211", height = 250)),
                                           column(6,
                                                  fluidRow(valueBoxOutput('precio211', width = 12))
                                           )
                                         )
                                ),
                                
                                
                                tabPanel('Comparación productos',
                                         fluidRow(
                                           column(3, 
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
                                                  
                                           ),
                                           column(4,
                                                  box(
                                                    title = "Comparemos los productos entre sí", width = 12, solidHeader = TRUE,background = "light-blue",
                                                    "Seleccione la Comunidad Autónoma y los productos")
                                           )
                                           
                                         ),
                                         fluidRow(
                                           plotOutput(outputId = 'evolucomp212', height = 250)
                                           
                                         )
                                         
                                         
                                ),
                                tabPanel('Económico',
                                         fluidRow(
                                           column(2, 
                                                  selectInput('select_ccaa213', label = 'Seleccione la CCAA', choices= ccaa, selected = ccaa[2]),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                           ),
                                           column(3,
                                                  selectInput('select_producto213', label ='Seleccione el producto', choices= productos , selected= productos[4]),
                                                  tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                           ),
                                           column(4, h1(' ')),
                                           box(
                                             title = "Indicadores económicos", width = 5, solidHeader = TRUE,background = "light-blue",
                                             "Analicemos la relación entre el consumo y el gasto per cápita. Seleccione el producto y la ccaa a estudiar")
                                           
                                         ),
                                         fluidRow(
                                           box(plotOutput(outputId = "barraslineas", height = 250),width = 12,solidHeader = TRUE)
                                           
                                         )
                                         
                                )
                                
                              )
                              
                     ),
                     
                     
                     tabPanel("Top productos",
                              fluidRow(
                                column(3, 
                                       selectInput('select_ccaa22', label = 'Seleccione CCAA', choices= ccaa_top, selected = ccaa[2]),
                                       tags$div(tags$style(HTML( ".selectize-dropdown, .selectize-dropdown.form-control{z-index:10000;}")))
                                ),
                                column(4,
                                       numericInput('select_topp22', label = 'Seleccione número de productos (3 o más)', value = 4,
                                                    min = 3, step = 1)),
                                box(
                                  title = "Top Productos", width = 5, solidHeader = TRUE,background = "light-blue",
                                  "Analicemos los productos más consumidos en cada Comunidad Autónoma.")
                                
                              ),
                              fluidRow(
                                column(9,
                                  box('Para poder observar cambios en las tendencias de compras derivados de la
                                      pandemia, se muestran los productos más populares de las Comunidades Autónomas
                                      en años anteriores a la pandemia y en el propio año de la pandemia. Además,
                                      también se muestra la comparativa de ventas de esos productos entre estos dos
                                      periodos de tiempo, para concluir si el consumo de los mismos ha aumentado, 
                                      mantenido o disminuido.',
                                    box(plotOutput(outputId = "top_productos_2019"), width = 6, 
                                        solidHeader = TRUE),
                                    box(plotOutput(outputId = "top_productos_2020"), width = 6,
                                        solidHeader = TRUE),
                                    width = 12
                                  )
                                ),
                                column(3,
                                  fluidRow(valueBoxOutput('diferencia_top22', width = 12)),
                                  fluidRow(valueBoxOutput('diferencia_producto_top22', width = 12))
                                )
                              )
                              
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
      group_by(producto, Texto) %>% 
      summarise(mean = mean(mean))
    
    pruebabuena11<-st_sf(map_data_spain11, st_geometry(autonomias3))
    
    hola <- tm_shape(pruebabuena11,
                     name = paste('Mapa de', input$select_variable11),
                     simplify = 1,
                     line.center = "midpoint") +
      tm_polygons(col = "mean", palette = "RdBu", midpoint = 0,title = input$select_variable11) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3.7056721,40.4169019,5))
    
    tmap_leaflet(hola)
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
      scale_x_date(date_breaks = "2 month", date_labels = "%b%y") +
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
  output$map12_1819 <- renderTmap({
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
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3.7056721,40.4169019,5))
    
  })

  # MAPA 12
  output$map12_2020 <- renderTmap({
    
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
      tm_polygons(col = "mean", breaks = round(vector,2), legend.show = TRUE,  palette = "RdBu", midpoint = 0,title = input$select_variable12) +
      tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3.7056721,40.4169019,5))
    
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
      labs(y = 'Valor (kg/€)')+
      theme_bw()+
      theme(axis.text = element_text(angle = 45, hjust = 1), 
            panel.grid.minor = element_blank()) +
      labs(y = 'Ingresos',
           title = 'Balance de las importaciones y exportaciones',
           subtitle = 'Datos para España, sector agroalimentario',
           caption = "Fuente: Eurostat")
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
  output$top_productos_2019 <- renderPlot({
    table_spider_2 <- table_spider_2()
    table_spider_1 <- table_spider_1()
    rownames(table_spider_2) <- c('Máximo', 'Mínimo', 'Pandemia', 'Normalidad')
    names1 <- names(table_spider_1)
    names2 <- names(table_spider_2)
    coincident_columns <- intersect(names1, names2)
    not_coincident_columns <- setdiff(names1, names2)
    
    table_spider_1 <- table_spider_1 %>% 
      select(coincident_columns, everything())
    
    table_spider_2 <- table_spider_2 %>% 
      select(coincident_columns, everything())
    colors_border <- rgb(0.2,0.5,0.5,0.9)
    colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
    
    b <- radarchart(table_spider_2, pcol=colors_border , pfcol=colors_in, 
                    plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                    caxislabels = seq(0, round(max(table_spider_2), 0), 
                                      round(max(table_spider_2)/8, 0)),
                    vlcex = 0.8, title = paste(input$select_ccaa22, 'cambiar'), seg = 7)
    b
    
    legend(x='bottomleft', legend = rownames(table_spider_2[-c(1,2),]), bty = "n", pch=20 , 
           col=colors_in , text.col = "azure4", cex=0.9, pt.cex=2)
  })
  
  output$top_productos_2020 <- renderPlot({
    table_spider_1 <- table_spider_1()
    table_spider_2 <- table_spider_2()
    rownames(table_spider_1) <- c('Máximo', 'Mínimo', 'Pandemia', 'Normalidad')
    names1 <- names(table_spider_1)
    names2 <- names(table_spider_2)
    coincident_columns <- intersect(names1, names2)
    not_coincident_columns <- setdiff(names1, names2)
    
    table_spider_1 <- table_spider_1 %>% 
      select(coincident_columns, everything())
    
    table_spider_2 <- table_spider_2 %>% 
      select(coincident_columns, everything())
    
    colors_border <- rgb(0.2,0.5,0.5,0.9)
    colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
    
    a1<- radarchart(table_spider_1, pcol=colors_border , pfcol= colors_in,
                    plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                    caxislabels=seq(0, round(max(table_spider_1), 0), 
                                    round(max(table_spider_1)/8, 0)),
                    vlcex=0.8, title= paste(input$select_ccaa22, 'cambiar'), seg = 7)
    a1
    
    legend(x='bottomleft', legend = rownames(table_spider_1[-c(1,2),]), bty = "n", pch=20 , 
           col=colors_in , text.col = "azure4", cex=0.9, pt.cex=2)
    
  })
  
  #Caja diferencia_top22
  output$diferencia_top22 <- renderValueBox({
    valueBox(value = NULL, 'más pruebas')
  })
  
  #Caja diferencia_producto_top22
  output$diferencia_producto_top22 <- renderValueBox({
    valueBox('Cerda asquerosa', 'que no vales na')
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
  
  
  # COmparacion de precios 211 
  output$evolucomp212 <- renderPlot({
    
    don<- filter(consumo, ccaa %in% input$select_ccaa212 & 
                   producto %in% input$select_producto212
    )
    
    ggplot(don, aes_string(x='date', y=input$select_variable212, color='producto', linetype='ccaa')) +
      geom_line() +
      geom_point() +
      labs(y= paste(capitalize(names(which(variables == input$select_variable212))), 
                    if (input$select_variable212 == 'precio') '(€/kg)' 
                    else if (input$select_variable212 == 'volumen') '(miles de kg)'
                    else if (input$select_variable212 == 'valor') '(miles de €)' 
                    else '(kg)'), 
           x = "Fecha",
           title = capitalize(names(which(variables == input$select_variable212))),
           subtitle = paste(don$producto, 'para', don$ccaa),
           caption = "Fuente: Ministerio de Agricultura, Pesca y Alimentación",
           color = 'Producto (color)', linetype = 'CCAA (tipo de línea)') +
      scale_x_date(date_breaks = "2 month", date_labels = "%Y-%b") +
      expand_limits(y= 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank()) 
  })
  
  
  # MAPA 211: caracteristicas
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
  
  # BOX 211: caracteristicas
  output$precio211 <- renderValueBox({
    
    data_spain211 <-data_spain %>% group_by(ccaa,producto) %>% summarise(mean=mean(.data[[input$select_variable211]],na.rm = TRUE))
    maximo <- data_spain211 %>% filter(producto == input$select_producto211) %>% arrange(desc(mean)) %>%  head(1)
    minimo <- data_spain211 %>% filter(producto == input$select_producto211) %>% arrange(mean) %>%  head(1)
    
    valueBox(input$select_producto211, tags$div('-El', tolower(names(which(variables == input$select_variable211))), 
                                                'más alto se ha registrado en', str_to_title(maximo$ccaa), 'siendo este' , 
                                                paste0(round(maximo$mean,2), ' ',
                                                       if (input$select_variable211 == 'precio') '€/kg'
                                                       else 'kg mensuales', '.'), tags$br(), '-El ',
                                                tolower(names(which(variables == input$select_variable211))), 
                                                'más bajo se ha registrado en', str_to_title(minimo$ccaa), 'siendo este', 
                                                paste0(round(minimo$mean,2), ' ',
                                                       if (input$select_variable211 == 'precio') '€/kg' 
                                                       else 'kg', '.')), "Progress", icon = NULL, color = "navy")
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
  
}


shinyApp(ui, server)


