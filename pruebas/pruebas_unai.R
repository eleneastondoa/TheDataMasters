# Gráfico 11 --------------------------------------------------------------
df_evolucion1 <- filter(consumo, ccaa == 'Andalucia' & 
                         producto == 'Patatas')
df_evolucion_pred1 <- consumo_pred %>% 
  filter(ccaa == 'Andalucia' &
           producto == 'Patatas')

p <- ggplot() +
  geom_line(data = df_evolucion1, aes_string('date', 'precio',
                                            color = "'Real'"),size = 1)+
  geom_line(data = df_evolucion_pred1, 
            aes_string('X', paste0('precio', '_predicted'), 
                       color = "'Predicción'"), size = 1, linetype = 'longdash')+
  labs(y= str_to_title('precio'), 
       x = "Fecha",
       title = paste("Evolución del", 'precio'),
       subtitle = paste(df_evolucion1$producto, 'para', df_evolucion1$ccaa),
       caption = "Fuente: datathon") +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y-%b") +
  scale_color_manual(name = 'Dato:', values = c('Real' = 'steelblue1', 'Predicción' = 'steelblue4'))+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank())
rango_min <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
rango_max<- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]

if (rango_min > rango_max*0.25){
  p <- p + annotate(geom = 'label', x = as.Date('2018-05-01'), 
                    y = rango_min - rango_min*0.1, label = 'No empieza en 0',
                    fill = brewer.pal(3, 'Blues')[2])
}
p
a = 'PrOSDnfecio'
if (a == 'Precio') print('Hola') else if (a == 'PrOSDnfecio') print('Adios')
paste('hola', if (a == 'precio') 'puta'
      else 'cerda')  

# Gráfico 212 -------------------------------------------------------------
don<- filter(consumo, ccaa %in% c('Nacional', 'Andalucia') & 
               producto %in% c('Patatas', 'Patatas frescas')
)

ggplot(don, aes_string(x='date', y='volumen', linetype='ccaa', color='producto')) +
  geom_line() +
  geom_point() +
  labs(y= str_to_title('volumen'), 
       x = "Fecha",
       title = 'Tu puta madre',
       subtitle = paste(don$producto, 'para', don$ccaa),
       caption = "Fuente: datathon") +
  scale_x_date(date_breaks = "2 month", date_labels = "%Y-%b") +
  labs(color = 'Producto (color)', linetype = 'CCAA (tipo de línea)') +
  expand_limits(y= 0)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45, hjust = 1), panel.grid.minor = element_blank())

variable <- 'CERDA'
aa <- paste(str(cat('hola\nadios', variable)))
aa  
paste('hola', 'adios', sep = '\n')
tags$div('hola buenas cerda', tags$br(), 'gracias por nada', 'puta')

prueba <- c('Primera' = 'Hola', 'Segunda' = 'Puta')
prueba['Primera']

names(which(prueba == 'Hola'))

if (input$select_variable211 == 'valor') 'Ingreso total'

# Gráfico 12 --------------------------------------------------------------
#Las que dan error son pimientos, naranjas, limones y judias verdes. En todo menos en la caja que dice el rango de la temporada


data_spain12_1819 <- temp1819
data_spain12_1819 <-data_spain12_1819 %>% group_by(ccaa, producto) %>%
  summarise(mean=mean(.data[['precio']],na.rm = TRUE))
data_spain12_1819 <-as_tibble(data_spain12_1819)
map_data_spain12_1819 <- municipalities_spain %>% left_join(data_spain12_1819, by = c("Texto"="ccaa")) 
map_data_spain12_1819 <- filter(map_data_spain12_1819, producto == 'Naranjas')
max_1819 <- max(map_data_spain12_1819['mean'])
min_1819 <- min(map_data_spain12_1819['mean'])

# 2020
data_spain12_2020 <- temp2020
data_spain12_2020 <-data_spain12_2020 %>% group_by(ccaa,producto) %>% 
  summarise(mean=mean(.data[['precio']],na.rm = TRUE))
data_spain12_2020 <-as_tibble(data_spain12_2020)

unique(data_spain12_2020$producto)

map_data_spain12_2020 <- municipalities_spain %>% left_join(data_spain12_2020, by = c("Texto"="ccaa")) 
map_data_spain12_2020 <- filter(map_data_spain12_2020, producto == 'Naranjas')


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
         name = paste('Mapa de', 'precio'),
         simplify = 1,
         line.center = "midpoint") +
  tm_polygons(col = "mean", palette = "RdBu",breaks = round(vector,1),midpoint = 0, 
              title = 'Precio') +
  tm_view(view.legend.position = c('right', 'bottom'), set.view = c(-3.7056721,40.4169019,5))


# Radarcharts 22 ---------------------------------------------------------
top_2020<- consumo_top %>% filter(year == 2020) %>% 
  select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor)) %>%  head(4) %>% spread(producto, valor)

nombres<- colnames(top_2020)

top_2019_20 <- consumo_top %>% filter(year == 2019) %>% 
  select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor))%>%filter(producto %in% nombres)%>% spread(producto, valor)


table_spider_1 <- rbind(max(top_2020[,-1], top_2019_20[,-1]), 
                        rep(0,times = 4), top_2020[,-1], top_2019_20[,-1])
table_spider_1 <- as.data.frame(table_spider_1)
rownames(table_spider_1) <- c('Máximo', 'Mínimo', '2020', '2019')

top_2019 <- consumo_top %>% filter(year == 2019) %>% 
  select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor)) %>%  head(4) %>% spread(producto, valor)

nombres_2 <- colnames(top_2019)

top_2020_19 <- consumo_top %>% filter(year == 2020) %>% 
  select(year, producto, ccaa, valor ) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor))%>%filter(producto %in% nombres_2)%>% spread(producto, valor)

table_spider_2 <- rbind(max(top_2019[,-1], top_2020_19[,-1]),
                        rep(0,times = 4), top_2020_19[,-1], top_2019[,-1])
table_spider_2 <- as.data.frame(table_spider_2)
rownames(table_spider_2) <- c('Máximo', 'Mínimo', '2020', '2019')

names1 <- names(table_spider_1)
names2 <- names(table_spider_2)
coincident_columns <- intersect(names1, names2)
not_coincident_columns <- setdiff(names1, names2)

colors_border <- rgb(0.2,0.5,0.5,0.9)
colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

a1<- radarchart(table_spider_1, pcol=colors_border , pfcol= colors_in,
                plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                caxislabels=seq(0, round(max(table_spider_1), 0), round(max(table_spider_1)/8, 0)),
                vlcex=0.8, title= paste('Andalucia', '2020'), seg = 7)
a1

legend(x=0.8, y=1.1, legend = rownames(table_spider_1[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "azure4", cex=1.2, pt.cex=3)

b <- radarchart(table_spider_2, pcol=colors_border , pfcol=colors_in, 
                plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                caxislabels = seq(0, round(max(table_spider_2), 0), round(max(table_spider_2)/8, 0)),
                vlcex = 0.8, title = paste('Andalucia', '2019'), seg = 7)
b

legend(x=0.8, y=1.1, legend = rownames(table_spider_2[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "azure4", cex = 1.2, pt.cex=3)

# Radarchart 22 2020 ----------------------------------------------------------
top_2020<- consumo_top %>% filter(year == 2020) %>% 
  select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor)) %>%  head(4) %>% spread(producto, valor)

nombres<- colnames(top_2020)

top_2019_20 <- consumo_top %>% filter(year != 2020) %>% 
  select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor))%>%filter(producto %in% nombres)%>% spread(producto, valor)


table_spider_1 <- rbind(max(top_2020[,-1], top_2019_20[,-1]), 
                        rep(0,times = 4), top_2020[,-1], top_2019_20[,-1])
table_spider_1 <- as.data.frame(table_spider_1)
rownames(table_spider_1) <- c('Máximo', 'Mínimo', 'Pandemia', 'Normalidad')

colors_border <- rgb(0.2,0.5,0.5,0.9)
colors_in <- c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))

a1<- radarchart(table_spider_1, pcol=colors_border , pfcol= colors_in,
                plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                caxislabels=seq(0, round(max(table_spider_1), 0), round(max(table_spider_1)/8, 0)),
                vlcex=0.8, title= paste('Andalucia', '2020'), seg = 7)
a1

legend(x='bottomleft', legend = rownames(table_spider_1[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "azure4", cex=1, pt.cex=1.3)

# Radarchart 22 2019 ------------------------------------------------------
top_2019 <- consumo_top %>% filter(year != 2020) %>% 
  select(year, producto, ccaa, valor) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor)) %>%  head(4) %>% spread(producto, valor)

nombres_2 <- colnames(top_2019)

top_2020_19 <- consumo_top %>% filter(year == 2020) %>% 
  select(year, producto, ccaa, valor ) %>% group_by(producto,ccaa) %>%
  summarise(valor = mean(valor)) %>% filter(ccaa == 'Andalucia') %>% 
  arrange(desc(valor))%>%filter(producto %in% nombres_2)%>% spread(producto, valor)

table_spider_2 <- rbind(max(top_2019[,-1], top_2020_19[,-1]),
                        rep(0,times = 4), top_2020_19[,-1], top_2019[,-1])
table_spider_2 <- as.data.frame(table_spider_2)
rownames(table_spider_2) <- c('Máximo', 'Mínimo', 'Pandemia', 'Normalidad')

names1 <- names(table_spider_1)
names2 <- names(table_spider_2)
coincident_columns <- intersect(names1, names2)
not_coincident_columns <- setdiff(names1, names2)

table_spider_1 <- table_spider_1 %>% 
  select(coincident_columns, everything())

table_spider_2 <- table_spider_2 %>% 
  select(coincident_columns, everything())


b <- radarchart(table_spider_2, pcol=colors_border , pfcol=colors_in, 
                plwd=1, cglcol="grey", cglty=1, axislabcol="azure4", axistype = 1, calcex = 0.8,
                caxislabels = seq(0, round(max(table_spider_2), 0), round(max(table_spider_2)/8, 0)),
                vlcex = 0.8, title = paste('Andalucia', '2019'), seg = 7)
b

legend(x=0.8, y=1.1, legend = rownames(table_spider_2[-c(1,2),]), bty = "n", pch=20 , 
       col=colors_in , text.col = "azure4", cex = 1.2, pt.cex=3)




