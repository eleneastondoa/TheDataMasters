# Gráfico 12 --------------------------------------------------------------
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
