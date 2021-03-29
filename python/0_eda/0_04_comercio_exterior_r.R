#Carga de datos
getwd()
df_comercioex <- read.csv('../../datos/datos_originales/Dataset4.- Comercio Exterior de España.txt', 
                          sep = '|')
names(df_comercioex)
df_comercioex_spread <- df_comercioex %>% 
  spread('INDICATORS', 'Value')
write.csv(df_comercioex_spread, '../../datos/datos_desarrollo/comercio_exterior_spread.csv')
