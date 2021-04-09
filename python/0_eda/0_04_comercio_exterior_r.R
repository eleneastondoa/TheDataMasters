#Carga de datos
getwd()
dir_in <- 'datos/datos_originales'
file_in <- 'Dataset4.- Comercio Exterior de España.txt'
df_comercioex <- read.csv(file.path(dir_in, file_in), sep = '|')
names(df_comercioex)[1] <- 'PERIOD'
names(df_comercioex)

df_comercioex_spread <- df_comercioex %>% 
  spread('INDICATORS', 'Value')
write.csv(df_comercioex_spread, 'datos/datos_desarrollo/comercio_exterior_spread.csv')
