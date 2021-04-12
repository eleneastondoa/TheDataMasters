# TheDataMasters UniversityHack 2021
A continuación se explicará la estructura del proyecto y cómo visualizar la herramienta desarrollada.

## Carpeta 'python'
En esta carpeta se encuentran todos los scripts de python en los que se realiza el preproceso de los datos y se crean las tablas finales. Se puede o no seguir el orden de ejecución determinado en los nombres de los mismos scripts, son independientes entre sí.

## Carpeta 'datos'
En esta carpeta hay a su vez dos subcarpetas
### 'datos_originales'
Los datos crudos recibidos por parte de cajamar.
### 'datos_desaroollo'
Todos los archivos generados del preproceso en python y los datos de fuentes externas recogidos (indicados en el pdf)

## Carpeta 'ccaa_mapita'
Carpeta en la que se encuentran los archivos que contienen la forma geométrica de las comunidades de españa, se utilizan estos archivos para realizar visualizaciones en la aplicación.

## Cómo visualizar la aplicación
Para visualizar la aplicación se pueden seguir dos métodos.
1. Utilizar el link de acceso que se inserta a continuación:  https://datamasters.shinyapps.io/TheDataMasters/
2. Acceder interactuando directamente con el código de R. Para lo cual hay que seguir los siguientes pasos:
	2.1. Si se quiere acceder a la aplicación interactuando directamente con el código de R, establecer, antes siquiera de abrir el script, el encoding 'UTF-8' como predeterminado del 		sistema. Para ello seguir los siguientes pasos.
		2.1.1 Abrir o crear un archivo cualquiera de R.
		2.1.2 En la esquina superior izquierda, pinchar en 'Archivo' --> 'Guardar con encoding' --> 'UTF-8' --> 'Establecer como predeterminado del sistema' --> 'Aceptar'
		2.1.3 A continuación abrir nuestro archivo y este se leerá con el encoding UTF-8, necesario para el idioma español.
	2.2. Instalar la versión 4.0.2 o superior de R y actualizar las librerías que se cargan en el script a la última versión disponible.
	2.3. Si no se altera la estructura del repositorio de github tras clonarlo, clickar en el botón de 'Ejecutar App'.
	2.4. En la esquina superior izquierda pinchar en 'Abrir en navegador'.
	2.5. Establecer pantalla completa para una mejor interactividad.

##Consideraciones
Si bien se puede acceder a la aplicación de las dos maneras especificadas, debido a las diferencias en las resoluciones de las pantallas de los ordenadores, es posible que la estructura de la aplicación se altere ligeramente si se accede a la aplicación interactuando directamente con el código R (el punto 2). Por este motivo recomendamos utilizar el link que se ha proporcionado previamente. 

