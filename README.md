## Repositorio 'TFM_UOC'

En este repositorio encontramos:

- El script desarrollado para generar el análisis y la aplicación web, denominado 'app.R'. 
- El directorio 'www', que contiene la imagen del logo de la aplicación web.
- El archivo de texto '.gitignore.txt' para ignorar la subida a GitHub de dos directorios y un archivo de texto que se generan al ejecutar la aplicación web en local e implementar la aplicación Shiny en RStudio Connect. 
- La licencia MIT. 

## Proyecto y descripción

Aplicación web para el análisis de los factores implicados en la relación entre el estrés y la depresión en modelos animales utilizando minería de textos de PubMed.

Este proyecto se enfoca en la aplicación de técnicas de minería de textos para analizar y agrupar la información disponible de la relación entre el estrés y la depresión en modelos animales. Para ello, se emplean artículos recopilados de PubMed en los que se utilizan diversas pruebas de comportamiento realizadas en ratones sometidos a diferentes estresores. A través de la identificación de factores que influyen en los resultados, como la cepa de ratón, la edad, el género, la región cerebral y el tipo de estresor aplicado (intensidad, momento, duración del factor estresante, etc.), así como las pruebas de comportamiento llevadas a cabo, se pretende lograr una integración más coherente de los hallazgos existentes. Esto permitirá crear una narrativa más sólida sobre cómo se desarrolla la depresión y los trastornos del estado de ánimo en los roedores. Además, se presenta una aplicación web interactiva que permitirá al usuario obtener los resultados mencionados de forma clara y sencilla. Al centrar nuestros esfuerzos en dicha tarea, se pretende mejorar la convergencia traslacional entre los modelos de roedores y los endofenotipos humanos, acelerando la tasa de descubrimiento y comprensión de los trastornos neuropsiquiátricos causados por el estrés.

## Requerimientos e instalación

En este proyecto se utilizó un entorno de desarrollo basado en R, versión 4.3.1. Los paquetes de R utilizados en el análisis y la aplicación web (script 'app.R') son:

	- shiny
	- shinydashboard
	- easyPubMed
	- RISmed
	- tm
	- pubmed.mineR
	- apcluster
	- lsa
	- XML
	- wordcloud
	- RColorBrewer
	- proxy
	- hash
	- stringr
	- slam
	- ggplot2
	- cicerone

Para instalar dichos paquetes usamos ```install.packages(c("shiny", "shinydashboard", "easyPubMed", "RISmed", "tm", "pubmed.mineR", "apcluster", "lsa", "XML", "wordcloud", "RColorBrewer", "proxy", "hash", "stringr", "slam", "ggplot2", "cicerone"))```.

## Estructura de la aplicación

Encontramos 6 pestañas en el menú lateral principal: 'Inicio', 'Artículos', 'Clusters', 'Factores influyentes', 'Evolución de la información' e 'Información'. En cada pestaña encontramos una guía.

- 'Inicio'. A su vez, encontramos las pestañas 'Gráficos', 'Autores', 	'Año de publicación' y 'Revistas'. 
	- 'Gráficos'. 
		- Wordcloud: puede seleccionar la cantidad de palabras del corpus primario para mostrar en el wordcloud. 
		- Dendrograma: dendrograma de las 100 palabras más frecuentes del corpus primario. 
	- 'Autores'. 
		- Tabla de los autores del corpus primario ordenados por su frecuencia de aparición. Puede buscar por nombre de autor. 
	- 'Año de publicación'.
		- Tabla de la cantidad de artículos del corpus primario publicados cada año. Puede buscar por año de publicación. 
	- 'Revistas'.
		- Tabla de la cantidad de artículos del corpus primario publicados en cada revista. Puede buscar por revista. 
- 'Artículos'.
	- Búsqueda por palabras: puede filtrar los artículos del corpus primario que contengan las palabras introducidas. Estos artículos contendrán todas las palabras. Debe introducir estas palabras separadas por un espacio. 
	- Tabla de los artículos filtrados: se muestra el PMID y los títulos de los artículos. Puede acceder al artículo en PubMed haciendo clic sobre su PMID. Adicionalmente, puede buscar entre estos artículos por palabras presentes en sus títulos.
- 'Clusters'. A su vez, encontramos las pestañas 'Información general sobre los clusters' y 'Agrupación por propagación de afinidad'.
	- Información general sobre los clusters'.
	- Dendrograma de los artículos ejemplares tras llevar a cabo la agrupación por propagación de afinidad. Las distancias entre ellos corresponden con la medida del coseno. 
	- 'Agrupación por propagación de afinidad'.
		- Tabla de los artículos ejemplares y sus palabras clave: puede ver las palabras clave asociadas a cada ejemplar y buscar ejemplares por palabras clave. Cuando un ejemplar no está asociado a ninguna palabra clave, aparece "No data available".
		- Desplegable con los ejemplares: puede seleccionar el ejemplar para analizar el cluster que representa. Se representará su gráfica y una tabla con los PMIDs de los artículos.
		- Gráfica del ejemplar seleccionado: se representan los artículos del cluster del ejemplar seleccionado con sus PMIDs. Las distancias entre ellos corresponden con la medida del coseno. 
		- Tabla del ejemplar seleccionado: puede ver los PMIDs de los artículos del cluster del ejemplar seleccionado. Puede acceder al artículo en PubMed haciendo clic sobre su PMID.
- 'Factores influyentes'.
	- Términos a introducir en el análisis de asociación: puede introducir uno o más términos a la lista inicial. Si introduce más de un término, estos deben ir separados por un espacio. Posteriormente, podrá seleccionar uno de estos términos para analizar su asociación con el resto de términos de la lista.
	- Lista de términos: puede seleccionar un término de la lista para analizar su asociación con el resto de términos listados. En esta lista aparecerán también los términos que haya introducido.
   	- Histograma de la fuerza de asociación utilizando la función de similitud del coseno y el análisis semántico latente (LSA) del término seleccionado con el resto de términos listados.
- 'Evolución de la información'.
	- Periodo de tiempo: puede seleccionar un periodo de tiempo para llevar a cabo el análisis de la evolución de la información. Se compararán cada uno de los periodos de tiempo seleccionados.
	- Añadir más periodos: puede añadir más periodos de tiempo haciendo clic sobre este botón.
	- Eliminar último periodo: puede eliminar el último periodo de tiempo haciendo clic sobre este botón.
	- Confirmar: una vez seleccionado, se llevará a cabo el análisis con los periodos presentes. Se mostrará un wordcloud por periodo de tiempo, donde se han eliminado las palabras comunes entre periodos.
- 'Información'. Se muestra información sobre la licencia y el 	contacto. 
