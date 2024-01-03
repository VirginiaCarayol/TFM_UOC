
###### Este es el archivo app.R, el cual comienza cargando los paquetes necesarios 
# (incluido el paquete shiny) y termina con una llamada a shinyApp().


# Cargamos todos los paquetes necesarios.

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
library(easyPubMed)
library(RISmed)
library(tm)
library(pubmed.mineR)
library(apcluster)
library(lsa)
library(XML)
library(wordcloud)
library(RColorBrewer)
library(proxy)
library(hash)
library(stringr)
library(slam)
library(DT)
library(ggplot2)
library(cicerone)


###### Obtención de los datos de partida y creación de la matriz de términos-documentos.

# Descargamos todos los abstracts con la función batch_pubmed_download() del paquete
# easyPubMed en formato 'xml' para, después, separarlos.

# Para hacer la consulta, usamos la función get_pubmed_ids(), que recibe una cadena de
# consulta como argumento (se aplica la sintaxis estándar de PubMed). Esta función 
# devuelve una lista que contiene toda la información para, posteriormente, acceder y 
# descargar estos resultados del servidor ('server').

# Los registros se pueden recuperar del servidor mediante la función fetch_pubmed_data(), 
# que recibe como argumento el resultado de la función get_pubmed_ids(). A su vez, los
# registros se pueden solicitar en formato XML o TXT con el argumento 'format'. En 
# particular, vamos a utilizar el formato 'xml'. Por defecto, los registros se recuperan 
# por lotes de 5000. 

# La función fetch_pubmed_data() devuelve un vector de tipo 'character'. Si el formato es 'xml',
# devuelve una única cadena con todos los registros de PubMed (con etiquetas XML incrustradas).

my_query <- "mice[Title/Abstract] AND stress[Title/Abstract] AND depression[Title/Abstract] AND behavior[Title/Abstract] AND test[Title/Abstract]"

# my_query <- "mice[Title/Abstract] AND stress[Title/Abstract] AND depression[Title/Abstract] AND behavior[Title/Abstract] AND test[Title/Abstract] AND amygdala[Title/Abstract]"

my_query_ids <- get_pubmed_ids(pubmed_query_string = my_query)

# Definimos los nombres de las carpetas donde se guardarán los datos que nos descargaremos.

my_dir <- "data"

xmls_folder <- "abstracts_xml"

# Si nuestro directorio donde guardamos los datos descargados ya existe, borramos
# todo lo que contiene, para así empezar siempre de cero. Si no existe, lo creamos.

if (dir.exists(my_dir)) {
  unlink(paste(my_dir, "*", sep = "/"), recursive = TRUE)
} else {
  dir.create(my_dir)
}

# La función batch_pubmed_download(), que realiza una consulta en PubMed (a través de
# la función get_pubmed_ids()), descarga los datos resultantes (a través de múltiples 
# llamadas a la función fetch_pubmed_data()) y luego guarda los datos en una serie 
# de archivos XML o TXT en la unidad local. 

# La función batch_pubmed_download() recibe, entre otros, los siguientes argumentos: 
# 'pubmed_query_string' (cadena de búqueda en PubMed), 'dest_dir' (nombre de la carpeta 
# existente donde se guardarán los archivos), 'format' (formato de los datos).

my_abstracts_download_xml <- batch_pubmed_download(
  pubmed_query_string = my_query, dest_file_prefix = "easyPubMed_data_xml", 
  dest_dir = my_dir, format = "xml")

separate_and_store_batch <- function(my_abstracts_download_xml, output_directory) {
  # Definimos la función separate_and_store_batch(), que separa y guarda el título y 
  # abstract (si existe) de los diferentes artículos en ficheros de texto individuales,
  # denominados por el PMID del artículo en cuestión. La función espera que el argumento
  # 'my_abstracts_download_xml' sea la lista de ficheros XML que contienen el lote
  # de artículos.
  for (fileName in my_abstracts_download_xml) {
    # Sacamos el camino completo al fichero XML que hemos descargado.
    filePath <- paste(my_dir, fileName, sep = "/")
    # Con la función xmlParse() del paquete XML, leemos el fichero para poder usar 
    # los datos en R.
    xml <- xmlParse(filePath)
    # Sacamos todos los distintos nodos de <PubmedArticle> que se encuentran dentro de
    # <PubmedArticleSet> con la función getNodeSet() del paquete XML (Nota: se ha empleado
    # la página web https://jsonformatter.org/xml-viewer para visualizar previamente uno
    # de los archivos XML y conocer los XPath).
    pubmedArticles <- getNodeSet(xml, "/PubmedArticleSet/PubmedArticle")
    for (pubmedArticle in pubmedArticles) {
      # Sacamos el nodo donde se encuentra el PMID, para poder usarlo como nombre del
      # fichero nuevo.
      pmid <- getNodeSet(pubmedArticle, "MedlineCitation/PMID")
      # Sacamos el texto del nodo con la función xmlValue() del paquete XML.
      pmidText <- xmlValue(pmid[[1]])
      # Nos aseguramos de que existe el directorio donde vamos a guardar los ficheros.
      dir.create(file.path(my_dir, output_directory), showWarnings = FALSE)
      # Establecemos el camino completo del nuevo fichero que vamos a crear.
      newFilePath <- paste(my_dir, "/", output_directory, "/", pmidText, ".txt", sep = "")
      # Sacamos el nodo donde se encuentra el título del artículo.
      articleTitle <- getNodeSet(pubmedArticle, "MedlineCitation/Article/ArticleTitle")
      # Sacamos el texto del nodo.
      articleTitleText <- xmlValue(articleTitle[[1]])
      # Creamos el nuevo fichero con el contenido del título con la función cat().
      cat(articleTitleText, file = newFilePath, sep = "\n")
      # Sacamos todos los distintos nodos de <AbstractText>.
      abstractTexts <- getNodeSet(pubmedArticle, 
                                  "MedlineCitation/Article/Abstract/AbstractText")
      # En caso de que abstractTexts tenga una longitud mayor a 0 (es decir, no esté vacío
      # (disponemos del abstract)): 
      if (length(abstractTexts) > 0) {
        for (abstractText in abstractTexts) {
          # Sacamos el texto de cada nodo <AbstractText> y lo añadimos al fichero creado
          # anteriormente (con el argumento 'append = TRUE').
          abstractTextText <- xmlValue(abstractTexts[[1]])
          cat(abstractTextText, file = newFilePath, append = TRUE, sep = "\n")
        }
      }
    }
  }
}

# Aplicamos la función definida anteriormente a nuestros datos:

separate_and_store_batch(my_abstracts_download_xml, xmls_folder)

# El objeto principal para gestionar documentos con el paquete tm es el objeto de clase
# 'Corpus', que representa una colección de documentos de texto. Para crear objetos 
# de clase 'Corpus', podemos emplear la función Corpus() pasándole como argumento 'x'
# un 'DirSource'. Éste recibe, entre otros, los siguientes argumentos: 'directory'
# (directorio donde se encuentran nuestros documentos .txt, uno para cada abstract)
# y 'pattern' (una expresión regular opcional; sólo se devolverán los nombres de
# los archivos que coincidan con la expresión regular). 

my_corpus_tm_raw <- Corpus(x = DirSource(paste(my_dir, xmls_folder, sep = "/"), 
                                         pattern = ".txt"))

# Lo primero que vamos a hacer es llevar a cabo el preprocesamiento inicial de los datos.
# Para ello, utilizaremos funciones de transformación con la función tm_map() del paquete
# tm. 

# Eliminamos las puntuaciones, los espacios en blanco adicionales y los números, y
# sacamos las derivaciones de las palabras.

my_corpus_tm <- tm_map(my_corpus_tm_raw, removePunctuation)
my_corpus_tm <- tm_map(my_corpus_tm, stripWhitespace)
my_corpus_tm <- tm_map(my_corpus_tm, removeNumbers)
my_corpus_tm <- tm_map(my_corpus_tm, stemDocument)

# También deseamos eliminar palabras comunes en inglés (palabras vacías). Para ello, 
# volvemos a emplear la función tm_map(), y utilizamos la función removeWords() y
# stopwords() (esta última devuelve palabras vacías para diferentes idiomas; en este 
# caso, usamos el inglés). 

my_corpus_tm <- tm_map(my_corpus_tm, removeWords, stopwords("english"))

# A continuación, expresamos nuestro corpus como una matriz de términos-documentos. 
# Para crear dicha matriz encontramos la función TermDocumentMatrix() del paquete tm.

my_tdm <- TermDocumentMatrix(my_corpus_tm)

###### Inicio: gráficos, autores, año de publicación y revistas.

###### Gráficos. 

###### Nube de palabras o wordcloud. 

# Comenzamos representando con una nube de palabras aquellos términos más frecuentes
# entre todos los documentos. Para ello, empleamos el paquete wordcloud, así como 
# RColorBrewer para los colores. 

# Transformamos el objeto de tipo 'TermDocument' en un dataframe para poder trabajar
# fácilmente con los datos.

my_df_tdm <- as.data.frame(as.matrix(my_tdm))

# Sumamos las frecuencias de los términos en todos los documentos con la función rowSums(), 
# y ordenamos los términos por orden descendente de frecuencia con la función sort(). 

words <- sort(rowSums(my_df_tdm), decreasing = TRUE)

# Creamos un dataframe con los términos y sus frecuencias, ordenados de forma descendente
# por su frecuencia. 

df_words <- data.frame(word = names(words), freq = words)

# Usamos la función wordcloud() para representar la nube de palabras de nuestro corpus. 
# Esta función recibe, entre otros, los siguientes argumentos: 'words' (las palabras),
# 'freq' (sus frecuencias), 'min.freq' (frecuencia mínima a partir de la cual las 
# palabras no se representarán), 'max.words' (número máximo de palabras que se 
# representarán), 'random.order' (si es 'TRUE', las palabras se representar de forma
# aleatoria; si no, se representar por orden decreciente), 'random.color' (si es 'FALSE',
# el color se eligirá en función de su frecuencia), 'rot.per' (proporción de palabras 
# representadas con una rotación de 90 grados) y 'colors' (colores para representar las
# palabras de menor a mayor frecuencia).

# Representamos dicho wordcloud() en la página web como parte del trabajo del 'server'.

###### Dendrograma de las 100 palabras más frecuentes. 

# Seleccionamos del dataframe 'TermDocument' los 100 términos más frecuentes hallados 
# anteriormente, y calculamos la distancia del coseno entre ellos con la función dist() 
# del paquete proxy y el argumento 'method = "cosine"'. Debemos tener en cuenta 
# que la función dist() calcula distancias, no similitudes. Por tanto, al utilizar el 
# método "cosine", se obtiene la distancia del coseno, no la similitud del coseno. Para
# obtener la similitud del coseno (valores mayores entre dos términos indican mayor 
# similitud), usamos '1 - distancia del coseno'. Finalmente, empleamos la función
# hclust() para generar el dendrograma. 

# Luego, en el 'server', utilizaremos la función plot() para representarlo.

terms_freq_tdm <- rownames(df_words)[1:100]

cosine_dist_terms_100 <- 1 - proxy::dist(my_df_tdm[terms_freq_tdm,], method = "cosine")

hclust_cosine_dist_terms_100 <- hclust(cosine_dist_terms_100, method = "ward")

###### Autores ordenados de manera decreciente por el número de publicaciones que poseen
# sobre el tema en cuestión.

# Para ello, empleamos el paquete RISmed. En particular, la función EUtilsSummary() 
# del paquete RISmed obtiene información resumida sobre los resultados de una
# consulta en cualquier base de datos de NCBI. Esta función recibe, entre otros argumentos,
# 'query' (corresponde con la consulta como se indica en el cuadro de búsqueda de NCBI; 
# utilizamos la query definida anteriormente para consultar PubMed), 'type' (tipo de 'NCBI Eutility'
# o utilidades electrónicas; en nuestro caso usamos la opción 'esearch', obteniendo la estructura
# de la consulta) y 'db' (base de datos de NCBI utilizada; en nuestro caso, 'pubmed'). 

# La función EUtilsSummary() devuelve un objeto de clase EUtilsSummary, y con ella 
# obtenemos un resultado equivalente al obtenido con la función get_pubmed_ids() del 
# paquete easyPubMed.

my_query_RISmed <- EUtilsSummary(query = my_query, type = "esearch", db = "pubmed")

# La función EUtilsGet() nos permite acceder a los resultados de una consulta en cualquier
# base de datos del NCBI. Esta función recibe cualquiera de los vectores de identificadores
# de registros NCBI, es decir, identificadores devueltos por EUtilsSummary o el objeto 
# EUtilsSummary en sí (argumento 'x'). Por tanto, la función EUtilsSummary() recopila información
# pertinente sobre las publicaciones de nuestra búsqueda y almacena la información para
# su análisis; podemos acceder a nuestros datos con la función EUtilsGet(). Además, recibe
# los argumentos 'type', que indica el  tipo de 'NCBI Eutility', y 'db', que indica la 
# base de datos de NCBI que se utiliza para hacer la consulta. 

# La función EUtilsGet() devuelve un objeto de clase Medline.

EUtilsGet_my_query <- EUtilsGet(x = my_query_RISmed, type = "efetch", db = "pubmed")

# Con estos datos podemos revisar cuántas publicaciones tienen los investigadores 
# sobre el tema en cuestión. Para ello, usamos la función Author() del paquete RISmed,
# que recibe el objeto de clase Medline creado anteriormente, y que devuelve una 
# lista de dataframes (un dataframe por cada artículo, que contiene, entre otros,
# los campos 'LastName' (apellidos), 'ForeName' (nombre) e 'Initials' (iniciales)).

authors <- Author(EUtilsGet_my_query)

# Calculamos la frecuencia de publicaciones de cada autor (incluyendo apellidos y nombre)
# empleando la función table() (el número de veces que aparece cada autor corresponde con
# el número de artículos que haya publicado). Para ello, extraemos los apellidos y nombres
# de los autores de la lista de dataframes (es decir, los autores de todos los artículos). 
# La función sapply() itera sobre cada dataframe en 'authors' y extrae los autores (sus 
# apellidos y nombres).

author_counts <- table(unlist(sapply(authors, function(x) paste(x$LastName, 
                                                                x$ForeName))))

# Creamos un dataframe con los autores (apellidos y nombre) y su frecuencia de publicación.

author_df <- data.frame(Author = names(author_counts), Frequency = as.numeric(author_counts))

# Ordenamos el dataframe por frecuencia en orden descendente con la función order().
# De este modo, tenemos los autores ordenados de manera decreciente por el número de
# publicaciones que poseen sobre el tema en cuestión.

author_df <- author_df[order(author_df$Frequency, decreasing = TRUE),]

# Eliminamos los nombres de las filas para que, al ordenar el dataframe por alguna de
# sus columnas, el nombre de la fila se vuelva a generar y así se represente de manera
# más ordenada en la aplicación web.

rownames(author_df) <- NULL

# Incluimos en el 'server' la información sobre los autores ordenados de manera decreciente
# por el número de publicaciones que poseen sobre el tema en cuestión.

###### Número de artículos publicados cada año.

# Extraemos el año de publicación de los artículos con la función YearArticleDate() 
# para mostrar esta información en la página web. 

year_article_date <- data.frame(table(YearArticleDate(EUtilsGet_my_query)))

colnames(year_article_date) <- c("Year", "Frequency")

# Incluimos en el 'server' la información sobre el año de publicación de los artículos. 

###### Principales revistas donde se han publicado los artículos del corpus primario.

# Empleamos de nuevo la función batch_pubmed_download(), pero, en este caso, usamos el 
# formato 'abstract' para guardar los datos como archivos de texto, ya que a continuación
# usaremos la función readabs() del paquete pubmed.mineR para crear un objeto de clase 
# 'Abstract'. 

my_abstracts_download <- batch_pubmed_download(pubmed_query_string = my_query, 
                                               dest_dir = my_dir, format = "abstract")

append_txt_batch <- function(my_abstracts_download, easyPubMed_data_final) {
  # Definimos la función append_txt_batch(), que crea un archivo de texto nuevo y 
  # añade el contenido de los archivos obtenidos por la función batch_pubmed_download()
  # a este ("easyPubMed_data_final" es una variable de tipo carácter que corresponde
  # con el nombre que queremos ponerle al archivo de texto final).
  # Creamos un archivo de texto nuevo con la función file.create(). 
  file.create(paste(my_dir, "/", easyPubMed_data_final, ".txt", sep = ""))
  # Iteramos sobre la variable my_abstracts_download, que contiene los archivos de 
  # texto descargados con la función batch_pubmed_download(), para añadir su contenido
  # al archivo creado anteriormente con la función file.append(). 
  for (batch_file in my_abstracts_download) {
    file.append(paste(my_dir, "/", easyPubMed_data_final, ".txt", sep = ""), 
                paste(my_dir, "/", batch_file, sep = ""))
  }
}

# Aplicamos la función append_txt_batch() definida anteriormente a nuestros datos:

append_txt_batch(my_abstracts_download, "easyPubMed_data_final")

# A continuación, utilizamos la función readabs() del paquete pubmed.mineR para crear 
# un objeto de clase 'Abstract' y un archivo de texto con encabezados ('Journal', 
# 'Abstract' y 'PMID') delimitados por tabulaciones (con el nombre de 'newabs.txt').
# Como hemos mencionado, tras concatenar los archivos descargados, en este caso, 
# "easyPubMed_data_final.txt" (es el nombre que le hemos puesto) contiene toda la 
# información.

abstract_Object <- readabs(paste(my_dir, "/easyPubMed_data_final.txt", sep = ""))

# Los objetos de clase 'Abstract' contienen tres propiedades, 'Journal', 'Abstract' 
# y 'PMID', para almacenar resúmenes de PubMed. En particular, 'Journal' es un objeto 
# de clase 'character' que almacena las revistas de los resúmenes de PubMed, 'Abstract'
# es un objeto de clase 'character' que almacena los resúmenes de PubMed, y 'PMID' es 
# un objeto de clase 'numeric' que almacena los PMIDs de los resúmenes de PubMed. Para
# acceder a estas propiedades, usamos '@' junto con dicha propiedad a continuación del
# nombre del objeto de clase 'Abstract'.

# A continuación, hallamos las principales revistas donde se han publicado los artículos
# del corpus primario. Para ello, utilizamos la propiedad 'Journal' del objeto de clase 
# 'Abstract'. Sin embargo, este elemento posee más información aparte de la revista, 
# por lo que usamos una expresión regular para seleccionar únicamente las revistas.

pattern <- '\\d+\\.\\s(.+?)\\.'

# La expresión regular '\\d+\\.\\s(.+?)\\.' se utiliza para buscar patrones en cadenas 
# de texto que contengan un número seguido de un punto, luego uno o más espacios en blanco,
# y finalmente capturar el texto que sigue hasta el siguiente punto. En particular: '\\d+' 
# (coincide con uno o más dígitos (números)), '\\.\\s' (coincide con un punto seguido de 
# uno o más espacios en blanco. El punto (\\.) y el espacio (\\s) están precedidos por 
# dos barras invertidas porque son caracteres especiales en las expresiones regulares y
# necesitan ser escapados), '(.+?)' (este es un grupo de captura que se utiliza para 
# capturar cualquier cosa (uno o más caracteres; en particular, '.+?' significa "cualquier 
# caracter, uno o más, pero tan pocos como sea posible". Esto asegura que se capture el 
# menor número de caracteres posible hasta que se satisfaga la siguiente parte de la 
# expresión regular, que como vemos a continuación, es que coincida con un punto), 
# y '\\.' (coincide con un punto).

# str_match() del paquete stringr se utiliza para aplicar esta expresión regular a
# las cadenas de texto contenidas en abstract_Object@Journal. Luego, '[, 2]' se utiliza 
# para acceder al contenido capturado por el grupo de captura, que es el texto que
# viene después del número y el punto inicial.

journals_pattern <- str_match(abstract_Object@Journal, pattern)[,2]

# Transformamos el vector de tipo carácter a un dataframe y cambiamos el nombre de 
# las columnas. 

journals_df <- data.frame(table(journals_pattern))

colnames(journals_df) <- c("Journal", "Frequency")

# Ordenamos el dataframe de manera decreciente por la columna 'Frequency' con la
# función order(). 

journals_df <- journals_df[order(-journals_df$Frequency),]

rownames(journals_df) <- NULL

###### Artículos: lista de los artículos disponibles que cumplen con los valores 
# especificados por el usuario para los factores influyentes. 

# Utilizamos el corpus con las modificaciones 'removePunctuation' y 'stripWhitespace' 
# (no utilizamos la derivación ya que si no tendríamos que aplicarla también a las
# palabras especificadas por el usuario).

my_corpus_tm_raw <- tm_map(my_corpus_tm_raw, removePunctuation)

my_corpus_tm_raw <- tm_map(my_corpus_tm_raw, stripWhitespace)

# Creamos la matriz de términos-documentos.

my_tdm_raw <- TermDocumentMatrix(my_corpus_tm_raw)

# Transformamos nuestra matriz en un dataframe con la función as.data.frame(), 
# transfórmandola previamente en una matriz con la función as.matrix().

my_matrix_tdm_raw <- as.matrix(my_tdm_raw)

my_df_tdm_raw <- as.data.frame(my_matrix_tdm_raw)

find_document_file_names <- function(search_string) {
  # Definimos la función find_document_file_names(), que proporciona los documentos 
  # que poseen los términos especificados por el usuario ('search_string') (estos 
  # documentos deben contener todos los términos especificados por el usuario).
  # Creamos un vector vacío. 
  document_file_names <- NULL
  # Separamos la cadena de búsqueda por espacios para obtener las distintas palabras
  # con la función strsplit().
  words <- unlist(strsplit(search_string, " "))
  # Iteramos sobre el vector de palabras.
  for (word in words) {
    # Ignoramos cadenas vacías (por ejemplo, esto sucederá si el usuario escribe 
    # espacios en blanco extra).
    if (word != "") {
      # Si la palabra no existe en el corpus primario, dejamos de buscar.
      # Reseteamos los documentos encontrados, por si acaso ya se hubiesen
      # encontrado algunos con la palabra anterior. 
      if (!word %in% rownames(my_df_tdm_raw)) {
        document_file_names <- NULL
        break
      }
      # En caso contrario, obtenemos los documentos que contienen dicha palabra,
      # fijándonos que la palabra salga por lo menos una vez en el documento (es decir,
      # que la frecuencia de dicha palabra en el documento sea mayor que 0). Para ello,
      # empleamos las funciones colnames(), subset() y colSums(). 
      word_df <- my_df_tdm_raw[word,]
      word_documents <- colnames(subset(word_df,
                                        select = c(colSums(word_df) > 0)))
      # Si es la primera búsqueda (el vector 'document_file_names' estará vacío), guardamos 
      # todos los documentos.
      if (is.null(document_file_names)) {
        document_file_names <- word_documents
      } 
      # Si no es la primera búsqueda, guardamos la intersección de los documentos
      # anteriores con los documentos encontrados ahora. Para ello, empleamos la función
      # intersect() (estamos interesados en los documentos que contienen todos los 
      # términos especificados por el usuario).
      else {
        document_file_names <- intersect(document_file_names, word_documents)
      }
      # Si hemos dejado de tener documentos encontrados, dejamos de buscar (esto hará
      # que la función sea más eficiente; para ello, empleamos la función length()). 
      if (length(document_file_names) == 0) {
        break
      }
    }
  }
  # Devolvemos los documentos encontrados. 
  return(document_file_names)
}

make_pmid_title_map <- function(my_abstracts_download_xml) {
  # Definimos la función make_pmid_title_map(), que crea un diccionario asociando 
  # los PMID de los artículos con sus títulos correspondientes.
  # Creamos un diccionario vacío con la función hash() del paquete hash.
  pmid_title_map <- hash()
  # Iteramos sobre los documentos descargados anteriormente con la función 
  # batch_pubmed_download() del paquete easyPubMed.
  for(fileName in my_abstracts_download_xml) {
    # Sacamos el camino completo al fichero XML que hemos descargado.
    filePath <- paste(my_dir, fileName, sep = "/")
    # Con la función xmlParse() del paquete XML, leemos el fichero para poder usar 
    # los datos en R.
    xml <- xmlParse(filePath)
    # Sacamos todos los distintos nodos de <PubmedArticle> que se encuentran dentro de
    # <PubmedArticleSet> con la función getNodeSet().
    pubmedArticles <- getNodeSet(xml, "/PubmedArticleSet/PubmedArticle")
    for(pubmedArticleIndex in 1:length(pubmedArticles)) {
      pubmedArticle <- pubmedArticles[[pubmedArticleIndex]]
      # Sacamos el nodo donde se encuentra el PMID.
      pmid <- getNodeSet(pubmedArticle, "MedlineCitation/PMID")
      # Sacamos el texto del nodo con la función xmlValue().
      pmidText <- xmlValue(pmid[[1]])
      # Sacamos el nodo donde se encuentra el título del artículo.
      articleTitle <- getNodeSet(pubmedArticle, "MedlineCitation/Article/ArticleTitle")
      # Sacamos el texto del nodo.
      articleTitleText <- xmlValue(articleTitle[[1]])
      # Añadimos al diccionario la asociación del PMID artículo con su título 
      # correspondiente. 
      pmid_title_map[[pmidText]] <- articleTitleText
    }
  }
  # Devolvemos el diccionario. 
  return(pmid_title_map)
}

# Creamos el diccionario con la función make_pmid_title_map() definida anteriormente:

pmid_title_map <- make_pmid_title_map(my_abstracts_download_xml)

link_for_document <- function(PMID_document){
  # Definimos la función link_for_document(), que genera el HTML del hiperenlace para PubMed
  # de un documento dado a partir de su PMID. 
  # Para ello, empleamos la función sprintf(), que devuelve un vector de caracteres que
  # contiene una combinación formateada de texto y valores de variables, para generar 
  # el HTML. 
  # La etiqueta HTML '<a>' define un hiperenlace, y tiene la siguiente sintaxis:
  # '<a href = "url"> texto del enlace </a>' (como 'texto del enlace', aparecerá el
  # PMID del documento). Por último, especificamos el destino del enlace con el atributo
  # 'target'; en particular, el enlace se abriá en una ventana nueva ('target = "_blank"').
  link <- sprintf('<a href = "https://pubmed.ncbi.nlm.nih.gov/%s" target="_blank">%s</a>', 
                  PMID_document, PMID_document)
  # Devolvemos el HTML. 
  return(link)
}

get_document_titles <- function(document_file_names, pmid_title_map) {
  # Definimos la función get_document_titles(), que extrae los títulos de los artículos
  # en función de los nombres de los ficheros que se crearon con la función 
  # separate_and_store_batch() (estos nombres contienen el PMID del artículo en cuestión).
  # Creamos un dataframe vacío con la función data.frame().
  pmid_titles <- data.frame(matrix(ncol = 2, nrow = 0))
  # Definimos los nombres de las columnas con la función colnames(). 
  colnames(pmid_titles) <- c("PMID", "Title")
  # Iteramos sobre el vector de documentos obtenido con la función find_document_file_names(),
  # que contiene los documentos que poseen los términos especificados por el usuario 
  # ('search_string') (estos documentos están denominados por el PMID del artículo
  # y ".txt").
  for (file_name in document_file_names) {
    # Eliminamos ".txt" de los nombres de los documentos para sacar el PMID. Para 
    # ello, utilizamos la función str_remove_all().
    pmid <- str_remove_all(file_name, ".txt")
    # Generamos los hiperenlaces para cada documento aplicando la función link_for_document()
    # definida anteriormente. 
    link_pmid <- link_for_document(pmid)
    # Buscamos el título asociado al PMID empleando el diccionario creado anteriormente
    # con la función make_pmid_title_map().
    title <- pmid_title_map[[pmid]]
    # Añadimos una fila nueva en el dataframe con el PMID y el título del artículo.
    pmid_titles[nrow(pmid_titles) + 1,] <- c(link_pmid, title)
  }
  # Devolvemos el dataframe con los PMID (y sus hiperenlaces a pubmed) y títulos de 
  # los artículos.
  return(pmid_titles)
}

# Incluimos en el 'server' la herramienta para filtrar los artículos por los términos 
# introducidos por el usuario empleando las funciones descritas anteriormente.

###### Clusters: información general y agrupación por propagación de afinidad.

# A continuación, proporcionamos visualizaciones de los datos en la aplicación web 
# que muestren qué documentos se agrupan por propagación de afinidad y cuántos grupos 
# o clusters se forman a partir del corpus primario. 

# Utilizaremos el paquete slam de R para calcular la medida del coseno entre los documentos
# del corpus primario, y posteriormente aplicaremos la función apcluster() del paquete
# apcluster a esta matriz de similitud. 

# Calculamos la similitud del coseno entre documentos siguiendo su definición matemática
# (es decir, su fórmula). 

# Comenzamos calculando el numerador: 
# 'crossprod_simple_triplet_matrix(my_tdm)' calcula el producto escalar entre los 
# documentos representados en la matriz término-documento 'my_tdm' creada anteriormente 
# con el paquete tm (el producto escalar entre dos documentos en el contexto de una
# matriz término-documento implica multiplicar las frecuencias de los términos en esos
# documentos y sumar esos productos; este procedimiento se repite para cada par de 
# documentos de la matriz). 

# En cuanto al denominador, encontramos: 
# 'col_sums(my_tdm^2) %*% t(col_sums(my_tdm^2))'. En particular, 'col_sums(my_tdm^2)'
# calcula las sumas de cuadrados de las columnas de la matriz término-documento. 
# 't(col_sums(my_tdm^2))' transpone el vector resultante. La transposición se realiza
# para que el vector se convierta en una matriz de 1 fila y n columnas, donde n es el
# número de términos en la matriz de términos-documentos. 
# 'col_sums(my_tdm^2) %*% t(col_sums(my_tdm^2))' calcula el producto exterior entre
# el vector original y su transpuesto. Esto da como resultado una matriz de tamaño
# nxn, donde cada elemento (i,j) representa el producto de la suma de cuadrados de
# la columna i con la suma de cuadrados de la columna j (siendo i y j dos documentos 
# del corpus).

cosine_dist_doc_slam <- crossprod_simple_triplet_matrix(my_tdm)/
  (sqrt(col_sums(my_tdm^2) %*% t(col_sums(my_tdm^2))))

# A continuación, el objetivo es formar clústeres con los documentos del corpus primario 
# empleando el paquete apcluster.

# La función apcluster() del paquete apcluster recibe, entre otros, los siguientes 
# argumentos: función adicional para manipular las similitudes (vamos a usar negDistMat()
# con 'r = 1') y 'x' (los datos que van a agruparse; en este caso, empleamos la matriz
# de similitud de coseno entre documentos creada anteriormente). 

cosine_dist_doc_slam_apcluster <- apcluster(negDistMat(r = 1), 
                                            x = cosine_dist_doc_slam)

# A continuación, vamos a representar un gráfico para cada ejemplar y sus clusters.
# Para obtener la información sobre los ejemplares y clusters:

exemplars <- cosine_dist_doc_slam_apcluster@exemplars

cluster_info <- cosine_dist_doc_slam_apcluster@clusters

# En ambos casos, eliminaremos la extensión ".txt" de los nombres. En el caso de los
# ejemplares:

exemplars <- str_remove_all(rownames(data.frame(exemplars)), ".txt")

# En el caso de cluster_info, lo haremos durante la representación. 

# Continuamos con el dendrograma de los ejemplares para ver qué clusters se asemejan más. 

# Creamos la matriz 'DocumentTerm' con la función DocumentTermMatrix() del paquete tm.

my_dtm <- DocumentTermMatrix(my_corpus_tm)

# Transformamos nuestra matriz en un dataframe con la función as.data.frame().

my_df_dtm <- as.data.frame(as.matrix(my_dtm))

# Seleccionamos los ejemplares de la matrix DocumentTerm. 

my_df_dtm_exemplars <- my_df_dtm[exemplars,]

# Al igual que hicimos anteriormente para los términos, usamos la función dist() del 
# paquete proxy y el argumento 'method = "cosine"', junto con '1 - distancia del coseno' 
# para obtener la similitud del coseno.

cosine_dist_exemplars <- 1 - proxy::dist(my_df_dtm_exemplars, method = "cosine")

# Finalmente, empleamos la función hclust() para generar el dendrograma. 

# Luego, en el 'server', utilizaremos la función plot() para representarlo.

hclust_cosine_dist_exemplars <- hclust(cosine_dist_exemplars, method = "ward")

# Para ver el tema que caracteriza a cada cluster, podemos analizar cuáles son
# las palabras clave o keywords de los ejemplares.

# Anteriormente usamos el paquete RISmed. En este paquete encontramos la función 
# Keywords(), que extrae las palabras clave de cada registro del objeto de clase 
# Medline, que denominamos 'EUtilsGet_my_query'. En particular, extraemos las palabras
# clave de los ejemplares de los clusters obtenidos anteriormente (utilizando sus PMID).

keywords_exemplars <- Keywords(EUtilsGet_my_query)[exemplars]

make_exemplar_keywords_df <- function(keywords_exemplars) {
  # Definimos la función make_exemplar_keywords_df() para transformar el resultado
  # obtenido con la función Keywords() del paquete RISmed en un dataframe con las 
  # columnas "Exemplar" y "Keywords". 
  # Creamos un dataframe con dos columnas denominadas "Exemplar" y "Keywords".
  exemplar_keywords_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(exemplar_keywords_df) <- c("Exemplar", "Keywords")
  # Iteramos sobre el resultado obtenido con la función Keywords() del paquete RISmed.
  for (i in 1:length(keywords_exemplars)) {
    # Guardamos el nombre del documento actual (corresponde con el PMID).
    exemplar <- names(keywords_exemplars[i])
    # Guardamos las keywords del documento actual. 
    keywords <- as.character(keywords_exemplars[i][[1]])
    # Creamos un string vacío para los casos en los que no encontremos keywords para
    # un documento. 
    keywords_as_string <- ""
    # Si encontramos keywords para el documento actual (es decir, 'keywords' no es "NA"),
    # usamos la función lapply() para devolver las diferentes keywords entre comillas,
    # y la función paste() para concatenerlas. Si no, devolvemos 'No data available'.
    if (!is.na(keywords[1])) {
      keywords <- lapply(keywords, shQuote)
      keywords_as_string <- paste(keywords, collapse = ', ')
    } else {
      keywords <- lapply(keywords, shQuote)
      keywords_as_string <- c("No data available")
    }
    # Añadimos al dataframe una fila nueva formada por el documento ejemplar y sus keywords.
    exemplar_keywords_df[nrow(exemplar_keywords_df) + 1,] <- c(exemplar, keywords_as_string)
  }
  # Devolvemos el dataframe. 
  return(exemplar_keywords_df)
}

# Aplicamos la función anterior a nuestros datos:

exemplar_keywords_df <- make_exemplar_keywords_df(keywords_exemplars)

# Añadimos dicha información en forma de tabla en el 'server' para que el usuario pueda
# elegir posteriormente el cluster en el que está interesado, ver su gráfica y los 
# documentos que forman parte de dicho cluster. 

# Con estos datos, añadimos en el 'server' también un desplegable para que el usuario 
# elija el ejemplar en el que está interesado para ver el cluster que representa 
# (gráfica, documentos del cluster, etc).

# Llevamos a cabo la representación del ejemplar con los documentos de su cluster 
# que el usuario haya elegido.

# Hallamos el número total de clusters.

num_clusters <- length(cluster_info)

# Creamos una lista para almacenar posteriormente los índices de los documentos 
# de cada cluster.

clusters_index <- list()

# Llenamos la lista con los índices de los documentos de cada cluster.

for (i in 1:num_clusters) {
  clusters_index[[i]] <- as.data.frame(cluster_info[[i]])[,1]
}

plot_for_exemplar_name <- function(exemplar_name){
  # Definimos la función plot_for_exemplar_name(), que, dado un ejemplar, representa
  # su cluster. 
  # Obtenemos el índice o posición del ejemplar.
  exemplar_index <- which(exemplars == exemplar_name)
  # Obtenemos los índices de los documentos del cluster del ejemplar.
  cluster_index <- clusters_index[[exemplar_index]]
  # Creamos el gráfico para el ejemplar con los documentos del cluster que representa.
  # Para ello, usamos las similitudes del coseno entre documentos halladas anteriormente. 
  plot(cosine_dist_doc_slam[cluster_index,], 
       col = exemplar_index, pch = 16,
       main = paste("Ejemplar", exemplar_name, "y documentos del cluster", exemplar_index),
       xlab = "", ylab = "")
  # Agregamos los nombres de los documentos, que corresponden con sus PMID. Para ello,
  # usamos la función text(). Eliminamos de los nombres de los documentos del cluster
  # la terminación .txt con la función sub(). 
  text(cosine_dist_doc_slam[cluster_index,], 
       labels = sub("\\.txt$", "", rownames(cosine_dist_doc_slam))[cluster_index], cex = 1, 
       col = "black")
}

# Mostramos una tabla con los documentos del cluster seleccionado por el usuario (incluido 
# el ejemplar), de forma que el usuario puede hacer clic sobre cualquiera de ellos y 
# acceder a PubMed para leer dicho artículo. Para ello, definimos la siguiente función. 

table_for_exemplar_name <- function(exemplar_name){
  # Definimos la función table_for_exemplar_name(), que, dado un ejemplar, nos devuelve
  # un dataframe con el ejemplar y los documentos de su cluster, cada uno de ellos con
  # su hiperenlace a PubMed. 
  # Obtenemos el índice o posición del ejemplar.
  exemplar_index <- which(exemplars == exemplar_name)
  # Obtenemos los índices de los documentos del cluster del ejemplar seleccionado.
  cluster_index <- clusters_index[[exemplar_index]]
  # Obtenemos los nombres de los documentos del cluster del ejemplar seleccionado. 
  documents_for_exemplar <- sub("\\.txt$", "", rownames(cosine_dist_doc_slam))[cluster_index]
  # Generamos los hiperenlaces para cada documento aplicando la función link_for_document()
  # definida anteriormente y lapply().
  documents_hyperlinks <- lapply(documents_for_exemplar, link_for_document)
  # Usamos la función paste() para concatenar los hiperenlaces del cluster del ejemplar 
  # seleccionado. 
  documents_hyperlinks <- paste(documents_hyperlinks, collapse = ', ')
  # Creamos un dataframe con dos columnas, llamadas 'Exemplar' y 'Documents'.
  exemplar_cluster_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(exemplar_cluster_df) <- c("Exemplar", "Documents")
  # Añadimos a dicho dataframe el nombre (PMID) del ejemplar seleccionado y los documentos de su
  # cluster. 
  exemplar_cluster_df[nrow(exemplar_cluster_df) + 1,] <- c(exemplar_name, documents_hyperlinks)
  # Devolvemos dicho dataframe con los hiperenlaces a pubmed. 
  return(exemplar_cluster_df)
}

###### Factores influyentes. 

# Es interesante comenzar con la función findAssocs() del paquete tm, que encuentra 
# asociaciones en una matriz documento-término o término-documento. En particular,
# recibe los siguientes argumentos: 'x' ('DocumentTermMatrix' o 'TermDocumentMatrix'), 
# 'terms' (un vector con los términos en los que estemos interesados) y 'corlimit' 
# (un vector numérico de la misma longitud que el vector de 'terms' para los límites 
# de correlación inferiores de cada término en el rango de cero a uno). 

my_tdm_Assocs <- findAssocs(x = my_tdm, terms = c("strain", "age", "gender", "sex",
                                                  "area", "stress", "test"),
                            corlimit = c(0.4, 0.4, 0.4, 0.4, 0.2, 0.19, 0.2))

# Nos ayudaremos de estos resultados para elegir los términos entre los cuáles queremos
# examinar la fuerza de asociación utilizando la función de similitud del coseno y 
# el análisis semántico latente (LSA). Además, podemos añadir otros términos aunque 
# no hayan salido en el resultado anterior, como las cepas más ampliamente utilizadas.

influencing_factors <- c("strain", "balbc", "c57bl", "age", "old", "adult", "young", 
                         "neonate", "male", "female", "suprachiasmat", "septal",
                         "nigra", "brain", "amygdala", "tegment", "infralimb", 
                         "hypothalamus", "limbic", "mesencephalon", "hippocampus", 
                         "social", "defeat", "restraint", "psycholog", "chronic", 
                         "terrifiedsound", "acute", "separation", "maternal",
                         "test", "forc", "swim", "fst", "suspen", "tail", "tst", 
                         "open", "field", "maze", "oft", "elev", "plus", "sucros")

# Antes de continuar, vamos a emplear la función stemCompletion() del paquete tm para
# completar las palabras derivadas. Esta función recibe los siguientes argumentos: 
# 'x' (vector de caracteres con las palabras derivadas que queremos completar), 
# 'dictionary' (un Corpus donde buscar las posibles palabras de las que provienen estas
# derivaciones) y 'type' (una cadena de caracteres indicando las heurísticas a utilizar: 
# en este caso, utilizamos 'type = "prevalent"', que toma la coincidencia más frecuente
# encontrada).

influencing_factors_with_stemCompl <- stemCompletion(x = influencing_factors, 
                                                dictionary = my_corpus_tm_raw, 
                                                type = "prevalent")

get_influencing_factors_stemCompl <-
  function(influencing_factors_with_stemCompl) {
    # Definimos la función get_influencing_factors_stemCompl() para crear un vector con las
    # palabras completas tras aplicar la función stemCompletion().
    # Creamos un vector vacío donde guardaremos los resultados.
    influencing_factors_stemCompl <- c()
    # Iteramos sobre el resultado de la función stemCompletion().
    for (i in 1:length(influencing_factors_with_stemCompl)) {
      stemmed <- names(influencing_factors_with_stemCompl)[i]
      completed <- influencing_factors_with_stemCompl[i][[1]]
      # En el caso del término "oft", se completa a "often", y nos referimos a "oft" como
      # las siglas del test 'open field maze'. Por tanto, hacemos una excepción con este
      # término y lo guardamos tal y como está.
      # Además, si el número de caracteres del término a completar es mayor que el número
      # de caracteres del término completado, nos quedamos con el término original.
      if (stemmed == "oft" || nchar(stemmed) > nchar(completed)) {
        influencing_factors_stemCompl <- append(influencing_factors_stemCompl, stemmed)
      }
      # Si no, nos quedamos con el término completado.
      else {
        influencing_factors_stemCompl <- append(influencing_factors_stemCompl, completed)
      }
    }
    # Devolvemos el vector con las palabras completas.
    return(influencing_factors_stemCompl)
  }

# Aplicamos la función anterior a nuestros datos:

influencing_factors_stemCompl <- get_influencing_factors_stemCompl(influencing_factors_with_stemCompl)

# A continuación, queremos añadir en la aplicación web la opción de que el usuario pueda 
# añadir a este vector uno o más términos separados por un espacio. Posteriormente, el 
# usuario podrá seleccionar el término que desee entre los presentes en dicho vector
# para examinar la fuerza de asociación entre el término seleccionado y el resto de 
# términos del vector (podrá seleccionar uno de los términos que acaba de incluir en 
# el vector u otro de los que ya estaba). 

include_factor <- function(new_factor_s) {
  # Definimos la función include_factor() para añadir uno o más términos al vector
  # de términos 'influencing_factors_stemCompl'.
  new_influencing_factors_stemCompl <- influencing_factors_stemCompl
  # Debido a que podemos recibir más de un término, separamos el string recibido
  # como argumento en los diferentes términos. 
  factor_s <- unlist(strsplit(new_factor_s, " "))
  # Iteramos sobre el vector 'factor_s', que puede contener uno o más términos.
  for (factor in factor_s) {
    # Si el término se encuentra entre los términos presentes en el corpus primario 
    # (es decir, en el dataframe 'my_df_tdm_raw'):
    if (factor %in% row.names(my_df_tdm_raw)){
      # Y si, además, dicho término no se encuentra en el vector 'influencing_factors_stemCompl', 
      # lo añadimos.
      if (factor != "" && !(factor %in% influencing_factors_stemCompl)) {
        new_influencing_factors_stemCompl <- append(new_influencing_factors_stemCompl, factor)
      } 
    }
  }
  # Devolvemos el vector actualizado 'new_influencing_factors_stemCompl'.
  return(new_influencing_factors_stemCompl)
}

cos_sim_calc_influencing_factors <- function(new_influencing_factors_stemCompl){
  # Definimos la función cos_sim_calc_influencing_factors(), que lleva a cabo el análisis
  # semántico latente (LSA) entre los términos del vector que recibe como argumento, y calcula
  # la similitud del coseno entre pares de términos. 
  # Creamos la matriz de términos-documentos con el paquete pubmed.mineR, en particular, 
  # con la función tdm_for_lsa(), la cual recibe los argumentos 'object' (objeto de 
  # clase 'Abstracts') e 'y' (vector de caracteres especificando los términos). Anteriormente
  # creamos el objeto de clase 'Abstracts', que denominamos 'abstract_Object'
  my_tdm_pubmedmineR_influ_factors <- tdm_for_lsa(object = abstract_Object, y = new_influencing_factors_stemCompl)
  # Para llevar a cabo el análisis semántico latente, usamos la función lsa() del paquete
  # lsa. Esta función recibe la matriz TermDocument y el número de valores singulares 
  # óptimo (generado por la función dimcalc_share()), y genera un nuevo espacio vectorial
  # donde las nuevas dimensiones corresponden con el número de valores singulares y el 
  # número de palabras analizado.
  lsa_tdm_pubmedmineR_influ_factors <- lsa(my_tdm_pubmedmineR_influ_factors, dims = dimcalc_share())
  # Convertimos el espacio latente semántico en una nueva matriz de términos-documentos
  # con la función as.textmatrix().
  latent_semantic <- as.textmatrix(lsa_tdm_pubmedmineR_influ_factors)
  # Con la función associate() del paquete lsa hallamos la similitud entre términos 
  # usando la similitud del coseno entre pares de términos. Elegimos como umbral ('threshold') 
  # 0 para que aparezcan todas las asociaciones entre los términos elegidos. 
  cos_influ_factors_pubmedmineR <- lapply(new_influencing_factors_stemCompl, function(x){
    associate(latent_semantic, x, measure = "cosine", threshold = "0")})
  names(cos_influ_factors_pubmedmineR) <- new_influencing_factors_stemCompl
  # Devolvemos el resultado, es decir, la similitud del coseno entre pares de términos.
  return(cos_influ_factors_pubmedmineR)
}

find_assocs_influ_factor_selec <- function(influ_factor_selec, cos_influ_factors_pubmedmineR) {
  # Definimos la función find_assocs_influ_factor_selec() para generar un dataframe a partir
  # de los valores de similitud del coseno entre pares de términos obtenidos con la función 
  # cos_sim_calc_influencing_factors() para un término dado (factor influyente). Para ello,
  # esta función recibe los argumentos 'influ_factor_selec' (término elegido para generar
  # el dataframe con los valores de similitud de este término con el resto de ellos; este
  # término podrá elegirlo el usuario) y 'cos_influ_factors_pubmedmineR' (resultado de la 
  # función cos_sim_calc_influencing_factors()). 
  # A continuación, hallamos el índice o posición del factor influyente que recibe la función como 
  # argumento en el resultado obtenido con la función cos_sim_calc_influencing_factors(). 
  # Si no se selecciona ningún factor, se calcula por defecto con el que aparece en primera
  # posición (índice 1)
  if (is.null(influ_factor_selec)) {
    influ_factor_selec_index <- 1
  } else {
    influ_factor_selec_index <- which(names(cos_influ_factors_pubmedmineR) == influ_factor_selec)
  }
  # Creamos el dataframe con dos columnas, los términos y los valores de similitud del conseno
  # con respecto al término que recibe la función como argumento. 
  influ_factor_selec_df <- data.frame(
    words <- names(cos_influ_factors_pubmedmineR[influ_factor_selec_index][[1]]),
    assocs <- cos_influ_factors_pubmedmineR[influ_factor_selec_index][[1]]
  )
  rownames(influ_factor_selec_df) <- NULL
  colnames(influ_factor_selec_df) <- c("words", "assocs")
  # Devolvemos el dataframe. 
  return(influ_factor_selec_df)
}

ggplot_assocs_influ_factor_selec <- function(influ_factor_selec, influ_factor_selec_df) {
  # Definimos la función ggplot_assocs_influ_factor_selec() para generar un histograma
  # con la función ggplot() del paquete ggplot2 a partir del dataframe devuelto por la
  # función find_assocs_influ_factor_selec(). 
  # Creamos el gráfico con la función ggplot().
  ggplot(influ_factor_selec_df, aes(x = reorder(words, assocs), y = assocs)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    coord_flip() +  labs(title = influ_factor_selec, x = "Words", y = "Cosine similarity") +
    theme_minimal()
} 

###### Evolución de la información. Para terminar, vamos a incluir en la aplicación web
# una herramienta para que el usuario pueda llevar a cabo un análisis de la evolución de
# la información a lo largo del tiempo. 

# Para ello, los términos siempre serán los mismos, 'mice', 'stress' y 'depression',
# pero podrá seleccionar los periodos de fechas que desee.

# En la aplicación web aparecerán dos calendarios por defecto donde el usuario podrá seleccionar
# dos periodos de tiempo en los que esté interesado. A continuación, podrá seleccionar añadir
# más periodos de tiempo o hacer el análisis solo con dos. 

# Las querys serán del tipo:
# "mice[Title/Abstract] AND stress[Title/Abstract] AND depression[Title/Abstract] AND aaaa/mm/dd:aaaa/mm/dd [DP]".

# Creamos un contador para llevar la cuenta de los 'date widgets' (creados en la interfaz 
# del usuario, como veremos posteriormente, por la función dateRangeInput()). Para ello,
# definimos la variable 'dates_counter'. Como mencionamos, aparecerán por defecto dos
# calendarios en la aplicación web. 

dates_counter <- 2

get_corpus_for_dates <- function(dates) {
  # Definimos la función get_corpus_for_dates(), que, dado un periodo de fechas (cadena 
  # del tipo aaaa/mm/dd:aaaa/mm/dd), crea un Corpus. 
  # Creamos la query.
  query <- sprintf("mice[Title/Abstract] AND stress[Title/Abstract] AND depression[Title/Abstract] AND %s [DP]",
                   dates)
  # Empleamos la función batch_pubmed_download() del paquete easyPubMed para descargarnos
  # los artículos en formato 'xml'.
  dates_for_file <- gsub(":", "_", gsub("/", "", dates))
  dest_file_prefix <- paste("easyPubMed_data_xml_", dates_for_file, sep = "")
  download <- batch_pubmed_download(pubmed_query_string = query, 
                                    dest_file_prefix = dest_file_prefix, 
                                    dest_dir = my_dir, format = "xml")
  # Aplicamos la función separate_and_store_batch(), definida anteriormente, para separar y
  # guardar en archivos de texto separados, denominados con el PMID del artículo en cuestión,
  # el título y abstract (si existe) de cada documento.
  output_dir <- paste("abstracts_xml_", dates_for_file, sep = "")
  separate_and_store_batch(download, output_dir)
  # Creamos el Corpus con la función Corpus del paquete tm. 
  corpus <- Corpus(x = DirSource(paste(my_dir, output_dir, sep = "/"), 
                                 pattern = ".txt"))
  # Devolvemos el Corpus. 
  return(corpus)
}

create_TermDocumentMatrix_from_corpus <- function(my_corpus_tm) {
  # Definimos la función create_TermDocumentMatrix_from_corpus(), que, dado un Corpus 
  # del paquete tm, aplica transformaciones y crea la matriz de términos-documentos. 
  # Aplicamos las transformaciones 'removePunctuation', 'stripWhitespace', 'removeNumbers',
  # 'stemDocument' y 'removeWords' con la función tm_map() del paquete tm. 
  new_corpus <- tm_map(my_corpus_tm, removePunctuation)
  new_corpus <- tm_map(new_corpus, stripWhitespace)
  new_corpus <- tm_map(new_corpus, removeNumbers)
  new_corpus <- tm_map(new_corpus, stemDocument)
  new_corpus <- tm_map(new_corpus, removeWords, stopwords("english"))
  # Creamos la matriz TermDocument con la función TermDocumentMatrix() de este mismo paquete.  
  my_tdm <- TermDocumentMatrix(new_corpus)
  # Devolvemos la matriz TermDocument. 
  return(my_tdm)
}

get_words_for_corpus <- function(corpus) {
  # Definimos la función get_words_for_corpus() que, dado un Corpus (del paquete tm), 
  # devuelve los 200 términos más frecuentes.
  # Creamos la matriz de términos-documentos empleando la función
  # create_TermDocumentMatrix_from_corpus() definida anteriormente.
  tdm <- create_TermDocumentMatrix_from_corpus(corpus)
  # Convertimos dicha matriz a dataframe. 
  dt_tdm <- as.data.frame(as.matrix(tdm))
  # Sumamos las frecuencias de los términos en todos los documentos con la función rowSums(), 
  # y ordenamos los términos por orden descendente de frecuencia con la función sort().
  words <- sort(rowSums(dt_tdm), decreasing = TRUE)
  # Devolvemos los 200 términos más frecuentes.
  return(words[1:200])
}

get_words_freqs_for_dates <- function(dates_inputs) {
  # Definimos la función get_words_freqs_for_dates() que, dado un vector que contiene
  # los periodos de tiempo seleccionados por el usuario en la aplicación web, devuelve 
  # una lista de dataframes con las palabras y frecuencias de cada periodo (tras eliminar
  # las palabras en común entre periodos). 
  # Creamos una lista vacía donde guardaremos los Corpus (uno para cada periodo).
  corpus_for_dates <- list()
  # Iteramos sobre el vector 'dates_inputs'.
  for (dates in dates_inputs) {
    # Utilizamos la función get_corpus_for_dates() definida anteriormente para crear
    # el Corpus. 
    corpus <- get_corpus_for_dates(dates)
    # Guardamos el Corpus en la lista 'corpus_for_dates'.
    corpus_for_dates <- c(corpus_for_dates, list(corpus))
  }
  # Creamos una lista vacía donde guardaremos los 200 términos más frecuentes de cada Corpus. 
  words_for_dates <- list()
  # Iteramos sobre la lista 'corpus_for_dates'. 
  for (corpus in corpus_for_dates) {
    # Utilizamos la función get_words_for_corpus() definida anteriormente para obtener
    # los 200 términos más frecuentes de cada Corpus.
    words <- get_words_for_corpus(corpus)
    # Guardamos el resultado en la lista 'words_for_dates'.
    words_for_dates <- c(words_for_dates, list(words))
  }
  # Creamos una lista vacía donde guardaremos los dataframes con los términos y sus
  # frecuencias de cada Corpus. 
  df_words_for_dates <- list()
  # Iteramos sobre la lista 'words_for_dates'. 
  for (words in words_for_dates) {
    df <- data.frame(word = names(words), freq = words)
    # Guardamos el resultado en la lista 'df_words_for_dates'.
    df_words_for_dates <- c(df_words_for_dates, list(df))
  }
  # Buscamos las palabras comunes entre los diferentes periodos de tiempo.
  # Comenzamos inicializando 'common_words' con las palabras del primer periodo de tiempo.
  common_words <- names(words_for_dates[[1]])
  # Iteramos sobre la lista 'words_for_dates' del resto de periodos restantes.
  for (words in words_for_dates[-1]) {
    # En cada iteración, se toma el conjunto actual de palabras y se encuentra la 
    # intersección con el conjunto de palabras comunes ('common_words'). Para ello,
    # empleamos la función intersect(). 
    common_words <- intersect(common_words, names(words))
  }
  # Después de completar el bucle, 'common_words' contendrá las palabras que son comunes
  # en todos los periodos de tiempo. 
  # Creamos una lista vacía donde guardaremos los dataframes con los términos y sus
  # frecuencias de cada Corpus sin las palabras comunes.  
  words_for_dates_without_common_words <- list()
  # Iteramos sobre la lista 'df_words_for_dates'.
  for (df_words in df_words_for_dates) {
    # Seleccionamos las filas donde las palabras no estén en el conjunto de palabras comunes
    # entre los periodos de tiempo. 
    new_df <- df_words[!df_words$word %in% common_words,]
    row.names(new_df) <- NULL
    # Guardamos el resultado en la lista 'words_for_dates_without_common_words'. 
    words_for_dates_without_common_words <- c(words_for_dates_without_common_words, list(new_df))
  }
  # Por último, completamos las palabras derivadas. Para ello, empleamos la función 
  # stemCompletion() del paquete tm. Esta función recibe los argumentos: 'x' (vector 
  # de caracteres con las palabras derivadas que queremos completar), 'dictionary' 
  # (un Corpus donde buscar las posibles palabras de las que provienen estas derivaciones)
  # y 'type' (utilizaremos 'type = "prevalent"'). 
  # Iteramos sobre el vector 'dates_inputs'. 
  for (i in 1:length(dates_inputs)) {
    # Hallamos el Corpus del periodo actual. 
    corpus <- corpus_for_dates[[i]]
    # Guardamos el dataframe con los términos y sus frecuencias sin las palabras comunes
    # del periodo actual. 
    df_words <- words_for_dates_without_common_words[[i]]
    # Guardamos los términos del periodo actual. 
    words <- df_words$word
    # Aplicamos la función stemCompletion() del paquete tm.
    words_with_stemComp <- stemCompletion(x = words, dictionary = corpus, type = "prevalent")
    # Aplicamos la función get_influencing_factors_stemCompl() definida anteriormente
    # para crear un vector con las palabras completas tras aplicar la función stemCompletion().
    words_with_stemComp_clean <- get_influencing_factors_stemCompl(words_with_stemComp)
    # Cambiamos las palabras derivadas por las palabras completas en el dataframe. 
    words_for_dates_without_common_words[[i]]$word <- words_with_stemComp_clean
  }
  # Devolvemos la lista 'words_for_dates_without_common_words'.
  return(words_for_dates_without_common_words)
}


###### Creamos una guía para cada pestaña de la aplicación utilizando la librería Cicerone.
# Para ello, debemos guardar cada guía en una variable distinta, que luego utilizaremos
# en el 'server'.
# Cada guía es creada con la función new() y le añadimos los pasos de cada parte
# que queremos describir de la página con la función step(). Esta última función
# recibe como argumentos el 'id' del atributo de la página, el título del paso de
# la guía y la descripción que aparecerá debajo del título.

guide_home_graphs <- Cicerone$
  new()$
  step(
    "num_words_wordcloud_slider",
    "Wordcloud",
    "Puede seleccionar la cantidad de palabras del corpus primario para mostrar en el wordcloud."
  )$
  step(
    "plot_dendrogram",
    "Dendrograma",
    "Dendrograma de las 100 palabras más frecuentes del corpus primario."
  )

guide_home_authors <- Cicerone$
  new()$
  step(
    "dataTable_freq_authors",
    "Tabla de los autores",
    "Tabla de los autores del corpus primario ordenados por su frecuencia de aparición. Puede buscar por nombre de autor."
  )

guide_home_published_year <- Cicerone$
  new()$
  step(
    "dataTable_year_article_date",
    "Tabla de los años de publicación",
    "Tabla de la cantidad de artículos del corpus primario publicados cada año. Puede buscar por año de publicación."
  )

guide_home_journals <- Cicerone$
  new()$
  step(
    "dataTable_journals",
    "Tabla de las revistas",
    "Tabla de la cantidad de artículos del corpus primario publicados en cada revista. Puede buscar por revista."
  )

guide_articles <- Cicerone$
  new()$
  step(
    "search_string",
    "Búsqueda por palabras",
    "Puede filtrar los artículos del corpus primario que contengan las palabras introducidas. Estos artículos contendrán todas las palabras. Debe introducir estas palabras separadas por un espacio."
  )$
  step(
    "dataTable_found_documents",
    "Tabla de los artículos filtrados",
    "Se muestra el PMID y los títulos de los artículos. Puede acceder al artículo en PubMed haciendo clic sobre su PMID. Adicionalmente, puede buscar entre estos artículos por palabras presentes en sus títulos."
  )

guide_clusters_group_info <- Cicerone$
  new()$
  step(
    "plot_dendrogram_exemplars",
    "Dendrograma",
    "Dendrograma de los artículos ejemplares tras llevar a cabo la agrupación por propagación de afinidad. Las distancias entre ellos corresponden con la medida del coseno."
  )

guide_clusters_affinity_groups <- Cicerone$
  new()$
  step(
    "dataTable_exemplar_keywords_df",
    "Tabla de los artículos ejemplares y sus palabras clave",
    "Puede ver las palabras clave asociadas a cada ejemplar y buscar ejemplares por palabras clave. Cuando un ejemplar no está asociado a ninguna palabra clave, aparece \"No data available\"."
  )$
  step(
    "selectInput_exemplar_name",
    "Desplegable con los ejemplares",
    "Puede seleccionar el ejemplar para analizar el cluster que representa. Se representará su gráfica y una tabla con los PMIDs de los artículos."
  )$
  step(
    "exemplar_name_cluster_plot",
    "Gráfica del ejemplar seleccionado",
    "Se representan los artículos del cluster del ejemplar seleccionado con sus PMIDs. Las distancias entre ellos corresponden con la medida del coseno."
  )$
  step(
    "dataTable_table_for_exemplar_name",
    "Tabla del ejemplar seleccionado",
    "Puede ver los PMIDs de los artículos del cluster del ejemplar seleccionado. Puede acceder al artículo en PubMed haciendo clic sobre su PMID."
  )

guide_influencing_factors <- Cicerone$
  new()$
  step(
    "new_factor_s",
    "Términos a introducir en el análisis de asociación",
    "Puede introducir uno o más términos a la lista inicial. Si introduce más de un término, estos deben ir separados por un espacio. Posteriormente, podrá seleccionar uno de estos términos para analizar su asociación con el resto de términos de la lista."
  )$
  step(
    "influencing_factors",
    "Lista de términos",
    "Puede seleccionar un término de la lista para analizar su asociación con el resto de términos listados. En esta lista aparecerán también los términos que haya introducido."
  )$
  step(
    "influencing_factor_assocs_plot",
    "Histograma de asociación",
    "Histograma de la fuerza de asociación utilizando la función de similitud del coseno y el análisis semántico latente (LSA) del término seleccionado con el resto de términos listados."
  )

guide_information_evolution <- Cicerone$
  new()$
  step(
    "dates_1",
    "Periodo de tiempo",
    "Puede seleccionar un periodo de tiempo para llevar a cabo el análisis de la evolución de la información. Se compararán cada uno de los periodos de tiempo seleccionados."
  )$
  step(
    "add_dates",
    "Añadir más periodos",
    "Puede añadir más periodos de tiempo haciendo clic sobre este botón."
  )$
  step(
    "remove_dates",
    "Eliminar último periodo",
    "Puede eliminar el último periodo de tiempo haciendo clic sobre este botón."
  )$
  step(
    "confirm_dates",
    "Confirmar",
    "Una vez seleccionado, se llevará a cabo el análisis con los periodos presentes. Se mostrará un wordcloud por periodo de tiempo, donde se han eliminado las palabras comunes entre periodos."
  )


###### A continuación, definimos la interfaz de usuario (UI) y el servidor o 'server' de la
# aplicación web.

###### Interfaz de usuario:

ui <- fluidPage(
  # Definimos el nombre de la aplicación que aparece en la pestaña del navegador con
  # la función titlePanel(). 
  titlePanel(title = "", windowTitle = "Estrés y depresión"),
  use_cicerone(),
  dashboardPage(
    skin = "black",
    # Utilizamos la función dashboardPage() del paquete shinydashboard para crear
    # un 'dashboard' para usarlo en la aplicación Shiny.
    
    # Creamos el encabezado del tablero con la función dashboardHeader().
    dashboardHeader(
      # Cambiamos el grosor del encabezado:
      titleWidth = 300,
      # Con div() creamos una sección o contenedor que contiene tanto la imagen del
      # logo (incluida con la función img()) como el texto (se crea otro contenedor div()
      # con la clase 'title-box'; dentro de este contenedor, se agrega un encabezado
      # con la función h1()).
      title = div(
        # Contenedor 'flex' para colocar la imagen y el texto uno al lado del otro.
        style = "display: flex; align-items: center; vertical-align: middle;",
        # Imagen en el encabezado.
        img(
          src = "logo.png",
          width = '17%',
          height = '17%'
        ),
        # Contenedor para el texto con clase 'title-box'.
        div(
          class = "title-box",
          # Agregamos un encabezado h1 con estilo y texto específicos.
          h1(
            class = "primary-title",
            style = "color: black;
            font-size: 22px;",
            "ESTRÉS Y DEPRESIÓN"
          )
        )
      )
    ),
    
    # Creamos la barra lateral del tablero con la función dashboardSidebar(). Esta
    # barra lateral contiene un menú, creado con sidebarMenu(). Agregamos 'menuItems'
    # a la barra lateral, con los nombres de pestaña apropiados ('tabNames').
    dashboardSidebar(
      # Ajustamos también el ancho de la barra lateral:
      width = 300,
      sidebarMenu(
        # Podemos añadir negrita o cursiva utilizando HTML(). También podemos añadir 
        # iconos con el argumento 'icon' y la función icon().
        menuItem(
          HTML("<b>Inicio</b>"),
          tabName = "home",
          icon = icon("house")
        ),
        menuItem(
          HTML("<b>Artículos</b>"),
          tabName = "articles",
          icon = icon("book")
        ),
        menuItem(
          HTML("<b><i>Clusters</i></b>"),
          tabName = "clusters",
          icon = icon("users-viewfinder")
        ),
        menuItem(
          HTML("<b>Factores influyentes</b>"),
          tabName = "assocs",
          icon = icon("code-compare")
        ),
        menuItem(
          HTML("<b>Evolución de la información</b>"),
          tabName = "dashboard",
          icon = icon("dashboard")
        ),
        menuItem(
          HTML("<b>Información</b>"),
          tabName = "about",
          icon = icon("circle-info")
        )
      )
    ),
    
    # Creamos el cuerpo del tablero con la función dashboardBody(). Añadimos 'tabItems'
    # con los valores correspondientes de 'tabName' (los mismos que hemos usado en
    # dashboardSidebar()):
    dashboardBody(
      # Agregamos un bloque de estilo CSS en la sección '<head>' del documento HTML que
      # Shiny genera para ocultar el elemento 'tfoot' (pie de tabla) de todas las tablas
      # de la página.
      tags$head(
        tags$style("tfoot {display: none;}"
        )
      ),
      tabItems(
        tabItem(
          tabName = "home",
          # Utilizamos la función fluidPage() para crear una pantalla que se ajusta 
          # automáticamente a las dimensiones de la ventana del navegador del usuario.
          fluidPage(
            # Creamos una barra de navegación o conjunto de pestañas con la función
            # tabsetPanel(). 
            tabsetPanel(
              id = "mainTabsHome",
              tabPanel(
                "Gráficos",
                # Añadimos el botón para activar la guía de esta página.
                div(
                  style = "float: right",
                  actionButton(inputId = "guide_home_graphs_button", label = "Guía")
                ),
                # Añadimos el wordcloud() con los términos más frecuentes del corpus primario.
                # Utilizamos la función sliderInput() para crear un control deslizante en 
                # la interfaz de usuario, de forma que permita seleccionar un valor dentro
                # de un rango específico.
                # El sliderInput() está encapsulado por un div() para poder referenciarlo
                # en la guía 'guide_home_graphs'.
                div(
                  id = "num_words_wordcloud_slider",
                  sliderInput(
                    "num_words_wordcloud",
                    "Número de palabras a mostrar:",
                    min = 1,
                    max = 200,
                    value = 70
                  )
                ),
                plotOutput("plot_wordcloud"),
                # Añadimos el dendrograma con los términos más frecuentes del corpus primario.
                plotOutput("plot_dendrogram")
              ),
              tabPanel("Autores",
                       # Añadimos el botón para activar la guía de esta página.
                       div(
                         style = "float: right",
                         actionButton(inputId = "guide_home_authors_button", label = "Guía")
                       ),
                       HTML("<br><br><br><br>"),
                       # Añadimos una tabla (dataTableOutput()) con los autores y sus frecuencias.
                       # El dataTableOutput() está encapsulado por un div() para poder referenciarlo
                       # en la guía 'guide_home_authors'.
                       div(
                         id = "dataTable_freq_authors",
                         dataTableOutput("freq_authors"))
                       ),
              tabPanel("Año de publicación",
                       # Añadimos el botón para activar la guía de esta página.
                       div(
                         style = "float: right",
                         actionButton(inputId = "guide_home_published_year_button", label = "Guía")
                       ),
                       HTML("<br><br><br><br>"),
                       # Añadimos una tabla (tableOutput()) con el número de artículos publicados cada año.
                       # El dataTableOutput() está encapsulado por un div() para poder referenciarlo
                       # en la guía 'guide_home_published_year'.
                       div(
                         id = "dataTable_year_article_date",
                         dataTableOutput("year_article_date"))
                       ),
              tabPanel("Revistas",
                       # Añadimos el botón para activar la guía de esta página.
                       div(
                         style = "float: right",
                         actionButton(inputId = "guide_home_journals_button", label = "Guía")
                       ),
                       HTML("<br><br><br><br>"),
                       # Añadimos una tabla (dataTableOutput()) con el número de artículos publicados en
                       # las diferentes revistas.
                       # El dataTableOutput() está encapsulado por un div() para poder referenciarlo
                       # en la guía 'guide_home_journals'.
                       div(
                         id = "dataTable_journals",
                         dataTableOutput("journals"))
                       )
            )
          )
        ),
        
        tabItem(
          tabName = "articles",
          fluidPage(
            # Añadimos el botón para activar la guía de esta página.
            div(
              style = "float: right",
              actionButton(inputId = "guide_articles_button", label = "Guía"),
            ),
            # Creamos un campo para introducir texto con textInput(). En particular,
            # especificamos los argumentos 'inputId', 'label' y 'placeholder' (este
            # último corresponde con una cadena de caracteres que le da al usuario
            # una pista sobre lo que se puede ingresar).
            textInput(
              inputId = "search_string",
              label = "Introduce las palabras para
                              filtrar los artículos:",
              placeholder = "amygdala fst"
            ),
            # El dataTableOutput() está encapsulado por un div() para poder referenciarlo
            # en la guía 'guide_articles'.
            div(
              id = "dataTable_found_documents",
              dataTableOutput("found_documents")
            )
          )
        ), 
        
        tabItem(
          tabName = "clusters",
          fluidPage(
            tabsetPanel(
              id = "mainTabsClusters",
              # Primera pestaña llamada "Información general sobre los clusters".
              tabPanel(
                HTML("Información general sobre los <i>clusters</i>"),
                # Añadimos el botón para activar la guía de esta página.
                div(
                  style = "float: right",
                  actionButton(inputId = "guide_clusters_group_info_button", label = "Guía")
                ),
                HTML("<br><br><br><br><br>"),
                # Mostramos un dendrograma de los ejemplares.
                plotOutput("plot_dendrogram_exemplars"),
              ),
              # Segunda pestaña llamada "Agrupación por propagación de afinidad".
              tabPanel(
                HTML("Agrupación por propagación de afinidad"),
                # Añadimos el botón para activar la guía de esta página.
                div(
                  style = "float: right",
                  actionButton(inputId = "guide_clusters_affinity_groups_button", label = "Guía")
                ),
                HTML("<br><br>"),
                # Añadimos una tabla (dataTableOutput()) con los ejemplares y sus keywords.
                # El dataTableOutput() está encapsulado por un div() para poder referenciarlo
                # en la guía 'guide_clusters_affinity_group'.
                div(
                  id = "dataTable_exemplar_keywords_df",
                  dataTableOutput("exemplar_keywords_df")
                ),
                # Creamos una lista de selección con selectInput() para que el usuario pueda
                # elegir un elemento de una lista de valores (esta lista de valores corresponde
                # con la lista de los ejemplares).
                # El selectInput() está encapsulado por un div() para poder referenciarlo
                # en la guía 'guide_clusters_affinity_group'.
                div(
                  id = "selectInput_exemplar_name",
                  selectInput(
                    inputId = "exemplar_name",
                    label = "Seleccione un ejemplar para analizar el cluster que representa:",
                    choices = exemplar_keywords_df$Exemplar,
                    width = "35%"
                  )
                ),
                # Mostramos el gráfico del cluster seleccionado por el usuario.
                plotOutput("exemplar_name_cluster_plot"),
                # Mostramos una tabla con el ejemplar seleccionado y los documentos del cluster
                # que representa. Esta tabla contiene los hiperenlaces a PubMed de todos los
                # documentos.
                # El dataTableOutput() está encapsulado por un div() para poder referenciarlo
                # en la guía 'guide_clusters_affinity_group'.
                div(
                  id = "dataTable_table_for_exemplar_name",
                  dataTableOutput("table_for_exemplar_name")
                )
              )
            )
          )
        ),
        
        tabItem(
          tabName = "assocs",
          fluidPage(
            tabsetPanel(
              id = "mainTabsInfluencingFactors",
              # Primera pestaña llamada "Asociaciones de los factores influyentes".
              tabPanel(
                HTML("Asociaciones de los factores influyentes"),
                # Añadimos el botón para activar la guía de esta página.
                div(
                  style = "float: right",
                  actionButton(inputId = "guide_influencing_factors_button", label = "Guía")
                ),
                # Creamos un campo para introducir texto con textInput(). En particular,
                # especificamos los argumentos 'inputId', 'label' y 'placeholder' (este
                # último corresponde con una cadena de caracteres que le da al usuario
                # una pista sobre lo que se puede ingresar).
                textInput(
                  inputId = "new_factor_s",
                  label = "Introduce el/los término/s que desee incluir en el análisis:",
                  placeholder = "mice",
                  width = "35%"
                ),
                # Posteriormente, en el 'server', crearemos una lista de selección con selectInput() 
                # para que el usuario pueda elegir un elemento de una lista de valores (esta lista 
                # de valores contiene diferentes valores de los factores influyentes, a la que se 
                # añade el valor especificado por el usuario en textInput()). Para ello, usamos
                # uiOutput().
                uiOutput("influencing_factors"),
                # Mostramos el gráfico del factor influyente seleccionado por el usuario.
                plotOutput("influencing_factor_assocs_plot")
              )  
            )
          )
        ),
        
        tabItem(
          tabName = "dashboard",
          fluidPage(
            tabsetPanel(
              id = "mainTabsEvolution",
              # Primera pestaña llamada "Evolución de la información a lo largo del tiempo".
              tabPanel(HTML("Evolución de la información a lo largo del tiempo"),
                       # Añadimos el botón para activar la guía de esta página.
                       div(
                         style = "float: right",
                         actionButton(inputId = "guide_information_evolution_button", label = "Guía")
                       ),
                       # Creamos dos 'date widgets' para que el usuario pueda seleccionar 
                       # los periodos de tiempo que quiere comparar. Para ello, empleamos
                       # la función dateRangeInput(). 
                       dateRangeInput(inputId = "dates_1",
                                      label = "Seleccione el primer periodo de tiempo:", 
                                      start = '2000-01-01', end = '2023-12-31', 
                                      startview = "decade", width = "50%"),
                       dateRangeInput(inputId = "dates_2", 
                                      label = "Seleccione el segundo periodo de tiempo:", 
                                      start = '2000-01-01', end = '2023-12-31', 
                                      startview = "decade", width = "50%"),
                       # Añadimos un botón para que el usuario pueda añadir más periodos con la función
                       # actionButton().
                       actionButton(inputId = "add_dates", label = "Añadir más periodos"),
                       # Posteriormente, en el 'server', definiremos que, si el usuario hace clic en el
                       # botón 'Añadir más periodos' (ID 'add_dates'), se añadirá otro dateRangeInput().
                       # Para ello, usamos uiOutput(). 
                       uiOutput("dynamic_date_inputs"),
                       # Añadimos otro botón para que el usuario pueda eliminar periodos (solo se podrán
                       # eliminar aquellos que han sido añadidos por el usuario, es decir, mínimo debe
                       # haber 2 periodos).
                       actionButton(inputId = "remove_dates", label = "Eliminar último periodo"),
                       # Añadimos un último botón para que el usuario pueda confirmar los periodos 
                       # seleccionados. 
                       actionButton(inputId = "confirm_dates", label = "Confirmar"),
                       # Añadimos los wordclouds, uno para cada periodo de tiempo seleccionado
                       # por el usuario. Para ello, utilizamos la función uiOutput(). 
                       uiOutput("wordclouds_for_dates")
              )
            )
          )
        ),
        
        tabItem(
          tabName = "about",
          fluidPage(
            h2("Licencia"),
            p("Esta aplicación Shiny fue desarrollada por Virginia Carayol Gordillo 
              como trabajo final en el área de bioinformática estadística y aprendizaje 
              automático del máster de bioinformática y bioestadística de la Universidad 
              Abierta de Cataluña (UOC). El código fuente está disponible gratuitamente 
              bajo licencia MIT en",
              HTML('<a href="https://github.com/VirginiaCarayol/TFM_UOC">GitHub</a>.'), 
              style = "text-align: justify;"),
            h2("Contacto"),
            p("Para cualquier comentario o pregunta, por favor contacte a vircarayolgordillo@gmail.com.")
          )
        )
        
      )
    )
  )
)



###### Servidor o 'server':

server <- function(input, output, session) {
  ###### Inicializamos con la función init() todas las guías que hemos creado, para
  # así poder mostrarlas cuando se haga clic en sus correspondientes botones.
  guide_home_graphs$init()
  guide_home_authors$init()
  guide_home_published_year$init()
  guide_home_journals$init()
  guide_articles$init()
  guide_clusters_group_info$init()
  guide_clusters_affinity_groups$init()
  guide_influencing_factors$init()
  guide_information_evolution$init()
  
  # Con la función observeEvent() podemos iniciar cada guía cuando el usuario haga clic
  # en su correspondiente botón.
  observeEvent(input$guide_home_graphs_button, {
    guide_home_graphs$start()
  })
  observeEvent(input$guide_home_authors_button, {
    guide_home_authors$start()
  })
  observeEvent(input$guide_home_published_year_button, {
    guide_home_published_year$start()
  })
  observeEvent(input$guide_home_journals_button, {
    guide_home_journals$start()
  })
  
  observeEvent(input$guide_articles_button, {
    guide_articles$start()
  })
  
  observeEvent(input$guide_clusters_group_info_button, {
    guide_clusters_group_info$start()
  })
  observeEvent(input$guide_clusters_affinity_groups_button, {
    guide_clusters_affinity_groups$start()
  })
  
  observeEvent(input$guide_influencing_factors_button, {
    guide_influencing_factors$start()
  })
  
  observeEvent(input$guide_information_evolution_button, {
    guide_information_evolution$start()
  })
  
  ###### Generamos el wordcloud() con los términos más frecuentes del corpus primario.
  # En particular, usamos la función reactive() para utilizar el valor seleccionado por
  # el usuario en el 'sliderInput'. 
  plot_wordcloud <- reactive({
    num_words <- input$num_words_wordcloud
    # Usamos la función wordcloud() para representar la nube de palabras de nuestro corpus. 
    # Esta función recibe, entre otros, los siguientes argumentos: 'words' (las palabras),
    # 'freq' (sus frecuencias), 'min.freq' (frecuencia mínima a partir de la cual las 
    # palabras no se representarán), 'max.words' (número máximo de palabras que se 
    # representarán), 'random.order' (si es 'TRUE', las palabras se representar de forma
    # aleatoria; si no, se representar por orden decreciente), 'random.color' (si es 'FALSE',
    # el color se eligirá en función de su frecuencia), 'rot.per' (proporción de palabras 
    # representadas con una rotación de 90 grados) y 'colors' (colores para representar las
    # palabras de menor a mayor frecuencia).
    # Se configuran los márgenes para el gráfico, asegurando que el wordcloud se muestre
    # sin márgenes, con la función par().
    par(mar = rep(0,4)) 
    wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 1, max.words = num_words,
              random.order = FALSE, random.color = FALSE, rot.per = 0.5,
              colors = brewer.pal(8, "Dark2"), scale = c(3.5, 0.5))
  })
  # Finalmente, generamos el wordcloud. 
  output$plot_wordcloud <- renderPlot({
    plot_wordcloud()
  }, bg = "transparent")
  
  ###### Generamos el dendrograma con los términos más frecuentes del corpus primario. 
  output$plot_dendrogram <- renderPlot({
    plot(hclust_cosine_dist_terms_100, cex = 1)
  }, bg = "transparent")
  
  ###### Filtramos los artículos por los términos introducidos por el usuario.
  output$found_documents <- renderDataTable({
    document_file_names <- find_document_file_names(input$search_string)
    get_document_titles(document_file_names, pmid_title_map)
  }, escape = FALSE)
  
  ###### Mostramos los autores del corpus primario.
  output$freq_authors <- renderDataTable({
    author_df
  }, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center',
                                                            targets = "_all"))))
  
  ###### Mostramos el año de publicación de los artículos del corpus primario.
  output$year_article_date <- renderDataTable({
    year_article_date
  }, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center',
                                                            targets = "_all"))))
  
  ###### Mostramos las revistas donde se han publicado los artículos del corpus primario.
  output$journals <- renderDataTable({
    journals_df
  }, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center',
                                                            targets = "_all"))))
  
  ###### Mostramos una tabla con los ejemplares hallados tras el agrupamiento y sus
  # palabras clave (keywords).
  output$exemplar_keywords_df <- renderDataTable({
    exemplar_keywords_df
  }, options = list(pageLength = 15))
  
  ###### Mostramos la gráfica del cluster del ejemplar seleccionado por el usuario. 
  output$exemplar_name_cluster_plot <- renderPlot({
    plot_for_exemplar_name(input$exemplar_name)
  }, bg = "transparent")
  
  ###### Mostramos la tabla del cluster del ejemplar seleccionado por el usuario. Esta
  # tabla contiene los hiperenlaces a PubMed de los documentos. 
  output$table_for_exemplar_name <- renderDataTable({
    table_for_exemplar_name(input$exemplar_name)}, options = list(dom = 't'), escape = FALSE
  )
  
  ###### Mostramos un dendrograma de los ejemplares.
  output$plot_dendrogram_exemplars <- renderPlot({
    plot(hclust_cosine_dist_exemplars, cex = 1)
  }, bg = "transparent")
  
  ###### Mostramos el gráfico de las asociaciones del factor influyente elegido por el
  # usuario. 
  
  # Antes, como mencionamos previamente, creamos una lista de selección con selectInput() 
  # para que el usuario pueda elegir un elemento de una lista de valores. Esta lista 
  # de valores contiene diferentes valores de los factores influyentes, y se actualiza
  # con el valor recibido en textInput(), es decir, se añade a la lista el valor 
  # especificado por el usuario. 
  
  output$influencing_factors <- renderUI({
    selectInput(
      inputId = "influencing_factors",
      label = "Seleccione un factor influyente para analizar su asociación con otros factores:",
      choices = include_factor(input$new_factor_s), 
      width = "35%"
    )
  })
  
  # Ahora sí, mostramos el gráfico mencionado. 
  output$influencing_factor_assocs_plot <- renderPlot({
    x <- include_factor(input$new_factor_s)
    y <- cos_sim_calc_influencing_factors(x)
    influ_factor_selec_df <- find_assocs_influ_factor_selec(input$influencing_factors, y)
    ggplot_assocs_influ_factor_selec(input$influencing_factors, influ_factor_selec_df)
  }, bg = "transparent")
  
  ###### Mostramos un dateRangeInput() más cada vez que el usuario haga clic en el 
  # botón con ID 'add_dates'.
  
  # Creamos una lista para almacenar los periodos seleccionados por el usuario.
  selected_periods <- list()
  
  # A continuación, usamos la función observeEvent(), que se utiliza para crear observadores 
  # reactivos que responden a eventos específicos o cambios en valores específicos.
  observeEvent(input$add_dates, {
    # Incrementamos el contador de periodos ('date widgets'), definido anteriormente por la 
    # variable 'dates_counter', cada vez que el usuario haga clic en el botón 'add_dates'
    # (por defecto, 'dates_counter' es 2 ya que tenemos inicialmente 2 'date widgets'). 
    # Utilizamos '<<-' para actualizar la variable global 'dates_counter'.
    dates_counter <<- dates_counter + 1
    
    # Creamos un nuevo dateRangeInput() con un ID único basado en el contador anterior. 
    new_date_input <- dateRangeInput(
      inputId = paste("dates_", dates_counter, sep = ""),
      label = paste("Seleccione el periodo de tiempo adicional ", dates_counter - 2, ":", sep = ""),
      start = '2000-01-01',
      end = '2023-12-31',
      startview = "decade",
      width = "50%"
    )
    
    # Insertamos el nuevo dateRangeInput() después del último existente. Para ello, 
    # empleamos la función insertUI(). 
    insertUI(selector = "#dynamic_date_inputs", ui = new_date_input)
  })
  
  ###### Eliminamos un dateRangeInput() cada vez que el usuario haga clic en el 
  # botón con ID 'remove_dates'.
  
  # Empleamos de nuevo la función observeEvent() para definir qué sucede cuando el usuario
  # hace clic sobre el botón con ID 'remove_dates'. Para eliminar un 'date widget', utilizamos
  # la función removeUI() (se eliminará el último usando el contador 'dates_counter'). 
  # Actualizamos el contador tras eliminar uno. 
  observeEvent(input$remove_dates, {
    if (dates_counter > 2) {
      removeUI(selector = paste("#dates_", dates_counter, sep = ""))
      dates_counter <<- dates_counter - 1
    }
  })
  
  ###### Obtenemos los valores de inicio y fin de los diferentes dataRangeInput() 
  # seleccionados por el usuario cuando este haga clic en el botón con ID 'confirm_dates'. 
  
  # Utilizamos la función observeEvent(). 
  observeEvent(input$confirm_dates, {
    # Utilizamos la función withProgress() para mostrar una barra de progreso mientras
    # se generan los gráficos. 
    style <- isolate(input$style)
    withProgress(message = 'Generando gráficas...', style = style, value = 0.1, {
      
      # Creamos un vector vacío donde guardaremos los periodos de tiempo seleccionados
      # por el usuario.
      dates_inputs <- c()
      
      # Aumentamos la barra de progreso. 
      incProgress(0.2)
      
      # Iteramos sobre los 'date widgets' utilizando el contador 'dates_counter'.
      for (i in 1:dates_counter) {
        # Hallamos el ID del 'date widget' actual.
        date_name <- paste("dates_", i, sep = "")
        # Guardamos su input.
        input_date <- input[[date_name]]
        # Guardamos los valores de inicio y fin del periodo de tiempo del input.
        start_date <- format(as.Date(input_date[1]), "%Y/%m/%d")
        end_date <- format(as.Date(input_date[2]), "%Y/%m/%d")
        # Guardamos el resultado del periodo actual separando los valores de inicio 
        # y fin por ":".
        date_input <- paste(start_date, end_date, sep = ":")
        # Añadimos dicho resultado al vector creado anteriormente ('dates_inputs').
        dates_inputs <- append(dates_inputs, date_input)
      }
      
      # Aumentamos la barra de progreso.
      incProgress(0.3)
      
      # Aplicamos la función get_words_freqs_for_dates() definida anteriormente para,
      # a partir del vector que contiene los periodos de tiempo seleccionados por 
      # el usuario en la aplicación web, obtener una lista de dataframes con las
      # palabras y frecuencias de cada periodo (tras eliminar las palabras en común
      # entre periodos). 
      words_freqs <- get_words_freqs_for_dates(dates_inputs)
      
      # Creamos un objeto dinámico para mostrar múltiples wordclouds en la interfaz
      # del usuario con la función renderUI(). 
      output$wordclouds_for_dates <- renderUI({
        # Creamos una lista que contiene objetos 'plotOutput' para cada wordcloud.
        # Se utiliza lapply() para iterar sobre los índices de los periodos de tiempo.
        plot_output_list <- lapply(1:length(words_freqs), function(i) {
          # Se genera un ID único para cada wordcloud.
          id <- paste("wordcloud_for_dates_", i, sep = "")
          # Se crea un objeto 'plotOutput' con el ID correspondiente.
          plotOutput(outputId = id)
        })
        # Para que la lista de items se muestre correctamente, convertimos la lista
        # en un tagList con la función do.call().
        do.call(tagList, plot_output_list)
      })
      
      # Aumentamos la barra de progreso. 
      incProgress(0.5)
      
      # Iteramos sobre los resultados de palabras y frecuencias para cada periodo de tiempo.
      for (i in 1:length(words_freqs)) {
        # Se utiliza 'local' para asegurar que las variables dentro del bucle sean 
        # tratadas de manera local. 
        local({
          # Accedemos a las palabras y frecuencias del periodo actual. 
          df_words <- words_freqs[[i]]
          # Generamos de nuevo el ID único para cada wordcloud para poder utilizarlo aquí.
          id <- paste("wordcloud_for_dates_", i, sep = "")
          # Se establece un renderPlot para cada wordcloud, asociado con el ID único. 
          output[[id]] <- renderPlot({
            # Se configuran los márgenes para el gráfico, asegurando que el wordcloud
            # se muestre sin márgenes. 
            par(mar = rep(0, 4))
            # Se generan los wordclouds utilizando los datos de palabras y frecuencias del
            # periodo de tiempo correspondiente. 
            wordcloud(
              words = df_words$word,
              freq = df_words$freq,
              random.order = FALSE,
              random.color = FALSE,
              rot.per = 0.5,
              colors = brewer.pal(8, "Dark2"),
              scale = c(3, 0.5)
            )
          }, bg = "transparent")
        })
      }
      
      # Finalizamos la barra de progreso. 
      incProgress(1)
    })
  })
}


###### Para terminar, ejecutamos la aplicación con la función shinyApp(), que recibe 
# la interfaz de usuario y el servidor. 

shinyApp(ui = ui, server = server)

