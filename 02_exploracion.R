  
<<<<<<< HEAD
knitr::opts_knit$set(root.dir = "/home/arobles/Intro_a_R")


# 1. COmprobar si existen 
=======
  # 1. COmprobar si existen 
>>>>>>> a03a7d4ab9b952d034f121d6d4a60fcbe9e655e3
  
  file.exists("data_raw/datos_sesion_02_blast/resultados_blast.tsv", 
              "data_raw/datos_sesion_02_blast/metadata_secuencias.csv", 
              "data_raw/datos_sesion_02_blast/secuencias.fasta")
  
  # 2. Importar las dos tablas con readr y conservar codigo_muestra como texto 
  
  blast <- readr::read_tsv(
    "data_raw/datos_sesion_02_blast/resultados_blast.tsv",
    show_col_types = FALSE
  )
  
  metadata <- readr::read_csv(
    "data_raw/datos_sesion_02_blast/metadata_secuencias.csv",
    col_types = readr::cols(codigo_muestra = readr::col_character()),
    show_col_types = FALSE
  )
  
  # 3. Mostrar dimensiones, nombres y tipos de columnas 
  
  dim(blast)
  str(blast)
  readr::spec(blast)
  
  dim(metadata)
  str(metadata)
  readr::spec(metadata)
  
  # 4. Identificar los query_id distintos y contar los resultados por consulta 
  
  unique(blast$query_id)
  length(unique(blast$query_id))
  
  unique(metadata$query_id)
  length(unique(metadata$query_id))
  
  # 5. Mostrar las filas con pident >= 90 y explicar qué representa este filtro 
  
  blast[blast$pident >= 90, ]
  # nos da las clumnas en las que el porcentaje de identidad fue mayor a 90
  
  # 6. Leer el FASTA e indicar cuántos encabezados contiene 
  
<<<<<<< HEAD
  lineas <- readLines("data_raw/datos_sesion_02_blast/secuencias.fasta")
  head(lineas, 6)
  
  # analizar cuántas secuencias tiene el fasta 
  encabezados <- grep("^>", lineas, value = TRUE)
=======
  lineas_fasta <- readLines("data_raw/datos_sesion_02_blast/secuencias.fasta")
  head(lineas_fasta, 6)
  
  # analizar cuántas secuencias tiene el fasta 
  encabezados <- grep("^>", lineas_fasta, value = TRUE)
>>>>>>> a03a7d4ab9b952d034f121d6d4a60fcbe9e655e3
  length(encabezados)
  
  # 7. Mostrar si alguna de las tablas tiene valores faltantes 
  
  resultados_NA <- colSums(is.na(blast))
  resultados_NA
  
  metadata_NA <- colSums(is.na(metadata))
  metadata_NA
  
  # Coment1: tenemos dos tablas y un fasta 
  # Coment2: no tenemos valores faltantes en ningún arhivo 
<<<<<<< HEAD
  # Coment3: se importaron las tablas etc 

  
=======
  # Coment3: hay 8 encabezados en el archivo
>>>>>>> a03a7d4ab9b952d034f121d6d4a60fcbe9e655e3
