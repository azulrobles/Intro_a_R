
# borramos memoria 
rm(list = ls())

archivo_blast <- "~/Intro_a_R/data_raw/datos_sesion_02_blast/resultados_blast.tsv"

readLines(archivo_blast, n = 5)

blast <- read.delim("~/Intro_a_R/data_raw/datos_sesion_02_blast/resultados_blast.tsv")
metadata <- read.csv("~/Intro_a_R/data_raw/datos_sesion_02_blast/metadata_secuencias.csv",
        colClasses = c(codigo_muestra = "character"))

class(blast)
head(metadata)
head(blast)

# usando esta librería nos lo guarda de cuatro formas, con la forma 
# anterior siempre solo te da dataframe 

library(readr)

blast <- readr::read_tsv("~/Intro_a_R/data_raw/datos_sesion_02_blast/resultados_blast.tsv",
                         show_col_types = FALSE)

head(blast) 
class(blast)


readLines("data_raw/datos_sesion_02_blast/blast_comentarios.tsv", n = 4)

blast_con_coments <- readr::read_tsv(
  "data_raw/datos_sesion_02_blast/blast_comentarios.tsv",
                                     comment = "#",
                                     show_col_types = FALSE
)  

head(blast_con_coments)

blast_na <- readr::read_tsv(
  "data_raw/datos_sesion_02_blast/blast_valores_faltantes.tsv",
  na = c("", "NA"),
  show_col_types = FALSE
)
anyNA(blast_na) 

readr::spec(blast)

readr::problems(blast) 

# Simulamos el contenido de un archivo TSV
blast_error <- read_tsv(
  I("secuencia\tidentidad
seq1\t98.5
seq2\t95.2
seq3\terror
seq4\t99.1"),
  col_types = cols(
    secuencia = col_character(),
    identidad = col_double()
  )
)

blast_error
problems(blast_error)

dim(blast)
nrow(blast)
ncol(blast)
names(blast)

unique(blast)

# table te da la frecuencia de cada var

colSums(is.na(blast))
anyNA(blast) 

-----
  
# 7.5.4 

nrow(blast)
ncol(blast)
dim(blast)

length(blast$query_id)

head(blast)

readr::spec(blast)
blast2 <- colSums(is.numeric(blast),)
blast2

anyNA(blast)

# ---------------------------
  
# Clase del 11 de nov
  # 7.6

ruta <- "data_raw/datos_sesion_02_blast/blast_separador_incorrecto.csv"
mal <- readr::read_csv(ruta, show_col_types = FALSE)
names(mal)
head(mal)

bien <- readr::read_tsv(ruta, show_col_types = FALSE)
names(bien)

# 7.6.3 Ausencias y registros repetidos

blast_na <- readr::read_tsv(
  "data_raw/datos_sesion_02_blast/blast_valores_faltantes.tsv",
  show_col_types = FALSE
)
colSums(is.na(blast_na))

# para saber si hay dos filas duplicadas 
anyDuplicated(blast_na)

# para preguntar si hay valores duplicados en una columna específica 
anyDuplicated(blast_na$query_id)


# 7.7 

# visualizar estructura de un fasta 
lineas <- readLines("data_raw/datos_sesion_02_blast/secuencias.fasta")
head(lineas, 6)

# analizar cuántas secuencias tiene el fasta 
encabezados <- grep("^>", lineas, value = TRUE)
encabezados

# contar el num de secuencias 
length(encabezados)


















