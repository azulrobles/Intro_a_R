
# borramos memoria 
rm(list = ls())

# clase 3 

# borramos memoria 
rm(list = ls())


# cargamos librerías 
library(readr)
library(dplyr)
library(tidyr)
library(tibble)


ruta_blast <- "data_raw/datos_sesion_02_blast/resultados_blast.tsv"
ruta_conteos <- "data_raw/datos_sesion_03_dplyr/conteos_genes.csv"
stopifnot(file.exists(ruta_blast), file.exists(ruta_conteos))

blast <- read_tsv(ruta_blast, show_col_types = FALSE)
conteos <- read_csv(ruta_conteos, show_col_types = FALSE)

glimpse(blast)
glimpse(conteos)
view(conteos)

class(blast)
class(conteos)

class(as.data.frame(blast))


# 8.3.3 El operador %>% 

blast %>%
  select(query_id, subject_id, pident)

blast %>%
    select(-evalue)

blast %>%
  select(starts_with("query"))

# filter() 
# & 

blast %>% 
  filter(pident >= 90)

blast %>% 
  filter(pident >= 90, length >= 150)

# uno u otro 

blast %>% 
  filter(query_id == "seq01" | query_id == "seq02")

# que lo busque en todos 

blast %>% 
  filter(query_id %in% c("seq01", "seq03", "seq05"))


# arrange 

blast %>% 
  arrange(desc(length))   # Mayor a menor

blast %>% 
  arrange(query_id, desc(bitscore))
# primero ordena una y luego la otra 

# 8.4.4 rename(): cambiar nombres de columnas

blast %>%
  rename(
    identidad_porcentaje = pident,
    longitud_alineamiento = length
  ) %>%
  select(query_id, identidad_porcentaje, longitud_alineamiento)


# 8.4.5 distinct(): identificar combinaciones únicas 

blast %>% 
  distinct(query_id, subject_id)


# 8.5 Crear y modificar columnas con mutate()

blast %>%
  mutate(
    identidad_proporcion = pident / 100,
    longitud_kb = length / 1000
  ) %>%
  select(query_id, pident, identidad_proporcion, length, longitud_kb) 


# 8.5.1 Columnas lógicas e if_else()

blast %>%
  mutate(
    supera_90 = pident >= 90,
    etiqueta = if_else(pident >= 90, "90 o más", "Menos de 90")
  ) %>%
  select(query_id, pident, supera_90, etiqueta) 


# 8.5.2 Más de dos categorías con case_when() 

blast %>%
  mutate(
    grupo_didactico = case_when(
      pident >= 95 ~ "Grupo A",
      pident >= 80 ~ "Grupo B",
      TRUE ~ "Grupo C"
    )
  ) %>%
  count(grupo_didactico)


blast %>% 
  count(query_id, sort = TRUE) %>%
  arrange(n)


# 8.6.2 group_by() y summarise()
# les agrega un resumen a cada grupo 
# normalmennte se usa el grupo by anidado 

resumen <- blast %>%
  group_by(query_id) %>%
  summarise(
    numero_alineamientos = n(),
    referencias_distintas = n_distinct(subject_id),
    bitscore_maximo = max(bitscore),
    identidad_promedio = mean(pident),
    .groups = "drop"
  )
resumen 
# drop deshace los grupos y regresa algo 
# con nsummarise un renglón por cada grupo 


# 8.6.3 Diferencia entre mutate() y summarise()

blast %>%
  group_by(query_id) %>%
  mutate(alineamientos_por_consulta = n()) %>%
  ungroup() %>%
  select(query_id, subject_id, alineamientos_por_consulta)

# todo lo que hicimos de esta operación por cada renglón que teníamso originalemtne 


# 8.7.2 pivot_longer(): de ancho a largo ----------------------------------
# transformar de formato ancho (dataframe) a largo se usa para ggplot 
# ancho es el dataframe completo y largo es el compacto 

conteos_largos <- conteos %>%
  pivot_longer(
    cols = starts_with("muestra_"),
    names_to = "muestra",
    values_to = "conteo"
  )
conteos_largos


# 8.7.3 pivot_wider(): de largo a ancho -----------------------------------

conteos_recuperados <- conteos_largos %>%
  pivot_wider(
    names_from = muestra,
    values_from = conteo
  )
conteos_recuperados


# 8.12.1 ¿Qué significa .groups = "drop" en summarise()? ------------------

library(dplyr)

datos <- tibble(
  especie = c("A", "A", "A", "A", "B", "B", "B", "B"),
  tratamiento = c("Control", "Control", "Tratado", "Tratado",
                  "Control", "Control", "Tratado", "Tratado"),
  valor = c(10, 20, 30, 40, 50, 60, 70, 80)
)

datos


resumen1 <- datos %>%
  group_by(especie, tratamiento) %>%
  summarise(
    promedio = mean(valor)
  )

resumen1 

group_vars(resumen1)

resumen1 %>%
  summarise(
    media = mean(promedio)
  )


# Caso 2. Utilizando .groups = "drop" -------------------------------------

resumen2 <- datos %>%
  group_by(especie, tratamiento) %>%
  summarise(
    promedio = mean(valor),
    .groups = "drop"
  )

resumen2 

group_vars(resumen2)

resumen2 %>%
  summarise(
    media = mean(promedio)
  )




# 8.9.1 Guardar resultados sin sobrescribir los datos originales ---------- 

if (!dir.exists("results")) dir.create("results")

readr::write_csv(resumen, "results/03_resumen_blast.csv")
readr::write_tsv(conteos_largos, "results/03_conteos_largos.tsv")


# 8.10 Ejercicios ---------------------------------------------------------

library(readr)
library(dplyr)

datos <- read_csv(
  "data_raw/expresion_genica_estres.csv",
  show_col_types = FALSE
)

glimpse(datos)

view(datos)
# 8.10.1 Ejercicio 1. Identificación de genes candidatos ------------------

#Utilizando dplyr:
  
 # Calcula el conteo promedio de cada gen en cada condición experimental.
#Organiza los resultados de manera que puedas comparar los promedios de Control y Estrés para cada gen.
#Calcula la razón entre el promedio en Estrés y el promedio en Control.
#Identifica los cinco genes con mayor aumento relativo y muestra su anotación funcional.
#Pregunta de interpretación: ¿Un gen con un aumento relativo grande necesariamente presenta un conteo alto de lecturas? Justifica tu respuesta.

# 1. 

resumen11 <- datos %>%
  group_by(gene_id, condicion) %>%
  summarise(
    promedio = mean(conteo)
  )

resumen11


# 2. 

resumen11 %>%
  group_by(condicion) %>%
  arrange(desc(promedio))   # Mayor a meno

resumen11

# 3. 

razon1 <- datos %>%
  group_by(gene_id, condicion, funcion) %>%
  summarise(promedio = mean(conteo), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = condicion, values_from = promedio) %>%
  mutate(razon = Estres / Control)

print(razon1)
view(razon1)

# 4. 

# 4. Identificar los 5 genes con mayor aumento relativo y mostrar anotación

top5_genes <- razon1 %>%
  slice_max(order_by = razon, n = 5)

print(top5_genes)













