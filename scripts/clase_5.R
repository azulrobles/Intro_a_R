

library(ggplot2)
library(readr)
library(dplyr)

# Resultados procesados en la sesión anterior
#generos_5 
#filos_5 
#totales_muestra 
#metadata_riz 
library(ggplot2)

# Resultados procesados en la sesión anterior
generos_5 <- read_tsv("results/generos_5.tsv")
filos_5 <- read_tsv("results/filos_5.tsv")
totales <- read_tsv("results/totales.tsv")


profundidad <- totales %>%
  left_join(
    metadata_riz %>%
      select(sample_id, host_name, country, crop_system),
    by = "sample_id",
    relationship = "one-to-one"
  )

stopifnot(!anyNA(profundidad$host_name))

glimpse(profundidad)

p_profundidad <- ggplot(profundidad, aes(x = sample_id, y = total_lecturas)) +
  geom_col(fill = "#367A90") +
  labs(title = "Conteos totales por muestra", x = "Muestra",
       y = "Lecturas contabilizadas") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
p_profundidad

p_profundidad_color <- ggplot(profundidad,
                              aes(x = sample_id, y = total_lecturas, fill = country,)) +
  geom_col() +
  scale_fill_viridis_d(option = "viridis") +
  labs(title = "Conteos totales por muestra y planta hospedera",
       x = "Muestra", y = "Lecturas contabilizadas", fill = "Ciudad") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        legend.position = "bottom")
p_profundidad_color

# para cambiar tipografía se especifca con base_family 

# boxplots 

ggplot(
  profundidad,
  aes(
    x = reorder(host_name, total_lecturas, FUN = median),
    y = total_lecturas
  )
) +
  geom_boxplot(fill = "#83B7C9") +
  scale_y_log10(labels = scales::label_comma()) +
  coord_flip() +
  labs(
    title = "Profundidad de lectura según la planta hospedera",
    x = "Planta hospedera",
    y = "Lecturas contabilizadas (escala logarítmica)"
  ) +
  theme_minimal(base_size = 12)

# POR FILOS 

orden_filos <- c(
  sort(setdiff(unique(filos_5$taxon_final), "Filos < 5%")),
  "Filos < 5%"
)

orden_generos <- c(
  sort(setdiff(unique(generos_5$taxon_final), "Géneros < 5%")),
  "Géneros < 5%"
)

colores <- as.character(
  paletteer::paletteer_d("colorBlindness::Blue2Orange8Steps")
)

colores <- c(colores[1:7], "#999999")

p_filos <- ggplot(filos_5,
                  aes(x = sample_id, y = abundancia_relativa, fill = taxon_final)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = colores,
                    breaks = orden_filos) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(title = "Composición taxonómica por filo",
       x = "Muestra", y = "Abundancia relativa", fill = "Filo") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
p_filos


### facetas 
hospederos_elegidos <- profundidad %>%
  count(host_name, name = "n_muestras", sort = TRUE) %>%
  arrange(desc(n_muestras), host_name) %>%
  slice_head(n = 3) %>%
  pull(host_name)

filos_seleccionados <- filos_5 %>%
  filter(host_name %in% hospederos_elegidos)

profundidad_seleccionada <- profundidad %>%
  filter(host_name %in% hospederos_elegidos)

hospederos_elegidos

p_facet_wrap <- ggplot(filos_seleccionados,
                       aes(x = sample_id, y = abundancia_relativa, fill = taxon_final)) +
  geom_col() +
  scale_fill_manual(values = colores,
                    breaks = orden_filos) +
  facet_wrap(~ host_name, scales = "free_x") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(title = "Composición por filo según planta hospedera",
       x = "Muestra", y = "Abundancia relativa", fill = "Filo") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
p_facet_wrap

# ahora con dos variables de metadatos 
# si no tiene datos de cierto lugar, deja espacios en blanco 

filos_pais <- filos_seleccionados %>%
  left_join(metadata_riz %>% select(sample_id, country),
            by = "sample_id", relationship = "many-to-one")

p_facet_grid <- ggplot(filos_pais,
                       aes(x = sample_id, y = abundancia_relativa, fill = taxon_final)) +
  geom_col() +
  scale_fill_manual(values = colores,
                    breaks = orden_filos) +
  facet_grid(host_name ~ country, scales = "free_x", space = "free_x") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  labs(title = "Composición por hospedero y país",
       x = "Muestra", y = "Abundancia relativa", fill = "Filo") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
p_facet_grid

## ajustes de presentacion 
p_publicacion <- p_facet_wrap +
  labs(subtitle = "Subconjunto real de MicroAgroBiome; umbral por muestra: 5 %",
       caption = "Comparación descriptiva; muestras de diferentes estudios") +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
p_publicacion 

# vamos a guardarlo 

dir.create("figures", showWarnings = FALSE)

ggsave("figures/composicion_filos_hospederos.png",
       plot = p_publicacion, width = 12, height = 7,
       units = "in", dpi = 300)

ggsave("figures/composicion_filos_hospederos.pdf",
       plot = p_publicacion, width = 12, height = 7,
       units = "in",
       dpi = 300)


# ejercicio 1 

#Utilizando generos_5 y metadata_riz:
  
#  Identifica las plantas hospederas que tienen al menos 5 muestras.
#Conserva únicamente las muestras correspondientes a esas plantas.
#Construye una gráfica de barras apiladas de abundancia relativa por género 
#con ggplot2, utilizando facet_wrap() para comparar las plantas hospederas.
#Personaliza la figura con una paleta de colores adecuada, etiquetas 
#legibles y la categoría "Géneros < 5%" al final de la leyenda.



# 1. Hospederos con al menos 5 muestras
hosp_5 <- metadata_riz %>%
  select(sample_id, host_name) %>%
  count(host_name, name = "n_muestras", sort = TRUE) %>%
  filter(n_muestras >= 5)

hosp_5

# 2. Conservar solo esas plantas
generos_hosp <- generos_5 %>%
  filter(host_name %in% hosp_5$host_name)
generos_hosp

# 3. Orden de la leyenda: alfabético y "Géneros < 5%" al final
orden_generos <- c(
  sort(setdiff(unique(generos_hosp$taxon_final), "Géneros < 5%")),
  "Géneros < 5%"
)
orden_generos

# Paleta: un color por género + gris para la categoría agrupada
num_gen <- length(orden_generos) - 1
colores_generos <- as.character(
  paletteer::paletteer_d("colorBlindness::Blue2DarkOrange12Steps"),
colores <- c(colores[1:7], "#999999"))

# 4. Gráfica
p_generos <- ggplot(generos_hosp,
                    aes(x = sample_id, y = abundancia_relativa,
                        fill = taxon_final)) +
  geom_col(width = 0.9) +
  scale_fill_manual(values = colores_generos, breaks = orden_generos) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
  facet_wrap(~ host_name, scales = "free_x") +
  labs(title = "Composición por género según planta hospedera",
       subtitle = "Hospederos con al menos 5 muestras; umbral por muestra: 5 %",
       x = "Muestra", y = "Abundancia relativa", fill = "Género") +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom")
p_generos













