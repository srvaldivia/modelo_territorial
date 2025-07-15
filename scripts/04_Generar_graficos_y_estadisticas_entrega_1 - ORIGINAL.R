
# 0. paquetes -------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(ggtext)
library(readxl)
library(ggplot2)


# 1. carga de datos -------------------------------------------------------
capas <- st_layers("datos_output/GIS_resultados_entrega_1.gdb")$name[seq(1, 31, 2)]
orden_regiones <- c("Arica y Parinacota", "Tarapacá", "Antofagasta",
                    "Atacama", "Coquimbo", "Valparaíso", "Metropolitana",
                    "O'Higgins", "Maule", "Ñuble", "Biobío",
                    "Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes")

lista <- list(); for (i in capas) {
  tmp <-
    st_read("datos_output/GIS_resultados_entrega_1.gdb",
            layer = i,
            as_tibble = TRUE) |> 
    mutate(es_cluster =
             case_when(!is.na(id_cluster_final) ~ 1,
                       is.na(id_cluster_final)  ~ 0),
           N_REGION = str_squish(N_REGION)) |> 
    st_drop_geometry()
  lista[[i]] <- tmp
}

viviendas_hdb <- bind_rows(lista) |> 
  mutate(N_REGION = str_to_title(N_REGION)) |> 
  mutate(N_PROVINCIA = str_to_title(N_PROVINCIA)) |> 
  mutate(N_REGION = case_when(N_REGION == "Arica Y Parinacota" ~ "Arica y Parinacota",
                              N_REGION == "O'higgins" ~ "O'Higgins",
                              .default = N_REGION)) |> 
  mutate(N_REGION = fct_rev(fct_relevel(N_REGION, orden_regiones)))




# 2. cálculo de estadísticas por escalas ----------------------------------
resumen_region <- viviendas_hdb |> 
  st_drop_geometry() |> 
  group_by(N_REGION, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas)

resumen_comuna <- viviendas_hdb |> 
  st_drop_geometry() |> 
  group_by(N_REGION, N_COMUNA, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(N_REGION = str_squish(N_REGION),
         prop_viviendas = viviendas / total_viviendas)

resumen_provincia <- viviendas_hdb |>
  st_drop_geometry() |> 
  group_by(N_REGION, N_PROVINCIA, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas)


resumen_viviendas_cluster_reg <- viviendas_hdb |>
  filter(!is.na(id_cluster_final)) |>
  group_by(N_REGION, id_cluster_final) |>
  summarise(n = n()) |> 
  summarise(n_cluster = n(),
            n_viv = sum(n),
            min_viv = min(n),
            max_viv = max(n),
            mean_viv = mean(n)) |> 
  ungroup() |> 
  mutate(dif = max_viv - min_viv,
         viv_por_cluster = n_viv / n_cluster)

resumen_viviendas_cluster_prov <- viviendas_hdb |>
  filter(!is.na(id_cluster_final)) |>
  group_by(N_REGION, N_PROVINCIA, id_cluster_final) |>
  summarise(n = n()) |> 
  summarise(n_cluster = n(),
            n_viv = sum(n),
            min_viv = min(n),
            max_viv = max(n),
            mean_viv = mean(n)) |> 
  ungroup() |> 
  mutate(dif = max_viv - min_viv)





# 3. graficos ESCALA NACIONAL ---------------------------------------------
## 3.1 RANGO DE VIVIENDAS ----
ggplot(resumen_viviendas_cluster_reg) +
  geom_segment(aes(x = min_viv, y = N_REGION,
                   xend = max_viv, yend = N_REGION),
               color = "#aeb6bf",
               size = 6.5,
               alpha = .5) +
  geom_point(aes(x = min_viv, y = N_REGION),
             colour = "#009688", size = 6) +
  geom_point(aes(x = max_viv, y = N_REGION),
             colour = "#762a83", size = 6) +
  geom_text(data = resumen_viviendas_cluster_reg |> filter(N_REGION != "Tarapacá"),
            aes(x = dif / 2,
                y = N_REGION,
                label = round(mean_viv, 1)),
            size = 2.5) +
  geom_text(data = resumen_viviendas_cluster_reg |> filter(N_REGION == "Tarapacá"),
            aes(x = max_viv,
                y = N_REGION,
                label = round(mean_viv, 1)),
            nudge_x = 900,
            size = 2.5) +
  labs(x = "viviendas rurales",
       y = NULL,
       caption = "El valor dentro de cada barra corresponde al promedio de viviendas por cluster en cada región") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10))


## 3.2 % DE VIVIENDAS CLUSTERIZADAS ----
ggplot(resumen_region) +
  geom_bar(aes(x = viviendas,
               # y = fct_reorder2(N_PROVINCIA, es_cluster, prop_viviendas),
               y = N_REGION,
               group = es_cluster,
               fill = fct_rev(as.character(es_cluster))),
           stat = "identity",
           position = "fill",
           alpha = 0.9) +
  geom_text(data = resumen_region |> filter(es_cluster == 1),
            aes(x = 0.3,
                y = N_REGION,
                label = str_glue("{round(prop_viviendas * 100, 1)} %")),
            hjust = "left",
            size = 4,
            colour = "white",
            fontface = "bold",
            inherit.aes = FALSE) +
  scale_x_continuous(labels = scales:::label_percent()) +
  scale_fill_manual(name = "viviendas",
                    values = c("0" = "grey", "1" = "#00CD66"),
                    labels = c("cluster", "ruido")) +
  labs(x = "porcentaje de viviendas rurales",
       y = NULL) +
  theme_minimal() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_text(size = 10, margin = margin(r = -10)),
        axis.ticks.x = element_line(color = "black"))



# 4. graficos ESCALA PROVINCIA ---------------------------------------------
## 4.1 RANGO DE VIVIENDAS ----
for (region in orden_regiones) {
  contenido_region <-
    resumen_viviendas_cluster_prov |> filter(N_REGION == region)
  grafico <- ggplot(contenido_region) +
    geom_segment(aes(x = min_viv, y = fct_reorder(N_PROVINCIA, dif),
                     xend = max_viv, yend = N_PROVINCIA),
                 color = "#aeb6bf",
                 size = 6.5,
                 alpha = .5) +
    geom_point(aes(x = min_viv, y = N_PROVINCIA),
               colour = "#009688", size = 6) +
    geom_point(aes(x = max_viv, y = N_PROVINCIA),
               colour = "#762a83", size = 6) +
    geom_text(aes(x = max_viv / 2,
                  y = N_PROVINCIA,
                  label = round(mean_viv, 1)),
              nudge_y = 0.3, # para algunos es 0,3 otros 0,35 otros 0,5
              hjust = "inward",
              size = 3) +
    labs(x = "viviendas rurales",
         y = "Provincia",
         caption = "El valor sobre cada barra corresponde al promedio de viviendas por cluster en cada provincia") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 10))
  
  # Nombres para guardar mis graficos
  nombre_archivo <- paste0("plots_output/entrega_1/RANGO_DE_VIVIENDAS_",
                           gsub(" ", "_", region), ".png")
  
  # Funcion para guardar
  ggsave(nombre_archivo,
         plot = grafico,
         width = 7,
         height = 3,
         dpi = 300)
}




## 4.2 % DE VIVIENDAS CLUSTERIZADAS ----
for (region in orden_regiones) {
  contenido_region <-
    resumen_provincia |> filter(N_REGION == region)
  grafico <- ggplot(contenido_region) +
    geom_bar(aes(x = viviendas,
                 y = fct_rev(fct_reorder2(N_PROVINCIA, es_cluster, prop_viviendas)),
                 # y = N_PROVINCIA,
                 group = es_cluster,
                 fill = fct_rev(as.character(es_cluster))),
             stat = "identity",
             position = "fill") +
    geom_text(data = resumen_provincia |>
                filter(N_REGION == region) |> 
                filter(es_cluster == 1),
              aes(x = 0.3,
                  y = N_PROVINCIA,
                  label = str_glue("{round(prop_viviendas * 100, 1)} %")),
              hjust = "left",
              size = 4,
              colour = "white",
              fontface = "bold",
              inherit.aes = FALSE) +
    scale_x_continuous(labels = scales:::label_percent()) +
    scale_fill_manual(name = "viviendas",
                      values = c("0" = "grey", "1" = "#00CD66"),
                      labels = c("cluster", "ruido")) +
    labs(x = "porcentaje de viviendas",
         y = "Provincia") +
    theme_minimal() +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 10),
          legend.position = "bottom")
  # Nombres para guardar mis graficos
  nombre_archivo <- paste0("plots_output/entrega_1/VIVIENDAS_CLUSTERIZADAS_",
                           gsub(" ", "_", region), ".png")
  
  # Funcion para guardar
  ggsave(nombre_archivo,
         plot = grafico,
         width = 7,
         height = 3,
         dpi = 300)
}