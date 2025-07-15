
# 0. paquetes -------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(ggtext)
library(readxl)



# 1. carga de datos -------------------------------------------------------
capas <- st_layers("datos_output/GIS_resultados_entrega_1.gdb/")$name[seq(1, 31, 2)]
clusters <- st_layers("datos_output/GIS_resultados_entrega_1.gdb/")$name[seq(2, 32, 2)]
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
    st_transform(4326)
    # st_drop_geometry()
  lista[[i]] <- tmp
}

viviendas_hdb <- bind_rows(lista) |> 
  mutate(N_REGION = str_to_title(N_REGION)) |> 
  mutate(N_PROVINCIA = str_to_title(N_PROVINCIA)) |> 
  mutate(N_REGION = case_when(N_REGION == "Arica Y Parinacota" ~ "Arica y Parinacota",
                              N_REGION == "O'higgins" ~ "O'Higgins",
                              .default = N_REGION)) |> 
  mutate(N_REGION = fct_rev(fct_relevel(N_REGION, orden_regiones))) |> 
  tibble() |> 
  janitor::clean_names() |> 
  select(-c(longitud, latitud, fid_via)) |> 
  st_as_sf() |> 
  st_transform(9153) |> 
  relocate(shape, .after = last_col()) |> 
  mutate(
    id_cluster =
      case_when(
        !is.na(id_cluster) ~ paste0(n_region,
                                    "-",
                                    str_pad(id_cluster,
                                            width = 3,
                                            side = "left",
                                            pad = "0")),
        is.na(id_cluster) ~ NA
        ),
    id_cluster_final =
      case_when(
        !is.na(id_cluster_final) ~ paste0(n_region,
                                          "-",
                                          str_pad(id_cluster_final,
                                                  width = 3,
                                                  side = "left",
                                                  pad = "0")),
        is.na(id_cluster_final) ~ NA
        ),
  )

diccionario <- tibble(
  campo = names(st_drop_geometry(viviendas_hdb)),
  tipo = lapply(st_drop_geometry(viviendas_hdb), typeof) |> unlist(),
  origen = c(rep("INE (APC 2023)", 10), rep("OCUC", 7)),
  descripción =
    c("Código Único Territorial",
      "Nombre región", "Nombre provincia",
      "Nombre comuna", "Nombre distrito",
      "Tipo distrito (Rural o Mixto)",
      "Fuente interna INE de digitalización de la edificación rual",
      "Uso de la edificación rural (Edificación, Otro uso, Vivienda y Vivienda colectiva)",
      "Coordenada UTM X (Este)", "Coordenada UTM Y (Norte)",
      "Altitud de la edificación (metros)", "Distancia al camino más cerano (metros)",
      "Identificador del Cluster de HDBSCAN (sin ajustar)",
      "Probabilidad de pertenencia a cluster 0 - 1 (1 indica máxima pertenencia)", 
      "Puntaje de outlier (puntajes más altos indican valores atípicos)",
      "Identificador del Cluster Final de HDBSCAN (remueve puntos con outlier > 0.9)",
      "Indica si edificación pertenece a algún cluster (1) o es ruido (0)"
    )
)


viviendas_hdb |>
  st_write("datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
           layer = "viviendas_agrupamiento_hdbs",
           append = FALSE)

diccionario |>
  st_drop_geometry() |> 
  st_write("datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
           layer = "diccionario_viviendas_agrupamiento_hdbs",
           append = FALSE)


# -------------------------------------------------------------------------
lista <- list(); for (i in clusters) {
  tmp <-
    st_read("datos_output/GIS_resultados_entrega_1.gdb",
            layer = i,
            as_tibble = TRUE) |> 
    rename(geometry = last_col()) |> 
    mutate(layer = str_glue("{i}")) |>
    st_transform(4326)
    # st_drop_geometry()
  lista[[i]] <- tmp
}

clusters_hdb <- bind_rows(lista) |> 
  select(-c("Shape_Length", "Shape_Area")) |> 
  mutate(layer = str_sub(layer, start = 2, end = 3)) |> 
  mutate(
    n_region =
      case_when(layer == "15" ~ "Arica y Parinacota",
                layer == "01" ~ "Tarapacá",
                layer == "02" ~ "Antofagasta",
                layer == "03" ~ "Atacama",
                layer == "04" ~ "Coquimbo",
                layer == "05" ~ "Valparaíso",
                layer == "13" ~ "Metropolitana",
                layer == "06" ~ "O'Higgins",
                layer == "07" ~ "Maule",
                layer == "16" ~ "Ñuble",
                layer == "08" ~ "Biobío",
                layer == "09" ~ "Araucanía",
                layer == "14" ~ "Los Ríos",
                layer == "10" ~ "Los Lagos",
                layer == "11" ~ "Aysén",
                layer == "12" ~ "Magallanes"), .before = 1) |> 
  mutate(n_region = fct_rev(fct_relevel(n_region, orden_regiones))) |> 
  select(-layer) |> 
  tibble() |> 
  st_as_sf() |> 
  st_transform(9153) |> 
  mutate(
    id_cluster_final =
      case_when(
        !is.na(id_cluster_final) ~ paste0(n_region,
                                          "-",
                                          str_pad(id_cluster_final,
                                                  width = 3,
                                                  side = "left",
                                                  pad = "0")),
        is.na(id_cluster_final) ~ NA
        ),
  ) |> 
  select(-persistence)





diccionario <- tibble(
  campo = names(st_drop_geometry(clusters_hdb)),
  tipo = lapply(st_drop_geometry(clusters_hdb), typeof) |> unlist(),
  descripción =
    c("Nombre región del cluster",
      "Identificador del Cluster Final de HDBSCAN (remueve puntos con outlier > 0.9)",
      "Total de viviendas dentro del cluster",
      "Probabilidad de pertenencia (promedio) de todas las viviendas dentro de un cluster",
      "Probabilidad de pertenencia (desv. estándar) de todas las viviendas dentro de un cluster",
      "Probabilidad de pertenencia (mínima) de todas las viviendas dentro de un cluster",
      "Probabilidad de pertenencia (máxima) de todas las viviendas dentro de un cluster",
      "Puntaje outlier (promedio) de todas las viviendas dentro de un cluster",
      "Puntaje outlier (desv. estándar) de todas las viviendas dentro de un cluster",
      "Puntaje outlier (mínima) de todas las viviendas dentro de un cluster",
      "Puntaje outlier (máxima) de todas las viviendas dentro de un cluster")
)

clusters_hdb |>
  st_write("datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
           layer = "cluster_viviendas_hdbs",
           append = FALSE)


diccionario |>
  st_drop_geometry() |> 
  st_write("datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
           layer = "diccionario_cluster_viviendas_hdbs",
           append = FALSE)
