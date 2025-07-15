
# numero de clusters por macrozona/region
# prop de viviendas clusterizadas por region/comuna

# 0. paquetes -------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(ggtext)
library(readxl)
library(ggplot2)
library(tmap)

orden_regiones <- c("Arica y Parinacota", "Tarapacá", "Antofagasta",
                    "Atacama", "Coquimbo", "Valparaíso", "Metropolitana",
                    "O'Higgins", "Maule", "Ñuble", "Biobío",
                    "Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes")

# 1. carga de datos -------------------------------------------------------
## 1.1 viviendas resultado HDBSCAN ----
viviendas_hdb <- st_read("datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
                         layer = "viviendas_agrupamiento_hdbs",
                         as_tibble = TRUE) |> 
  # st_drop_geometry() |> 
  mutate(
    macrozona =
      case_when(n_region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Norte",
                n_region %in% c("Valparaíso", "Metropolitana", "O'Higgins","Maule") ~ "Centro",
                n_region %in% c("Ñuble", "Biobío", "Araucanía", "Los Ríos") ~ "Sur",
                n_region %in% c("Los Lagos", "Aysén", "Magallanes") ~ "Austral"),
    macrozona = fct_relevel(macrozona, c("Austral", "Sur", "Centro", "Norte")),
    n_region = fct_rev(fct_relevel(n_region, orden_regiones)),
    n_comuna = str_to_title(n_comuna)
  ) |> 
  mutate(
    codigo_region =
      case_when(
        n_region == "Arica y Parinacota" ~ 15,
        n_region == "Tarapacá" ~ 1,
        n_region == "Antofagasta" ~ 2,
        n_region == "Atacama" ~ 3,
        n_region == "Coquimbo" ~ 4,
        n_region == "Valparaíso" ~ 5,
        n_region == "Metropolitana" ~ 13,
        n_region == "O'Higgins" ~ 6,
        n_region == "Maule" ~ 7,
        n_region == "Ñuble" ~ 16,
        n_region == "Biobío" ~ 8,
        n_region == "Araucanía" ~ 9,
        n_region == "Los Ríos" ~ 14,
        n_region == "Los Lagos" ~ 10,
        n_region == "Aysén" ~ 11,
        n_region == "Magallanes" ~ 12
      )
  ) |> 
  mutate(
    across(.cols = c(n_region:n_comuna, n_distrito),
           .fns = ~ str_squish(.x)),
    across(.cols = c(n_region:n_comuna, n_distrito),
           .fns = ~ str_to_title(.x)),
    id_distrito = paste0(n_comuna, "-", n_distrito)
    )



## 1.2 dpa regional ----
sf_region <- chilemapas::mapa_comunas |>
  filter(!codigo_comuna %in% c("05201", "05104", "05104")) |> 
  st_set_geometry("geometry") |>
  st_make_valid() |> 
  st_cast("POLYGON") |> 
  rmapshaper::ms_simplify() |>
  group_by(codigo_region) |>
  summarise(n = n()) |>
  ungroup() |>
  rmapshaper::ms_explode() |> 
  mutate(area = as.numeric(st_area(geometry)),
         codigo_region = as.numeric(codigo_region)) |> 
  slice_max(order_by = area, prop = 0.3) |> 
  group_by(codigo_region) |>
  summarise() |> 
  ungroup() |> 
  st_cast("MULTIPOLYGON")



## 1.3 dpa comunal ----
gdbs <- list.files("datos_input", pattern = "\\.gdb$", full.names = TRUE)
lista <- list(); for (i in gdbs) {
  tmp <-
    st_read(i,
            layer = "Comunal",
            as_tibble = TRUE) |> 
    st_transform(4326) |> 
    st_cast("MULTIPOLYGON")
  lista[[i]] <- tmp
}

sf_comuna <- bind_rows(lista) |>
  select(cut = CUT,
         n_region = N_REGION,
         n_comuna = N_COMUNA) |> 
  mutate(n_region = str_squish(n_region),
         n_comuna = str_to_title(n_comuna)) |> 
  mutate(
    n_region = 
      case_when(
        n_region == "Arica Y Parinacota" ~ "Arica y Parinacota",
        n_region == "O'higgins" ~ "O'Higgins",
        .default = n_region
      )
    ) |> 
  mutate(
    macrozona =
      case_when(n_region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Norte",
                n_region %in% c("Valparaíso", "Metropolitana", "O'Higgins","Maule") ~ "Centro",
                n_region %in% c("Ñuble", "Biobío", "Araucanía", "Los Ríos") ~ "Sur",
                n_region %in% c("Los Lagos", "Aysén", "Magallanes") ~ "Austral"),
    macrozona = fct_relevel(macrozona, c("Austral", "Sur", "Centro", "Norte")),
    n_region = fct_rev(fct_relevel(n_region, orden_regiones))
  )
  

## 1. dpa distrito censal ----
rm(lista)
lista <- list(); for (i in gdbs) {
  tmp <-
    st_read(i,
            layer = "Distrital",
            as_tibble = TRUE) |> 
    st_transform(4326) |> 
    st_cast("MULTIPOLYGON")
  lista[[i]] <- tmp
}

sf_distrito <- bind_rows(lista) |>
  select(cut = CUT,
         n_region = N_REGION,
         n_provincia = N_PROVINCIA,
         n_comuna = N_COMUNA,
         n_distrito = N_DISTRITO,
         cod_distrito = COD_DISTRITO,
         tipo_distrito = TIPO_DISTRITO) |> 
  mutate(
    across(.cols = c(n_region:n_comuna, n_distrito),
           .fns = ~ str_squish(.x)),
    across(.cols = c(n_region:n_comuna, n_distrito),
           .fns = ~ str_to_title(.x))
    ) |> 
  mutate(
    n_region = 
      case_when(
        n_region == "Arica Y Parinacota" ~ "Arica y Parinacota",
        n_region == "O'higgins" ~ "O'Higgins",
        .default = n_region
      )
  ) |> 
  mutate(
    macrozona =
      case_when(n_region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Norte",
                n_region %in% c("Valparaíso", "Metropolitana", "O'Higgins","Maule") ~ "Centro",
                n_region %in% c("Ñuble", "Biobío", "Araucanía", "Los Ríos") ~ "Sur",
                n_region %in% c("Los Lagos", "Aysén", "Magallanes") ~ "Austral"),
    macrozona = fct_relevel(macrozona, c("Austral", "Sur", "Centro", "Norte")),
    n_region = fct_rev(fct_relevel(n_region, orden_regiones)),
    id_distrito = paste0(n_comuna, "-", n_distrito)
  )





# 2. Calcular estadísticas para mapear ------------------------------------
resumen_cluster_reg <- viviendas_hdb |>
  st_drop_geometry() |> 
  filter(!is.na(id_cluster_final)) |>
  group_by(macrozona, n_region, codigo_region, id_cluster_final) |>
  summarise(n = n()) |> 
  summarise(n_cluster = n(),
            n_viv = sum(n),
            min_viv = min(n),
            max_viv = max(n),
            mean_viv = mean(n)) |> 
  ungroup() |> 
  mutate(dif = max_viv - min_viv,
         viv_por_cluster = n_viv / n_cluster,
         cluster_por_viv = n_cluster / n_viv * 10000) |> 
  mutate(macrozona = fct_relevel(macrozona, c("Norte", "Centro", "Sur", "Austral")))



resumen_viviendas_com <- viviendas_hdb |>
  st_drop_geometry() |> 
  group_by(macrozona, codigo_region, n_region, cut, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas) |> 
  filter(es_cluster == 1) |>
  rename(viviendas_cluster = viviendas,
         prop_viviendas_cluster = prop_viviendas) |> 
  mutate(viviendas_ruido = total_viviendas - viviendas_cluster,
         .after = viviendas_cluster) |>
  mutate(prop_viviendas_ruido = 1 - prop_viviendas_cluster) |> 
  select(-es_cluster)

  
resumen_viviendas_dis <- viviendas_hdb |>
  st_drop_geometry() |> 
  group_by(macrozona, codigo_region, n_region, cut, id_distrito, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas) |> 
  filter(es_cluster == 1) |>
  rename(viviendas_cluster = viviendas,
         prop_viviendas_cluster = prop_viviendas) |> 
  mutate(viviendas_ruido = total_viviendas - viviendas_cluster,
         .after = viviendas_cluster) |>
  mutate(prop_viviendas_ruido = 1 - prop_viviendas_cluster) |> 
  select(-es_cluster)


# 3. Pegar stats a geometrías ---------------------------------------------
mapas_region <- 
  left_join(x = sf_region,
            y = resumen_cluster_reg,
            by = join_by(codigo_region))

mapas_comuna <- 
  left_join(x = sf_comuna,
            y = resumen_viviendas_com,
            by = join_by(cut), keep = TRUE) |> 
  mutate(
    across(.cols = c(viviendas_cluster:total_viviendas),
           .fns = ~ replace_na(.x, 0))
    ) |> 
  select(cut = cut.x,
         n_comuna,
         n_region = n_region.x,
         macrozona = macrozona.x,
         viviendas_cluster:last_col())

mapas_distrito <- 
  left_join(x = sf_distrito,
            y = resumen_viviendas_dis,
            by = join_by(id_distrito), keep = TRUE) |> 
  select(
    n_region = n_region.x,
    cut = cut.x,
    n_provincia,
    n_comuna,
    n_distrito,
    id_distrito = id_distrito.x,
    viviendas_cluster:last_col()
    ) |> 
  mutate(
    across(.cols = c(viviendas_cluster:total_viviendas),
           .fns = ~ replace_na(.x, 0))
    )

# 4. test mapas -----------------------------------------------------------
# ## no es tan informativo porque a mayor población mayor n de clusters
# tm_shape(mapas_region) +
#   tm_polygons(fill = c("n_cluster"),
#               fill.legend = tm_legend(title = "Total de cluster")) +
#   tm_facets(by = "macrozona", ncol = 4)
# 
# ## Tasa de cluster cada 10.000 viviendas
# tm_shape(mapas_region) +
#   tm_polygons(fill = c("cluster_por_viv"),
#               fill.legend = tm_legend(title = "Cluster cada 10.000 viviendas")) +
#   tm_facets(by = "macrozona", ncol = 4)
# 
# ## Tamaño de clusters
# tm_shape(mapas_region) +
#   tm_polygons(fill = c("viv_por_cluster"),
#               fill.legend = tm_legend(title = "Viviendas por cluster")) +
#   tm_facets(by = "macrozona", ncol = 4)
# 
# tm_shape(mapas_comuna) +
#   tm_polygons(fill = c("viviendas_cluster")) +
#   tm_facets(by = "macrozona", ncol = 4)
# 
# mapview::mapview(mapas_comuna, zcol = "prop_viviendas_cluster")


# 5. exportar -------------------------------------------------------------

st_write(mapas_region, "datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
         layer = "mapas_region", append = FALSE)

st_write(mapas_comuna, "datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
         layer = "mapas_comuna", append = FALSE)

st_write(mapas_distrito, "datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
         layer = "mapas_distrito", append = FALSE)


tibble(
  campo = names(st_drop_geometry(mapas_region)),
  tipo = lapply(st_drop_geometry(mapas_region), typeof) |> unlist(),
  descripción =
    c("Código regional",
      "Macrozona a nivel país",
      "Nombre de región",
      "Número de cluster por región",
      "Número de viviendas clusterizadas por región",
      "Número de viviendas del cluster que menos viviendas posee por región",
      "Número de viviendas del cluster que más viviendas posee por región",
      "Promedio de viviendas de los clusters por región",
      "Diferencia entre el número de viviendas máximo y número de viviendas mínimo",
      "Número de viviendas por cluster por región (total de viviendas clusterizadas / total de clusters)",
      "Número de clusters cada 10.000 viviendas rurales por región (tasa)"
    )
  ) |> 
  write_excel_csv2("datos_output/Entrega 1/mapas_region.csv")
  

tibble(
  campo = names(st_drop_geometry(mapas_comuna)),
  tipo = lapply(st_drop_geometry(mapas_comuna), typeof) |> unlist(),
  descripción =
    c("Código único territorial comunal",
      "Nombre de comuna",
      "Nombre de región",
      "Macrozona a nivel país",
      "Número de viviendas clusterizadas por comuna",
      "Número de viviendas no clusterizadas (ruido) por comuna",
      "Total de viviendas rurales por comuna (viviendas clusterizadas + no clusterizadas)",
      "Porcentaje de viviendas clusterizadas por comuna",
      "Porcentaje de viviendas no clusterizadas (ruido) por comuna")
  ) |> 
  write_excel_csv2("datos_output/Entrega 1/mapas_comuna.csv")

tibble(
  campo = names(st_drop_geometry(mapas_distrito)),
  tipo = lapply(st_drop_geometry(mapas_distrito), typeof) |> unlist(),
  descripción =
    c("Nombre de región",
      "Código único territorial comunal",
      "Nombre provincia",
      "Nombre de comuna",
      "Nombre de distrito",
      "Identificador único de cada distrito a nivel país",
      "Número de viviendas clusterizadas por distrito censal",
      "Número de viviendas no clusterizadas (ruido) por distrito censal",
      "Total de viviendas rurales por distrito censal (viviendas clusterizadas + no clusterizadas)",
      "Porcentaje de viviendas clusterizadas por distrito censal",
      "Porcentaje de viviendas no clusterizadas (ruido) por distrito censal")
  ) |> 
  write_excel_csv2("datos_output/Entrega 1/mapas_distrito.csv")


