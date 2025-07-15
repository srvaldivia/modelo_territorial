
# 0. paquetes -------------------------------------------------------------
library(tidyverse)
library(sf)
library(ggrepel)
library(ggtext)
library(readxl)
library(ggplot2)


# 1. carga de datos -------------------------------------------------------
# capas <- st_layers("datos_output/GIS_resultados_entrega_1.gdb")$name[seq(1, 31, 2)]
orden_regiones <- c("Arica y Parinacota", "Tarapacá", "Antofagasta",
                    "Atacama", "Coquimbo", "Valparaíso", "Metropolitana",
                    "O'Higgins", "Maule", "Ñuble", "Biobío",
                    "Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes")
# 
# lista <- list(); for (i in capas) {
#   tmp <-
#     st_read("datos_output/GIS_resultados_entrega_1.gdb",
#             layer = i,
#             as_tibble = TRUE) |>
#     mutate(es_cluster =
#              case_when(!is.na(id_cluster_final) ~ 1,
#                        is.na(id_cluster_final)  ~ 0),
#            n_region = str_squish(n_region)) |>
#     st_drop_geometry()
#   lista[[i]] <- tmp
# }
# 
# viviendas_hdb <- bind_rows(lista) |>
#   mutate(n_region = str_to_title(n_region)) |>
#   mutate(n_provincia = str_to_title(n_provincia)) |>
#   mutate(n_region = case_when(n_region == "Arica Y Parinacota" ~ "Arica y Parinacota",
#                               n_region == "O'higgins" ~ "O'Higgins",
#                               .default = n_region)) |>
#   mutate(n_region = fct_rev(fct_relevel(n_region, orden_regiones)))


viviendas_hdb <- st_read("datos_output/Entrega 1/GIS_resultados_entrega_1.gdb",
                         layer = "viviendas_agrupamiento_hdbs",
                         as_tibble = TRUE) |> 
  st_drop_geometry() |> 
  mutate(
    macrozona =
      case_when(n_region %in% c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo") ~ "Norte",
                n_region %in% c("Valparaíso", "Metropolitana", "O'Higgins","Maule") ~ "Centro",
                n_region %in% c("Ñuble", "Biobío", "Araucanía", "Los Ríos") ~ "Sur",
                n_region %in% c("Los Lagos", "Aysén", "Magallanes") ~ "Austral"),
    macrozona = fct_relevel(macrozona, c("Austral", "Sur", "Centro", "Norte")),
    n_region = fct_rev(fct_relevel(n_region, orden_regiones)),
    n_comuna = str_to_title(n_comuna)
    )

# 2. cálculo de estadísticas por escalas ----------------------------------
resumen_region <- viviendas_hdb |> 
  st_drop_geometry() |> 
  group_by(n_region, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas)

resumen_comuna <- viviendas_hdb |> 
  st_drop_geometry() |> 
  group_by(n_region, n_comuna, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(n_region = str_squish(n_region),
         prop_viviendas = viviendas / total_viviendas)

resumen_provincia <- viviendas_hdb |>
  st_drop_geometry() |> 
  group_by(n_region, n_provincia, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas)

resumen_provincia2 <- viviendas_hdb |>
  st_drop_geometry() |> 
  group_by(n_region, n_provincia, macrozona, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas)


resumen_viviendas_cluster_reg <- viviendas_hdb |>
  filter(!is.na(id_cluster_final)) |>
  group_by(n_region, id_cluster_final) |>
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
  group_by(n_region, n_provincia, id_cluster_final) |>
  summarise(n = n()) |> 
  summarise(n_cluster = n(),
            n_viv = sum(n),
            min_viv = min(n),
            max_viv = max(n),
            mean_viv = mean(n)) |> 
  ungroup() |> 
  mutate(dif = max_viv - min_viv)

resumen_viviendas_cluster_prov2 <- viviendas_hdb |>
  filter(!is.na(id_cluster_final)) |>
  group_by(n_region, n_provincia, macrozona, id_cluster_final) |>
  summarise(n = n()) |> 
  summarise(n_cluster = n(),
            n_viv = sum(n),
            min_viv = min(n),
            max_viv = max(n),
            mean_viv = mean(n)) |> 
  ungroup() |> 
  mutate(dif = max_viv - min_viv,
         n_provincia_fct = fct_reorder(n_provincia, dif),
         n_provincia_num = as.numeric(n_provincia_fct))
  




# 3. graficos ESCALA NACIONAL ---------------------------------------------
## 3.1 RANGO DE VIVIENDAS ----
ggplot(resumen_viviendas_cluster_reg) +
  geom_segment(aes(x = min_viv, y = n_region,
                   xend = max_viv, yend = n_region),
               color = "#aeb6bf",
               lwd = 6.5,
               alpha = .5) +
  geom_point(aes(x = min_viv, y = n_region),
             colour = "#009688", size = 6) +
  geom_point(aes(x = max_viv, y = n_region),
             colour = "#762a83", size = 6) +
  geom_text(data = resumen_viviendas_cluster_reg |> filter(n_region != "Tarapacá"),
            aes(x = dif / 2,
                y = n_region,
                label = round(mean_viv, 1)),
            size = 2.5) +
  geom_text(data = resumen_viviendas_cluster_reg |> filter(n_region == "Tarapacá"),
            aes(x = max_viv,
                y = n_region,
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
               # y = fct_reorder2(n_provincia, es_cluster, prop_viviendas),
               y = n_region,
               group = es_cluster,
               fill = fct_rev(as.character(es_cluster))),
           stat = "identity",
           position = "fill",
           alpha = 0.9) +
  geom_text(data = resumen_region |> filter(es_cluster == 1),
            aes(x = 0.3,
                y = n_region,
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
    resumen_viviendas_cluster_prov |> filter(n_region == region)
  grafico <- ggplot(contenido_region) +
    geom_segment(aes(x = min_viv, y = fct_reorder(n_provincia, dif),
                     xend = max_viv, yend = n_provincia),
                 color = "#aeb6bf",
                 size = 6.5,
                 alpha = .5) +
    geom_point(aes(x = min_viv, y = n_provincia),
               colour = "#009688", size = 6) +
    geom_point(aes(x = max_viv, y = n_provincia),
               colour = "#762a83", size = 6) +
    geom_text(aes(x = max_viv / 2,
                  y = n_provincia,
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
  
  # # Nombres para guardar mis graficos
  # nombre_archivo <- paste0("plots_output/entrega_1/RANGO_DE_VIVIENDAS_",
  #                          gsub(" ", "_", region), ".png")
  # 
  # # Funcion para guardar
  # ggsave(nombre_archivo,
  #        plot = grafico,
  #        width = 7,
  #        height = 3,
  #        dpi = 300)
}

## 4.1 RANGO DE VIVIENDAS2 ----
for (i in unique(resumen_viviendas_cluster_prov2$macrozona)) {
  contenido_region <- resumen_viviendas_cluster_prov2 |> filter(macrozona == i)
  
  valor <- round(max(contenido_region$n_viv)) * 0.05
  
  
  grafico <- ggplot(contenido_region) +
    geom_segment(aes(x = min_viv, y = fct_reorder(n_provincia, dif),
                     xend = max_viv, yend = n_provincia),
                 color = "#aeb6bf",
                 size = 6.5,
                 alpha = .5) +
    geom_point(aes(x = min_viv, y = n_provincia),
               colour = "#009688", size = 6) +
    geom_point(aes(x = max_viv, y = n_provincia),
               colour = "#762a83", size = 6) +
    geom_text(data = contenido_region |> filter(dif >= 500),
              aes(x = max_viv / 2,
                  y = n_provincia,
                  label = round(mean_viv)),
              size = 3) + 
    geom_text(data = contenido_region |> filter(dif < 500),
              aes(x = max_viv * 0.5,
                  y = n_provincia,
                  label = round(mean_viv)),
              nudge_x = 1000,
              size = 3) +
    labs(x = "viviendas rurales",
         y = "Provincia",
         title = str_glue("Macrozona {i}"),
         caption = "El valor dentro de cada barra corresponde al promedio de viviendas por cluster en cada provincia") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(size = 10),
          plot.title = 
            element_text(face = "bold",
                         hjust = 0.5))

  print(grafico)
  # Nombres para guardar mis graficos
  nombre_archivo <- paste0("plots_output/entrega_1/MACROZONAS/MZ_RANGO_DE_VIVIENDAS_",
                           gsub(" ", "_", i), ".png")

  # Funcion para guardar
  ggsave(nombre_archivo,
         plot = grafico,
         width = 7,
         height = 7,
         dpi = 300)
}



## 4.2 % DE VIVIENDAS CLUSTERIZADAS ----
for (region in orden_regiones) {
  contenido_region <-
    resumen_provincia |> filter(n_region == region)
  grafico <- ggplot(contenido_region) +
    geom_bar(aes(x = viviendas,
                 y = fct_rev(fct_reorder2(n_provincia, es_cluster, prop_viviendas)),
                 # y = n_provincia,
                 group = es_cluster,
                 fill = fct_rev(as.character(es_cluster))),
             stat = "identity",
             position = "fill") +
    geom_text(data = resumen_provincia |>
                filter(n_region == region) |> 
                filter(es_cluster == 1),
              aes(x = 0.3,
                  y = n_provincia,
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

## 4.2 % DE VIVIENDAS CLUSTERIZADAS2 ----
  
  
for (i in unique(resumen_provincia2$macrozona)) {
  contenido_region <- resumen_provincia2 |> filter(macrozona == i)
  
  grafico <- ggplot(contenido_region) +
    geom_bar(aes(x = viviendas,
                 y = fct_rev(fct_reorder2(n_provincia, es_cluster, prop_viviendas)),
                 # y = n_provincia,
                 group = es_cluster,
                 fill = fct_rev(as.character(es_cluster))),
             stat = "identity",
             position = "fill") +
    geom_text(data = resumen_provincia2 |>
                filter(macrozona == i) |> 
                filter(es_cluster == 1),
              aes(x = 0.3,
                  y = n_provincia,
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
         y = "Provincia",
         title = str_glue("Macrozona {i}")) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = "bottom",
          axis.text.y = element_text(size = 10, margin = margin(r = -10)),
          axis.ticks.x = element_line(color = "black"),
          plot.title = 
            element_text(face = "bold",
                         hjust = 0.5))
  
  print(grafico)
  # Nombres para guardar mis graficos
  nombre_archivo <- paste0("plots_output/entrega_1/MACROZONAS/MZ_VIVIENDAS_CLUSTERIZADAS_",
                           gsub(" ", "_", i), ".png")

  # Funcion para guardar
  ggsave(nombre_archivo,
         plot = grafico,
         width = 7,
         height = 7,
         dpi = 300)
}






# TABLAS ------------------------------------------------------------------
anexo2 <- viviendas_hdb |>
  st_drop_geometry() |> 
  group_by(n_region, n_provincia, n_comuna, es_cluster) |> 
  summarise(viviendas = n()) |> 
  mutate(total_viviendas = sum(viviendas)) |> 
  ungroup() |> 
  mutate(prop_viviendas = viviendas / total_viviendas) |> 
  filter(es_cluster == 1) |> 
  rename(viviendas_cluster = viviendas) |> 
  mutate(viviendas_ruido = total_viviendas - viviendas_cluster,
         prop_viviendas_cluster = 1- prop_viviendas) |> 
  arrange(fct_rev(n_region)) |> 
  writexl::write_xlsx("datos_output/Entrega 1/Anexo2.xlsx")
  



# 1.2. cálculo de estadísticas por macrozona - densidad boxplot y puntos ----------------------------------

# Calculo x macrozona
viviendas_por_cluster <- viviendas_hdb |>
  filter(!is.na(id_cluster_final)) |>
  group_by(macrozona, id_cluster_final) |>
  summarise(n = n()) |> 
  ungroup()



# Funcion para guardar
for (i in unique(viviendas_por_cluster$macrozona)) {
  datos_zona <- viviendas_por_cluster |> filter(macrozona == i)
  p <-
    ggplot(datos_zona, aes(x = n)) +
    geom_jitter(
      aes(y = macrozona),
      height = 0.25,
      alpha = 0.5,
      size = 0.5,
      color = "#009688", 
    ) +
    geom_boxplot(
      aes(y = macrozona),
      width = 0.25,
      fill = "white",
      color = "#009688",
      alpha = 0.5,
      # outliers = FALSE,
      # outlier.shape = NA
      # position = position_nudge(y = -0.0003)
    ) +
    # ggdist::stat_slab(
    #   adjust = 2
    #   )
    # geom_density(
    #   fill = "#008B45",
    #   colour = NA,
    #   alpha = 0.5,
    #   adjust = 1/5
    #   )
    # theme_minimal() +
    labs(x = NULL, y = NULL) +
    theme_minimal()
    # theme(
    #   strip.text = element_text(size = 11, face = "bold"),
    #   axis.text.y = element_blank(),
    #   axis.ticks.y = element_blank(),
    #   axis.title.y = element_text(size = 11, face = "bold"),
    #   axis.text.x = element_text(),
    #   axis.ticks.x = element_line(),
    #   axis.title.x = element_text(),
    #   panel.grid.major.y = element_blank(),
    #   panel.grid.minor.y = element_blank(),
    #   panel.grid.minor.x = element_blank()
    # ) 
  
  print(p)
  # Guardar gráfico
  # ggsave(
  #   filename = paste0("/data/seba/10_modelo_MOP/plots_output/grafico_macrozona_", zona, ".png"),
  #   plot = p,
  #   width = 7,
  #   height = 3,
  #   dpi = 300
  # )
}





# graficos bonitos

df <- viviendas_hdb |> 
  st_drop_geometry() |> 
  filter(es_cluster == 1) |> 
  group_by(macrozona, id_cluster_final) |> 
  summarise(n = n()) |> 
  ungroup()

my_pal <- c("#E68310", "#11A579", "#7F3C8D", "#3969AC")

ggplot(
  # data = df |> filter(macrozona == "Norte"),
  data = df,
  aes(x = macrozona,
      y = n,
      fill = macrozona,
      colour = macrozona)
) +
  # gghalves::geom_half_point(
  #   side = "l",
  #   position = position_nudge(x = -0.05),
  #   size = 0.3,
  #   alpha = 0.3,
  #   # range_scale = .3
  # ) +
  # geom_boxplot(
  #   width = 0.15,
  #   fill = "white",
  #   size = 0.5,
  #   outlier.shape = NA,
  #   alpha = 0.5,
  #   position = position_nudge(x = -0.24)
  # ) +
  geom_boxplot(
    width = 0.25,
    fill = "white",
    size = 0.5,
    # outlier.shape = NA,
    alpha = 0.5,
    position = position_nudge(x = -0.24)
  ) +
  gghalves::geom_half_violin(
    side = "r",
    scale = "width",
    adjust = 0.5
  ) +
  coord_flip() +
  # scale_x_discrete(labels = c("Escolaridad\n(UV original)",
  #                             "Random\n(UV original)",
  #                             "Escolaridad\n(UV maxp)", "Random\n(UV maxp)")) +
  scale_colour_manual(values = my_pal, guide = "none") +
  scale_fill_manual(values = my_pal, guide = "none") +
  labs(x = NULL) +
  theme_minimal()




# mapas -------------------------------------------------------------------

resumen_viviendas_cluster_reg <- resumen_viviendas_cluster_reg |> 
  mutate(id_join = janitor::make_clean_names(string = n_region, allow_dupes = TRUE)) |> 
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
  )

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
  st_cast("MULTIPOLYGON") |> 
  mutate()



mapas_region <- 
  left_join(x = sf_region,
            y = resumen_viviendas_cluster_reg,
            by = join_by(codigo_region))

mapview::mapview(mapas_region, zcol = "n_cluster")
