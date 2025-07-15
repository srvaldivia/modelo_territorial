
# 0. paquetes ------------------------------------------------------------------
library(tidyverse)
library(sf)
library(dbscan)
library(terra)
library(cli)
# library(qgisprocess)


# 1. parametros globales --------------------------------------------------
tictoc::tic()
region <- "R15"
gdb_region <- "datos_input/APC2023_R15.gdb"
capa_vialidad <- "Eje_Vial"
capa_vivienda <- "Puntos_Edificacion_Rural_utm"
capa_distrito <- "Distrital"
rst_dem <- "datos_input/dem/15.jp2"
crs_codigo <- 32719

# 1. carga de datos ------------------------------------------------------------

## 1.1 red vial ----
red_vial <- st_read(dsn = gdb_region,
                    layer = capa_vialidad,
                    as_tibble = TRUE)

## 1.2 viviendas ----
viviendas <- st_read(dsn = gdb_region,
                     layer = capa_vivienda,
                     as_tibble = TRUE)

## 1.3 distritos ----
distritos <- st_read(dsn = gdb_region,
                     layer = capa_distrito,
                     as_tibble = TRUE)

# 1.4 dem ----
dem <- rast(rst_dem)
# 
# dem_a <- rast("datos_input/dem/12 norte.jp2")
# dem_b <- rast("datos_input/dem/12 sur.jp2")
# 
# dem_b <- terra::project(dem_b, crs(dem_a))





# 2. manipulación/transformación -----------------------------------------------

## 2.1 red vial ----
red_vial <- red_vial |> 
  st_cast("MULTILINESTRING") |>
  st_cast("LINESTRING") |> 
  st_transform(crs_codigo) |> 
  st_make_valid() |> 
  rename(geometry = last_col())

## 2.2 viviendas ----
viviendas <- viviendas |> 
  rename(geometry = last_col())

# 2.3 dem ----
names(dem) <- "dem"
if (crs(dem) == "" & abs(ext(dem)[1]) <= 180) {
  cat("Asignando CRS...\n")
  crs(dem) <- "EPSG:4326"
} else if (crs(dem) == "" & abs(ext(dem)[1]) > 180) {
  cat("Asignando CRS...\n")
  crs(dem) <- paste0("EPSG:", crs_codigo)  
} else {
  cat("Reproyectando...\n")
  dem <- terra::project(dem, crs(viviendas), threads = TRUE)
}


# 2.4 slope ----
# slope <- terrain(dem, v = "slope", unit = "degrees")

# 2.4 distritos ----
distritos <- distritos |> 
  st_transform(crs_codigo) |> 
  st_cast("MULTIPOLYGON") |> 
  st_cast("POLYGON")
  

# 3. calcular y unificar -------------------------------------------------------
viviendas <- viviendas |>
  st_join(y = red_vial |>
            mutate(geom_vial = geometry, FID_via = row_number()) |> 
            select(FID_via, geom_vial, geometry),
          join = st_nearest_feature) |>
  st_join(y = distritos |>
            select(N_DISTRITO, TIPO_DISTRITO),
          join = st_nearest_feature) |>
  relocate(c(N_DISTRITO, TIPO_DISTRITO), .after = N_COMUNA) |> 
  rowwise() |>   
  mutate(POINT_D = st_distance(geom_vial, geometry)[,1]) |> 
  ungroup() |>
  select(-geom_vial) |> 
  cbind(extract(dem, viviendas, ID = FALSE)) |> 
        # extract(slope, viviendas, ID = FALSE)) |> 
  rename(POINT_Z = last_col(1)) |> 
  relocate(c(POINT_X, POINT_Y, POINT_Z, POINT_D),
           .before = last_col()) |> 
  tibble() |> 
  st_as_sf()


# 4. exportar ------------------------------------------------------------------
st_write(obj = viviendas,
         dsn = gdb_region,
         layer = paste0(region, "_puntos_edificacion_rural"),
         append = FALSE)
print(paste0(region, "_puntos_edificacion_rural"))

# write_rds(viviendas,
#           file = paste0("datos_input/",
#                         region,
#                         "_puntos_edificacion.rds"));print(paste0("datos_input/",
#                                                                  region,
#                                                                  "_puntos_edificacion.rds"))
tictoc::toc()