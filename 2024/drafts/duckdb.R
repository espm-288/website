library(stars)
library(tmap)
library(tidyverse)
library(dplyr)
library(duckdbfs)

ineq <- st_read("/vsicurl/https://dsl.richmond.edu/panorama/redlining/static/mappinginequality.gpkg") |> st_make_valid()
st_crs(ineq)
ineq <- ineq[st_is_valid(ineq),]
ineq <- ineq |> mutate(grade = trimws(grade),
                       grade = na_if(grade, ""))

tmap_mode("view")
ineq |> filter(grade %in% c("A", "B", "C", "D")) |>
tm_shape() + tm_polygons("grade")




redlines <- duckdbfs::open_dataset("/vsicurl/https://dsl.richmond.edu/panorama/redlining/static/mappinginequality.json") |>
  filter(st_isValid(geom), !is.na(grade)) |> 
  mutate(area = st_area(geom)) 

gbif <- duckdbfs::open_dataset("/home/shared-data/gbif/occurrence/2023-10-01/occurrence.parquet/") |>
  filter(countrycode=="US") |>
  filter(stateprovince=="Connecticut") |>
  mutate(geom = st_point(decimallongitude, decimallatitude)) 

gbif_cities <- redlines |>
  spatial_join(gbif, join="inner")

df <- gbif_cities |> 
  filter(class=='Aves') |>
#  distinct(class, city, grade, area_id, area, genus) |>
  count(class, city, grade, area_id, area) |>
  mutate(density = n/area) |> 
  group_by(city, class, grade) |> 
  summarise(bio = mean(density)) |> 
  collect()

df |> ggplot(aes(grade, bio, fill=grade)) + geom_col() +
  facet_wrap(~city, scales = "free") + scale_y_log10()


tmap_mode("plot")
gbif_cities  |> 
  filter(city=="New Haven") |>
  count(area_id, grade, city, geom) |> 
  to_sf() |>
  tm_shape() + tm_polygons("n")  +  tm_borders("grade", lwd=2)



new_haven <- redlines |> filter(city == "New Haven") |> to_sf()
box <- c(st_bbox(new_haven))
attach(as.list(box))

nh_bio <- gbif |> filter(countrycode=="US",
               stateprovince=="Connecticut",
               class == "Aves",
               between(decimallongitude, {xmin}, {xmax}),
               between(decimallatitude, {ymin}, {ymax}),
               ) |>
  mutate(geom = st_point(decimallongitude, decimallatitude)) |> 
  to_sf() |> select(gbifid)

tmap_mode("plot")
tm_shape(new_haven) + tm_borders("grade") +
  tm_shape(nh_bio) + tm_dots()




NH <- st_join(new_haven, nh_bio, left=FALSE)

#curl::curl_download("https://minio.carlboettiger.info/ebird/Feb-2023/ebd_relFeb-2023.txt.gz", "/home/shared-data/ebird/ebd_relFeb-2023.txt.gz")
ebird <- duckdbfs::open_dataset("https://minio.carlboettiger.info/ebird/Feb-2023/ebd_relFeb-2023.txt.gz")

ebird <- duckdbfs::open_dataset("/home/shared-data/ebird/ebd_relFeb-2023.txt")

yale <- ebird |> 
  select(COUNTRY, STATE, LONGITUDE, LATITUDE, `SCIENTIFIC NAME`, `GLOBAL UNIQUE IDENTIFIER`) |>
  filter(COUNTRY=="United States",
                 STATE=="Connecticut",
                 between(LONGITUDE, {xmin}, {xmax}),
                 between(LATITUDE, {ymin}, {ymax})) |>
  mutate(geom = st_point(LONGITUDE, LATITUDE)) 


yale_redline <- redlines |>
  spatial_join(yale, join="inner") |>
  write_dataset("yale.parquet")

yale_sf <- yale_redline  |>
  count(area_id, grade, city, geom) |> 
  to_sf() 



library(tmap)
tmap_mode("plot")

|>
  tm_shape() + tm_polygons("n")  +  tm_borders("grade", lwd=2)

  

gbif_cities  |> 
  filter( city=="New Haven") |> count(class)
