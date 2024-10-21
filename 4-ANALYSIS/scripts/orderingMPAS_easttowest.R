# MPAs
mpa_layer_all = st_read(list.files(pattern = "SAMPAZ_OR_2021_Q3.shp",recursive=TRUE,full.names = TRUE))
# remove prince edward islands and group by number
mpa_layer_reduced = mpa_layer_all %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(MPA_NUMBER,CUR_NME) %>%
  summarise()
# get centroids
library(sf)
centroids = unlist(st_centroid(st_as_sf(mpa_layer_reduced))$geometry)
centroids = centroids[which(centroids>0)]
mpa_layer_reduced$longitudes = centroids
rm(centroids)
# reorder
mpa_layer_reduced = mpa_layer_reduced %>%
  arrange(longitudes)
mpa_layer_reduced$order_WtoE = 1:nrow(mpa_layer_reduced)
mpa_layer_reduced$geometry = NULL
mpa_layer_reduced$longitudes = NULL

mpa_layer_all = left_join(mpa_layer_all,mpa_layer_reduced)

# write
write.csv(mpa_layer_reduced,"mpas_ordered.csv",row.names = F)
