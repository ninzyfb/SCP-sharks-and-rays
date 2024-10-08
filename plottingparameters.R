# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads any features required for plotting
# This script also sets some plotting parameters to ensure uniform plots
# ---------------------------------


# ---------------------------------
# PLOTTING FEATURES
# ---------------------------------
# South African EEZ
eez = shapefile(list.files(pattern="eez.shp", recursive = TRUE, full.names = TRUE)) # load eez

# West, South and East marine regions as defined by Ebert et al.
regions = shapefile(list.files(pattern = "ebert_regions.shp", recursive = TRUE,full.names = TRUE))

# isobaths
contours = shapefile(list.files(pattern="contoursGEBCO.shp", recursive = TRUE, full.names=TRUE)) 

# South African province outline
#sa  <- raster::getData("GADM",country="South Africa",level=1)
sa = shapefile(list.files(pattern="sa.shp", recursive = TRUE, full.names=TRUE)) 

# Names of coastal cities
places = shapefile(list.files(pattern="ebert_placenames.shp", recursive = TRUE, full.names=TRUE)) 

# South African continental marine protected areas
mpas = st_read(list.files(pattern ="SAMPAZ_OR_2021_Q3.shp" ,recursive = TRUE, full.names = TRUE)[1])
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# REGIONS # -----------
# turn region names to upper case
regions$Region = toupper(regions$Region)
# create west south and east vector
# this will come into handdy during plotting
range = c("WEST","SOUTH","EAST")

# ISOBATHS # -----------
# extract 250m isobath
contours = contours[which(contours$DEPTH=="-250"),]

# PROVINCE OUTLINE # -----------
# simplify provinces to reduce the weight of the file
sa = st_as_sf(sa)
sa <- st_simplify(sa,dTolerance=100, preserveTopology = TRUE)
sa = as(sa, Class = "Spatial")

# PLACES # -----------
# adjust coordinates for some cities 
# this is to ensure the name is displayed in a "pretty" way on the map
adjustedcoords = coordinates(places)[c(10,14),]
adjustedcoords[,2] = adjustedcoords[,2]+0.2

# MPAS # -----------
# simplify MPA shapefile
mpas = mpas %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(CUR_NME,CUR_ZON_TY,CUR_ZON_NM) %>%
  summarise(GIS_AREA = sum(GIS_AREA))
# simplify mpas
mpas = st_simplify(mpas,dTolerance = 100)
# simpler MPA shapefile with ID going from west to east
mpas_simple = mpas %>%
  group_by(CUR_NME) %>%
  summarise(GIS_AREA = sum(GIS_AREA))
# get centroids  
centroids = st_centroid(mpas_simple)$geometry
longitudes = data.frame(lons = st_coordinates(centroids)[,1])
# add ID
longitudes$ID = seq(1:nrow(longitudes))
mpas_simple$ID =  seq(1:nrow(longitudes))
mpas_simple = left_join(longitudes,mpas_simple)

# sort by longitude from west to east
mpas_simple = mpas_simple %>%
  arrange(lons)
mpas_simple$ID =  seq(1:nrow(mpas_simple))
write_sf(mpas_simple,"plotting_features/mpas_simple/mpas_simple.shp",overwrite=T)
mpas = as(mpas, Class = "Spatial")

# add take and no-take specification to mpas dataframe
#mpas@data = mpas%>%
#  mutate(type = ifelse(CUR_ZON_TY %in% mpas_notake$CUR_ZON_TY,"no-take","take"))

# OTHER # -----------
# set intervals for modelling
intervals = seq(0,1000,200)

# set intervals for planning plot
intervals2 = seq(0,1,0.2)

# colours for plots
cols <- colorRampPalette(c("white","darkgreen"))
cols2 <- colorRampPalette(c("yellow"))

# Expert extents
load(list.files(pattern = "points.RData", recursive = TRUE, full.names = TRUE))
expert_extent = points
colnames(expert_extent)[1] = "Scientific_name" 
expert_extent = as(expert_extent, Class = "Spatial")
expert_extent$Scientific_name = tolower(expert_extent$Scientific_name)
rm(points)

# blank template (for raw plots script)
blank_template = raster(list.files(pattern = "template_10km.tif$", recursive = TRUE, full.names = TRUE))
values(blank_template) = NA

# legend coordinates (for raw plots script)
legend = data.frame()
legend[1,1] = 36
legend[1,2] = -36.5
legend[2,1] = 36
legend[2,2] = -37
legend[3,1] = 36
legend[3,2] = -37.5
legend[4,1] = 36
legend[4,2] = -38
coordinates(legend) <- ~V1+V2

# color palette
manual.col = colorRampPalette(c("#f7f6fd","#4635d0"))

# list of bounding boxes
bboxes = list()
# kzn
bbox_temp = bbox(sa[2,])
bbox_temp[1,] = c(30,36)
bbox_temp[2,] = c(-26,-32)
bboxes[[1]] = bbox_temp
# eastern cape
bbox_temp = bbox(sa[1,])
bbox_temp[1,] = c(23,31)
bbox_temp[2,] =c(-31,-38)
bboxes[[2]] = bbox_temp
# western cape
bbox_temp = bbox(sa[4,])
bbox_temp[1,] = c(14,23.5)
bbox_temp[2,] = c(-33,-38)
bboxes[[3]] = bbox_temp
# northern cape
bbox_temp = bbox(sa[3,])
bbox_temp[1,] = c(14,19)
bbox_temp[2,] = c(-28,-33)

bboxes[[4]] = bbox_temp
names(bboxes) = c("KZN","EC","WC","NC")

# depth contour legend line
depth_xcoords <- c(33,34)
depth_ycoords <- c(-37.1,-37.1)
# create spatial points
depth_points <- sp::SpatialPoints(cbind(depth_xcoords,depth_ycoords))
# use as to convert to line
depth_line <- as(depth_points,"SpatialLines")
rm(depth_xcoords,depth_ycoords,depth_points)

# iucn legend line
iucn_xcoords <- c(33,34)
iucn_ycoords <- c(-37.6,-37.6)
# create spatial points
iucn_points <- sp::SpatialPoints(cbind(iucn_xcoords,iucn_ycoords))
# use as to convert to line
iucn_line <- as(iucn_points,"SpatialLines")
rm(iucn_xcoords,iucn_ycoords,iucn_points)

# binary legend line
binary_xcoords <- c(33,34)
binary_ycoords <- c(-36.7,-36.7)
# create spatial points
binary_points <- sp::SpatialPoints(cbind(binary_xcoords,binary_ycoords))
# use as to convert to line
binary_line <- as(binary_points,"SpatialLines")
rm(binary_xcoords,binary_ycoords,binary_points)

# legend box (top left)
x_coord <- c(23.5,  13,  13, 23.5, 23.5)
y_coord <- c(-28.5, -28.5, -26, -26, -28.5)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))

# Title box (top centre)
x_coord <- c(31,  19,  19, 31, 31)
y_coord <- c(-27, -27, -28, -28, -27)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
tb_top = SpatialPolygons(list(ps))
rm(x_coord,y_coord,xym,p,ps)
# ---------------------------------

