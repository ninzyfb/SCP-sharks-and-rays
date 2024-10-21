# ---------------------------------------------------------------------------------
# AUTHORS: Nina Faure Beaulieu, Dr. Victoria Goodall (2021)
# PROJECT: Shark and ray protection project, WILDOCEANS a programme of the WILDLANDS CONSERVATION TRUST
# CONTACT: ninab@wildtrust.co.za; victoria.goodall@mandela.ac.za 
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# this script calculates the cover per species per area, that is what percentage of each species' range is within each MPA or new area
# it also does this for only the no takes and only the no sharks
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# load packages
requiredpackages = c("terra","units","rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code specifies to look through all folders
path =  "/home/nina/" # path
my.directory = paste0(path,"Dropbox/6-WILDOCEANS")
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# PLANNING UNITS
# ---------------------------------
# Load the planning unit grid at 10 x 10 km
# Each grid cell has a value of 1 which represents the cost of that grid cell
pu = rast(list.files(pattern = "template_10km.tif$",full.names = TRUE,recursive = TRUE)[2])
# ---------------------------------


# ---------------------------------
# SA coast
# ---------------------------------
# Load the planning unit grid at 10 x 10 km
# Each grid cell has a value of 1 which represents the cost of that grid cell
sa = st_read(list.files(pattern = "sa.shp$",full.names = TRUE,recursive = TRUE))
# ---------------------------------


# ---------------------------------
# SPECIES INFO
# ---------------------------------
# load data summary sheet
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE)[1],sheet = 1)
# ---------------------------------


# ---------------------------------
# BIODIVERSITY FEATURES
# ---------------------------------
# this script loads all of the SDMs and packages them in a stack
source(list.files(pattern = "Biodiversityfeatures.R", recursive = TRUE)) 
# ---------------------------------


# ---------------------------------
# NEW AREA SHAPEFILE
# ---------------------------------
newareas = read_sf(list.files(pattern = "areas_v3.shp",recursive=T,full.names=T))
plot(newareas)
# set area variable
newareas$CUR_NME = paste0("newarea_",newareas$id)
newareas$area_km2_sum = set_units(st_area(newareas),km^2)
# get individual area names
newareas$id = NULL
newareas$version = NULL
newareas$area = NULL
# ---------------------------------


# ---------------------------------
# MPA SHAPEFILE
# ---------------------------------
# MPAs (all of them)
mpa_shapefile = st_read(list.files(pattern = "SAMPAZ_OR_2021_Q3.shp",recursive=TRUE,full.names = TRUE))

# remove prince edward islands and group by name
mpa_layer_all = mpa_shapefile %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  group_by(CUR_NME) %>%
  summarise(area_km2_sum = sum(area_km2)) 

# get individual MPA names
mpa_names = unique(mpa_layer_all$CUR_NME)

# remove prince edward islands and group by name
mpa_layer_notake = mpa_shapefile %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  filter(CUR_ZON_TY %in% c("Restricted","Wilderness","Sanctuary")) %>%
  group_by(CUR_NME) %>%
  # area from hectares to km2
  summarise(area_km2_sum = sum(area_km2)) 
plot(mpa_layer_notake)

# remove prince edward islands and group by name
mpa_layer_noshark = mpa_shapefile %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  mutate(noshark = ifelse(CUR_ZON_TY %in% c("Restricted","Wilderness","Sanctuary"),"yes",
                          ifelse(CUR_NME %in% c("Aliwal Shoal Marine Protected Area","iSimangaliso Marine Protected Area","Protea Banks Marine Protected Area","uThukela Marine Protected Area"),"yes","no")))%>%
  filter(noshark == "yes") %>%  
  group_by(CUR_NME) %>%
  # area from hectares to km2
  summarise(area_km2_sum = sum(area_km2)) 
plot(mpa_layer_noshark)

rm(mpa_shapefile)

# add attribute column to specify if MPA is coastal
sa = st_transform(sa,crs(mpa_layer_all))
mpa_layer_all$coastal = 0
for(i in 1:nrow(mpa_layer_all)){
  touches_temp = unlist(st_intersects(sa$geometry[c(1,4,8,9)],mpa_layer_all[i,]))
  if(length(touches_temp)==0){touches_temp = 0}
  mpa_layer_all$coastal[i] = touches_temp
}

# add attribute column to specify if MPA is coastal
sa = st_transform(sa,crs(newareas))
newareas$coastal = 0
for(i in 1:nrow(newareas)){
  touches_temp = unlist(st_intersects(sa$geometry[c(1,4,8,9)],newareas[i,]))
  if(length(touches_temp)==0){touches_temp = 0}
  newareas$coastal[i] = touches_temp
}
# ---------------------------------


# ---------------------------------
# COMBINE AREAS AND MPAS SHAPEFILES
colnames(mpa_layer_all)
colnames(newareas)
mpa_layer_all$type = "MPA"
newareas = st_transform(newareas,st_crs(mpa_layer_all))
allareas = rbind(mpa_layer_all,newareas)
rm(newareas,sa)
allareas = mpa_layer_notake
# ---------------------------------


# ---------------------------------
# COVER BY MPA and AREA (for SDMS)
# ---------------------------------
# make valid geometries
allareas = st_make_valid(allareas)

# species names
species = names(sdms_thresholds)

# area names
allareanames = unique(allareas$CUR_NME)
allareas = allareas

# for notakes only
allareanames = unique(mpa_layer_notake$CUR_NME)
allareas = mpa_layer_notake
plot(allareas)

# for noshark only
allareanames = unique(mpa_layer_noshark$CUR_NME)
allareas = mpa_layer_noshark
plot(allareas)


# get total combination of species names and mpas
length_total = nrow(expand.grid(allareanames,species))

# dataframe to fill
# every row will be cover for that species in that mpa
cover_byarea = data.frame(speciesrange = 1:length_total,
                          area_km2 = 1:length_total,
                          perc_in_area = 1:length_total,
                          range_in_area = 1:length_total,
                          CUR_NME = 1:length_total,
                          SPECIES_SCIENTIFIC =1:length_total )

counter = 0

for(i in 1:length(allareanames)){
  
  # AREA NAME
  temp_name = allareanames[i]
  # isolate single MPA polygon
  mpa_poly_temp = allareas %>% filter(CUR_NME ==temp_name)
  # get area in km2 from shapefile
  mpa_area_km2 = mpa_poly_temp$area_km2_sum
  
  # now for this MPA you want to now what % of it is covered by each species' range
  # i.e. 20% of TMNP is covered by C. carcharias
  for(j in 1:nlayers(sdms_thresholds)){
    
    # counter
    counter = counter+1
    
    # species
    sp = sdms_thresholds[[j]]
    
    # convert all values with presence to 1
    values(sp)[which(values(sp)!=0)] = 1
    values(sp)[which(values(sp)!=1)] = NA
    
    # get area range of species in # PUs
    # times by 100 to get in KM2
    range = sum(values(sp),na.rm=T)*100
    # convert species raster to polygon
    sp_poly = rasterToPolygons(sp,dissolve = T,na.rm = T)
    # convert to sf object
    sp_poly = st_as_sf(sp_poly)
    # transform crs
    sp_poly = st_transform(sp_poly,crs(mpa_poly_temp))
    # crop species polygon using  mpa polygon
    # this give you amount of distribution in MPA
    sp_inmpa = st_intersection(sp_poly,mpa_poly_temp)
    # area inside MPA is in m2 so convert to km2
    sp_inmpa = set_units(st_area(sp_inmpa),km^2)
    # get percentage of species range covered by MPA
    perc_in_mpa = sp_inmpa/range
    if(length(perc_in_mpa)==0){perc_in_mpa = 0}
    if(length(sp_inmpa)==0){sp_inmpa = 0}
    
    # add to data frame
    cover_byarea$CUR_NME[counter] = temp_name
    cover_byarea$area_km2[counter] = mpa_area_km2
    cover_byarea$speciesrange[counter] = range
    cover_byarea$range_in_area[counter] = sp_inmpa
    cover_byarea$perc_in_area[counter] = perc_in_mpa
    cover_byarea$SPECIES_SCIENTIFIC[counter] = str_replace(names(sp),"\\."," ")
    
    print(paste0(round((counter/length_total)*100,0),"%"))
    rm(sp,range,perc_in_mpa)
  } # end of j loop
  rm(mpa_area_km2,mpa_poly_temp)
  write.csv(cover_byarea,"cover_byallareas_sdms.csv",row.names = F)
} # end of i loop

rm(i,j,length_total,counter)

# save spreadsheet
write.csv(cover_byarea,"cover_byallareas_nosharks_sdms.csv",row.names = F)
# ---------------------------------


# ---------------------------------
# COVER BY MPA and AREA (for IUCN RANGES)
# ---------------------------------


# ---------------------------------
# IUCN FEATURES
# ---------------------------------
iucn_stack_all = readRDS(list.files(path = "/home/nina/Dropbox/",pattern = "iucn_file_list.RDS",recursive = TRUE,full.names=TRUE)[2])
# ---------------------------------

# species names
species = names(iucn_stack_all)

# area names
allareanames = unique(allareas$CUR_NME)

# for notakes only
allareanames = unique(mpa_layer_notake$CUR_NME)
allareas = mpa_layer_notake

# for noshark only
allareanames = unique(mpa_layer_noshark$CUR_NME)
allareas = mpa_layer_noshark

# get total combination of species names and mpas
length_total = nrow(expand.grid(allareanames,species))

# dataframe to fill
# every row will be cover for that species in that mpa
cover_byarea_iucn = data.frame(speciesrange = 1:length_total,
                               area_km2 = 1:length_total,
                               perc_in_area = 1:length_total,
                               range_in_area = 1:length_total,
                               CUR_NME = 1:length_total,
                               SPECIES_SCIENTIFIC =1:length_total )

counter = 0

for(i in 1:length(allareanames)){
  
  # AREA NAME
  temp_name = allareanames[i]
  # isolate single MPA polygon
  mpa_poly_temp = allareas %>% filter(CUR_NME ==temp_name)
  # get area in km2 from shapefile
  mpa_area_km2 = mpa_poly_temp$area_km2_sum
  
  # now for this MPA you want to now what % of it is covered by each species' range
  # i.e. 20% of TMNP is covered by C. carcharias
  for(j in 1:length(iucn_stack_all)){
    
    # counter
    counter = counter+1
    
    # species
    species_scientific = names(iucn_stack_all)[j]
    species_scientific = str_split(species_scientific,"\\/")[[1]][6]
    species_scientific = str_split(species_scientific,"\\.")[[1]][1]
    
    # extract layer
    sp = iucn_stack_all[[j]]
    
    # range of IUCN layer
    range = set_units(st_area(sp),km^2)
    
    # transform crs
    sp = st_transform(sp,crs(mpa_poly_temp))
    sp = st_make_valid(sp)
    sp = st_transform(sp,crs(mpa_poly_temp))
    
    # crop species polygon using  mpa polygon
    # this give you amount of distribution in MPA
    sp_inmpa = st_intersection(sp,mpa_poly_temp)
    # area inside MPA is in m2 so convert to km2
    sp_inmpa = set_units(st_area(sp_inmpa),km^2)
    # get percentage of species range covered by MPA
    perc_in_mpa = sp_inmpa/range
    if(length(perc_in_mpa)==0){perc_in_mpa = 0}
    if(length(sp_inmpa)==0){sp_inmpa = 0}
    
    # add to data frame
    cover_byarea_iucn$CUR_NME[counter] = temp_name
    cover_byarea_iucn$area_km2[counter] = mpa_area_km2
    cover_byarea_iucn$speciesrange[counter] = range
    cover_byarea_iucn$range_in_area[counter] = sp_inmpa
    cover_byarea_iucn$perc_in_area[counter] = perc_in_mpa
    cover_byarea_iucn$SPECIES_SCIENTIFIC[counter] = species_scientific
    
    print(paste0(round((counter/length_total)*100,0),"%"))
    rm(sp,range,perc_in_mpa)
  } # end of j loop
  rm(mpa_area_km2,mpa_poly_temp)
  write.csv(cover_byarea_iucn,"cover_bynotakes_iucnranges.csv",row.names = F)
} # end of i loop

rm(i,j,length_total,counter)

# save spreadsheet
write.csv(cover_byarea_iucn,"cover_bynotakes_iucnranges.csv",row.names = F)
# ---------------------------------
