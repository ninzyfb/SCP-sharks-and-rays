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
requiredpackages = c("units","rgeos","sf","dplyr","tidyr","prioritizr","gurobi","stringr","rasterVis","viridis","raster","scales","readxl","fasterize","sdmvspecies","RColorBrewer")
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
setwd("/Users/nfb/Library/CloudStorage/Dropbox/6-WILDOCEANS/wildoceans-scripts") 
# ---------------------------------

# ---------------------------------
# NEW AREA SHAPEFILE
# ---------------------------------
newareas = read_sf(list.files(path = "/Users/nfb/Library/CloudStorage/Dropbox/6-WILDOCEANS/",pattern = "areas_v3.shp",recursive=T,full.names=T))
# set area variable
newareas$CUR_NME = paste0("newarea_",newareas$id)
newareas$area_km2_sum = set_units(st_area(newareas),km^2)
# get individual area names
newareas$id = NULL
newareas$version = NULL
newareas$area = NULL

# area names
area_names = unique(newareas$CUR_NME)
allareas = newareas
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

# remove prince edward islands and group by zone type as well
mpa_layer_zonetype = mpa_shapefile %>%
  filter(CUR_NME != "Prince Edward Island Marine Protected Area") %>%
  mutate(ZONETYPE = ifelse(CUR_ZON_TY %in% c("Restricted","Wilderness","Sanctuary"),"NOTAKE","CONTROLLED")) %>%
  group_by(CUR_NME,ZONETYPE) %>%
  summarise(area_km2_sum = sum(area_km2))

mpa_layer_zonetype$combinedname = paste0(mpa_layer_zonetype$CUR_NME,"_",mpa_layer_zonetype$ZONETYPE)

# get individual MPA names
mpa_names = unique(mpa_layer_zonetype$combinedname)
# area names
area_names = unique(mpa_layer_zonetype$combinedname)
allareas = mpa_layer_zonetype
# ---------------------------------


# ---------------------------------
# COMBINE AREAS AND MPAS SHAPEFILES
colnames(mpa_layer_all)
colnames(newareas)
mpa_layer_all$type = "MPA"
newareas = st_transform(newareas,st_crs(mpa_layer_all))
allareas = rbind(mpa_layer_all,newareas)
rm(newareas)
# area names
area_names = unique(allareas$CUR_NME)
#
rm(mpa_shapefile,mpa_layer_all)
# ---------------------------------


# ---------------------------------
# HABITAT SHAPEFILE
# ---------------------------------
files = list.files(pattern = ".shp",path = "/Users/nfb/Library/CloudStorage/Dropbox/6-WILDOCEANS/1-ConservationPlan/Modelling/environmental_variables/NationalBiodiversityAssessment_2018/", recursive=T,full.names=T)
SUBSTRATE = st_read(files[1])
rm(files)
# ---------------------------------


# ---------------------------------
# CBA SHAPEFILE
# ---------------------------------
files = list.files(pattern = "Critical Biodiversity Area Restore.shp",path = "/Users/nfb/Library/CloudStorage/Dropbox/6-WILDOCEANS/", recursive=T,full.names=T)
CBA_R = st_read(files)

files = list.files(pattern = "Critical Biodiversity Area Natural.shp",path = "/Users/nfb/Library/CloudStorage/Dropbox/6-WILDOCEANS/", recursive=T,full.names=T)
CBA_N = st_read(files)

rm(files)

# add CBA type
CBA_N$type = "natural"
CBA_R$type = "restore"

CBAs = rbind(CBA_N,CBA_R)
CBAs = st_make_valid(CBAs)
rm(CBA_N,CBA_R)
habitats = unique(CBAs$type)
# ---------------------------------


# ---------------------------------
# OVERLAP AREAS AND HABITAT (not percentage overlap)
# ---------------------------------
# this takes while to run and cant really be stopped 
#overlap = st_intersection(SUBSTRATE,allareas)
#write.csv(overlap,"overlap_AREAS-HABITATS.csv",row.names = F)
# ---------------------------------


# ---------------------------------
# COVER BY MPA and AREA
# ---------------------------------

# make valid
SUBSTRATE = st_make_valid(SUBSTRATE)

# habitat types
variables = colnames(SUBSTRATE)[c(6:9)]

for(a in variables){
  substrate_temp = SUBSTRATE[,a]
  
  # rename column to generic name so that you can group
  colnames(substrate_temp)[1] = "generic"
  
  # group by 
  substrate_temp = substrate_temp %>%
  group_by(generic) %>%
  summarise()

  # habitat names
  substrate_temp_nogeom = substrate_temp
  substrate_temp_nogeom$geometry = NULL
  habitats = as.vector(unique(substrate_temp_nogeom$generic))
  rm(substrate_temp_nogeom)
  
# get total combination of species names and mpas
length_total = nrow(expand.grid(area_names,habitats))

# dataframe to fill
# every row will be cover for that species in that mpa
cover_byarea = data.frame(area_km2 = 1:length_total,
                          perchabitat_in_mpa = 1:length_total,
                          CUR_NME = 1:length_total,
                          substratum = 1:length_total,
                          perchabitat_of_mpa = 1:length_total)

counter = 0

for(i in 1:length(area_names)){
  
  # AREA NAME
  temp_name = area_names[i]
  # isolate single MPA polygon
  mpa_poly_temp = allareas %>% filter(CUR_NME == temp_name)
  # get area in km2 from shapefile
  mpa_area_km2 = mpa_poly_temp$area_km2_sum
  
  # now for this MPA you want to now what % of it is covered by each species' range
  # i.e. 20% of TMNP is covered by C. carcharias
  for(j in 1:length(habitats)){
    
    # counter
    counter = counter+1
    
    # substrate type
    substrate_typetemp = habitats[j]
    
    # habitat
    #habitat_temp = substrate_temp[which(substrate_temp$generic == substrate_typetemp),]
    habitat_temp = CBAs[which(CBAs$type == substrate_typetemp),] # for CBAs
    
    # total area of habitat
    habitat_temp_totalarea = set_units(st_area(habitat_temp),km^2)
    
    # transform crs
    habitat_temp = st_transform(habitat_temp,crs(allareas))
    # crop species polygon using  mpa polygon
    # this give you amount of distribution in MPA
    habitat_inmpa = st_intersection(habitat_temp,mpa_poly_temp)
    # area inside MPA is in m2 so convert to km2
    habitat_inmpa = set_units(st_area(st_make_valid(habitat_inmpa)),km^2)
    
    # get percentage of habitat range within the MPA
    perc_in_mpa = (habitat_inmpa/habitat_temp_totalarea)*100
    
    # get percentage of MPA covered by that habitat
    perc_of_mpa = (habitat_inmpa/mpa_area_km2)*100
    
    
    if(length(perc_in_mpa)==0){perc_in_mpa = 0}
    if(length(perc_of_mpa)==0){perc_of_mpa = 0}
    
    # add to data frame
    cover_byarea$CUR_NME[counter] = temp_name # MPA name
    cover_byarea$area_km2[counter] = mpa_area_km2 # MPA area
    cover_byarea$substratum[counter] = substrate_typetemp # substrate type
    cover_byarea$perchabitat_in_mpa[counter] = perc_in_mpa # percent of habitat in mpa
    cover_byarea$perchabitat_of_mpa[counter] = perc_of_mpa # percent habitat for mpa
    
    write.csv(cover_byarea,paste0("substratecover_byallMPAs-",a,".csv"),row.names = F)
    
    print(paste0(round((counter/length_total)*100,0),"%"))
  } # end of j loop
  rm(mpa_area_km2,mpa_poly_temp)
  
} # end of i loop

rm(i,j,length_total,counter)

# save spreadsheet
write.csv(cover_byarea,paste0("substratecover_byallMPAs-OVERALLZONETYPE-",a,".csv"),row.names = F)
}
# ---------------------------------

habitat = read.csv(list.files(pattern = "substratecover_byallMPAs.csv", recursive=T))

habitat %>%
  filter(substratum == "Agulhas Bays")%>%
  filter(perchabitat_in_mpa>0) %>%
  ggplot(aes(x = perchabitat_in_mpa, y = CUR_NME))+
  geom_col()

# for habitat in single mpa
cover_byarea %>%
  filter(CUR_NME == "iSimangaliso Marine Protected Area")%>%
  ggplot(aes(x = perchabitat_of_mpa, y = substratum))+
  geom_col()

# for habitats across all MPAs
cover_byarea %>%
  group_by(substratum) %>%
   #summarise(sum_inmpa = sum(perchabitat_in_mpa)) %>%
  ggplot()+
  geom_boxplot(aes(y = perchabitat_in_mpa, x = substratum))
write.csv(cover_byarea,"broadecosy_permpa.csv",row.names = F)

#### code to plot outputs
getwd()
setwd("/Users/nfb/Library/CloudStorage/Dropbox/6-WILDOCEANS/")
files = list.files(pattern = "substratecover", recursive = TRUE,full.names=T)
library(ggplot2)

for(i in 1:length(files)){

  test = read.csv(files[i])
  
  name = str_split(files[i],"/")[[1]][3]
  
  look = test %>%
    filter((CUR_NME %in% c("newarea_2","newarea_3","newarea_4","newarea_5","newarea_6"))) %>%
    group_by(CUR_NME,substratum,perchabitat_in_mpa,perchabitat_of_mpa)%>%
    summarise() %>%
    filter(perchabitat_in_mpa>0)
  
  write.csv(look,paste0("newareas_",name))

}

### showing how new areas compliment MPA network
files = list.files(pattern = "newareas_substratecover",recursive=T,full.names = T)
temp = read.csv(files[5],sep =";")


