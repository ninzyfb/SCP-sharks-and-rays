# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads all the IUCN shapefiles and creates a rasterstack
# ---------------------------------


# ---------------------------------
# DATA
# ---------------------------------
# extract names of IUCN range maps
files = list.files(path = "wildoceans-scripts/IUCN/Sharks_rays_SA_raw",pattern = paste(".gpkg",sep=""), recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------

# extract scientific name from file name
names = toupper(files)
names = str_split(names,toupper("wildoceans-scripts/IUCN/Sharks_rays_SA_raw/"), simplify = TRUE)[,2]
names = str_split(names,".GPKG", simplify = TRUE)[,1]

# all iucn maps
iucn_stack_all = rast()
for(i in 1:length(files)){
  temp = st_read(files[i])
  temp = rasterize(temp,pu)
  iucn_stack_all = c(iucn_stack_all,temp)
  rm(temp)
}

rm(i,files)

# add scientific names to raster stack layers
names(iucn_stack_all) = names
rm(names)

# remove CARCHARHINUS_AMBLYRHYNCHOS from iucn stack as no distribution in SA
# and this then messes with the planning software
id = which(names(iucn_stack_all) == "CARCHARHINUS AMBLYRHYNCHOS")
iucn_stack_all = subset(iucn_stack_all,id,negate=T)
# ---------------------------------