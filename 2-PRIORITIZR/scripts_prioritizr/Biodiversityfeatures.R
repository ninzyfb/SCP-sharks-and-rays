# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------


# ---------------------------------
# DATA
# ---------------------------------
# load pelagic species that we remove for the conservation plan
source(list.files(pattern = "pelagic_spp.R", recursive = TRUE)) 

# species distribution file names (continuous)
files = list.files(path = paste0(getwd(),"/wildoceans-scripts/"),pattern = "ensemblemean.tiff$", recursive = TRUE,full.names = TRUE)

# create raster stack (115 layers)
feature_stack = rast()
for(i in 1:length(files)){
  temp = rast(files[i])
  temp = project(temp,pu)
  feature_stack = c(feature_stack,temp)
}
rm(i,files,temp) # remove unnecessary variables

# threshold values (to turn continuous distributions to binary)
threshs = list.files(path = paste0(path,"Dropbox/6-WILDOCEANS/wildoceans-scripts/"),pattern = "thresh.csv", recursive = TRUE, full.names = TRUE)
# ---------------------------------


# ---------------------------------
# FORMATTING
# ---------------------------------
# turn all NA values to 0 (prioritizr does not like NA values)
values(feature_stack)[is.na(values(feature_stack))] = 0

# mask with pu
feature_stack = c(mask(feature_stack,pu))

# extract scientific name from stack of distributions
featurenames = as.data.frame(names(feature_stack))
colnames(featurenames) = "featurename"
for(i in 1:nrow(featurenames)){
  # extract model type
  featurenames$modeltype[i] = strsplit(featurenames$featurename,"_")[[i]][3]
  featurenames$modeltype[i] = strsplit(featurenames$modeltype,"ensemblemean")[[i]][1]
  # extract scientific name by pasting genus and species name from file name
  featurenames$species_scientific[i] = paste(strsplit(featurenames$featurename,"_")[[i]][1] ,strsplit(featurenames$featurename,"_")[[i]][2])}

rm(i) # remove unnecessary variables

# turn species names to upper case
featurenames$species_scientific = toupper(featurenames$species_scientific)

# turn headers to capital
colnames(featurenames) = toupper(colnames(featurenames))

# turn model type (season) to upper case
featurenames$MODELTYPE = toupper(featurenames$MODELTYPE)

# ---------------------------------
# FORMATTING
# ---------------------------------

# divide all values by 1000 to get actual probability values between 0 and 1
# only divide non NA values by 1000
for(i in 1:nlyr(feature_stack)){ 
  values(feature_stack[[i]])[!is.na(values(feature_stack[[i]]))] = values(feature_stack[[i]])[!is.na(values(feature_stack[[i]]))]/1000
}

# turn any negative values to 0
for(i in 1:nlyr(feature_stack)){ 
  feature_stack[[i]][values(feature_stack[[i]])<0] = 0
}

# clamp values to turn extremely small values to 0
feature_stack = clamp(feature_stack, lower = 1e-6, values = FALSE)
# turn all NA values to 0 (prioritizr does not like NA values)
values(feature_stack)[is.na(values(feature_stack))] = 0

rm(i) # remove unnecessary variables

# create feature stack where threshold values are used to filter low probability portion of occurences
sdms_thresholds = rast()
for(i in 1:nlyr(feature_stack)){
  temp = feature_stack[[i]]
  thresh_value = read.csv(threshs[i])
  thresh_value = (thresh_value$thresh)/1000
  values(temp)[values(temp)<thresh_value] = 0
  sdms_thresholds = c(sdms_thresholds,temp)
  names(temp)
  #writeRaster(temp,paste0(names(temp),"_clipped.tif"))
}
rm(temp)

names(sdms_thresholds) = featurenames$SPECIES_SCIENTIFIC
names(sdms_thresholds) = str_remove(names(sdms_thresholds)," ASEASONAL")
# remove problem species
library(stringr)
idx = which(str_replace(names(sdms_thresholds),"\\."," ") %in% problem_species_all)
sdms_thresholds = subset(sdms_thresholds,idx,negate=TRUE)
rm(idx)

# clean up
rm(feature_stack,featurenames,i,keep,problem_species_all,thresh_value,threshs)
