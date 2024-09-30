# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------

####
#THIS SCRIPT: loads the fishing pressure layers and assigns the correct cost to each species
####

# ---------------------------------
# DATA
# ---------------------------------
# nba file names
nbafiles = list.files(pattern = "NBA5km.tif$", recursive = TRUE,full.names = TRUE) # find file names
# convert to  raster stack
costs = rast(nbafiles)
# project to planning unit
costs = project(costs,pu)
rm(nbafiles)

# raw file
#temp = raster(list.files(pattern = "Pole Tuna Intensity.tif",recursive = TRUE,full.names = TRUE)[1])
#pu_projected = projectRaster(pu,temp)
#temp2 = mask(temp,pu)

# fishing threats
#threats = read_xlsx(list.files(pattern = "fisheries-risk.xlsx", recursive = TRUE,full.names = TRUE),skip=1)

# ---------------------------------
# FORMATTING
# ---------------------------------

# remove nba5km from stack names
names = vector()
for(i in 1:length(names(costs))){
  names[i] = str_split(names(costs),"_NBA5km")[[i]][1]}
names(costs) = names
rm(names, i) # remove unnecessary variables

# Function to scale a vector between 0 and 1
scale01 <- function(x) {
  (x - min(x,na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x,na.rm = TRUE))
}

# Iterate over each layer in the raster stack
costs_scaled = rast()
for (i in 1:nlyr(costs)) {
  # Get the current layer
  layer <- costs[[i]]
  
  # Scale the layer between 0 and 1
  scaled_layer <- scale01(values(layer))
  values(layer) = scaled_layer
  
  # Replace the original layer with the scaled layer in the raster stack
  costs_scaled= c(costs_scaled,layer)
}

plot(costs_scaled)
plot(costs)

costs_summed = sum(costs_scaled,na.rm = TRUE)
costs_summed = mask(costs_summed,pu)
plot(costs_summed)

# creating planning unit layer
costs_pu = costs_summed+1 #values of 0 are not like by prioritizr
plot(costs_pu)

# for each layer only keep top 20% of pressure
for(i in 1:nlyr(costs_scaled)){
  values(costs_scaled[[i]])[which(values(costs_scaled[[i]])<0.8)] = 0
  values(costs_scaled[[i]])[which(is.na(values(costs_scaled[[i]])))] = 0
}

# create one main layer as summed costs
costs_all = sum(costs_scaled,na.rm = TRUE)
costs_all = mask(costs_all,pu)
# you need to add pu other other cells will have cost of 0
costs_all = costs_all+pu
plot(costs_all)

# turn all non value cells to NA
values(costs_all)[which(values(costs_all)==1)] = NA
values(costs_all)[which(!is.na(values(costs_all)))] = 1
plot(costs_all)

# ---------------------------------
# FISHING PRESSURE
# ---------------------------------
# Option 1
# create a binary layer based on the fishing pressure scores
# this is based on a quantile threshold
# used to lock out planning units with high fishing pressure
# binary layer based on the top 5th percentile of pressure scores
#fp_threshold = raster::quantile(costs_all, probs = (1 - 0.05), na.rm = TRUE, names = FALSE)
#fp_binary = round(costs_all >= fp_threshold)

# Option 2
# prevent total amount of fishing pressure scores from exceeding a certain threshold
# add a constraint to only select pus with fishing pressure scores
# that sum to a total of less than 20% of the total fishing pressure scores
#fp_threshold = raster::cellStats(costs_all, "sum") * 0.2
# ---------------------------------


# get cost layer where no MPAs are less costly
#plot(pu)
#plot(lockedin[[3]])
#pu_mpas= calc(stack(pu,lockedin[[3]]),sum,na.rm=T)
#plot(pu_mpas)
#values(pu_mpas)[which(values(pu_mpas)==2)]=0
#plot(pu_mpas)
#pu_mpas = mask(pu_mpas,pu)
#plot(pu_mpas)

#rm(costs_scaled,costs)

# now create final fishing cost layer where mpas are less costly and fishing pressure is included
# turn everything to NA
#values(pu_mpas)[which(values(pu_mpas)==0)]=NA
#costs_all = mask(costs_all,pu_mpas)
# now assign value of 0 to mpa
#values(costs_all)[which(is.na(values(costs_all)))]=0
# mask to pu layer
#costs_all = mask(costs_all,pu)
# add 1 to all costs so that MPA still has value of 1 and not 0
#values(costs_all) = values(costs_all)+1
# make sure only pressure cells ahve value otherwise entire pUS are locked out
#values(costs_all)[which(values(costs_all)!=2)] = NA

#rm(fp_binary)

