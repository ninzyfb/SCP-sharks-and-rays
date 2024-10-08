# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script loads the occurrence data and formats it properly
# this script also adds season to the dataset
# Summer months: Sep,Oct,Nov,Dec,Jan,Feb
# Winter months: Mar,Apr,May,Jun,Jul,Aug
# the outputs of this script is a spatialpointsdataframe named obs.data
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# detach biomod2 if attached, it seems to mess with other packages i.e. tidyr
if("biomod2" %in% (.packages())){detach("package:biomod2", unload=TRUE) }
# ---------------------------------


# ---------------------------------
# DATA INPUT
# ---------------------------------
# read in occurrences of target species

# TO RUN CODE ON EXAMPLE DATA
# the example_data.csv has data from GBIF on ACROTERIOBATUS ANNULATUS and can be used to try the code
if(exampledata == "yes"){
# assign FILENAME to the name of your occurrence data csv file
FILENAME = "example_data.csv"
# assign file to location of example file
file = list.files(pattern = FILENAME, recursive = TRUE, full.names = TRUE)
}

if(exampledata == "no"){
  # file name
  FILENAME = paste(toupper(target),".rds",sep="")
  # identifies location of occurrence file on computer
  file = list.files(path =str_remove(my.directory,"/wildoceans-scripts"),pattern = FILENAME,recursive = TRUE, full.names = TRUE)
  }
# ---------------------------------

# ---------------------------------
# FORMATTING
# ---------------------------------
# not all species from master sheet have data
# this if statement runs the script only if the target species has associated data
if(length(file)>0){ 
  
# load in data file
  obs.data = readRDS(file)

# convert latitude and longitude to numeric variables
obs.data$LONGITUDE = as.numeric(obs.data$LONGITUDE)
obs.data$LATITUDE = as.numeric(obs.data$LATITUDE)

# convert column names to upper
colnames(obs.data) = toupper(colnames(obs.data))

# this if statement further ensures that the script should only by run if the dataframe has any data
if(nrow(obs.data)>0){

  # verify there are no NAs in latitude and longitude
# if there are a warning message will appear
if(unique(!is.na(obs.data$LATITUDE)) == FALSE){
  print("SOME NAs ARE PRESENT IN THE LATITUDE DATA")}
if(unique(!is.na(obs.data$LONGITUDE)) == FALSE){
  print("SOME NAs ARE PRESENT IN THE LONGITUDE DATA")}

# remove any 0 latitude and longitude
obs.data = obs.data %>%
  filter(LONGITUDE != 0) %>%
  filter(LATITUDE != 0)

# verify duplicates (for latitude, longitude, and date)
dups = duplicated(obs.data[c("LATITUDE","LONGITUDE", "DATE")])
# print number of duplicate records, TRUE are number of duplicates
print(table(dups))
# remove duplicates from data
obs.data = obs.data[!dups,]
rm(dups)

# add month variable from Date
obs.data = obs.data %>%
  mutate(MONTH = month(obs.data$DATE))

# check for seasonality column
if(!"SEASON" %in% colnames(obs.data)){obs.data$SEASON = NA}

# group observations by season
# important: some datasets came with season already specified
obs.data = obs.data %>%
  # if SEASON variable is empty, convert to NA
  mutate(SEASON = ifelse(SEASON == "",NA,SEASON)) %>%
  # specify winter months
  mutate(SEASON = ifelse(is.na(SEASON) & MONTH %in% c(3,4,5,6,7,8),"Winter",
                         # specify summer months
                         ifelse(is.na(SEASON) & MONTH %in% c(9,10,11,12,1,2), "Summer",
                                # if season was stated as autumn then turn to winter
                                ifelse(SEASON == "Autumn","Winter",
                                       # if season was stated as spring then turn to summer
                                       ifelse(SEASON == "Spring","Summer",SEASON)))))

# convert data to spatial points data frame
coordinates(obs.data) =  ~ cbind(obs.data$LONGITUDE,obs.data$LATITUDE)

# this prevents the loop running through on multiple species name with the same example file
if(unique(obs.data$SPECIES_SCIENTIFIC) != target){print("SPECIES NAME IN MASTER SHEET AND IN DATA DO NOT MATCH")}

}else{length(file) = 0}}
rm(FILENAME,file)
# ---------------------------------