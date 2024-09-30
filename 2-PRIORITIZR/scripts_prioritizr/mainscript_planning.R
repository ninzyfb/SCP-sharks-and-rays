# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script is the parent spatial planning script, it calls all sub-scripts
# IMPORTANT: Run each subscript one at a time as running the whole parent script at once seems to cause some issues
# ---------------------------------


# ---------------------------------
# PACKAGES
# ---------------------------------
# load packages
requiredpackages = c("sf","dplyr","tidyr","prioritizr","gurobi","stringr","scales","readxl")
lapply(requiredpackages,require, character.only = TRUE)
rm(requiredpackages)
# ---------------------------------


# ---------------------------------
# DEFINE WORKING DIRECTORY
# ---------------------------------
# set directory to same parent folder where sub-scripts are found
# the subs-scripts can be in folders within this directory as the code will look through all the folders
my.directory = getwd()
# set directory
setwd(my.directory) 
# ---------------------------------


# ---------------------------------
# PLANNING UNITS
# ---------------------------------
# Load the planning unit grid at 10 x 10 km or 5 x 5 km resolution
# Each grid cell has a value of 1 which represents the cost of that grid cell
library(terra)
pu = rast(list.files(pattern = "template_10km.tif$",full.names = TRUE,recursive = TRUE))
# ---------------------------------


# ---------------------------------
# SPECIES INFO
# ---------------------------------
library(readxl)
# load data summary sheet
master = read_xlsx(list.files(pattern = "data_summary_master.xlsx", recursive = TRUE,full.names = TRUE)[1],sheet = 1)
# ---------------------------------


# ---------------------------------
# TARGETS
# ---------------------------------
library(dplyr)
library(tidyr)
# load target file
unique(master$STATUS)
targets = read_xlsx(list.files(pattern = "perc_targets", recursive = TRUE,full.names = TRUE))
targets = targets %>%
  pivot_longer(!STATUS,names_to = "ENDEMIC.STATUS",values_to = "target")
# add targets to master sheet
master$ENDEMIC.STATUS = as.character(master$ENDEMIC.STATUS )
master = left_join(master,targets)
rm(targets)
# ---------------------------------


# ---------------------------------
# BIODIVERSITY FEATURES
# ---------------------------------
# this script loads all of the SDMs and packages them in a stack
source(list.files(pattern = "Biodiversityfeatures.R", recursive = TRUE, full.names = TRUE)) 
# ---------------------------------


# ---------------------------------
# IUCN FEATURES
# ---------------------------------
source(list.files(pattern = "iucnmaps.R", recursive = TRUE)) 
# ---------------------------------


# ---------------------------------
# LOCKED IN AREAS
# ---------------------------------
source(list.files(pattern = "Lockedin.R", recursive = TRUE))
# ---------------------------------


# ---------------------------------
# COSTS
# ---------------------------------
source(list.files(pattern = "costs_2018NBA.R", recursive = TRUE))
# ---------------------------------


# ---------------------------------
# BIODIVERSITY FEATURE GROUPS
# SPECIAL SPECIES 1 (CR, EN, VU, ENDEMICS TO SOUTH AFRICA)
# ---------------------------------
# Filter master sheet to extract those species
special_species_1 = master %>%
  filter(ENDEMIC.STATUS %in% c("1") | STATUS %in% c("CR","EN","VU"))

# filter biodiversity raster stacks to extract special species 1
# filtered stack
idx = which(str_replace(names(sdms_thresholds),"\\."," ") %in% special_species_1$SPECIES_SCIENTIFIC)
sdms_specialspp1 = raster::subset(sdms_thresholds,idx)

# add which species are in group 1
master$priority_group1 = "no"
master$priority_group1[which(master$SPECIES_SCIENTIFIC %in% special_species_1$SPECIES_SCIENTIFIC)] = "yes"
# ---------------------------------


# ---------------------------------
# BIODIVERSITY FEATURE GROUPS
# SPECIAL SPECIES 2 (CR, EN, VU, ENDEMICS TO SOUTHERN AFRICA)
# ---------------------------------
# Filter master sheet to extract those species
special_species_2 = master %>%
  filter(ENDEMIC.STATUS %in% c("1","2") | STATUS %in% c("CR","EN","VU"))

# filter biodiversity raster stacks to extract special species 2
# filtered stack
idx = which(str_replace(names(sdms_thresholds),"\\."," ") %in% special_species_2$SPECIES_SCIENTIFIC)
sdms_specialspp2 = subset(sdms_thresholds,idx)

# add which species are in group 2
master$priority_group2 = "no"
master$priority_group2[which(master$SPECIES_SCIENTIFIC %in% special_species_2$SPECIES_SCIENTIFIC)] = "yes"
rm(idx)

# if using IUCN stack and wanting equivalent species
iucnstack_specialspp2 = subset(iucn_stack_all,which(names(iucn_stack_all) %in% names(sdms_specialspp2)))
# ---------------------------------


# ---------------------------------
# TURN ALL FEATURES TO BINARY
# ---------------------------------
values(sdms_thresholds)[which(values(sdms_thresholds)>0)] = 1
values(sdms_specialspp1)[which(values(sdms_specialspp1)>0)] = 1
values(sdms_specialspp2)[which(values(sdms_specialspp2)>0)] = 1
# ---------------------------------


# ---------------------------------
# ADD CBA LAYER TO FEATURE GROUPS FOR FINAL MANAGEMENT SCENARIO
# ---------------------------------
cba_layer = rast(list.files(pattern = "cba_nr.tif$",recursive = T, full.names=T))
sdms_thresholds_cba = c(sdms_thresholds,cba_layer)
sdms_specialspp1_cba = c(sdms_specialspp1,cba_layer)
sdms_specialspp2_cba = c(sdms_specialspp2,cba_layer)
rm(cba_layer)
# ---------------------------------


# ---------------------------------
# BUILDING AND SOLVING A CONSERVATION PROBLEM
# ---------------------------------

# parent folder to save all solution outputs
solutionsfolder = "wildoceans-scripts/Outputs/planning/scenario_outputs_raw/"

# turn off scientific numbering
options(scipen = 100) 

# scenarios
# read in different scenarios depending on what you want to run
scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/wildoceans-scripts/scenarios_manuscript.xlsx"),sheet = 2)
scenario_sheet = read_xlsx(list.files(pattern = "scenarios_biodiversity_technicaldocumentfinaloutputs",recursive = T,full.names = T),sheet = 2)
# projection scenarios
#scenario_sheet = read_xlsx(path=paste0(path,"Dropbox/6-WILDOCEANS/ConservationPlan/Planning/scenarios_projectionexercise.xlsx"),sheet = 1)

# pre-compute the boundary data
library(prioritizr)
library(gurobi)
sa_boundary_data <- boundary_matrix(pu)
# rescale boundary data
sa_boundary_data@x <- scales::rescale(sa_boundary_data@x, to = c(0.01, 100))

# Building and solving conservation problems
# these are all outlined in the scenario sheet
# the following loop goes through each row of the scenario sheet and outputs a solution
for(i in 30:nrow(scenario_sheet)){
  
  # problem number
  problem_number=i
  
  # scenario name (Control, MPA, Fishing)
  scenario = scenario_sheet$scenario[i]
  
  # cost layer
  costs = get(scenario_sheet$costs[i])
 
   # features
  features = get(scenario_sheet$features[i])
  features = mask(features,costs)
  
  # isolate species being used to get appropriate targets
  featurenames_temp = master[master$SPECIES_SCIENTIFIC %in% str_replace(names(features),"\\."," "),]
  # extract vector of targets
  if(scenario_sheet$targets[i] == "t"){t = featurenames_temp$target}else{t = as.numeric(scenario_sheet$targets[i])}
  # add target for cba layer if included in scenario
  if("cba_nr" %in% names(features)){t = c(t,0.3)}
  # adjust targets if required
  t = t + scenario_sheet$target_adj[i]
  
  # solving method and gap value
  solving_method = scenario_sheet$solving_method[i]
  gap_value = scenario_sheet$Gap[i]
  # budget value (when applicable for solving method)
  budget_value = as.numeric(scenario_sheet$Budget[i])
  
  # boundary penalty
  p = scenario_sheet$penalty[i]
  
  # areas to be locked-in
  locked_in = scenario_sheet$lockedin[i] 
  locked_out = scenario_sheet$lockedout[i] 
  

  # BASIC CONSERVATION PROBLEM
  problem_single= problem(costs,features)%>%
    add_relative_targets(t) %>%
    add_gurobi_solver(gap=gap_value) %>%
    add_gap_portfolio(number_solutions=100, pool_gap = gap_value)%>%
    add_binary_decisions() %>%
    add_boundary_penalties(penalty = p, data = sa_boundary_data)
  
  # add solving mechanism
  if(solving_method == "add_min_set_objective"){problem_single = problem_single %>% add_min_set_objective()}
  if(solving_method == "add_min_shortfall"){problem_single = problem_single %>% add_min_shortfall_objective(10000)}
  if(solving_method == "add_min_largest_shortfall"){problem_single = problem_single %>% add_min_largest_shortfall_objective(budget_value)}
  if(solving_method == "add_max_feature_objective"){problem_single = problem_single %>% add_max_features_objective(budget_value)}
    
  # add locked_in constraints if applicable
  if(locked_in != "none"){problem_single = problem_single %>% add_locked_in_constraints(lockedin[[locked_in]])}
  
  # add locked_out constraints if applicable
  if(locked_out != "none"){problem_single = problem_single %>% add_locked_out_constraints(costs_all)}
  
  # add maximum budget of 10% when using cost layer as fishing pressure affects number of cells to pick
  if(scenario_sheet$Budget[i] != "none"){problem_single = problem_single %>% add_linear_constraints(threshold = as.numeric(scenario_sheet$Budget[i]), sense = "<=", data = pu)}
  
  # solve problem
  solution_single = solve(problem_single,force=T)
    
  # create raster stack from solutions
  solution_single = c(rast(solution_single))
  
  # create solution frequency raster
  # this sums all the solutions together
  # it outlines the most frequently chosen areas for the given conservation problem
  solution_sum= sum(solution_single)
    
  # change the 0 to NA
  values(solution_sum)[which(values(solution_sum)==0)] = NA
    
  # save stack of solutions as r object
  saveRDS(solution_single,paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario_allsolutions.RDS"))
  # save problem
  saveRDS(problem_single,paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario_problem.RDS"))
  # save solution as raster
  writeRaster(solution_sum,paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario.tiff"),overwrite = TRUE)
  # very basic plot to glance at results
  png(file=paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario.png"),width=3000, height=2000, res=300)
  plot(solution_sum)
  dev.off()
  
  ## PERFORMANCE 1 - AVERAGE PROTECTION PER SPECIES
  # this takes a total of ~2-3min
  # create and empty data-frame
  summary_temp_allsols = data.frame(matrix(ncol = 10))
  # get targets achieved for each species for each of the 100 solutions
  # and create giant data-frame 
  for(j in 1:nlayers(solution_single)){
    summary_temp = eval_target_coverage_summary(problem_single,solution_single[[j]])
    summary_temp$sol_n = j
    colnames(summary_temp_allsols) = colnames(summary_temp)
    summary_temp_allsols = rbind(summary_temp_allsols,summary_temp)
  }
  # remove empty first row
  summary_temp_allsols = summary_temp_allsols[-1,]
  # fix scientific name
  summary_temp_allsols$feature = str_to_sentence(summary_temp_allsols$feature)
  summary_temp_allsols$feature = str_replace(summary_temp_allsols$feature,"\\."," ")
  # save 
  write.csv(summary_temp_allsols,paste0(solutionsfolder,"p",str_pad(problem_number,3,pad = "0"),"_",scenario,"_scenario.csv"),row.names = F)
  rm(summary_temp_allsols,summary_temp,j)
}
