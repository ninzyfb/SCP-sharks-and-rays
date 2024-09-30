# ---------------------------------------------------------------------------------
# SCRIPT AUTHOR: Nina Faure Beaulieu
# PROJECT: Shark and ray systematic conservation plan
# CONTACT: nina-fb@outlook.com
# ---------------------------------------------------------------------------------


# ---------------------------------
# SCRIPT DESCRIPTION
# ---------------------------------
# This script runs and projects the models
# ---------------------------------


# ---------------------------------
# OUTPUT FOLDER DESTINATION
# ---------------------------------
# create output folders if they do not already exists
# this is where the outputs will be saved to
if(!dir.exists("Outputs")){dir.create("Outputs")}
if(!dir.exists("Outputs/modelling")){dir.create("Outputs/modelling")}
if(!dir.exists("Outputs/modelling/evaluations")){dir.create("Outputs/modelling/evaluations")}
if(!dir.exists("Outputs/modelling/prettyplots")){dir.create("Outputs/modelling/prettyplots")}
if(!dir.exists("Outputs/modelling/rasters")){dir.create("Outputs/modelling/rasters")}

evaluationfolder = paste0(my.directory,"/Outputs/modelling/evaluations/")
rasterfolder = paste0(my.directory,"/Outputs/modelling/rasters/")
# ---------------------------------


# ---------------------------------
# MODELLING
# ---------------------------------

# Build individual aseasonal models

# this static models object will contain 60 model projections
# this is because we are running 3 model algorithms (GLM, MAXENT, GAM)
# each model algorithm is being run 10 times as a cross-validation approach
# and these 10 runs are being run on one set of background points
# 3 * 10 = 30
# this can be VERY time consuming (<1h) depending on your machine

# if you want to reduce the computing time, reduce
# 1 - the number of model algorithms
# 2- nb.rep to 5 or less

# it takes ~60 min to run 3 algorithms on nb.rep = 1
# on a mac with 2.4 GHz Dual-Core Intel Core i5 and Memory of 8 GB 1600 MHz DDR3

# it takes ~ 24 min to run 3 algorithms on nb.rep = 1
# on a pc with 11th Gen Intel® Core™ i7-1165G7 @ 2.80GHz × 8 and Memory of 15.4 GB
library(doParallel)
cl = makeCluster(8)
doParallel::registerDoParallel(cl)

static_models <- BIOMOD_Modeling(
  data, # your biomod object
  var.import = 5,
  #models = c('GAM'), 
  models = c('GAM','GLM','MAXENT.Phillips'), # 3 modelling algorithms run for project
  #bm.options  = mxtPh, # modified model parameters, unnecessary if you are happy with default biomod2 parameters
  nb.rep = 10, # 10-fold cross validation (number of evaluations to run)
  data.split.perc = 75, # 75% of data used for calibration, 25% for testing
  metric.eval  = c('TSS'), # evaluation method, TSS is True Statistics Skill
  #save.output  = TRUE, # keep all results on hard drive 
  scale.models = FALSE, # if true, all model prediction will be scaled with a binomial GLM
  modeling.id = target, # name of model = species name (target)
  nb.cpu = 8,
  do.progress = TRUE
  ) 

# get important variables
variables = as.data.frame(get_variables_importance(static_models))
# save
write.csv(variables,paste0(evaluationfolder,model_type,target,"_variableimportance.csv"), row.names = FALSE)

#rm(i,pa_xy,exp,pa,temp,pts_env,pts_env_seasons)
bm_PlotEvalBoxplot(bm.out = static_models, group.by = c('algo', 'run'))

# Build ensemble model
static_ensemblemodel  <- BIOMOD_EnsembleModeling(
  bm.mod  = static_models, # all model projections
  models.chosen = 'all', # use all your models
  em.by='all', # the way the models will be combined to build the ensemble models.
  metric.select  = 'TSS', # which metric to use to keep models for the ensemble (requires the threshold below)
  metric.select.thresh  = c(0.7), # only keep models with a TSS score >0.7
  em.algo = c('EMmean','EMcv')
  #prob.mean = T, #  Estimate the mean probabilities across predictions
  #prob.cv = T, # Estimate the coefficient of variation across predictions
)

# Individual model projections over current environmental variables
static_modelprojections =
  BIOMOD_Projection(
    proj.name = paste0(target,model_type), # new folder will be created with this name
    bm.mod  = static_models, # your modelling output object
    new.env = stack_model, # same environmental variables on which model will be projected
    models.chosen  = "all", # which models to project, in this case only the full ones
    metric.binary = "TSS",
    compress = 'xy', # to do with how r stores the file
    build.clamping.mask = FALSE,
    nb.cpu=8)

# Ensemble model projection 
static_ensembleprojection = BIOMOD_EnsembleForecasting(
  bm.em = static_ensemblemodel,
  bm.proj = static_modelprojections)

# get all models evaluation scores
all_evals = get_evaluations(static_models)
ensemble_evals = get_evaluations(static_ensemblemodel)
write.csv(all_evals,paste0(evaluationfolder,model_type,target,"_allevals.csv"))
write.csv(ensemble_evals,paste0(evaluationfolder,model_type,target,"_ensembleevals.csv"))

# Threshold calculation
# this function calculates the threshold at which a probability can be considered a presence
predictions = get_predictions(static_ensemblemodel)# predicted variables
predictions = predictions%>%
  filter(algo == "EMmean")
predictions = predictions$pred
response = get_formal_data(static_models) # response variable 
response = response@data.species
response[which(is.na(response))] = 0 # change NA to 0
thresh = bm_FindOptimStat(metric.eval='TSS',
                         fit = predictions,
                         obs = response,
                         nb.thresh = 100)[2]
thresh = as.data.frame(thresh) # save the output as a dataframe
write.csv(thresh,paste0(evaluationfolder,model_type,target,"_thresh.csv")) # save the dataframe
rm(predictions,response) # remove unnecessary variables
# ---------------------------------


# ---------------------------------
# OUTPUTS
# ---------------------------------
# isolate ensemble prediction raster
en_preds = get_predictions(static_ensembleprojection) 
# ensemble projection
writeRaster(en_preds[[1]],paste0(rasterfolder,target,"_",model_type,"_ensemblemean.tiff"), overwrite = TRUE)
# coefficient of variation
writeRaster(en_preds[[2]],paste0(rasterfolder,target,"_",model_type,"ensemblecv.tiff"),  overwrite = TRUE)
# ---------------------------------