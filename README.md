# A shark and ray systematic conservation plan for South Africa

## Project description

-   This repository contains all the necessary code used to develop a shark and ray conservation plan developed for South Africa
-   The published paper is open-source and freely available: [Faure-Beaulieu et al., 2023. A systematic conservation plan identifying critical areas for improved chondrichthyan protection in South Africa](https://www.sciencedirect.com/science/article/pii/S0006320723002641)
-   The code consists of two main sections: modelling and conservation planning

## Species distribution maps for download

-   All the outputs from the modelling and planning code which include the species distribution models and conservation planning solutions can be shared upon request via wetransfer by emailing me at nina-fb\@outlook.com, they are too heavy to be stored on this repository.

## Description of each folder contents

-   See below for a detailed description of the contents of each folder

### 1. MODELLING folder

-   The code used to produce the species distribution models (SDMs) is found here
-   To run it, start with `mainscript-modelling.R` as this is the parent script through which the other scripts are run. This script contains the order in which to run the scripts and also describes what each one is for.
-   All models were run on the same subset of environmental variables detailed in *selectedvariables_all.csv*. Environmental variables were chosen by testing for collinearity using the `independentvariableselection.R` script and they are available for download in the folder *ALLLAYERS10kmresolution*

**!!IMPORTANT 1!! how to run scripts on your own data or example data provided**:\
\* The occurrence data used to run the conservation plan is confidential and not available for download \* If you wish to run the code there is an **example.csv** file provided which allows for the modelling scripts to be run on freely available data from GBIF and OBIS for *Acroteriobatus annulatus*. Steps to use the example.csv file are as follows:\
+ make sure exampledata = "yes" in `mainscript-modelling.R` - this ensures that `species_data.R` runs on the example data + the script is built to run on a loop going through each species and calling all subscripts in order, however this is not useful if wanting to learn what each script does, so i suggest manullay setting i=1 and running through each line and subscript individually rather than running the whole loop \* If you wish to run the code on your own data, ensure your data has the same headings as the example.csv file and simply replace example_data.csv with your filename.

**!!IMPORTANT 2!! installing java may be required**:\
To run the modelling scripts as is, java is required to run MaxEnt as one of the modelling algorithms. If you do not wish to install java, then it may be that maxent will not be able to run.

### 2. PRIORITIZR folder

This folder contains the code to run the spatial planning algorithm *prioritizr*. The *prioritizr* package has an [online tutorial](https://prioritizr.net/articles/prioritizr.html) and [github page](https://github.com/prioritizr/prioritizr).\
Similar to the modelling scripts, start with`mainscript-planning.R` as this is the parent script through which the other scripts are run.

### 4. IUCN folder

This folder contains range maps for several shark and ray species in South Africa. These are also too heavy to be stored on the github, please email me at nina-fb\@outlook.com and I will be happy to send these via wetransfer.

### 6. plotting_features folder

This folder contains the layers necessary to produce plots with outputs from the modelling and planning scripts. These are also too heavy to be stored on the github, please email me at nina-fb\@outlook.com and I will be happy to send these via wetransfer. The modelling and planning scripts produce rasters which can be plotted as annotated maps and saved as images using the features from plotting parameters.
