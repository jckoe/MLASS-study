# MLASS-study
Code for feature extraction of Autism Social Interaction study

# 1. Software Requirements
This code is has been tested on _Mac_ (Version 12.6) and _Linux_ (Ubuntu 20.04), but is also supported for _Windows_. 

It uses data as outputted from _OpenFace_ (https://github.com/TadasBaltrusaitis/OpenFace) and _Motion Energy Analysis_ (https://psync.ch/downloads/).

_R_ (https://cran.rstudio.com) and _R Studio_ (https://posit.co/download/rstudio-desktop/) have to be installed. 


# 2. Instructions
## MEA scripts:
The following steps are performed with this code:
1. Calculate interpersonal synchrony per dyad and task
2. Feature extraction from cross-correlation matrix

## OpenFace scripts:
The following steps are performed with this code:
1. Preprocess data
2. Calculate interpersonal synchrony of single action units per dyad and task
3. Feature extraction from cross-correlation matrix

The run time for this script for a normal desktop computer is about 24 hours.

## Intra scripts:
The following steps are performed with this code:
1. Create head movement vector per person per task
2. Calculate head-body coordination per person per task
3. Feature extraction from cross-correlation matrix

