# r script to see how stopping is affected by visual information

rm(list=ls())

# define dirs
scriptdir = "~/Desktop/Dropbox/GenDocuments/PHD/scripts/Graphs/Basic"
setwd(scriptdir)
source('AGfunctions2.R')

# lees yDat file, defineer condities (outlier met negative SSRT worden hier ook al verwijderd)
source('~/Desktop/Dropbox/scripts/CiRaS/scanner/Graph/FitRL-test/conditions_yDat_RLtest.R', chdir = TRUE)