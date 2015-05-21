# run if first time use
# install.packages("devtools")
# devtools::install_github("rstudio/leaflet")

# packages <- c("leaflet", "shinydashboard", "plyr", "dplyr", "data.table", "rgdal", "sp", "raster", "ggplot2", "scales", "grid", "gridExtra", "data.table", "ggvis", "ggtern")
# new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# lapply(packages,function(x){library(x,character.only=TRUE)}) 
# rm(packages, new.packages)

library(shiny)
library(shinydashboard)
library(plyr)
library(dplyr)
library(data.table)
library(sp)
library(rgdal)
library(raster)
library(leaflet)
library(ggplot2)
library(ggvis)
library(ggtern)
library(scales)
library(grid)
library(gridExtra)

load("./data/dataExport.RData")
load("./data/polygons.RData")
load("./data/polygon.RData")
load("./data/soilContent.RData")
load("./data/data_farmer.RData")

maxYield <- setDT(dataExport)[, .SD[which.max(GrainYield)], by=TreatmentDescription]

data(USDA)
USDA.LAB = ddply(USDA, 'Label', function(df) {
  apply(df[, 1:3], 2, mean)
})

# Tweak
USDA.LAB$Angle = 0
USDA.LAB$Angle[which(USDA.LAB$Label == 'Loamy Sand')] = -35

palette <- c("firebrick2", "white", "dodgerblue2")

a_gdd = c(-20, 20)
a_pre = c(-20, 20)

