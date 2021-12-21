################################################################################
#Workflow to obtain NEON AOP lidar from neonUtilities
#Updated Apr. 7, 2021
#this works with the Version 3.1.2 of lidR
################################################################################
setwd("preprint_12212021/data/")
######################## Packages ##############################################
library('neonUtilities')
#####################################################################################
#                 
#                 DOWNLOADING DATA
#     
#####################################################################################
#####################################################################################
####################### NEON site spatial info - base plots ####################
plots <- read.csv("./NEON_plot_centers_04232021.csv")

####################### Download AOP LiDAR from NEON data portal ################
#Download can only occur in a single site by year combination so manually uncomment which
#site and year is wanted
#SITECODE <- "ABBY" 
#SITECODE <- "GRSM"
#SITECODE <- "STEI"
#SITECODE <- "UKFS"
SITECODE <- "UNDE"
plots <- plots[plots$siteID == SITECODE, ]

EASTING <- plots[, "easting"]
NORTHING <- plots[, "northing"]

#select which year is needed
#YEAR <- 2018
#YEAR <- 2019
YEAR <- 2020

byTileAOP(dpID = "DP1.30003.001", site = SITECODE, year = YEAR, 
          easting = EASTING, northing = NORTHING, check.size = T, buffer = 90, 
          savepath= paste0("./neon_las_tiles_download_04232021/", SITE, "_", YEAR))

#####################################################################################
