################################################################################
#Workflow to clean and clip neon lidar baseplots
#Updated August 17, 2021
#this works with the R version 3.6.3 and Version 3.1.2 of lidR 
################################################################################
setwd("preprint_12212021/data/")
######################## Packages ##############################################
library('lidR') 
#####################################################################################
# step 1: download data
# This workflow is meant to be run for one site and one year of data at a time
#####################################################################################
#####################################################################################
####################### NEON site spatial info - base plots ####################
#contains the plot centroids for NEON sites
plots <- read.csv("./NEON_plot_centers_04232021.csv")

####################### Download AOP LiDAR from NEON data portal ################
#must manually enter which site to be downloaded in this step for the entire workflow (year & site) 
#site and year combos
#ABBY_2019 GRSM_2018 STEI_2019 UKFS_2019 UNDE_2020
SITECODE <- "UNDE"
YEAR <- 2020
plots <- plots[plots$siteID == SITECODE, ]

EASTING <- plots[, "easting"]
NORTHING <- plots[, "northing"]
#####################################################################################
#####################################################################################
#raw data downloaded April 2021 from NEONutilities
#all the raw .laz tiles were placed in a single folder where specified
#open as a las catalog object
ctg <- readLAScatalog(folder = paste0("./neon_las_tiles_download_04232021/", 
                                      SITECODE, "_", YEAR, "/"))
las_check(ctg)
plot(ctg)
#####################################################################################
#                 Step 2 - create a polygon of plot areas
#####################################################################################
#####################################################################################
#create a shape file of buffer plot centroids for one NEON site
#taken from NEON dataskills tutorial
#https://www.neonscience.org/resources/learning-hub/tutorials/field-data-polygons-centroids
#####################################################################################

#create CRS for each site's UTM zone
plots$utmZone <- as.factor(plots$utmZone)
UTM <- droplevels(plots[1, "utmZone"])
crs.s <- CRS(paste("+proj=utm +zone=", UTM," +datum=WGS84 +units=m +no_defs", sep=""))

# set the radius for a 80 x 80 m buffer around the plot centroid
radius <- 40 #radius in meters (actually a square not a circle)

# define the plot edges based upon the plot radius. 
yPlus <- plots$northing+radius
xPlus <- plots$easting+radius
yMinus <- plots$northing-radius
xMinus <- plots$easting-radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

plots$plotID <- as.factor(plots$plotID)
ID= droplevels(plots$plotID)

polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string = crs.s)

#Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df.buffer <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
plot(polys.df.buffer, col=rainbow(50, alpha=0.5))

#####################################################################################
#####################################################################################
#Step 3 - clip the lidar tiles with the plot buffer polygon
#####################################################################################
#Tell the code where you want your clipped plots to go.
# {plotID} is telling the code to name the output files by their plot numbers.
opt_output_files(ctg) <- paste0("./Processed/",SITECODE, YEAR,"/buffer/{id}", sep="") 

#clip the area within the point clouds from within the plot buffer polygon 
#this step takes a while depending on your RAM and processor
clipped.plot.s <- clip_roi(ctg, polys.df.buffer) 

##################################################################################### 
#####################################################################################
#STEP 4: Remove outliers and correct for Z height to account for elevation ##
#####################################################################################
# --------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#function to calculate and remove unwanted outliers in readLAS()
outliers <- function(las_file) {
  #remove outlier points using mean and sd statistics
  Z_sd=sd(las_file@data$Z)
  Z_mean=mean(las_file@data$Z)
  #using 6 sd as a coarse filter due to topo variation
  f= paste("-drop_z_below",(Z_mean-6*Z_sd),"-drop_z_above",(Z_mean+6*Z_sd))
  return(f)
}
# --------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#running these plots through a for loop because they are relatively small
#and sometimes the buffer areas overlap - cannot use spatially overlapping files
#in catalog functions to correct for elevation
dir.create(paste("./Processed/",SITECODE, YEAR, "/rm_noise/", sep=""))
buffer.names <- list.files(paste0("./Processed/", SITECODE, YEAR, "/buffer", sep="")) 

for(i in 1:length(buffer.names)){
  #reading in individual las of each plot
  las_file<- readLAS(file.path(paste("./Processed/",SITECODE, YEAR, "/buffer/", buffer.names[i], sep="")))
  print(buffer.names[i])
  print("Z range pre filter")
  print(range(las_file@data$Z))
  
  #drop out outliers with SD filter
  f <- outliers(las_file)
  las_file <- readLAS(file.path(paste("./Processed/",SITECODE, YEAR, "/buffer/", buffer.names[i], sep="")), filter = f)
  print("Z range post SD filter")
  print(range(las_file@data$Z))
  
  #drop out finer outliers with an IVF filter
  #ivf = isolated voxels filter, which finds points with only a few other points
  #in their surrounding 3 x 3 x 3 = 27 voxels neighborhood
  #res = voxel resolution, n = maximal number of other points in the 27 voxels
  #similar to lasnoise from LAStools https://rapidlasso.com/lastools/lasnoise/
  buffer.noise <- classify_noise(las_file, ivf(res=3,n=0)) 
  #remove points with a classification (18) of noise 
  las_denoise <- filter_poi(buffer.noise, Classification != LASNOISE)
  print("Z range post ivf filter")
  print(range(las_denoise@data$Z)) #after filter Z range
  
  #correct for ground height
  #might get a deprecated points warning, but the function automatically removes these
  las_file <- normalize_height(las_denoise, tin())
  
  #save new clean buffer file
  writeLAS(las_file, paste("./Processed/",SITECODE, YEAR, "/rm_noise/", buffer.names[i], sep=""))
}

# --------------------------------------------------------------------------------------
#read in newly created and correct buffer plot .las files into a catalog
#---------------------------------------------------------------------------------------
rm_noise <- readLAScatalog(folder = paste0("./Processed/",SITECODE, YEAR,"/rm_noise/", sep=""))
plot(rm_noise)

######################################################################################
#####################################################################################
#Step 5 create a shape file of 40 x 40 m plots for final clip
#####################################################################################
#create CRS for each site's UTM zone
UTM <- droplevels(plots[1, "utmZone"])
crs.s <- CRS(paste("+proj=utm +zone=", UTM," +datum=WGS84 +units=m +no_defs", sep="")) 

# set the radius for the plots
radius <- 20 # radius in meters

# define the plot edges based upon the plot radius. 
yPlus <- plots$northing+radius
xPlus <- plots$easting+radius
yMinus <- plots$northing-radius
xMinus <- plots$easting-radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

ID= droplevels(plots$plotID)

polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string = crs.s)

#Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df.baseplot <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
plot(polys.df.baseplot)

##################################################################################### 
#####################################################################################
#STEP 6: Clip final 40 x 40 m plot .las
#####################################################################################
#####################################################################################
## Data will be saved in specified folder
opt_output_files(rm_noise) <-  paste0("./Processed/",SITECODE, YEAR, "/baseplots/{id}", "_", YEAR, sep="")

#clip 40 x 40 m plot. las
baseplots <- clip_roi(rm_noise, polys.df.baseplot)
plot(baseplots)

#####################################################################################
#copied the processed 40 x 40 m point clouds .las files created above 
#and put them into one folder to make step 2 
#run through data in one folder ("./all_baseplots/")