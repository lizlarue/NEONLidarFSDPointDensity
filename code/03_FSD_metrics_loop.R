################################################################################
#Workflow to obtain FSD metrics thinned las files for each plot .las
#Updated August 17, 2021
#this works with the Version 3.1.2 of lidR and R version 3.6.3
################################################################################
setwd("preprint_12212021/data/")
######################## Packages ##############################################
library('lidR')
library('leafR')

#####################################################################################
#### Structural Metric calculations
# These are setup in two functions and then several from the leafR package in a for loop
##################################################################

######################################################
chm_groundpts_metrics <- function(data.40m) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 3 m as zero because to get rid of ground points
  data.40m@data$Z[data.40m@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 0)
  if(sum(data.40m@data$Z) > 0) {
    chm <- grid_canopy(data.40m, res = 1, p2r()) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are < 3 m)
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > 3 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################
structural_diversity_metrics <- function(data.40m) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(data.40m, res = 1, p2r()) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics
  vert.sd <- cloud_metrics(data.40m, sd(Z, na.rm = TRUE)) 
  meanH <- cloud_metrics(data.40m, mean(Z, na.rm = TRUE)) 
  vertCV <- vert.sd / meanH
  vertq <- cloud_metrics(data.40m, quantile(Z, na.rm = TRUE))
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(data.40m, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- data.40m@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 3 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0= 3) #ignore points < 3 m
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#step 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation
las.names <- list.files("./thinned/") #set location where all plot .las were added
OUT <- NULL
#i=1
for(i in 1:length(las.names)){
  #-----------------------------------------------------------------
  #----------------
  #reading in individual las plots without filtering to get area and density
  data.40m <- readLAS(file.path(paste("./thinned/", las.names[i], sep="")))
  print(las.names[i])
  
  #get area to check for cropped plots
  plot_area_ground <- area(data.40m)
  
  #total number of points divided by plot area
  den <- length(data.40m@data$Z)/plot_area_ground
  
  #max height
  maxZ <- max(data.40m@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(data.40m)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  data.40m <- readLAS(file.path(paste("./thinned/", las.names[i], sep="")), filter = "-drop_z_below .5")
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  data.40m <- filter_duplicates(data.40m)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(data.40m)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(data.40m)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste("./thinned/", las.names[i], sep="")
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
 
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)
}

colnames(OUT)[1] <- "filename"
write.csv(OUT, file = "./density_FSD_08172021.csv")
##add a site and replicate column manually in excel because file 
#names have uneven number of numeric values
