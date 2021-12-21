################################################################################
#Randomly generate different point densities for each plot
#Updated March 25, 2021
#this works with the Version 3.1.2 of lidR and R version 3.6.3
################################################################################
setwd("preprint_12212021/data/")
######################## Packages ##############################################
library('lidR')
 #####################################################################################
#about the data
#45 were chosen that had 25 - 26 pt/m2 for 5 NEON sites
#these sites vary in their forest structure but all the plots were visually
#checked to determine outliers were removed 

#FSD metrics are measured at different densities for each plot
#points at randomly thinned to the specified density
#NEON has not collected data using the different resolution lidar sensors within
#the same year, so random thinning is the only option to compare the 
#effect of density on FSD metric values

#####################################################################################
# --------------------------------------------------------------------------------------
#####################################################################################
#manipulate point density and write new files
#had to manually remove an outlier from UKFS_021_2019.las

#####################################################################################
#readLAS(filter = "-help")
#Filter points with simple thinning. -keep_random_fraction 0.1
pt_density <- c(25, 20, 15, 10, 8, 6, 4, 2) #pt/m2
replicates <- seq(1:5)

las.names <- list.files("./all_baseplots/") #pull names of plots
for(i in 1:length(las.names)){
  #-----------------------------------------------------------------
  #reading in individual las plots without filtering 
  data.40m <- readLAS(file.path(paste("./all_baseplots/", las.names[i], sep="")))
  print(las.names[i])
  
  #get density
  orig_den <- length(data.40m@data$Z)/area(data.40m)
  
  ########     ##########     ########     ########
  #thin by random fraction to obtain specified pt/m2
  for(n in 1:length(pt_density)) {
    for(f in 1:length(replicates)) {
      
    thin_frac <- pt_density[n] / orig_den
    
    data.40m <- readLAS(file.path(paste("./all_baseplots/", las.names[i], sep="")), 
                        filter = paste("-keep_random_fraction", thin_frac))

    writeLAS(data.40m, file = paste("./thinned/","density_", pt_density[n], "_", 
                                    replicates[f], "_", las.names[i], sep=""))
    }
  }
}