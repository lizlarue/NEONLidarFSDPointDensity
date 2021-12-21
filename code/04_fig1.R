################################################################################
#Create figure 1 of point clouds of different densities from one plot
#Updated March 25, 2021
#this works with the Version 3.1.2 of lidR and R version 3.6.3
################################################################################
setwd("preprint_12212021/data/")
######################## Packages ##############################################
library('lidR')
library('plot3D')
#####################################################################################
#Figure 1 - ABBY_029 pull the data
#################################################################################
J2 <- readLAS("./thinned/density_2_1_ABBY_029_2019.las")
J2 <- J2@data
J4 <- readLAS("./thinned/density_4_1_ABBY_029_2019.las")
J4 <- J4@data
J6 <- readLAS("./thinned/density_6_1_ABBY_029_2019.las")
J6 <- J6@data
J8 <- readLAS("./thinned/density_8_1_ABBY_029_2019.las")
J8 <- J8@data
J10 <- readLAS("./thinned/density_10_1_ABBY_029_2019.las")
J10 <- J10@data
J15 <- readLAS("./thinned/density_15_1_ABBY_029_2019.las")
J15 <- J15@data
J20 <- readLAS("./thinned/density_20_1_ABBY_029_2019.las")
J20 <- J20@data
J25 <- readLAS("./thinned/density_25_1_ABBY_029_2019.las")
J25 <- J25@data
################################# 3D plots

tiff("Fig1.tiff", res = 300, width = 8, height = 6, units = 'in')
my_palette <- colorRampPalette(col=c("#a903e6", "#d293ed", "#f2c530", "#f09c14", "#ff2b2b", "#850b21"))(n = 599) #purple orange red
col_breaks = c(seq(-.5, 7, length=100), seq(7.001, 14, length=100), seq(14.001, 21, length=100), seq(21.001, 28, length=100), seq(28.001, 35, length=100), seq(35.001, 47, length=100)) 
par(mfrow=c(2,4), mar = c(.5,.5,.5,.5)) #bottom, left, top and right 

scatter3D(J2$X, J2$Y, J2$Z, colvar = J2$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J4$X, J4$Y, J4$Z, colvar = J4$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J6$X, J6$Y, J6$Z, colvar = J6$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J8$X, J8$Y, J8$Z, colvar = J8$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J10$X, J10$Y, J10$Z, colvar = J10$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J15$X, J15$Y, J15$Z, colvar = J15$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J20$X, J20$Y, J20$Z, colvar = J20$Z, col=my_palette, breaks=col_breaks, 
          colkey=FALSE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)

scatter3D(J25$X, J25$Y, J25$Z, colvar = J25$Z, col=my_palette, breaks=col_breaks, 
          colkey=TRUE, cex = .15, bty='b2',
          theta = 15, phi = 20, xlab = NA, ylab = NA, zlab = NA, pch = 19)
dev.off()

#figure is cleaned up in powerpoint to add labels et

