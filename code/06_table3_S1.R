################################################################################
#segmented regressions to see which point density at which FSD metrics stablilize
#Updated March 25, 2021
#this works with the R Version 3.6.3
################################################################################
setwd("preprint_12212021/data/")
################################################################################
#FSD metrics for 8 bins of random thinning
dat <- read.csv("./density_FSD_08172021.csv")
#trim data down to only metrics of focus
names(dat)
dat <- dat[,c("plotID", "density","replicate", "site", "mean.max.canopy.ht", "q25",
              "q50", "q75", "q100", "VAI", "deepgap.fraction", "GFP", "rumple", 
              "top.rugosity","vert.sd", "sd.sd", "VCI", "vertCV", "fhd", "gini")]
#################################################################################
library(segmented)
# mostly from here: https://rpubs.com/MarkusLoew/12164
# http://cran.r-project.org/doc/Rnews/Rnews_2008-1.pdf
#################################################################################
#segmented regression run on each site by metric combo (720 of them)
plots <- unique(dat$plotID)
FSD <- c("mean.max.canopy.ht", "q25",
         "q50", "q75", "q100", "VAI", "deepgap.fraction", "GFP", "rumple", "top.rugosity", 
         "vert.sd", "sd.sd", "VCI", "vertCV", "fhd", "gini")
OUT <- NULL
for(i in 1:length(plots)) {
  #narrow to one plot
  dat.i <- dat[dat$plotID == plots[i], ]
  
for(n in 1:length(FSD)) {
dat.n <- dat.i[, c("density", FSD[n])]
colnames(dat.n)[2] <- "y"

# create a linear model
my.lm <- lm(y ~ density, data = dat.n)

#pscore.test to test for the existence of one change point (default k = 10)
nullH <- pscore.test(my.lm, seg.Z = ~ density, n.break =1)

if(nullH$p.value < 0.05){
  # have to provide estimates for change points after looking at the data
  my.seg <- segmented(my.lm, 
                      seg.Z = ~ density, 
                      psi = 8) #psi = estimated change point
  
  #only calc change point for those that segmented regression calcs any change points
  if(length(my.seg$coefficients) < 3){
    breaks <- matrix(c(NA, NA, NA),ncol=3)
    colnames(breaks) <- c("Initial", "Est.", "St.Err")
  }else{
    breaks <- my.seg$psi
  }
  
  } else {
    breaks <- matrix(c(NA,NA,NA),ncol=3)
    colnames(breaks) <- c("Initial", "Est.", "St.Err")
  }
  out.n <- cbind.data.frame(plots[i], FSD[n], nullH$p.value, breaks)
  OUT <- rbind(OUT, out.n)
}
}

colnames(OUT) <- c("plotID", "FSDmetric", "pscore_pvalue", "initial_psi", "breakpoint", "breakpointSE")

#adjust for multiple pvalues with a false discovery rate correction for 720 tests
OUT$FDR_padjust <- p.adjust(OUT[,"pscore_pvalue"], method = "fdr", n=length(OUT[,"pscore_pvalue"]))

write.csv(OUT, "./segmented_regression_FSD_08222021.csv")




