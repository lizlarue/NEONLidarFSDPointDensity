################################################################################
#Creates Fig. 2 and 3 that show FSD average values across LiDaR point densities
#Updated August 19, 2021
#this works with R version 3.6.3
################################################################################
setwd("preprint_12212021/data/")
################################################################################
library("ggplot2")
library("gridExtra")
library("cowplot")
################################################################################ 
#FSD metrics for 8 bins of random thinning
dat <- read.csv("./density_FSD_08172021.csv")
################################################################################
#take the mean of the 5 replicates x density values and standard deviation 
meanFSD <- aggregate(dat[,9:27], by = list(density = dat$density, site = dat$site, plotID = dat$plotID), mean)
meanFSD$site_den <- paste(meanFSD$site, meanFSD$density, sep = "_")

seFSD <- aggregate(dat[,9:27], by = list(density = dat$density, site = dat$site, plotID = dat$plotID), sd)
seFSD$site_den <- paste(seFSD$site, seFSD$density, sep = "_")
seFSD <- seFSD[,-1]
seFSD <- seFSD[,-1]

#standard deviation not standard error
colnames(seFSD) <- c("plotID", "serumple", "sedeepgap.fraction", "semean.max.canopy.ht", 
                     "semax.canopy.ht", "setop.rugosity", "severt.sd", "sesd.sd",            
                     "seGFP", "seVAI","seVCI", "severtCV","seq0", "seq25",               
                     "seq50", "seq75", "seq100", "sefhd", "segini", "seLAI_subcanopy",
                     "site_den")

dat <- merge(meanFSD, seFSD, by = c("site_den", "plotID"))
rm(meanFSD, seFSD)
mycol <- c("red", "blue", "black", "lightgreen", "gray46")
###################################################################################
#Fig 2
g <- ggplot(dat, aes(x=density, y= mean.max.canopy.ht, group=plotID, color= site)) + 
  geom_line(show.legend = FALSE) + geom_point(size = .05, show.legend = FALSE) + xlab("Density") + ylab("MOCH") + scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

 h  <-ggplot(dat, aes(x=density, y= q25, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("Q25") +scale_colour_manual(values=mycol) +
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank())
 
 i <- ggplot(dat, aes(x=density, y= q50, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("Q50") +scale_colour_manual(values=mycol) +
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank())
 
 j <- ggplot(dat, aes(x=density, y= q75, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("Q75") +scale_colour_manual(values=mycol) +
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank())
   
k <- ggplot(dat, aes(x=density, y= q100, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("Q100") +scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

l <-ggplot(dat, aes(x=density, y= VAI, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("VAI") +scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

m <- ggplot(dat, aes(x=density, y= deepgap.fraction, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("DGF") +scale_colour_manual(values=mycol) 

n <- ggplot(dat, aes(x=density, y= GFP, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("GFP") +scale_colour_manual(values=mycol) 

pdf("Fig2.pdf")
plot_grid(g,h,i,j, k, l, m, n, labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
          ncol = 2, nrow = 4)
dev.off()
################################################################################################
#fig. 3
a <- ggplot(dat, aes(x=density, y= rumple, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE) + xlab("Density") + ylab("Rumple") + scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
b <- ggplot(dat, aes(x=density, y= top.rugosity, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("Top Rugosity") + scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

c <- ggplot(dat, aes(x=density, y= vert.sd, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("SD(ht)") +scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

d <- ggplot(dat, aes(x=density, y= sd.sd, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("SD(SD(ht))") +scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
e <- ggplot(dat, aes(x=density, y= VCI, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("VCI") +scale_colour_manual(values=mycol) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

f <- ggplot(dat, aes(x=density, y= vertCV, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ ylab("CV(ht)") +scale_colour_manual(values=mycol) +
   theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

x <- ggplot(dat, aes(x=density, y= fhd, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05, show.legend = FALSE)+ xlab("Density") + ylab("FHD") +scale_colour_manual(values=mycol)

y <- ggplot(dat, aes(x=density, y= gini, group=plotID, color=site)) + 
  geom_line(show.legend = FALSE)+ geom_point(size = .05,show.legend = FALSE)+ xlab("Density") + ylab("Gini Index") +scale_colour_manual(values=mycol)

pdf("Fig3.pdf")
plot_grid(a, b, c, d, e, f, x, y, labels=c("A", "B", "C", "D", "E", "F", "G", "H") ,ncol = 2, nrow = 4)
dev.off()

