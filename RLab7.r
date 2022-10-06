# Regeneration Experiment

## This data from a regeneration inventory made on four clearcuts (A, B, C and D),
## where density (stems/ha) of Norway spruce and birch was measured in sample plots 6 years after regeneration.
## Birch was naturally regenerated and the spruce seedlings was planted.
## The same plot size was used on all clearcuts but the number of plots varied between sites.

## What general findings could be made about the regeneration success and species proportion on clearcuts 6 years after planting?
  
## Calculate the mean stem density for spruce and birch respectively. Compare the results of

## a) calculating the mean value of all inventory plots from all sites  with
## b) first calculating the site mean and thereafter the mean of all sites.

## Visualize the site variation with box and whiskers plot (library lattice, bwplot()).

## Why is the difference between the two calculations more apparent for birch than for spruce? 
  ## The data include:
  
  ## site=site id A, B, C or D
## plot=plot id 1-7
## species=spruce or birch
## density=stems per ha

## Loading Libraries
library(doBy)
library(lattice)

### Importing the data set
regen <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab 7/regenerationsprucebirch.txt",
           header = T, sep = '\t')

### Splitting data according to species
spr <- subset(regen, species == 'spruce')

brch <- subset(regen, species == 'birch')

## Seeing the experimental design
table(spr$site, spr$plot)

## Birch and Spruce boxplot visualization
bwplot(density~site, data = brch)

bwplot(density~site, data = spr)

## Mean density of species regardless of sites and plots

grand_mean <-summaryBy(density~species,
                       data = regen,
                       FUN = c(mean, sd, length))
## Mean density per sites for each species

sitemean <- summaryBy(density~species+site,
                      data = regen,
                      FUN = mean,
                      na.rm = T,
                      keep.names = T)
## Mean of different sites by species
grand_mean_2 <- summaryBy(density~species,
          data = sitemean,
          FUN = c(mean, sd, length))

### Calculating the standard error for correctly calculated mean values
grand_mean$st.err <- grand_mean$density.sd/
  sqrt(grand_mean$density.length)

grand_mean_2$st.err <- grand_mean_2$density.sd/
  sqrt(grand_mean_2$density.length)

## Visualizing the plot density
plot(regen$plot, regen$density,
     col = as.factor(regen$species))


## Lodgepolepine Scarification Experiment
## This is the measured sample plots from a soil scarification experiment with lodgepole pine. 
## The treatments was planting after soil scarification using deep plowing (deep), or planting 
## without any site preparation (Control). The size of the treatment plots were 30*30 m. The 
## plantation was made 1988 and the experiment was remeasured after several years, in 2012, 
## when all trees in the treatment plots was cross calipered at dbh (mm).
## Explore the data and then calculate basal area/ha and stem density/ha for every block and 
## treatment. Can you find any significant effect on basal area on living trees?

L_pine <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab 7/deepcult_lodgepolepine.txt",
                     header = T, sep = '\t')
L_pine$dead[is.na(L_pine$dead)] <- 0
L_pine1 <- subset(L_pine, dead != 1)

## visualizing the data
bwplot(dbh1~factor(plot) | treatment, data = L_pine1)

## dbh for every tree in meters
xyplot(dbh1~dbh2,
       data = L_pine1)

L_pine1$dbh <- (L_pine1$dbh1 + L_pine1$dbh2)/2000

L_pine1$ba <- (L_pine1$dbh/2)^2 * pi
lpba <- summaryBy(ba+dbh~plot+block+treatment,
                  data = L_pine1,
                  na.rm = T,
                  keep.names = T,
                  FUN = sum)
lpba$bah <- lpba$ba * 10000 / (30*30)
