# Scots Pine Experiment
## This is a long term experiment in Scots Pine
## The treatments are 4 different spacings 1X1, 1.5X1.5, 2X2 or 2.5X2.5

## For this exercise, we'll be using the
## 'doBY' library package
## Installing the package needed for this exercise
install.packages("doBy")

#stand information
# The stand contains 8 plots of varying sizes
# and four treatments will be used on the sites, 2 varying sites to a treatment

## Creating the plots
plot = c(11:14, 21:24)

##size of the plots in ha
areaha = c(0.04, 0.0324, 0.0288, 0.0288, 0.04, 0.0324, 0.0288, 0.0288)

##spacing or treatment of the experiment
treatment = c(2.5, 2, 1.5 ,1)
### **nb**: 2.5 implies 2.5*2.5 and 2 implies 2*2 and so on.

##dataframe for the plots with their properties
exp1012 <- data.frame(plot, areaha, treatment)

# A table containing data for measurements got from individual trees of each plot is imported

# importing the file
read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab2/dbhlist_exp1012.txt",
           header = T, sep = "\t", na.strings = "NA", dec = ".", strip.white = TRUE)

# Assigning imported file to an object, let's call this dbh1012
dbh1012 <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab2/dbhlist_exp1012.txt",
                      header = T, sep = "\t", na.strings = "NA", dec = ".", strip.white = TRUE)

## Information about imported data
### - plot: the plot number
### - nr: the tree number
### - d1: diameter at breast height 1
### - d2: diameter at breast height 2
### - dm: mean of both diameter
### - dd: diameter raise to power 2
### - ba: basal area

--------------

# Interviewing the data

## quick summary of the data
summary(dbh1012)

## checking data structure
str(dbh1012)

##short overview of data
head(dbh1012)

####quick plot to see if the data is strange
plot(dbh1012$d1, dbh1012$d2)

####mean of average dbh of every trees in the observations
dbh1012$dm <- (dbh1012$d1+dbh1012$d2)/2

####dbh^2 of meandbh^2 value for calculating the quadratic mean
####the value will be needed later on.
dbh1012$dd <- dbh1012$dm^2

####Calculating the basal area per tree "ba"
dbh1012$ba = pi*((dbh1012$dm/2)^2)

----------------

  #accessing the doBy package
library(doBy)

# Summarizing the dbh1012 data using the doBy
#We summarise the basal area, ba, deom the data dbh1012,
#with the function sum being the function of interest now.
plotba <- summaryBy(ba~plot, data = dbh1012, FUN = sum)

##The summary is merged to the stand information and assigned to the object 'site1012'
site1012 <- merge(exp1012, plotba, all = T)

##calculating the basal area per hectare for each plot
site1012$baha <- 1/site1012$areaha * site1012$ba.sum

##converting to basal area per hectare from mm2 to m2
site1012$baham2 <- site1012$baha/1000000

##getting the mean basal area per treatment, treatments are 1, 1.5,2,2.5.
trtmean1012 <- summaryBy(baham2~treatment,
                         data = site1012, FUN = mean)
##rounding up values
trtmean1012 <- round(trtmean1012, 1)


'##another way to summarize this is using tapply
tapply(site1012$bahamil, site1012$treatment, mean)

##rounding to one decimal place
round(tapply(site1012$bahamil,
             site1012$treatment, mean),1)'

## Next we derive the number of trees per plot
plotdens <- summaryBy(nr~plot, data = dbh1012, FUN = length)

##merging plotdens to site1012
site1012 <- merge (site1012, plotdens, all = T)

## renaming some column
names(site1012)[7] <- "trees_per_plot"

##getting the trees density per hectare assigned to densha
site1012$dens_ha <- round((site1012$trees_per_plot * (1/site1012$areaha)), 1)



##visualizing the relationship between treatment and density
plot(site1012$treatment, site1012$dens_ha, type = "p",
     xlab = "density",
     ylab = "treatment", main = "Treatment and Density relationship",
     pch = 20, col = "red")


##deriving Arithmetic mean (AMD) and Quadratic mean (QMD) from dbh1012
##For better understanding of what arithmethic mean and QMD means
##check https://en.wikipedia.org/wiki/Quadratic_mean_diameter

plotdbh <- summaryBy(dm+dd~plot, data = dbh1012, FUN = c(mean, sum))

##merging plotdbh values to site1012 to display the arithmethic mean diameter
site1012 <- merge(site1012, plotdbh, all = T)

##Converting dm.mean and dm.sum from millimeters to centimeter
site1012$dm.mean_cm <- site1012$dm.mean/10

## "site1012$dm.mean_cm" is synonymous to AMD
#therefore
site1012$amd = site1012$dm.mean_cm

site1012$dm.sum_cm <- site1012$dm.sum/10

#converting dd.mean and dd.sum from mm^2 to cm ^2
site1012$dd.mean_cm <- site1012$dd.mean/100
site1012$dd.sum_cm <- site1012$dd.sum/100

#TO get QMD
site1012$qmd <- sqrt(site1012$dd.sum/site1012$nr.length)/10

-------------
  
# Visualizing result

## Basal area vs density relationship
plot(site1012$dens_ha, site1012$baham2,
     main = "Basal area vs Density",
     xlab = "Basal Area",
     ylab = "Density",
     pch = 24,
     col = site1012$plot)
legend("topleft", legend = c(site1012$plot),
         pch = 24,
         col = site1012$plot)

## visualizing amd and qmd vs treatments
plot(x = site1012$treatment,
     y = site1012$amd,
     main = 'AMD and QMD comparison',
     ylim = c(8, 16),
     pch = 21,
     col = "darkred",
     xlab = "Treatment",
     ylab = "AMD and QMD(cm)")
points (x = site1012$treatment,
     y = site1012$qmd,
     type = "p",
     ylim = c(8, 16),
     pch = 19,
     col = "darkgreen",
     xlab = "Treatment",
     ylab = "AMD and QMD(cm)")
legend ("topleft",
          legend = c("AMD", "QMD"),
          pch = c(21, 19),
          col = c("darkred", "darkgreen"))

