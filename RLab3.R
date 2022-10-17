# Regression and Correlation Exercise

#importing plot 11 tree data - 'trees1012rev2_plot11selection.txt' and assigning to object plot11trees
plot11trees <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab3/trees1012rev2_plot11selection.txt",
                     header = T, na.strings = "NA", dec = ".", sep = "\t", strip.white = TRUE)

##The data above is incomplete containing some missing data

library(doBy)

#Converting heights to meters named 'hm' and dbh to centimeter named 'dcm

## Converting decimeter to meter
plot11trees$hm <- plot11trees$height/10

##Converting from millimeter to centimeter
plot11trees$dcm <- plot11trees$dbh/10

#selecting the sample trees, i.e trees with both height above 0m

sample_trees <- subset(plot11trees, hm > 0)
## subset function here returns subset of the vectors, matrices or data frame that meet its conditions.
## here the function returns plot11trees with height(hm) more than 0 meter
## The syntax is subset(x, condition)

#Estimating volume using Sweden generated vol function for Scots pine

#scot pine function:
"vol function Volume=10^(-1.38903)*dbh^1.84493
(dbh+20)^0.06563*height^2.02122*(height-1.3)^(-1.01095)"

sample_trees$vol <- ((10^-1.38903) * (sample_trees$dcm^1.84493) *
  ((sample_trees$dcm + 20)^0.06563) * sample_trees$hm^2.02122*
  ((sample_trees$hm-1.3)^-1.01095))/1000


##Simple visualization of the newly generated volume
plot(sample_trees$dcm, sample_trees$vol, pch = 13,
     col = "darkblue", main = "Diameter vs Volume",
     xlab= substitute(paste(bold("Diameter"))),
     ylab = substitute(paste(bold("Volume"))))

# LINEAR REGRESSION FOR THE SAMPLE TREES

##First we get the log of both dcm and vol
### log of Volume
sample_trees$logvol <- log(sample_trees$vol)

### log of Diameter
sample_trees$logdcm <- log(sample_trees$dcm)

#Fitting linear models for the sample trees with lm(y~x)
M.vol <- lm(sample_trees$logvol~sample_trees$logdcm)

plot(sample_trees$logdcm, sample_trees$logvol, pch = 13,
     col = "darkblue", main = "Diameter vs Volume",
     xlab= substitute(paste(bold("Diameter"))),
     ylab = substitute(paste(bold("Volume"))),
     abline (M.vol, col = "red"))
legend ("topleft",
          legend = c("Intercept = -7.734953",
                     " ",
                     "slope = 2-020352"),
          col = "green")

## Extracting the coefficients
coef(M.vol)

## adding the coefficients to plot11trees table
plot11trees$a <- coef(M.vol)[1]
plot11trees$b <- coef(M.vol)[2]

#estimating the volume
plot11trees$est_vol <- exp(plot11trees$a + 
                             plot11trees$b *
                             log(plot11trees$dcm))

##getting an overview of the estimated volume and calculated volume
plot(plot11trees$dcm,plot11trees$est_vol,
     pch = 20,
     col = "red",
     xlab = substitute(paste(bold("diameter(cm)"))),
     ylab = substitute(paste(bold("Volume(m^3)"))),
     main = "Estimated volume vs Calculated volume") + 
  points(sample_trees$dcm, sample_trees$vol,
         pch = 19,
         col = "green",
         xlab = substitute(paste(bold("diameter(cm)"))),
         ylab = substitute(paste(bold("Volume(m^3)")))) +
  legend ("topleft",
            legend = c("Estimated volume", "Calculated volume"),
            pch = c(20,19),
            col = c("red", "green"))


##Exercise
"The tvol1012 is a data  consisting of revised data from 
2 separate years, year 1980 and 1987, with this data we will:
  
  - evaluate the volume growth for individual trees
  - estimate the periodic annual increment (PAI),
  - estimate the annual or yearly increment and
  - estimate the plot and treatment volume growth"


#importing year 1980 and year 1987 tree volume data 'tvol1012.txt'
tvol1012 <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab3/tvol1012.txt",
                       header = TRUE, na.strings = "NA", sep = "\t", dec = ".", strip.white = TRUE)

#Estimating the PAI
tvol1012$pai = (tvol1012$voldm3.1987 - tvol1012$voldm3.1980)/7

#Estimating annual growth or yearly increment
tvol1012$ai = tvol1012$pai/7

#Estimating plot data for the trees
plotvol <- summaryBy(voldm3.1980 +
            voldm3.1987 +
            pai~plot,
          data = tvol1012, FUN = sum)
## Merging data with the data table 'site1012' from RLab2
site1012 <- merge(site1012, plotvol, all = T)

site1012$volm80ha <- site1012$voldm3.1980.sum/(site1012$areaha*1000)
site1012$volm87ha <- site1012$voldm3.1987.sum/(site1012$areaha*1000)
site1012$paiha <- site1012$pai.sum/(areaha*1000)

#visualizing the mean annual increment
barplot(site1012$paiha,
        names.arg = c(11:14,21:24), 
        main= "Periodic Annual Increment",
        xlab = substitute(paste(bold("plots"))),
        ylab = substitute(paste(bold("Volume Per Hectare Per Year"))),
        col = c(8,2,3,4)) +
  legend("bottomright",
         legend = c(2.5,2.0,1.5,1.0),
         pch = 16,
         col =c (8,2,3,4))


#Visualizing the density per hectare
barplot(site1012$densha, col = c(3,5,8,6),
        names.arg = c(as.character(site1012$plot)), 
        main= "Density per Hectare",
        sub = "4 treatments (2.5,2,1.5,1)",
        xlab = substitute(paste(bold("plots"))),
        ylab = substitute(paste(bold("Stand Density"))),
        ) +
  legend("topleft",
         legend = c(2.5,2.0,1.5,1.0),
         pch = 16,
         col = c(3,5,8,6))

tapply(site1012$paiha,
       site1012$treatment,
       FUN = mean)

# Estimating the percentage of stand seedlings have been removed
##first we estimate the density after thinning which is provided in site1012
density <- summaryBy(densha~treatment,
          data = site1012,
          FUN = mean)
 
## densha.mean is the density after thinning, so we rename to 'after_thinning'
names(density)[2] <- 'after_thinning'

##now we estimate the density before thinning
density$before_thinning <- 10000/(density$treatment^2)

##percentage change in the stand can be estimated now.
## percentage change = (old - new)/old *100
density$percent_removed <- (density$before_thinning-
                              density$after_thinning)/density$before_thinning * 100

# Checking the effect of treatment on  stem density, quadratic mean diameter,
# and Periodic Annual Increment (PAI)

## Stem density and treatment(spacing) design effect
barplot(tapply(site1012$densha,
               site1012$treatment,
               FUN = mean),
        xlab = substitute(paste(bold('treatment(spacing)'))),
        ylab = substitute(paste(bold(('stem density')))),
        main = 'Treatment Effect on Stem Density',
        col = c(5:9))
### The space is having an effect on the diameter, the smaller the spacing,
### the greater the stem/stand density

## Treatment(spacing) effect on QMD
barplot(tapply (site1012$qmd,
                site1012$treatment,
                FUN = mean),
        xlab = substitute(paste(bold('treatment(spacing)'))),
        ylab = substitute(paste(bold('mean diameter(qmd)'))),
        main = 'treatment effect on QMD',
        col = c(10:13))
### Quadratic mean diameter increases with spacing effect according
### to the bar plot, the effect might diminish if spacing increases

## Treatment(spacing) effect on Periodic annual increment(PAI)
barplot(tapply(site1012$paiha,
               site1012$treatment,
               FUN = mean),
        xlab = substitute(paste(bold("Initial Spacing Treatment"))),
        ylab = substitute(paste(bold("PAI, m3/ha & year"))),
        col = c(2,13,15,20),
        main = "Effect of Spacing on PAI")

