# This is a different data set of two species
## mainly spruce and birch

### Description of data
'site = the species site, 1 = spruce and 6 = birch
age = age of the stand
stdens = stand density
ba = basal area of stand
stand_vol	= volume of stand
harv_vol = harvested volume
mor_vol = volume of dead trees at particular age or mortality volume
spruce_dgv = quadratic mean diameter
birch_dgv = quadratic mean diameter'

## data.table will be used for this task

library(data.table)
lab4mai <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab 4/lab4mai (2).txt",
                      header = T, sep = "\t", na.strings = "NA", dec = ",", strip.white = T)

str(lab4mai)
## After checking the structure of the data, all the variables are
## in character format except age and site.
## all values will be converted to integer.

lab4mai <- as.data.frame(sapply(lab4mai, as.numeric))

## checking the new structure
str(lab4mai)



## Total volume estimation
lab4mai$tot_vol <-lab4mai$stand_vol +
  lab4mai$harv_vol + lab4mai$mor_vol

birch <- subset(lab4mai, site==6)
spruce <- subset(lab4mai, site==1)

## Stand density age relationship
birch <- as.data.frame(sapply(birch, as.numeric))

### Birch
plot(birch$age, birch$stdens, type = "l",
     ylim = c(0,2500),
     col = 'green',
     xlab = 'age',
     ylab = 'Stand_density',
     main = 'Stand density and height relationship'
     ) +
  points(birch$age, birch$stdens,
         col = 'black')

### Spruce
plot(spruce$age, spruce$stdens, type = "l",
     ylim = c(0,2500),
     col = 'green',
     xlab = 'age',
     ylab = 'Stand_density',
     main = 'Stand density and height relationship'
) +
  points(spruce$age, spruce$stdens,
         col = 'black')

## Next the accumulated volume production, yield and growth functions will be estimated
## growth functions are CAI (current annual increment), MAI (mean annual increment)

### Birch
birch$sum_harv <- cumsum(birch$harv_vol)
birch$sum_mor <- cumsum(birch$mor_vol)

#### total yield or volume for birch
birch$sumvol <- birch$sum_harv + birch$sum_mor + birch$stand_vol

### spruce
spruce$sum_harv <- cumsum(spruce$harv_vol)
spruce$sum_mor <- cumsum(spruce$mor_vol)



#### total yield or volume for spruce
spruce$sumvol <- spruce$sum_harv + spruce$sum_mor + spruce$stand_vol

## visualizing the stand_development and the standing volume

### Birch
plot(birch$age, birch$stand_vol,
     col = 'green',
     xlab = substitute(paste(bold('Age'))),
     ylab = substitute(paste(bold('Volume'))),
     main = 'Birch Stand Development',
     pch = 19,
     type = 'b',
     xlim = c(0,140),
     ylim = c(0,1000)) +
  points (birch$age,birch$sumvol,
          pch = 19,
         col ='red') +
  lines(birch$age, birch$sumvol,
        col = 'red') +
  legend ("topleft",
         legend = c('standing volume', 'total_volume'),
         pch = 19,
         col =c('green', 'red'))

### Spruce
plot(spruce$age, spruce$stand_vol,
     col = 'green',
     type = 'l',
     main = 'Spruce Stand Development',
     xlab = substitute(paste(bold(Age))),
     ylab = substitute(paste(bold(Volume)))) +
  points(spruce$age,spruce$stand_vol,
         col = 'green',
         pch = 15)+
  points(spruce$age,spruce$sumvol,
         col ='red',
         pch = 15) +
  points(spruce$age, spruce$harv_vol,
         col = 'purple',
         pch = 15) +
  points(spruce$age, spruce$mor_vol,
         col = 'orange',
         pch = 15)+
  legend("topleft",
         legend = c('standing volume', 'total_volume',
                    'harvest_volume','Mortality volume'),
         pch = 15,
         col =c('green', 'red','purple','orange'))

## Estimating CAI and MAI

### The CAI is calculated by subtracting the
### current volume minus the previously recorded volume(usually 5
### to 10 years ago - depending on research frequency, but 5 year is used here)
### then divided by the year difference or period(5 in this case)

### To estimate the difference, it would be nice to
### have the previous measurement on the current row.

## Birch CAI
birch$last_vol <- shift(birch$stand_vol)
birch$cai <- (birch$stand_vol + birch$harv_vol + 
  birch$mor_vol - birch$last_vol)/5

##Birch MAI
birch$mai <- birch$sumvol/birch$age

## Spruce CAI
spruce$last_vol <- shift(spruce$stand_vol)
spruce$cai <- (spruce$stand_vol + spruce$harv_vol + 
                spruce$mor_vol - spruce$last_vol)/5

## Spruce MAI
spruce$mai <- spruce$sumvol/spruce$age


## Visuals of MAI and CAI
### Birch
plot(birch$age, birch$mai,
     type = 'b',
     pch = 18,
     col = 'red',
     ylim = c(0,15),
     xlim = c(0, 140),
     main = 'MAI and CAI',
     ylab = substitute(paste(bold('Growth'))),
     xlab = substitute(paste(bold('age')))) +
  points(birch$age, birch$cai,
        type = 'b',
        pch = 20,
        col = 'green') +
  legend("topleft",
         legend = c("MAI", "CAI"),
         pch = c(18, 20),
         col = c('red', 'green'))

###Spruce
plot(spruce$age, spruce$mai,
     type = 'b',
     pch = 18,
     col = 'red',
     ylim = c(0,20),
     xlim = c(0, 140),
     main = 'MAI and CAI',
     ylab = substitute(paste(bold('Growth'))),
     xlab = substitute(paste(bold('age')))) +
  points(spruce$age, spruce$cai,
         type = 'b',
         pch = 20,
         col = 'green') +
  legend("topleft",
         legend = c("MAI", "CAI"),
         pch = c(18, 20),
         col = c('red', 'green'))
