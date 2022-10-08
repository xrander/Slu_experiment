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

## Adjusting thinned Values.
### Usually in the year of harvest, we are supposed to have two volumes
### one is the volume before harvest and the volume harvest
### we have the volume harvested alone and we need to estimate the volume before harvest
### therefore we adjust the standing volume and we also change the age
### to ensure that this is the age before harvest

### Birch
birch_thinned <- subset(birch, harv_vol>0)
birch_thinned$stand_vol <- birch_thinned$stand_vol + birch_thinned$harv_vol
birch_thinned$age <- birch_thinned$age  - 0.01

### Spruce
spruce_thinned <- subset(spruce, harv_vol >0)
spruce_thinned$stand_vol <- spruce_thinned$stand_vol + spruce_thinned$harv_vol
spruce_thinned$age <- spruce_thinned$age  - 0.01

## The results are merge with the main data frame to give
## a more complete data

### Birch
birch_new <- merge(birch, birch_thinned, all = T)

### Spruce
spruce_new <- merge(spruce, spruce_thinned, all = T)


## Visualizing the new data frame

### Birch_new
#### Stand_vol total_vol comparison
plot(birch_new$age, birch_new$stand_vol,
     col = 'green',
     xlab = substitute(paste(bold('Age'))),
     ylab = substitute(paste(bold('Volume'))),
     main = 'Birch Stand Development, Thinning age included',
     pch = 19,
     type = 'b',
     xlim = c(0,140),
     ylim = c(0,1000)) +
  points (birch_new$age,birch_new$sumvol,
          pch = 19,
          col ='red') +
  lines(birch_new$age, birch_new$sumvol,
        col = 'red') +
  legend ("topleft",
          legend = c('standing volume', 'total_volume'),
          pch = 19,
          col =c('green', 'red'))
 
#### MAI and CAI
plot(birch_new$age, birch_new$mai,
     type = 'b',
     pch = 18,
     col = 'red',
     ylim = c(0,15),
     xlim = c(0, 140),
     main = 'MAI and CAI, Thinning age included',
     ylab = substitute(paste(bold('Growth'))),
     xlab = substitute(paste(bold('age')))) +
  points(birch_new$age, birch_new$cai,
         type = 'b',
         pch = 20,
         col = 'green') +
  legend("topleft",
         legend = c("MAI", "CAI"),
         pch = c(18, 20),
         col = c('red', 'green'))


### Spruce_new
#### Stand_vol total_vol comparison
plot(spruce_new$age, spruce_new$stand_vol,
     col = 'green',
     type = 'b',
     ylim = c(0,1350),
     main = 'Spruce Stand Development',
     pch = 15,
     xlab = substitute(paste(bold('Age (years)'))),
     ylab = substitute(paste(bold('Volume (m3)')))) +
  points(spruce_new$age,spruce_new$sumvol,
         col ='red',
         pch = 15,
         type = 'b') +
  points(spruce_new$age, spruce_new$harv_vol,
         col = 'purple',
         pch = 15,
         type = 'b') +
  points(spruce_new$age, spruce_new$mor_vol,
         col = 'orange',
         pch = 15,
         type = 'b')+
  legend("topleft",
         legend = c('standing volume', 'Cummulative sum of volumes',
                    'harvest_volume','Mortality volume'),
         pch = 15,
         col =c('green', 'red','purple','orange'))

#### MAI and CAI with thinning age included
plot(spruce_new$age, spruce_new$mai,
     type = 'b',
     pch = 18,
     col = 'red',
     ylim = c(0,18),
     xlim = c(0, 140),
     main = 'MAI and CAI, Thinning age include',
     ylab = substitute(paste(bold('m3 ha-1 yr-1'))),
     xlab = substitute(paste(bold('Age (years)')))) +
  points(spruce_new$age, spruce_new$cai,
         type = 'b',
         pch = 20,
         col = 'green') +
  legend("topleft",
         legend = c("MAI", "CAI"),
         pch = c(18, 20),
         col = c('red','green'))



# Some questions
## 1- How many thinnings were made for the spruce and birch stands
plot(spruce$age, spruce$stand_vol,
     col = 'red',
     type = 'b',
     pch = 19,
     main = 'Birch and Spruce stand development',
     xlab = substitute(paste(bold('Age(years)'))),
     ylab = substitute(paste(bold('volume (m3/ha)')))) + 
  points(birch$age, birch$stand_vol,
         type = 'b',
         pch = 17,
         col = 'blue') +
  legend ("bottomright",
          legend = c('Spruce', 'Birch'),
          pch = c(19,17),
          col = c('red', 'blue'))

###  Answer: Birch was thinned once and Spruce was thinned twice.


par(mar=c(5, 4, 4, 6) + 0.1)

## 2- Were the thinnings heavy or not?

plot(x = spruce_new$age, y = spruce_new$stdens,
     col = 'green',
     pch = 17,
     type = 'b',
     main = 'Thinning Intensity and Frequency',
     ylim = c(0, 2400),
     xlab = substitute(paste(bold('Age'))),
     ylab = substitute(paste(bold('Stand Density'))))
par(new = TRUE)
plot(x = spruce_new$age,y =spruce_new$stand_vol,
     col = 'red',
     type = 'b',
     pch = 15,
     axes = FALSE,
     ylab = " ",
     xlab = " ")
axis(side = 4, at = pretty(range(spruce_new$stand_vol)))
mtext(substitute(paste(bold('Stand Volume (m3)'))),side = 4, line = 4)
legend("right",
         legend = c('stand_density', 'standing volume(m3)'),
         pch = c(17, 15),
         col =c('green', 'red','purple','orange'))

### Answer: The first thinning is a heavy thinning, with more than 54% of the stand thinned out,
### the second and third thinnings were light thinning compared to the first thinning.
### The second and third thinnings have thinnings ranging from 35% to 40% of stands removed respectively.
### The chart from the code above gives a better explanation of the changes

## Included parameter to adjust chart margins
par(mar=c(5, 4, 4, 6) + 0.1)

## The cai is having NA, so this is changed to zero
spruce_new$cai[is.na(spruce_new$cai)] <- 0

## 3- What happened to the CAI after the first thinning
plot(spruce_new$age, spruce_new$stand_vol,
     col = 'blue',
     type = 'b',
     pch = 18,
     main = 'Stand volume showing thinnings with CAI of Spruce',
     xlab = substitute(paste(bold('Age (years)'))),
     ylab = substitute(paste(bold('Stand Volume(m3)'))))
par(new = TRUE)
plot (spruce_new$age, spruce_new$cai,
        col = 'red',
        pch = 17,
        axes = FALSE, 
        type = 'b',
        xlab = "",
        ylab = "")
axis(side = 4, at = pretty(range(spruce_new$cai)))
mtext(substitute(paste(bold('CAI (me ha-1 yr-1)'))), side =4, line = 3)
legend("bottomright",
          legend = c('stand_volume(m3)', 'CAI'),
          pch = c(18, 17),
          col = c('blue', 'red'))

### Answer: the cai reduces after thinning



## Some Interesting graphics

### - Stand density of both spruce and birch.It shows thinning regimes but doesn't show reduction due to mortality
### of the trees.
plot(spruce_new$age, spruce_new$stdens,
     col = 'blue',
     type = 'b',
     pch = 18,
     main = 'Spruce and Birch Stand density ',
     xlab = substitute(paste(bold('Age'))),
     ylab = substitute(paste(bold('No of Trees'))))
points (birch_new$age, birch_new$stdens,
          col = 'red',
          pch = 17,
          type = 'b')
legend("topright",
         legend = c('Spruce', 'Birch'),
         pch = c(18, 17),
         col = c('blue', 'red'))

### - Volume lost due to mortality in both Spruce and Birch'.
plot(spruce_new$age, spruce_new$mor_vol,
     col = 'blue',
     type = 'b',
     pch = 18,
     main = 'Spruce and Birch mortality ',
     xlab = substitute(paste(bold('Age (years)'))),
     ylab = substitute(paste(bold('Volume of dead trees (m3) '))))
points (birch_new$age, birch_new$mor_vol,
          col = 'red',
          pch = 17,
          type = 'b')
legend("topleft",
         legend = c('Spruce', 'Birch'),
         pch = c(18, 17),
         col = c('blue', 'red'))

### - Comparison of the species growth using the mai
plot(spruce_new$age, spruce_new$mai,
     col = 'blue',
     type = 'b',
     pch = 18,
     main = 'Spruce and Birch Development overtime',
     xlab = substitute(paste(bold('Age (years)'))),
     ylab = substitute(paste(bold('MAI (m3 ha-1yr-1)'))))
points (birch_new$age, birch_new$mai,
          col = 'red',
          pch = 17,
          type = 'b')
legend("topright",
         legend = c('Spruce', 'Birch'),
         pch = c(18, 17),
         col = c('blue', 'red'))

