# Sverige LantbruksUniversitet (SLU) Permanent Forest Experiments

![](https://i0.wp.com/odlandestadsbasarer.se/wp-content/uploads/2017/09/SLU-2.jpg?ssl=1)

## Brief Introduction

This is an analysis done as part of the **Sustainable Forestry in
Southern Sweden** course in the Euroforester program at the SLU campus
at the south of Sweden in
[Alnarp](https://www.slu.se/en/departments/southern-swedish-forest-research-centre/).

The data used for this analysis were provided mainly by the research
centre at Tonnersjoheden and have been [uploaded
here](https://github.com/xrander/SLU-Plantation-Experimentation/tree/master/Data).

**Data Exploration** Package used is the doBy library doBy, dplyr,
lattice, ggplot, TukeyC To install the package run the commandlike this
`install.packages('doBy')` as an example.

``` r
library(doBy)
library(dplyr)
library(lattice)
library(ggplot2)
library(TukeyC)
```

## Clone Performance Test

The first data used is the ‘popdata’ which is accessible at
[popdata.txt](https://raw.githubusercontent.com/xrander/SLU-Plantation-Experimentation/master/Data/popdata.txt).
It is an experiment to see the performance of clones. This is a part of
a much larger experiment and only a part is used for the first analysis.

``` r
pop <- read.table('https://raw.githubusercontent.com/xrander/SLU-Plantation-Experimentation/master/Data/Lab1/popdata.txt', header = T)
head(pop)
```

    ##   block cutw height dia clone fert
    ## 1     1  2.4     71 0.6     A    3
    ## 2     1  0.7     67 1.4     A    3
    ## 3     1  6.5    211 3.5     A    3
    ## 4     1  1.1     69 1.0     A    3
    ## 5     2  2.0    116 1.4     A    3
    ## 6     2  4.9    123 3.2     A    3

**Data description**

-   block: experimental block

-   cutw: cultivar width

-   height: height of the plant

-   dia = diameter

-   clone: clone class

-   fert: Fertilized or not(1 = fertilized and 3 = control)

Creating a column to give name to the values of the fert

``` r
pop$fert_value <- ifelse(pop$fert==1, 'fertilized', 'control')
head(pop)
```

    ##   block cutw height dia clone fert fert_value
    ## 1     1  2.4     71 0.6     A    3    control
    ## 2     1  0.7     67 1.4     A    3    control
    ## 3     1  6.5    211 3.5     A    3    control
    ## 4     1  1.1     69 1.0     A    3    control
    ## 5     2  2.0    116 1.4     A    3    control
    ## 6     2  4.9    123 3.2     A    3    control

Height diameter relationship of the control and fertilized seedlings

``` r
plot(pop$dia, pop$height,
     text(x=01.5, y=450,labels = "Growth rate of fertilized and unfertilized seedlings",col = "red", cex = 0.7),
     xlim = c(0,5.5), ylim = c(0, 600),xlab = 'diameter(mm)',
     ylab = 'Height(mm)', main = "Height vs Diameter",
     pch = c(16,17), col = c('red', 'blue'))
legend("topleft",
       legend = c("Control","Fertilized"),
       pch= c(16,17),
       col = c('red','blue'))
```

![](Readme_files/figure-markdown_github/unnamed-chunk-4-1.png)

Deriving the height to diameter ratio

``` r
pop$hd <- pop$height/pop$dia
```

The mean height and diameter of the control and fertilized of the
different clones is given as

``` r
pop_summary <- summaryBy(height + dia ~ fert + clone, data = pop, FUN=mean)
pop_summary
```

    ##   fert clone height.mean dia.mean
    ## 1    1     A    325.5926 3.281481
    ## 2    1     B    361.3243 3.418919
    ## 3    1     C    364.7941 3.788235
    ## 4    3     A    118.7917 2.070833
    ## 5    3     B    165.8387 2.293548
    ## 6    3     C    124.7222 2.108333

The mean height to diameter of the different treatment is given as

``` r
pop_summary$h_d <- pop_summary$height.mean/pop_summary$dia.mean
names(pop_summary)[5] <- 'height_dia_ratio'
barplot(pop_summary$height_dia_ratio,
        names.arg = pop_summary$clone,
        col = pop_summary$fert,
        xlab = 'clone',
        ylab = 'height_diameter_ratio',
        main = 'Seedling Clone Performance',
        legend = TRUE)
legend('topright',
       legend = c('fertilized', b = 'control'),
       col = c('black', 'green'),
       pch = 18)
```

![](Readme_files/figure-markdown_github/unnamed-chunk-7-1.png) It is
evident that the fertilized are performing than the control, with the B
clone class being the highest performing either fertilized or not.

------------------------------------------------------------------------

## Spacing Effect on Growth of Scot Pine

This is a long term experiment to test the effect of four different
spacing treatments 1m, 1.5m, 2m, and 2.5m across eight plots. The
experiment is designed such that two plots are assigned a treatment, in
this case the plots. The plots are also of varying sizes.

### The Design of the experiment using R

``` r
plot = c(11:14, 21:24)
## plot here denotes the plots number or names

areaha = c(0.04, 0.0324, 0.0288, 0.0288, 0.04, 0.0324, 0.0288, 0.0288)
## areaha is the area per hectare of each plots

treatment = c(2.5, 2, 1.5 ,1)
### **nb**: 2.5 implies 2.5*2.5 and 2 implies 2*2 and so on.

### creating the data frame for the plots with their properties
exp1012 <- data.frame(plot, areaha, treatment)

### importing data
dbh1012 <- read.table("https://raw.githubusercontent.com/xrander/SLU-Plantation-Experimentation/master/Data/Lab2/dbhlist_exp1012.txt",
           header = T, sep = "\t", na.strings = "NA", dec = ".", strip.white = TRUE)
head(dbh1012)
```

    ##   plot nr  d1  d2
    ## 1   11  2 152 159
    ## 2   11  3 134 126
    ## 3   11  4 156 171
    ## 4   11  6 158 160
    ## 5   11  7  97  90
    ## 6   11  8 154 159

The data used for this analysis can be obtained here
[experiment_data](https://raw.githubusercontent.com/xrander/SLU-Plantation-Experimentation/master/Data/Lab2/dbhlist_exp1012.txt).

**Data Description**

-   plot: the plot number

-   nr: the tree number measured

-   d1: cross caliper diameter measurement 1

-   d2: cross caliper diameter measurement 2, 90 degrees to measurement
    1

------------------------------------------------------------------------

### Analyzing the data

The data will be investigated to see if there’s anything strange with
the data

``` r
plot(dbh1012$d1, dbh1012$d2,
     xlab = 'd1',
     ylab = 'd2',
     col = c('purple', 'green'),
     pch = c(10,21))
legend('topleft',
       legend = c('d1', 'd2'),
       col = c('purple', 'green'),
       pch = c(10,21))
```

![](Readme_files/figure-markdown_github/unnamed-chunk-9-1.png) The data
seems to be alright, we can now proceed with the analysis

#### Basal Area Estimation

First we estimate the average of the two diameters, we also square the
result to get values needed to estimate the quadratic mean then we
calculate the basal area

``` r
## average of the two diameter
dbh1012$dm <- (dbh1012$d1 + dbh1012$d2)/2

## squared valued to be used for quadratic mean estimation
dbh1012$dd <- dbh1012$dm^2

## basal area estimation
dbh1012$ba <- pi * ((dbh1012$dm/2)^2)
```

-   dm: mean of both diameter

-   dd: diameter raised to the power 2

-   ba: basal area

The values will be summarized to give a clear value for each plots then
combined with the plot characteristics for further analysis

``` r
plotba <-summaryBy(ba~plot, data = dbh1012, FUN = sum)

site1012 <- merge(exp1012, plotba, all = T)

site1012
```

    ##   plot areaha treatment    ba.sum
    ## 1   11 0.0400       2.5  993838.7
    ## 2   12 0.0324       2.0 1025256.4
    ## 3   13 0.0288       1.5  968972.4
    ## 4   14 0.0288       1.0  963524.9
    ## 5   21 0.0400       2.5 1043748.0
    ## 6   22 0.0324       2.0 1045694.8
    ## 7   23 0.0288       1.5  953649.5
    ## 8   24 0.0288       1.0  903224.4

With this table we can estimate the basal area per hectare and the basal
area per treatment

``` r
## Estimating the basal area per hectare
<<<<<<< HEAD
site1012$baha <- (1/site1012$areaha) * site1012$ba.sum

##converting to basal area per hectare from mm2 to m2
site1012$baham2 <- round((site1012$baha/1000000),2)
=======
site1012$baha <- 1/site1012$areaha * site1012$ba.sum

##converting to basal area per hectare from mm2 to m2
site1012$baham2 <- round((site1012$baha/1000000), 2)
>>>>>>> 49653b069e39fe7089f7c89fde50bbe9af7762cf

## Estimating the basal area per treatment
trtmean1012 <- round(summaryBy(baham2~treatment,
                         data = site1012, FUN = mean), 1)
site1012
```

<<<<<<< HEAD
    ##   plot areaha treatment    ba.sum     baha baham2
    ## 1   11 0.0400       2.5  993838.7 24845968  24.85
    ## 2   12 0.0324       2.0 1025256.4 31643716  31.64
    ## 3   13 0.0288       1.5  968972.4 33644876  33.64
    ## 4   14 0.0288       1.0  963524.9 33455726  33.46
    ## 5   21 0.0400       2.5 1043748.0 26093700  26.09
    ## 6   22 0.0324       2.0 1045694.8 32274532  32.27
    ## 7   23 0.0288       1.5  953649.5 33112830  33.11
    ## 8   24 0.0288       1.0  903224.4 31361958  31.36
=======
    ##   plot areaha treatment      ba.sum         baha   baham2
    ## 1   11 0.0400       2.5 23046044774 576151119349 576151.1
    ## 2   12 0.0324       2.0 21173303890 653497033649 653497.0
    ## 3   13 0.0288       1.5 17425557531 605054080930 605054.1
    ## 4   14 0.0288       1.0 14825571265 514776780045 514776.8
    ## 5   21 0.0400       2.5 25523624363 638090609077 638090.6
    ## 6   22 0.0324       2.0 20460102641 631484649423 631484.7
    ## 7   23 0.0288       1.5 18077494007 627690764122 627690.8
    ## 8   24 0.0288       1.0 12361439129 429216636420 429216.6
>>>>>>> 49653b069e39fe7089f7c89fde50bbe9af7762cf

``` r
barplot(site1012$baham2,
        names.arg = site1012$plot,
<<<<<<< HEAD
        col = c(2,3,4,5),
        ylab = 'Basal Aream2/ha',
=======
        ylim = c(0,40),
        col = c(2,3,4,5),
        ylab = 'm2/ha',
>>>>>>> 49653b069e39fe7089f7c89fde50bbe9af7762cf
        xlab = 'plot',
        main = 'Basal area across sites')
legend('right',
       legend = c('11','12','13','14'),
       pch = 18,
       col = c(2,3,4,5))
```

![](Readme_files/figure-markdown_github/unnamed-chunk-13-1.png)

<<<<<<< HEAD
#### Stand Density Estimation

=======
>>>>>>> 49653b069e39fe7089f7c89fde50bbe9af7762cf
Given the figures, we need extra information to be able to estimate the
density of the stand. To do that, we estimate the plot density then
extrapolate to a hectare

``` r
## To derive the plot density
plotdens <- summaryBy(nr~plot, data = dbh1012, FUN = length)


## we merge the plot density to the site information
site1012 <- merge (site1012, plotdens, all = T)

<<<<<<< HEAD

## After this we can get the density per hectare
site1012$dens_ha <- round((site1012$nr.length * (1/site1012$areaha)), 1)
=======
##renaming plot density column 
names(site1012)[7] <- 'trees_per_plot'

## After this we can get the density per hectare
site1012$dens_ha <- round((site1012$trees_per_plot * (1/site1012$areaha)), 1)
>>>>>>> 49653b069e39fe7089f7c89fde50bbe9af7762cf

barplot(site1012$dens_ha,
        names.arg = site1012$treatment,
        xlab = "density",
        ylab = "treatment",
        main = "Treatment and Density relationship",
        col = c(2,3,4,5))
```

<<<<<<< HEAD
![](Readme_files/figure-markdown_github/unnamed-chunk-14-1.png) \####
Deriving Arithmetic Mean Diameter(AMD) and Quadratic Mean Diameter(QMD)

``` r
plotdbh <- summaryBy(dm+dd~plot, data = dbh1012, FUN = c(mean, sum))

## Merging values to site1012
site1012 <- merge(site1012, plotdbh, all = T)

##Converting dm.mean and dm.sum from millimeters to centimeter
site1012$dm.mean_cm <- site1012$dm.mean/10

site1012$dm.sum_cm <- site1012$dm.sum/10

## "site1012$dm.mean_cm" is synonymous to AMD therefore
site1012$amd = site1012$dm.mean_cm

#converting dd.mean and dd.sum from mm^2 to cm ^2
site1012$dd.mean_cm <- site1012$dd.mean/100

site1012$dd.sum_cm <- site1012$dd.sum/100

#TO get QMD
site1012$qmd <- sqrt(site1012$dd.sum/site1012$nr.length)/10
```

#### Basal area stand density relationship

``` r
site1012[,c("baham2", "dens_ha")]
```

    ##   baham2 dens_ha
    ## 1  24.85  1600.0
    ## 2  31.64  2222.2
    ## 3  33.64  2847.2
    ## 4  33.46  3750.0
    ## 5  26.09  1550.0
    ## 6  32.27  2469.1
    ## 7  33.11  2604.2
    ## 8  31.36  3784.7

``` r
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
```

![](Readme_files/figure-markdown_github/unnamed-chunk-17-1.png) \###
Regression and Correlation
=======
![](Readme_files/figure-markdown_github/unnamed-chunk-14-1.png)
>>>>>>> 49653b069e39fe7089f7c89fde50bbe9af7762cf
