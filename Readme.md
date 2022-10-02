# Sverige LantbruksUniversitet (SLU) Permanent Forest Experiments

![SLU](https://i0.wp.com/odlandestadsbasarer.se/wp-content/uploads/2017/09/SLU-2.jpg?ssl=1)

## Brief Introduction

This is an analysis done as part of the **Sustainable Forestry in
Southern Sweden** course in the Euroforester program at the SLU campus
at the south of Sweden in
[Alnarp](https://www.slu.se/en/departments/southern-swedish-forest-research-centre/).

The data used for this analysis were provided mainly by the research
centre at Tonnersjoheden and have been [uploaded
here](https://github.com/xrander/SLU-Plantation-Experimentation/tree/master/Data).

**Data Exploration** Package used is the doBy library To install the
package run the command `install.packages('doBy')`

``` r
library(doBy)
library(dplyr)
```

## Clone Performance Test

The first data used is the ‘popdata’ which is accessible at
[popdata.txt](https://raw.githubusercontent.com/xrander/SLU-Plantation-Experimentation/master/Data/popdata.txt)

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

## Spacing Effect on Growth on Scot Pine

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

-   nr: the tree number

-   d1: diameter at breast height 1

-   d2: diameter at breast height 2

-   dm: mean of both diameter

-   dd: diameter raise to power 2

-   ba: basal area
