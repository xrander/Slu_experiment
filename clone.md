---
title: "Clone Performance Test - Experiment I"
author: "Olamide Adu"
date: "2023-03-25"
output:
   prettydoc::html_pretty:
    theme: Cayman
    toc: true
    toc_float: false
    highlight: github
    code_theme: zenburn
    keep_md: true
---

# Clone Performance Test - Exp I

<img src= "https://ichef.bbci.co.uk/news/304/mcs/media/images/48772000/jpg/_48772031_aspen.jpg" height = '200' width = '250'>

<br> *source: BBC* <br>

The first experiment will be a clone performance experiment. The data used is the 'popdata' which is accessible [here](https://raw.githubusercontent.com/xrander/SLU-Plantation-Experimentation/master/Data/popdata.txt).

This analysis will seek to answer the question: - Is the treatment having an influence - Which clone is performing best


``` r
library(doBy)
library(ggplot2)
```

*Importing the data*


``` r
pop <- read.table('Data/Lab1/popdata.txt', header = T)

head(pop)
```

```
##   block cutw height dia clone fert
## 1     1  2.4     71 0.6     A    3
## 2     1  0.7     67 1.4     A    3
## 3     1  6.5    211 3.5     A    3
## 4     1  1.1     69 1.0     A    3
## 5     2  2.0    116 1.4     A    3
## 6     2  4.9    123 3.2     A    3
```

**Data description**

-   block: experimental block

-   cutw: cultivar weight

-   height: height of the plant

-   dia = diameter

-   clone: clone class

-   fert: Fertilized or not(1 = fertilized and 3 = control)

We can create a column now and assign the names to the different values.

*Creating a column to give name to the values of the fert*


``` r
pop$fert_value <- ifelse(pop$fert==1, 'fertilized', 'control')
pop$fert <- as.factor(pop$fert)
head(pop)
```

```
##   block cutw height dia clone fert fert_value
## 1     1  2.4     71 0.6     A    3    control
## 2     1  0.7     67 1.4     A    3    control
## 3     1  6.5    211 3.5     A    3    control
## 4     1  1.1     69 1.0     A    3    control
## 5     2  2.0    116 1.4     A    3    control
## 6     2  4.9    123 3.2     A    3    control
```

## Questions

```         
-   Plot the height diameter relationship of different treatment of seedlings

- estimate the index of slenderness of the stand

-   Plot the performance of the seedlings and state which of the clones performing the best
```

## Height diameter relationship of the control and fertilized seedlings

The first step of the analysis is visualize the effect of treatment on diameter and height


``` r
ggplot(pop, aes(dia, height, col = fert_value)) +
  geom_point()+ 
  labs(title = 'Height vs Diameter',
       col = 'Treatment',
       x = 'diameter (mm)',
       y = 'height (mm)')+
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        legend.title =  element_text(face = 'bold'))
```

![](clone_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Index of Slenderness

The height diameter ratio or index of slenderness is an important measure as it can be used to evaluate a tree stability. To read more on height diameter relationship click [here](<https://www.mdpi.com/1999-4907/10/1/70/htm#>:\~:text=Height%2Dto%2Ddiameter%20ratio%20(,of%20tree%20and%20stand%20stability.). The formula for HDr is given below

$$HDr = height/diameter$$
Where HDr =height diameter ratio


``` r
pop$hd <- pop$height/pop$dia
```

Given the data we can estimate the average height and diameter for the clones and treatments


``` r
pop_summary <- summaryBy(height + dia ~ fert + clone,
                         data = pop, FUN=mean)
head(pop_summary)
```

```
##   fert clone height.mean dia.mean
## 1    1     A    325.5926 3.281481
## 2    1     B    361.3243 3.418919
## 3    1     C    364.7941 3.788235
## 4    3     A    118.7917 2.070833
## 5    3     B    165.8387 2.293548
## 6    3     C    124.7222 2.108333
```


The mean height to diameter of the different treatment as given with the formula above can be estimated.


``` r
ggplot(pop_summary, aes(clone, height.mean, fill = fert))+
  geom_bar(stat = 'identity',
           position = 'dodge')+
  labs(title = 'Seedlings clone performance',
       y = 'height diameter ratio',
       x = 'clone')+
  scale_fill_discrete(name = 'Treatment', labels = c('Control', 'Fertilized'))+
  theme(plot.title = element_text(face = 'bold'),
        axis.title = element_text(face = 'bold'),
        legend.title = element_text(face = 'bold'))
```

![](clone_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
It is evident that the fertilized are performing than the control, with the B clone class being the highest performing either fertilized or not.

[Thinning Intensity and Frequency Experiment](pct.html) <br>

[Homepage](index.html) <br>

[Growth Measure Test](growth_experiment.html) <br>

[Back to portfolio](https://olamideadu.com)
