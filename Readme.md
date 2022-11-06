# Sverige LantbruksUniversitet (SLU) Permanent Forest Experiments

![](https://i0.wp.com/odlandestadsbasarer.se/wp-content/uploads/2017/09/SLU-2.jpg?ssl=1)
<br> *slu.se* <br>

Forestry is a big business in Sweden. The country is mostly dominated by
forests and is vital for the nations economy. Sweden is one of the top
supplier of wood and wood related products in the world. A popular that
can be related to is the Ikea, an organization that designs and sells
RTA(ready to assemble) and DIY (Do it Yourself) furniture, home
accessories, kitchen appliances and other home services.

Sverige LantbruksUniversitet (SLU) or Swedish Agricultural University is
a stand out research university in Sweden. This university is not under
the Ministry of Education like other University in Sweden, but under the
Ministry Economic Affairs. They are charged with executing quality
research on agricultural, veterinary, envionmental/landscape and forest
related issues.

They have operations in more than 25 locations in the country spanning
from the North to the South of the country. They have four major
faculties located in Alnarp (1), Umeå(1) abd Uppsala(2). In addition to
this they have several educational research stations.

# Brief Introduction of the Analysis

![](http://www.chemical-ecology.net/alg56b.gif)

<br> *Map of Alnarp. Source: chemical-ecology.net* <br> This is an
analysis done as part of the **Sustainable Forestry in Southern Sweden**
course in the Euroforester program at the SLU campus located in Southern
Sweden in
[Alnarp](https://www.slu.se/en/departments/southern-swedish-forest-research-centre/).

## Tönnersjöheden

Tönnersjöheden is one of the oldest experiment forest established in
Sweden at around 1923 having more than 1000 hectare.
![](https://student.slu.se/globalassets/ew/org/inst/ssv/fras/phd-kurs-t-heden-nov-18/t4-i-hosttsolen.jpg)
Various experimental parks provide land for different types of field
trials. Major experiments are within the following subjects:

-   Genetic diversity and breeding

-   Regeneration and establishment

-   Stand treatment, growth and yield

-   Ecosystem, soil and water conditions

-   Wood biomass for energy production

-   Peat land forestry.

The research station is usually in cooperation with departments at SLU
and other universities and research institutes in Sweden.

Some of the data used for this analysis were provided mainly by this
research station and have been [uploaded
here](https://github.com/xrander/SLU-Plantation-Experimentation/tree/master/Data).

I will be taking us through the analysis journey. Terms relating to
forestry will be explained, while syntax of R used won’t be explained.

**Tools and Packages Used** RStudio was mainly used for the analysis of
the data. Libraries used were mainly doBy, lattice, ggplot, TukeyC, and
Car. To install the package run the command like this
`install.packages('package name')` as an example
`install.packages('dplyr')`.

====NB: For most part of the visualization base-r and lattice library
were used, ggplot was used at times====

*Loading the libraries*

``` r
library(doBy)
library(dplyr)
library(lattice)
library(ggplot2)
library(car)
library(data.table)
library(TukeyC)
```

# Experiments

Various experiments ranging from regeneration experiment to thinning
experiment were carried out and date was analyzed to see the effect of
various treatments (stand tending operations) at different stand
development stage. The experiments are:

-   **[Fertilization Experiment](fertilizer.md)**:
    ![](https://www.mdpi.com/forests/forests-12-00298/article_deploy/html/images/forests-12-00298-g001.png)

<br> *Total deposition of inorganic N (NO3-N + NH4-N) to spruce forest
2017, according to the MATCH model \[37\]. The black dots are the three
modeling sites: Högbränna in Northern Sweden, Södra Averstad in Central
Sweden and Västra Torup in Southern Sweden. Source: Lucander et
al. (2021)* <br> Fertilization of plantations in Sweden is area
specific. In the Northern part of Sweden the use of fertilizer is
common, while in the Southern region its use is prohibited. The Southern
part is having rich soil and have no need to fertilize the soil. Swedish
forest fertilization mainly involves the application of Nitrogen which
is normally the limiting nutrient for high stand growth.

This experiment is set to see the effect of fertilization frequency on
the growth of a stand. The experiment is a young Norway Spruce stands
which was established with 5 blocks having randomly distributed
treatments in 0.1 ha plots. The treatments are with 3 different
intensities in fertilization

    - F1:   Fertilized every year

    - F2:   Fertilized every second year

    - F3:   Fertilized every third year

    -  C:   Control without Fertilzation.

-   **[Cuttings Experiment](cuttings.md)**:

     <br>
    
    ![](https://www.sgaonline.org.au/images/pics/hardwoodcuttings.jpg)
    
    <br>
    *source: Sustainable Gardening Australia*
    <br>
    Cuttings are one of the ways to propagate tree seedlings, another is through seeding.
    The later is the common in Sweden’s nursery. This experiment test the effect of fertilization on different clones of poplar cuttings.

-   **[Thinning Frequency and Intensity Experiment](pct.md)**: Basic
    data exploration and data visualization on a stand after being
    thinned giving insight on the influence of thinning frequency and/or
    intensity on some growth yield parameters.

-   **[Clone Trial Experiment](clone.md)**: Experiment to test the
    effect of fertilization on the performance of different clones.

-   **[Growth Experiment](growth_experiment.md)**: Estimation of some
    forestry growth parameters.

-   **[Spacing Experiment](spacing_experiment.md)**: Experiment to test
    the effect of spacing density on the diameter and basal of a stand.

-   **[Thinning Experiment on Scots Pine](Thinning_experiment.md)**:
    <br>
    
    ![](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT5O_EthfXgRFnxQrU2yVt2ESZcXRCQrvmaVg&usqp=CAU)
    
    <br>
    
    An experiment to evaluate the effect of three different thinning regimes on the total volume produced in a stand.

-   **[Mixed Forest Experiment](mixed_forest.md)**: Volume estimation
    between pure monoculture and a mixed forest of 80:20 of monoculture
    and 50:50 of monoculture

-   **[Site Preparation Experiment](lodgepole.md)**: A site preparation
    experiment to evaluate the growth and survival of lodgepole pine
    under scarified and unscarified site. <br>

[Portfolio Page](https://xrander.github.io)
