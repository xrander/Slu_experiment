# Sverige LantbruksUniversitet (SLU) Permanent Forest Experiments

![](https://i0.wp.com/odlandestadsbasarer.se/wp-content/uploads/2017/09/SLU-2.jpg?ssl=1)

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

This is an analysis done as part of the **Sustainable Forestry in
Southern Sweden** course in the Euroforester program at the SLU campus
located in Southern Sweden in
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

-   *[Fertilization Experiment](fertilizer.md)*

-   *[Cuttings Experiment](cutting.md)*

-   *[Precommercial Thinning and Thinning Experiment](pct.md)*

-   *[Clone Trial Experiment](clone.md)*

-   *[Growth Experiment](growth_experiment,md)*

-   *[Spacing Experiment](spacing_experiment.md)*

-   *[Thinning Experiment](Thinning_experiment.md)*

-   *[Mixed Forest Experiment](mixed_forest.md)*

[Back to home page](https://xrander.github.io)
