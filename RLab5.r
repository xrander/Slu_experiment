## Experiment 5 - Poplar Cutting Experiment
## Question:
# - Investigate if the volume of 12 weeks seedlings is related to the initial cutting weight.
# - Make a regression of height and dbh,then determine if dbh can be used to predict height if height data is missing.

# Importing data
pop2 <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab 5/pop2.txt",
           header = T, sep = '\t', dec = '.', na.strings = 'NA', strip.white = T)
poplar <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab 5/poplar.txt",
                     header = T, sep = '\t', dec = '.', na.strings = 'NA', strip.white = T)
spruce2 <- read.table("C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab 5/spruce.txt",
                      header = T, sep = '\t', dec = '.', na.strings = 'NA', strip.white = T)
# Data description 
## pop2 and poplar
  # - block:1-5 
  # - cutw:total dryweight biomass (g)
  # - height:aboveground height (mm)
  # - dia:root collar diameter (mm)
  # - clone: A,B,C
  # - fert: 1=fertilized, 3= control

## Spruce
# The data is just a short list of sampled saplings of Norway spruce with dbh (mm) and height (dm).
# The data is taken from a short interval of heights and the relation between height and diameter is still linear.
# (compared to the range that you usually have in a stand)
# That is why, we in this lab are interested in testing whether we could use a linear model to fit a
# regression line of the relationship of height and dbh.


# Little exploration
str(pop2)
summary(pop2)
plot (pop2$dia)
## There is a wrong diameter value that is far from the average mean.
## Also, we have missing value in the height variable.

# Dealing with Missing Data
pop2[is.na(pop2$height), ]
## This shows we have one Na Value as it is true in row 109

pop2[complete.cases(pop2), ] ## can be written as 
pop2[complete.cases(pop2$height), ]## still the same.
## This shows only the complete cases or rows without missing values

## Let's say we found the value where we recorded the data we can simply replace it using the chunk below
pop2[is.na(pop$height), ] <- 331

### now we run the summary again to see if there's a missing data
is.na(pop2)
### The result is false all through

# Dealing with Outliers
# With outliers we need to careful as they can sometime be true and not errors.
# identifying the outlier
summary(pop2)
# we check the mean, quantiles, min, and max value to see where the outlier exist
#dia is having an outlier
pop2[(pop2$dia> 5),]
#row 98 is having the data with an outlier


