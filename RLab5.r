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
# row 98 is having the data with an outlier
# On checking the original data written from the field, the result was 38

## changing data
pop2$dia <- ifelse(pop2$dia > 38, 3.9, pop2$dia)


# Fitting Linear function to the data
plot(poplar$cutw, poplar$vol)

# Linear model
lmpop <-lm(vol~cutw, data = poplar)

anova (lmpop)

# checking the distribution of the data.
hist(lmpop$residuals)

# Checking for homoscedactisity, 
# i.e the assumption for similar variance for a group being compared.
plot(lmpop$fitted.values, lmpop$residuals,
     xlab = 'Fitted Values',
     ylab = 'Residuals')
abline(c(0,0), col = 2)

#Making a qqplot (quantile-quantile plot) to check for normal distribution
qqnorm(lmpop$residuals)
qqline(lmpop$residuals, col = 'red')


# Now we can use the predicted values of the linear model as a function to estimat4e a value when we have the cutting weight available


# Let's use the function which we have
## To get the value for the function, we extract the intercept and slope
##intercept
intcpt <- coef(lmpop)[1]

## slope
slp <- coef(lmpop)[2]

## Creating a simulated value for cutting weights
simul.cutw <- c(0.1, 2, 4,5,6, 9, 10, 14, 16)

### Applying the model
simul.vol <- intcpt + (slp * simul.cutw)

###
simul <- data.frame(simul.cutw, simul.vol)

##Plotting the data the relationship
plot(simul$simul.cutw, simul$simul.vol,
     pch = 16,
     col = 'red',
     xlab = 'cutting width',
     ylab = 'volume',
     main = 'Cutting width vs volume relationship in Poplar')
lines(simul$simul.cutw, simul$simul.vol,
      lwd = 1.1,
      col = 'blue')
points(poplar$cutw, poplar$vol,
       pch = 2,
       cex = 0.6,
       col = 'black')

## SPruce Stand
## Data exploration
summary(spruce2)
## quick visualization
plot(spruce2$height, spruce2$dbh)

## Linear regression of Spruce
lmspruce <- lm(spruce2$height~spruce2$dbh)

anova(lmspruce)

## Checking the distribution of the residuals
hist(lmspruce$residuals)

# Checking for homoscedactisity, 
# i.e the assumption for similar variance for a group being compared.
plot(lmspruce$fitted.values, lmspruce$residuals,
     xlab = 'fitted values',
     ylab = 'residuals')
abline(c(0,0), col = 'red')
#checking the qq-plot to see if the quantiles fit

qqnorm(lmspruce$residuals)
qqline(lmspruce$residuals, col = 'green')

int_spr <-coef(lmspruce)[1]
slp_spr <- coef(lmspruce)[2]

## Testing the model
### Generating random numbers
sprce_dbh = sample(5:50, replace = TRUE)
sprce_height = int_spr + (slp_spr * sprce_dbh)
sprce <- data.frame(sprce_height, sprce_dbh)

plot(sprce$sprce_height, sprce_dbh,
     col = 'red')
lines(sprce$sprce_height, sprce_dbh,
      col = 'black')
points(spruce2$height, spruce2$dbh)
