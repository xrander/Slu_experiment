# Experiment with Fertilization Regime in Norway Spruce

## An experiment in young Norway Spruce stands were established with 5 blocks
## with randomly distributed treatments in 0.1 ha plots
## was established with 5 blocks with randomly distributed treatments
## in 0.1 ha plots. The treatments were 3 different intensities in fertilization;
## F1: every year, F2: every second year and F3 every third year +C: control without fertilization. 
## The amount of applied nutrients over time was calculated to be
## more or less the same in F1,F2 and F3.

## Library used is the lattice library, tukeyc and car
### installing the packages
install.packages('lattice')
install.packages('TukeyC')
install.packages('car')

## Loading packages
library(lattice)
library(TukeyC)
library(car)

## Reading the data to be used
expfert <- read.table('C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab6/expfert.txt',
                      header = T, sep = "\t", na.strings = "NA",
                      dec = ",", strip.white = T)

poptr <- read.table('C:/Users/aduol/Documents/SLU/RSust/SLU project/Data/Lab6/poplartreatment.txt',
                    header = T, sep = "\t", na.strings = "NA",
                    dec = ",", strip.white = T)

## Investigating the case
head(expfert)
summary(expfert)
str(expfert)

### Changing data types
expfert$block <- factor(expfert$block)
expfert$revision <- factor (expfert$revision)
expfert$volume <- as.numeric(expfert$volume)
expfert$domheight <- as.numeric(expfert$domheight)
expfert$CAI <- as.numeric(expfert$CAI)

ftable(expfert$block, expfert$treatment, expfert$revision)

## Making a plot of stand age in the revision
xyplot(age~revision, data = expfert,
       col = expfert$revision)

## The plot with panels
xyplot(age~revision|block, data = expfert,
       col = c(expfert$revision))

## Visualizing numerical values as if it was factor
xyplot(age~factor(revision)|block,
       data = expfert,
       col = 1,
       pch = 16,
       type = 'b')

## changing the panel head color 
xyplot(age~factor(revision)|block,
       data = expfert,
       col = 10,
       pch = 20,
       type = 'b',
       strip = strip.custom(bg = 'green'))

## To see how the CAI changed over the revisions
xyplot(CAI~factor(revision), data = expfert)

##showing the treatments
xyplot(CAI~factor(revision),
       groups = treatment,
       data = expfert)

## adding legends
xyplot(CAI~factor(revision),
       groups =treatment,
       data = expfert,
       par.settings = simpleTheme(col = c(3:6),
                                  pch = c(16:19)),
       auto.key = list(corner = c(0.02, 0.94),
                       border = 'black',
                       cex = 1.5,
                       points = T)
       )

xyplot(volume~domheight |treatment,
       groups = block,
       data = expfert,
       type = 'p',
       par.settings = simpleTheme(col = c(3:7),
                                  pch = c(16:20)),
       auto.key = list(corner = c(0.02, 0.94),
                       border = 'red',
                       cex = 1,
                       points = T)
       )

## Using the lattice package barchart
### Volume
barchart(volume~treatment|block, data = expfert,
         subset = revision == 1)

barchart(volume~treatment|block, data = expfert,
         subset = revision == 4)

barchart(volume~treatment|block, data = expfert,
         subset = revision == 5)

barchart(volume~treatment|block, data = expfert,
         subset = revision == 6)

### CAI
barchart(CAI~treatment|block, data = expfert,
         subset = revision == 4)

barchart(CAI~treatment|block, data = expfert,
         subset = revision == 5)

barchart(CAI~treatment|block, data = expfert,
         subset = revision == 6)


### BOXPLOT

bwplot(domheight~treatment, subset=revision==1, data = expfert,
       col = treatment)

bwplot(domheight~treatment, subset=revision==4, data = expfert)

bwplot(domheight~treatment, subset=revision==5, data = expfert)

bwplot(domheight~treatment, subset=revision==6, data = expfert)

## Anova
M.vol <- lm(volume~block+treatment, 
            data = expfert[expfert$revision==4,])
M.vol
Anova(M.vol)

## FOllow up test using T.vol
T.vol <- TukeyC(x = M.vol, which = 'treatment')
summary(T.vol)

## Checking for patterns in the residuals with a residual plot
plot(M.vol$fitted.values, M.vol$residuals,
     xlab = 'Fitted Values',
     ylab = 'Residuals')
abline (c(0,0), col = 2)

## Use Base R plot function

plot(M.vol)

## Anova for other revisions
### Vol
M.vol5 <- lm(volume~block+treatment,
             data = expfert[expfert$revision == 5,])
Anova(M.vol5)

T.vol5 <- TukeyC(x = M.vol5, which = 'treatment')

summary(T.vol5)

plot(M.vol5$fitted.values, M.vol5$residuals,
     xlab ='fitted values',
     ylab = 'residuals')
abline(c(0,0), col = 'black')

M.vol6 <- lm(volume~block+treatment,
             data = expfert[expfert$revision == 6,])
Anova(M.vol6)

T.vol6 <- TukeyC(x = M.vol6, which = 'treatment')

summary(T.vol6)

plot(M.vol6$fitted.values, M.vol6$residuals,
     xlab ='fitted values',
     ylab = 'residuals')
abline(c(0,0), col = 'black')


### CAI
m.cai4 <- lm(CAI~block+treatment,
             data = expfert[expfert$revision == 4,])
Anova(m.cai4)

t.cai4 <- TukeyC(x = m.cai4, which = 'treatment')

summary(t.cai4)

plot(m.cai4$fitted.values, m.cai4$residuals,
     xlab ='fitted values',
     ylab = 'residuals')
abline(c(0,0), col = 'black')



m.cai5 <- lm(CAI~block+treatment,
            data = expfert[expfert$revision == 5,])
Anova(m.cai5)

t.cai5 <- TukeyC(x = m.cai4, which = 'treatment')

summary(t.cai5)

plot(m.cai5$fitted.values, m.cai5$residuals,
     xlab ='fitted values',
     ylab = 'residuals')
abline(c(0,0), col = 'black')

m.cai6 <- lm(CAI~block+treatment,
            data = expfert[expfert$revision == 6,])
Anova(m.cai6)

t.cai6 <- TukeyC(x = m.cai4, which = 'treatment')

summary(t.cai6)

plot(m.cai6$fitted.values, m.cai6$residuals,
     xlab ='fitted values',
     ylab = 'residuals')
abline(c(0,0), col = 'black')
