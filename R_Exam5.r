## Importing the data
mix <- read.table('https://raw.githubusercontent.com/xrander/Slu_experiment/master/Data/Last%20straw/TaskE_mix.txt',
                  header = T,sep = '\t',
                  na.strings = 'NA',
                  strip.white = T,
                  dec = '.')


mix # View the data imported

## Little Exploration
str(mix)
summary(mix)

## Changing the data types of some columns
mix$plotno <- as.factor(mix$plotno)
mix$block <- as.factor(mix$block)
mix$treatment <- as.factor(mix$treatment)

# Question 1

## Estimating total VOlume produced
mix$total_vol <- mix$standingvol + mix$harvestvol + mix$dbh

Total_vol_mix <- summaryBy(total_vol~treatment, data = mix, FUN = sum)

Total_vol_mix ## Preview the result

## Some Visualizations 
mixb <- barplot(total_vol.sum~treatment,
                data = Total_vol_mix,
                ylab = substitute(paste(bold('Total Volume (m3)'))),
                col = c(3:5), ## You can change this color you can use c*(11:13)
                xlab = substitute(paste(bold('Treatments'))),
                main = 'Total Volume produced by each treatments',
                ylim = c(0, 1600))
text (x = mixb, y = Total_vol_mix$total_vol.sum, label = Total_vol_mix$total_vol.sum, pos = 3, cex = 0.45)

## You can use any of the two
barchart(total_vol~treatment | block,
         data = mix,
         ylab = substitute(paste(bold('Total Volume (m3)'))),
         group = block,
         xlab = substitute(paste(bold('Treatments'))),
         main = 'Total Volume produced by each treatments at the different Blocks',
         box.ratio = 2,)


# Question 2
## Checking the experimental design
table(mix$block, mix$treatment)


## Analysis of Variance

## pai
mix_paiha <- lm(paiha ~ block+treatment , data = mix)

##tot_vol
mix_tvol <- lm(total_vol ~ block+treatment , data = mix)

##dbh
mix_dbh <- lm(dbh~block+treatment, data = mix)


## total Volume
anova(mix_tvol)

##dbh
anova(mix_dbh)

##pai
anova(mix_paiha)

## Post Hoc Test
### pai
summary(TukeyC(mix_paiha, which = 'treatment'))

### dbh
summary(TukeyC(mix_dbh, which = 'treatment'))

### total volume
summary(TukeyC(sa_tvol, which = 'treatment'))