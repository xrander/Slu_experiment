## Thinning Experiment in Scot Pine

scot <- read.table('C:/Users/aduol/Documents/SLU/RSust/Exam/TaskB_GG.txt',
           sep = '\t', strip.white = T,
           header = T,
           na.strings = 'NA',
           dec = )
### **Data Exploration**
str(scot)

summary(scot)

scot$site <- as.factor(scot$site)
scot$treatment <- as.factor(scot$treatment)
scot$plotno <- as.factor(scot$plotno)

ftable(scot$site, scot$treatment, scot$plot)

scot$tot_vol <- scot$standingvol + scot$harvestvol + scot$mortvol


## Thinning Treatments
summaryBy(tot_vol~treatment, data = scot)

barchart(tot_vol~treatment | site,
         data = scot,
         group = treatment)

summaryBy(tot_vol + paiha + dbh ~ treatment,
          FUM = mean,
          data = scot)

### Linear Model
## pai
s_paiha <- lm(paiha ~ site+treatment , data = scot)

##tot_vol
s_tvol <- lm(tot_vol ~ site+treatment , data = scot)

##tot_dbh
s_dbh <- lm(dbh~site+treatment, data = scot)

### Follow up for
## tot_vol
anova(s_tvol)

##dbh
anova(s_dbh)

##pai
anova(s_paiha)

## Post hoc test
summary(TukeyC(s_tvol, which = 'treatment'))


