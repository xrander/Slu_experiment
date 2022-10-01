#importing the dataframe from user directory and it is assigned to the pop

pop <- read.table('C:/Users/aduol/Documents/Projects/SLU-Plantation-Experimentation/Data/Lab1/popdata.txt',
                  header = T, sep = "\t", na.strings = "NA", dec = ".", strip.white = T)

#printing the first six and last five of the dataframe

#for first six values + the header
head(pop)

#printing the last six values + the header
tail(pop)

#printing the names of the variables
names(pop)

#checking the properties of the dataframe
str(pop)

#getting a short overview of the dataframe, giving basic descriptive statistics
# like mean, median, mode, min and max value 25th and 75th quartile
summary(pop)

#reading the first row and column
pop[1,1]

#reading the first four rows and column 3 and 4
pop[1:4,c(3,4)]

#a simpler code with similar result is:
pop[1:4, 3:4]

#another way is by calling the column name
pop[1:4, c('height', 'dia')]

#giving names to the values of fertilized data
pop$ffert <- ifelse(pop$fer==1, 'fert', 'control')

View(pop)

# num of fertilized seedlings
fertilized = sum(pop$fert == 1)
fertilized

# num of unfertilized seedlings
unfertilized = sum(pop$fert != 1)
unfertilized

#getting unique values of blocks and clones
unique(pop$block)
unique(pop$clone)

range(pop$height)


#minimum diameter
min(pop$dia)

#max height
max(pop$height)

#deriving height to diameter ratio
pop$hd <- pop$height/pop$dia

#getting the mean height to diameter ratio
mean(pop$hd)

plcol<- c(20,10)
pchdata <- c(15, 16)

#plotting a simple graph of height vs diameter
plot(pop$dia, pop$height,
     text(x=01.5, y=450,labels = "Growth rate of fertilized and unfertilized seedlings",col = "red", cex = 0.7),
     xlim = c(0,5.5), ylim = c(0, 600),xlab = 'diameter(mm)',
     ylab = 'Height(mm)', main = "Height vs Diameter",
     pch = c(16,17), col = c('red', 'blue'))
legend("topleft",
       legend = c("Control","Fertilized"),
       pch= c(16,17),
       col = c('red','blue'))

pop$height <- as.numeric(pop$height)
pop$dia <- as.numeric(pop$dia)
     