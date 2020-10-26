#Project Part 1# 

#Importing the Data#
install.packages(haven)
library(haven) #Loads package that can read Stata datasets 
dataset <- read_dta("/Users/jakedemarco/Desktop/Project_part 1_Data_Original.dta") #Importing the dataset and storing it in a variable named dataset
View(dataset)

#Cleaning the dataset#

#Deleting Columns#

#Deleting Individual Columns#
dataset$perpoverty <- NULL #Remove perpoverty colum - almost entirely N/A responses (1)
dataset$vtdummy <- NULL #Remove vtdummy columns - not relevant to study (2)
dataset$nvdummy <- NULL #Remove nvdummy columns - not relevant to study (3)
dataset$statedum <-NULL #Remove statedum columns - not relevant to study (4)
dataset$ctdummy <- NULL #Remove ctdummy columns - not relevant to study (5)
dataset$medummy <- NULL #Remove medummy columns - not relevant to study (6)
dataset$ordercount <- NULL #Remove ordercount columns - not relevant to study (7) 
dataset$na <- NULL #Remove na columns - not relevant to study (9)
dataset$state <- NULL #Remove state columns - not relevant to study (13)
dataset$expenditures <- NULL #Removes expenditures columns - not relevant to study (14)
dataset$locationid <- NULL #Removes locationid columns - not relevent to study (15)
dataset$dogkill <- NULL #Removes Dogs Killed Column - Only one county in 2010 with a reported dog killed (16)

#Deleting multiple columns at once#
dataset <- dataset[, -c(12:16)] #Removes country_vcrime, state_vcrime_report, state_vcrime_set, countyid, and merge - not relevant and almost entirely N/A resonses (8)
dataset <- dataset[, -c(14:19)] #Removes countlag, explag, minority, kill, stateid, and nbdunny - not relevant to study (10) 
dataset <- dataset[, -c(15:28)] #Removes lb, ub, t, t2, t3, killlag, logexpenditures, logmedianincome, logpopulation, logblack, logcounty_vcrime, logminority, logexplag, dogkilllag - not relevant to study (11)
dataset <- dataset[, -c(15:18)] #Removes changecount, changecountdog, meancount, meancountdog (12)

#Deleting Observations#
dataset <- na.omit(dataset) #Removes any row with NA value 

#Selecting 2010# 
library(plyr) 
count(dataset$year) #Counts frequency of observations
library(dplyr)
dataset <- filter(dataset, year>2009 & year<2011) #Removes all observations other than 2010
library(ggplot2)
ggplot(dataset, aes(count)) + geom_boxplot() + ggtitle("Box Plot of People Killed by Police") + theme(plot.title = element_text(hjust = 0.5)) #Creates a box plot to confirm outlier
dataset <- filter(dataset, count<16) #Removes an invalid observation

write.csv(dataset, "Project_Part 1_Data_TIDY.csv") #Export TIDY dataset

#Data Analysis#

#Distribution of Variables#
#Generating Variable Histograms#
hist(dataset$medincome, 
     main = "Histogram for Median Income", 
     xlab = "Median Income")
hist(dataset$population, 
     main = "Histogram for Population", 
     xlab = "Population")
hist(dataset$unemployed, 
     main = "Histogram for Unemployment",
     xlab = "Unemployment")
hist(dataset$count, 
     main = "Histogram for Count", 
     xlab = "Count")
hist(dataset$black, 
     main = "Histogram for Black Population",
     xlab = "Black Population")
hist(dataset$drugest, 
     main = "Histogram for Drug-Using Population", 
     xlab = "Drug-Using Population")
hist(dataset$hisp, 
     main = "Histogram for Hispanic Population", 
     xlab = "Hispanic Population")
hist(dataset$poverty, 
     main = "Histogram for Poverty", 
     xlab = "Impoverished Population")

#Testing for Normality#
install.packages('tseries')
library(tseries)

jarque.bera.test(dataset$population) #Not-normally Distributed
jarque.bera.test(dataset$medincome) #Normally Distributed 
jarque.bera.test(dataset$unemployed) #Not-normally Distributed
jarque.bera.test(dataset$poverty) #Not-normally Distributed
jarque.bera.test(dataset$count) #Not-normally Distributed 
jarque.bera.test(dataset$black) #Not-normally Distributed
jarque.bera.test(dataset$hisp) #Not-normally distributed
jarque.bera.test(dataset$drugest)#Normally Distributed

#Generating Scatter Plots#
#Relationships between Poverty#
install.packages("MASS")
library(MASS)
ggplot(dataset, aes(x = unemployed, y = poverty )) + geom_point() + geom_smooth(span = 1) + ggtitle("Scatter Plot Between Unemployment and Poverty") + theme(plot.title = element_text(hjust = 0.5)) #creates a scatter plot between unemployed and poverty 
ggplot(dataset, aes(x = medincome, y = poverty)) + geom_point() + geom_smooth(method = 'rlm') + ggtitle("Scatter Plot Between Median Income and Poverty") + theme(plot.title = element_text(hjust = 0.5)) #creates a scatter plot between medincome and poverty
ggplot(dataset, aes(x = population, y = poverty)) + geom_point() + geom_smooth(span = 1) + ggtitle("Scatter Plot Between Population and Poverty") + theme(plot.title = element_text(hjust = 0.5))#creates a scatter plot between population and poverty 

#Relationship between minorities and Poverty#
ggplot(dataset, aes(x = black, y = poverty)) + geom_point() + geom_smooth(method = 'rlm') + ggtitle("Scatter Plot Between Black and Poverty") + theme(plot.title = element_text(hjust = 0.5))  #creates a scatter plot between Black and poverty
ggplot(dataset, aes(x = hisp, y = poverty)) + geom_point() + geom_smooth(method = 'rlm') + ggtitle("Scatter Plot Between Hispanic and Poverty") + theme(plot.title = element_text(hjust = 0.5)) #creates a scatter plot between Hispanic and poverty

#Relationships between minorities and police violence#
ggplot(dataset, aes(x = black, y = count)) + geom_point() + geom_smooth(method = 'rlm') + ggtitle("Scatter Plot Between Black and Police Killings") + theme(plot.title = element_text(hjust = 0.5))
ggplot(dataset, aes(x = hisp, y = count)) + geom_point() + geom_smooth(method = 'rlm') + ggtitle("Scatter Plot Between Hispanic and Police Killings") + theme(plot.title = element_text(hjust = 0.5))

#Generating Correlation Matrix#
cor <- cor(dataset[,3:10])
upper<-cor
upper[upper.tri(cor)]<-""
upper<-as.data.frame(upper)
upper





