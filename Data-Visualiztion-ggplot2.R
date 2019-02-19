#set work Directory
setwd('C:/Users/nora/Desktop/database')

#install libraries
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)

# Load Casualities data for analysis. Open in spreadsheet view.
Casualities <- read.csv("Casualties0515.csv",sep='\t')

#Load Variables' names
names(Casualities)

# Set up factors
#
#
#----------------------------------WHY FACTORS------------?
#Factors are the data objects which are used to categorize the data and store it as levels. 
#They can store both strings and integers. They are useful in the columns
#which have a limited number of unique values. Like "Male, "Female" and True, False etc. 
#They are useful in data analysis for statistical modeling.
#
#
Casualities$ï..Accident_Index <- as.factor(Casualities$ï..Accident_Index)
Casualities$Vehicle_Reference <- as.factor(Casualities$Vehicle_Reference)
Casualities$Casualty_Reference <- as.factor(Casualities$Casualty_Reference)
Casualities$Casualty_Class <- as.factor(Casualities$Casualty_Class)
Casualities$Sex_of_Casualty <- as.factor(Casualities$Sex_of_Casualty)
Casualities$Age_of_Casualty <- as.factor(Casualities$Age_of_Casualty)
Casualities$Age_Band_of_Casualty <- as.factor(Casualities$Age_Band_of_Casualty)
Casualities$Casualty_Severity <- as.factor(Casualities$Casualty_Severity)
Casualities$Pedestrian_Location <- as.factor(Casualities$Pedestrian_Location)
Casualities$Pedestrian_Movement <- as.factor(Casualities$Pedestrian_Movement)
Casualities$Car_Passenger <- as.factor(Casualities$Car_Passenger)
Casualities$Bus_or_Coach_Passenger <- as.factor(Casualities$Bus_or_Coach_Passenger)
Casualities$Pedestrian_Road_Maintenance_Worker <- as.factor(Casualities$Pedestrian_Road_Maintenance_Worker)
Casualities$Casualty_Type <- as.factor(Casualities$Casualty_Type)
Casualities$Casualty_Home_Area_Type <- as.factor(Casualities$Casualty_Home_Area_Type)

#
# We'll start our visual analysis of the data focusing on questions
#

#
# -----------First question - What was the Casualty_Class? 
#
# As Casualty_Class is a factor (i.e., categorical) variable, a bar chart 
# is a great visualization to use.
#
#--------------------Remove NA values From the data
data = subset(Casualities,!is.na(Casualty_Severity))
data = subset(data,data$Sex_of_Casualty != -1)
data = subset(data,!is.na(Casualty_Class))
data = subset(data,data$Age_Band_of_Casualty != -1)
#
#Plotting
#
ggplot(data, aes(x = Casualty_Severity)) + 
  geom_bar()


# If you really want percentages.
prop.table(table(Casualities$Casualty_Severity))

# Add some customization for labels and theme.
ggplot(data, aes(x = Casualty_Severity)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Casualty_count",
       title = "Casualities-Casualty_Severity")


#
# Second question - What was the Casualty_Severity by gender? 
#
# We can use color to look at two aspects (i.e., dimensions)
# of the data simultaneously.
#
ggplot(data, aes(x = Sex_of_Casualty, fill = Casualty_Severity)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Casuality Count",
       title = "Casualities severity by Sex")



#
# Third question - What was the Casuality severity by casuality-class 
#                   and gender?
#
# We can leverage facets to further segment the data and enable
# "visual drill-down" into the data.
#
ggplot(data, aes(x = Sex_of_Casualty, fill = Casualty_Severity)) + 
  theme_bw() +
  facet_wrap(~ Casualty_Class) +
  geom_bar() +
  labs(y = "Casuality Count",
       title = "Casuality severity Rates by Casuality Class and Sex")




#
# Next, we'll move on to visualizing continuous (i.e., numeric)
# data using ggplot2. We'll explore visualizations of single 
# numeric variables (i.e., columns) and also illustrate how
# ggplot2 enables visual drill-down on numeric data.
#


#
# Forth Question - What is the distribution of Casuality ages?
#
# The histogram is a staple of visualizing numeric data as it very 
# powerfully communicates the distrubtion of a variable (i.e., column).
#
ggplot(data, aes(x = Age_Band_of_Casualty)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Casuality Count",
       x = "Age",
       title = "Casuality Age Distribtion")


#
# Fifth Question - What are the survival rates by age?
#
ggplot(data, aes(x = Age_Band_of_Casualty, fill = Casualty_Severity)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Casuality Count",
       x = "Age",
       title = "Casuality Severity Rates by Age")

# Another great visualization for this question is the box-and-whisker 
# plot.
ggplot(data, aes(x = Casualty_Severity, y = Age_Band_of_Casualty)) +
  theme_bw() +
  geom_boxplot() +
  labs(y = "Age",
       x = "Casualty_Severity",
       title = "Casualty_Severity Rates by Age")


#
# Sixth Question - What is the survival rates by age when segmented
#                    by gender and class of ticket?
#
# A related visualization to the histogram is a density plot. Think of
# a density plot as a smoothed version of the histogram. Using ggplot2
# we can use facets to allow for visual drill-down via density plots.
#
ggplot(data, aes(x = Age_Band_of_Casualty, fill = Casualty_Severity)) +
  theme_bw() +
  facet_wrap(Sex_of_Casualty ~ Casualty_Class) +
  geom_density(alpha = 0.5) +
  labs(y = "Count",
       x = "Age",
       title = "Casuality severity Rates by Age, casuality-class and Sex")


