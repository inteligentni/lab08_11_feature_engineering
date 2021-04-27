###########################################
# Data preparation and feature engineering
###########################################

###################  
# Titanic data set
###################  

# load Titanic train ("data/train.csv") and test sets ("data/test.csv")
train_data <- read.csv("data/train.csv")
test_data <- read.csv("data/test.csv")

# print the structure of the train set
str(train_data)

# print the structure of the test set
str(test_data)


###########################
# Detecting missing values
###########################

# print the summary of the train set
summary(train_data)

# print the summary of the test set
summary(test_data)

# number of observations with empty Cabin variable
sum(train_data$Cabin == "" | train_data$Cabin == "-" | train_data$Cabin == " ")
sum(test_data$Cabin == "")

# get indices of observations with no Cabin value from the first class, in the train set
no_cabin_1st_class <- which(train_data$Cabin[train_data$Pclass == 1] == "")
train_data$Cabin[no_cabin_1st_class] <- NA
which(is.na(train_data$Cabin))

# get indices of observations with no Cabin value from the first class, in the test set
no_cabin_1st_class <- which(test_data$Cabin[test_data$Pclass == 1] == "")
test_data$Cabin[no_cabin_1st_class] <- NA

# set the Cabin value for identified passangers to NA in the train and test sets

# print the number of missing Cabin values in the train and test sets
sum(is.na(train_data$Cabin))
sum(is.na(test_data$Cabin))

# checking the presence of empty strings in character variables in the train set
char_vars <- c(4,5,12)
apply(train_data[, char_vars], 2, function(x) sum(x=="" | x =="-" | x==" "))

# do the same in the test set 
char_vars <- c(3,4,11)
apply(test_data[, char_vars], 2, function(x) sum(x=="" | x =="-" | x==" "))

# set the empty Embarked values to NA in the train set
train_data$Embarked[train_data$Embarked == "" | train_data$Embarked == "-" | train_data$Embarked == " "] <- NA
sum(is.na(train_data$Embarked))


#install.packages('Amelia')
# load Amelia library
library(Amelia)

# set the display area to show two plots in the same row 
par(mfrow = c(1,2))

# # use the missmap f. to visualise the missing data in the train set
missmap(train_data, main = "Train data", legend = FALSE)
# use the missmap f. to visualise the missing data in the test set
missmap(test_data, main = "Test data", legend = FALSE)
# revert the plotting area to the default (one plot per row)
par(mfrow = c(1,1))

###########################
# Handling missing values
###########################

###############################################################
## Categorical variables with a small number of missing values
###############################################################

# get the number of unique values for the Embarked variable in both sets


# create the contingency table for the values of the Embarked variable
table(train_data$Embarked)

# replace all NA values for the Embarked variable with 'S' in the train set
train_data$Embarked[is.na(train_data$Embarked)] <- 'S'

# print the contingency table for the values of the Embarked variable
table(train_data$Embarked)

# transform the Embarked variable into a factor in both sets
train_data$Embarked <- as.factor(train_data$Embarked)
test_data$Embarked <- as.factor(test_data$Embarked)

###############################################################
## Numerical variables with a small number of missing values
###############################################################

# test the Fare variable for normality
shapiro.test(test_data$Fare)

# get the class of the observation with missing Fare variable
pclass_miss_fare <- test_data$Pclass[is.na(test_data$Fare)]

# calculate the median value for the Fare variable of all passangers from the 3rd class
# we have to set this to true as Fare has one NA value
mdn_fare <- median(test_data$Fare[test_data$Pclass == pclass_miss_fare], na.rm = TRUE)

# set the median value to the Fare variable of the passanger with a missing Fare
test_data$Fare[is.na(test_data$Fare)] <- mdn_fare

# print the summary of the test set
summary(test_data$Fare)

####################
# Feature selection
####################

#################################################################
## Examining the predictive power of variables from the data set
#################################################################

# transofrm the Sex variable into factor
train_data$Sex <- as.factor(train_data$Sex)

# get the summary of the Sex variable
summary(train_data$Sex)

# compute the proportions table of the Sex variable
prop.table(summary(train_data$Sex))

# create a contingency table for Sex vs. Survived
table(train_data$Sex, train_data$Survived)

# compute the proportions for Sex vs. Survived
 # proportions are computed at the row level (each row sums to 1)
prop.table(table(train_data$Sex, train_data$Survived), margin = 1)

# transform the Survived variable into factor
train_data$Survived <- factor(train_data$Survived, labels = c("No", "Yes"))

# transform the Pclass variable into factor
train_data$Pclass <- factor(train_data$Pclass, levels = 1:3, 
                            labels = c("1st", "2nd", "3rd"))

# plot the number of passengers for different classes and Survived values
library(ggplot2)
g1 <- ggplot(train_data,
       aes(x = Pclass, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.5) +
  theme_bw()

# add the Sex facet to the plot
g1 + facet_wrap(~Sex)

# plot the number of passengers for different ports and Survived values



# examine the relation between Embarked and Survived, but with proportions


# examine the relation between Fare and Survived
ggplot(train_data,
       aes(x = Fare, fill=Survived)) +
  geom_density(alpha = 0.55) +
  theme_minimal()


######################
# Feature engineering
######################

# add the Survived variable to the test set


# transform the Pclass variable into factor (in the test set)

# transform the Sex variable into factor (in the test set)

# transform the Embarked variable into factor (in the test set)

# merge train and test sets


##################################
## Creating an age proxy variable
##################################

# print a sample of the Name variable


# split the name of the first observation on , or . characters

# split the name of the first observation on , or . characters and unlist

# split the name of the first observation on , or . characters, unlist and take the 2nd elem.

# create a variable Title based on the value of the Name variable 

# remove the leading space character from the Title

# print the contingency table for the Title values


# create a vector of all women (adult female) titles
adult.women <- c("Dona", "Lady", "Mme", "Mrs", "the Countess")

# create a vector of all girl (young female) titles
girls <- c("Ms", "Mlle", "Miss")

# create a vector of all men (adult male) titles
adult.men <- c("Capt", "Col", "Don", "Dr", "Major", "Mr", "Rev", "Sir")

# create a vector of all boy (young male) titles
boys <- c("Master", "Jonkheer")

# introduce a new character variable AgeGender

# set the AgeGender value based on the vector the Title value belongs to

# print the contingency table for the AgeGender values

# plot the distribution of the Age attribute in the Young_Female group


# plot the distribution of the Age attribute in the Adult_Male group


# set the AgeGender to 'Adult_Female' for all 'Young_Female' with age over 18


# set the AgeGender to 'Young_Male' for all 'Adult_Male' with age under 18


# print the contingency table for the AgeGender variable

# print the proportions table for the AgeGender variable

# transform the AgeGender to factor


# plot the AgeGender against Survived attribute


# calculate the proportions for AgeGender and Survived values


# transform the proportions table in a dataframe

# change the name of the last column to better reflect its meaning


# plot the AgeGender vs. Proportion vs. Survived


###################################
## Creating the FamilySize variable
###################################

# examine the values of the SibSp variable

# examine the values of the Parch variable


# create a new variable FamilySize based on the SibSp and Parch values

# print the contingency table for the FamilySize

# compute the proportion of FamilySize >= 3 in all passangers

# set the FamilySize to 3 to all observations where FamilySize > 3


# transform FamilySize into factor

# plot the FamilySize vs. Survived


#####################################
## Making use of the Ticket variable
#####################################

# print a sample of Ticket values

# compute the number of distinct values of the Ticket variable

# use tapply to compute the number of occurrences of each unique Ticket value 

# print the contingency table for the number of occurrences


# create a data frame with ticket name and ticket count as variables

# print first few rows of the new data frame


# merge titanic.all and ticket.count.df datasets on the Ticket variable


# change the name of the newly added column to PersonPerTicket


# print the contingency table for PersonPerTicket


# set the PersonPerTicket to 4 to all observations where PersonPerTicket > 3


# convert PersonPerTicket to factor


# print the contingency table for the PersonPerTicket


# plot PersonPerTicket vs. Survived


# calculate the proportions of the PersonPerTicket vs. Survived table


# convert the table into a data frame


# change the name of the last column to better reflect its meaning

# plot the PersonPerTicket vs. Proportion chart, split based on the Survived attribute


##################################
# Save the augmented data set
##################################

# split into train and test set based on whether the Survived is present


# save both data sets to a file

