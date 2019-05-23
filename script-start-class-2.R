###########################################
# Data preparation and feature engineering
###########################################

###################  
# Titanic data set
###################  

# load Titanic train ("data/train.csv") and test sets ("data/test.csv")
titanic.train <- read.csv("data/train.csv", stringsAsFactors = F)
titanic.test <- read.csv("data/test.csv", stringsAsFactors = F)

# print the structure of the train set
str(titanic.train)

# print the structure of the test set
str(titanic.test)

###########################
# Detecting missing values
###########################

# print the summary of the train set
summary(titanic.train)

# print the summary of the test set
summary(titanic.test)

# number of observations with empty Cabin variable
length(which(titanic.train$Cabin==""))
length(which(titanic.test$Cabin==""))

# get indices of observations with no Cabin set from the first class, in the train set
train.class1.no.cabin <- which(titanic.train$Pclass==1 & titanic.train$Cabin=="")
length(train.class1.no.cabin)

# get indices of observations with no Cabin set from the first class, in the train set
test.class1.no.cabin <- which(titanic.test$Pclass==1 & titanic.test$Cabin=="")
length(test.class1.no.cabin)

# set the Cabin value for identified passangers to NA in the train and test sets
titanic.train$Cabin[train.class1.no.cabin] <- NA
titanic.test$Cabin[test.class1.no.cabin] <- NA

# print the number of missing Cabin values in the train and test sets
length(which(is.na(titanic.train$Cabin)))
length(which(is.na(titanic.test$Cabin)))

# test for empty values in string variables in the train set 
apply(X = titanic.train[,c("Name","Sex","Ticket","Embarked")],
      MARGIN = 2,
      FUN = function(x) length(which(x=="")))

# test for empty values in string variables in the test set 
apply(X = titanic.test[,c("Name","Sex","Ticket","Embarked")],
      MARGIN = 2,
      FUN = function(x) length(which(x=="")))

# set the empty Embarked values to NA in the train set
titanic.train$Embarked[titanic.train$Embarked==""] <- NA

#install.packages('Amelia')
# load Amelia library
library(Amelia)

# set the display area to show two plots in the same row 
par(mfrow=c(1,2))

# print the missmap for the train set
missmap(obj = titanic.train, main = "Training set", legend = FALSE)

# print the missmap for the test set
missmap(obj = titanic.test, main = "Test set", legend = FALSE)

# revert the plotting area to the default (one plot per row)
par(mfrow=c(1,1)) 

###########################
# Handling missing values
###########################

###############################################################
## Categorical variables with a small number of missing values
###############################################################

# get the number of unique values for the Embarked variable in both sets
unique(titanic.train$Embarked)
unique(titanic.test$Embarked)

# print the contingency table for the values of the Embarked variable
xtabs(~Embarked, data = titanic.train)

# replace all NA values for the Embarked variable with 'S' in the train set
titanic.train$Embarked[is.na(titanic.train$Embarked)] <- 'S'

# print the contingency table for the values of the Embarked variable
xtabs(~Embarked, data = titanic.train)

# transform the Embarked variable into a factor in both sets
titanic.train$Embarked <- factor(titanic.train$Embarked)
titanic.test$Embarked <- factor(titanic.test$Embarked)

###############################################################
## Numerical variables with a small number of missing values
###############################################################

# test the Fare variable for normality
shapiro.test(titanic.test$Fare)

# get the class of the observation with missing Fare variable
missing.fare.pclass <- titanic.test$Pclass[is.na(titanic.test$Fare)]

# calculate the median value for the Fare variable of all passangers from the 3rd class
median.fare <- median(x = titanic.test$Fare[titanic.test$Pclass == missing.fare.pclass], 
                      na.rm = T) # we have to set this to true as Fare has one NA value

# set the median value to the Fare variable of the passanger with a missing Fare
titanic.test$Fare[is.na(titanic.test$Fare)] <- median.fare

# print the summary of the test set
summary(titanic.test$Fare)

####################
# Feature selection
####################

#################################################################
## Examining the predictive power of variables from the data set
#################################################################

# transofrm the Sex variable into factor
titanic.train$Sex <- factor(titanic.train$Sex)

# print the summary of the Sex variable
summary(titanic.train$Sex)

# print the proportions table of the Sex variable
prop.table(summary( titanic.train$Sex ))

# print the contingency table for Sex vs. Survived
sex.survived.counts <- xtabs(~Sex + Survived, data = titanic.train)
sex.survived.counts

# print the proportions of the contingency table for Sex vs. Survived
sex.surv.tbl <- prop.table(sex.survived.counts, 
                           margin = 1) # proportions are computed at the row level (each row sums to 1)
sex.surv.tbl

library(ggplot2)

# transform the Survived variable into factor
titanic.train$Survived <- factor(titanic.train$Survived, 
                                 levels = c(0,1), labels = c('No','Yes'))

# transform the Pclass variable into factor
titanic.train$Pclass <- factor(titanic.train$Pclass, 
                               levels = c(1,2,3),
                               labels = c("1st", "2nd", "3rd"))

# plot the number of passengers for different classes and Survived values
gp1 <- ggplot(titanic.train, aes(x = Pclass, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4) +
  ylab("Number of passengers") + 
  xlab("Passenger class") +
  theme_bw()
gp1

# add the Sex facet to the plot
gp2 <- gp1 + facet_wrap(~Sex)
gp2

# plot the number of passengers for different embarkment places and Survived values
gp3 <- ggplot(titanic.train, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge", width = 0.45) +
  ylab("Number of passengers") + 
  xlab("Place of embarkment") +
  theme_bw()
gp3

######################
# Feature engineering
######################

# add the Survived variable to the test set
titanic.test$Survived <- factor(NA, levels = c(1,2), labels = c("No", "Yes"))

# transform the Pclass variable into factor (in the test set)
titanic.test$Pclass <- factor(x = titanic.test$Pclass, 
                              levels = c(1,2,3),
                              labels = c("1st", "2nd", "3rd"))

# transform the Set variable into factor (in the test set)
titanic.test$Sex <- factor(titanic.test$Sex)

# transform the Embarked variable into factor (in the test set)
titanic.test$Embarked <- factor(titanic.test$Embarked)

# merge train and test sets
titanic.all <- rbind(titanic.train, titanic.test)

##################################
## Creating an age proxy variable
##################################

# print a sample of the Name variable
titanic.all$Name[1:10]

# split the name of the first observation on , or . characters
strsplit(x = titanic.all$Name[1], split = "[,|.]")

# split the name of the first observation on , or . characters and unlist
unlist(strsplit(x = titanic.all$Name[1], split = "[,|.]"))

# split the name of the first observation on , or . characters, unlist and take the 2nd elem.
unlist(strsplit(x = titanic.all$Name[1], split = "[,|.]"))[2]

# create a variable Title based on the value of the Name variable 
titanic.all$Title <- sapply(titanic.all$Name, 
                            FUN = function(x) unlist(strsplit(x, split = "[,|.]"))[2] )

# remove the leading space character from the Title
titanic.all$Title <- trimws(titanic.all$Title, which = "left")

# print the contingency table for the Title values
table(titanic.all$Title)

# create a vector of all women titles
adult.women <- c("Dona", "Lady", "Mme", "Mrs", "the Countess")

# create a vector of all girl titles
girls <- c("Ms", "Mlle", "Miss")

# create a vector of all men titles
adult.men <- c("Capt", "Col", "Don", "Dr", "Major", "Mr", "Rev", "Sir")

# create a vector of all boy titles
boys <- c("Master", "Jonkheer")

# introduce a new character variable AgeGender
titanic.all$AgeGender <- vector(mode = "character", length = nrow(titanic.all))

# set the AgeGender value based on the vector the Title value belongs to
titanic.all$AgeGender[ titanic.all$Title %in% adult.women ] <- "AdultWomen"
titanic.all$AgeGender[ titanic.all$Title %in% adult.men ] <- "AdultMen" 
titanic.all$AgeGender[ titanic.all$Title %in% girls ] <- "Girls" 
titanic.all$AgeGender[ titanic.all$Title %in% boys ] <- "Boys" 

# print the contingency table for the AgeGender values


# plot the distribution of Girls for different Age values 


# plot the distribution of AdultMen for different Age values 


# print the number of Girls who has the Age value set


# set the AgeGender to 'AdultWomen' for all 'girls' with age over 18


# print the number of AdultMen who has the Age value set


# set the AgeGender to 'Boys' for all 'AdultMen' with age under 18


# print the contingency table for the AgeGender variable


# print the proportions table for the AgeGender variable


# transform the AgeGender to factor


# plot the AgeGender against Survived attribute


# calculate the proportions for AgeGender and Survived values


# transform the proportions table in a dataframe


# plot the AgeGender vs. Freq vs. Survived


##################################
## Creating FamilySize variable
##################################

# print the summary of the SibSp variable


# print the summary of the SibSp Parch


# create a new variable FamilySize based on the SibSp and Parch values


# print the contingency table for the FamilySize


# print the proportion of FamilySize >= 3 in all passangers


# set the FamilySize to 3 to all observations where FamilySize > 3


# transform FamilySize into factor


# plot the FamilySize vs. Survived


#####################################
## Making use of the Ticket variable
#####################################

# print the sample for the Ticket variable


# print the unique values of the Ticket variable


# use tapply to compute the number of occurrences of each unique Ticket value 


# create a data frame with ticket name and ticket count as variable


# print a sample of the values from the new data frame


# print the contingency table of the count variable


# merge titanic.all and ticket.count.df datasets on the Ticket variable


# change the name of the newly added column to PersonPerTicket


# set the PersonPerTicket to 3 to all observations where PersonPerTicket > 3


# convert PersonPerTicket to factor


# print the contingency table for the PersonPerTicket


# print the contingency table for the PersonPerTicket vs. FamilySize


# plot all survived passangers (without NAs)


# calculate the proportions of the PersonPerTicket vs. Survived table


# convert the table into a data frame


# plot the PersonPerTicket vs. Freq barchart, split based on the Survived attribute


##################################
# Save the augmented data set
##################################

# split into train and test set based on whether the Survived is present


# save both data sets to a file

