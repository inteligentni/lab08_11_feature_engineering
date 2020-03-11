###########################################
# Data preparation and feature engineering
###########################################

###################  
# Titanic data set
###################  

# load Titanic train ("data/train.csv") and test sets ("data/test.csv")


# print the structure of the train set


# print the structure of the test set


###########################
# Detecting missing values
###########################

# print the summary of the train set


# print the summary of the test set


# number of observations with empty Cabin variable


# get indices of observations with no Cabin value from the first class, in the train set


# get indices of observations with no Cabin value from the first class, in the test set


# set the Cabin value for identified passangers to NA in the train and test sets


# print the number of missing Cabin values in the train and test sets


# checking the presence of empty strings in character variables in the train set


# do the same in the test set 


# set the empty Embarked values to NA in the train set


#install.packages('Amelia')
# load Amelia library


# set the display area to show two plots in the same row 


# # use the missmap f. to visualise the missing data in the train set


# use the missmap f. to visualise the missing data in the test set


# revert the plotting area to the default (one plot per row)


###########################
# Handling missing values
###########################

###############################################################
## Categorical variables with a small number of missing values
###############################################################

# get the number of unique values for the Embarked variable in both sets


# create the contingency table for the values of the Embarked variable


# replace all NA values for the Embarked variable with 'S' in the train set


# print the contingency table for the values of the Embarked variable


# transform the Embarked variable into a factor in both sets



###############################################################
## Numerical variables with a small number of missing values
###############################################################

# test the Fare variable for normality


# get the class of the observation with missing Fare variable


# calculate the median value for the Fare variable of all passangers from the 3rd class


# set the median value to the Fare variable of the passanger with a missing Fare


# print the summary of the test set


####################
# Feature selection
####################

#################################################################
## Examining the predictive power of variables from the data set
#################################################################

# transofrm the Sex variable into factor


# get the summary of the Sex variable


# compute the proportions table of the Sex variable


# create a contingency table for Sex vs. Survived


# compute the proportions for Sex vs. Survived


# transform the Survived variable into factor


# transform the Pclass variable into factor


# plot the number of passengers for different classes and Survived values


# add the Sex facet to the plot


# plot the number of passengers for different embarkment places and Survived values


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


# print the number of young females who has the Age value set


# set the AgeGender to 'Adult_Female' for all 'girls' with age over 18



# print the number of adult males who has the Age value set


# set the AgeGender to 'Young_Male' for all 'Adult_Male' with age under 18



# create the contingency table for the AgeGender variable


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



# create a data frame with ticket name and ticket count as variables


# print first few rows of the new data frame


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


# change the name of the last column to better reflect its meaning


# plot the PersonPerTicket vs. Proportion barchart, split based on the Survived attribute


##################################
# Save the augmented data set
##################################

# split into train and test set based on whether the Survived is present


# save both data sets to a file

