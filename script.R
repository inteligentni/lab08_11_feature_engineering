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


# checking the presence of empty strings or other irregular values in character variables in the train set
# first for one variable, and then for all


# do the same in the test set 


# check the irregular values present in the Embarked variable 

# set the empty Embarked values to NA in the train set


# get indices of observations with no Cabin value from the first class, in the train set


# get indices of observations with no Cabin value from the first class, in the test set

# set the Cabin value for identified passengers to NA in the train and test sets

# print the number of missing Cabin values in the train and test sets


###########################
# Handling missing values
###########################

###############################################################
## Categorical variables with a small number of missing values
###############################################################

# create the contingency table for the values of the Embarked variable


# replace all NA values for the Embarked variable with 'S' in the train set


# print the contingency table for the values of the Embarked variable

# transform the Embarked variable into a factor in both sets


###############################################################
## Numerical variables with a small number of missing values
###############################################################

# test the Fare variable for normality


# get the class of the observation with missing Fare variable


# calculate the median value for the Fare variable of all passengers from the 3rd class
# we have to set this to true as Fare has one NA value

# set the median value to the Fare variable of the passenger with a missing Fare


# print the summary of the test set


####################
# Feature selection
####################

#################################################################
## Examining the predictive power of variables from the data set
#################################################################

# transform the Sex variable into factor


# get the summary of the Sex variable


# compute the proportions table of the Sex variable

# create a contingency table for Sex vs. Survived


# transform the Survived variable into factor

# create the table again, now labels for Survived will be available


# compute the proportions for Sex vs. Survived


# transform the Pclass variable into factor


# plot the number of passengers for different classes and Survived values


# add the Sex facet to the plot


# Instead of counts, plot the proportions



# plot the number of passengers for different ports and Survived values


# examine the relation between Embarked and Survived, but with proportions

# examine the relation between Fare and Survived

######################
# Feature engineering
######################

# add the Survived variable to the test set


# transform the Pclass variable into factor (in the test set)


# transform the Sex variable into factor (in the test set)

# merge train and test sets

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
