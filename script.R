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

# checking the presence of empty strings or other irregular values in character variables in the train set
# first for one variable, and then for all
sum(titanic.train$Cabin=="" | titanic.train$Cabin==" " | titanic.train$Cabin=="-" | is.na(titanic.train$Cabin))

char_vars = c("Name", "Sex", "Cabin", "Embarked", "Ticket")
apply(X = titanic.train[,char_vars],
      MARGIN = 2,
      FUN = function(x) sum(x=="" | x==" " | x=="-" | is.na(x)))

# do the same in the test set 
apply(X = titanic.test[,char_vars],
      MARGIN = 2,
      FUN = function(x) sum(x=="" | x==" " | x=="-" | is.na(x)))

# check the irregular values present in the Embarked variable 
head(sort(unique(titanic.train$Embarked)))

# set the empty Embarked values to NA in the train set
titanic.train$Embarked[titanic.train$Embarked==""] <- NA

# get indices of observations with no Cabin value from the first class, in the train set
train.class1.no.cabin <- which(titanic.train$Pclass==1 & titanic.train$Cabin=="")
length(train.class1.no.cabin)

# get indices of observations with no Cabin value from the first class, in the test set
test.class1.no.cabin <- which(titanic.test$Pclass==1 & titanic.test$Cabin=="")
length(test.class1.no.cabin)

# set the Cabin value for identified passengers to NA in the train and test sets
titanic.train$Cabin[train.class1.no.cabin] <- NA
titanic.test$Cabin[test.class1.no.cabin] <- NA

# print the number of missing Cabin values in the train and test sets
sum(is.na(titanic.train$Cabin))
sum(is.na(titanic.test$Cabin))

###########################
# Handling missing values
###########################

###############################################################
## Categorical variables with a small number of missing values
###############################################################

# create the contingency table for the values of the Embarked variable
xtabs(~Embarked, data = titanic.train)

# replace all NA values for the Embarked variable with 'S' in the train set
titanic.train$Embarked[is.na(titanic.train$Embarked)] <- 'S'

# print the contingency table for the values of the Embarked variable
xtabs(~Embarked, data = titanic.train)

# transform the Embarked variable into a factor in both sets
titanic.train$Embarked <- factor(titanic.train$Embarked,
                                 levels = c("S","C","Q"),
                                 labels = c("Southampton", "Cherbourg", "Queenstown"))
titanic.test$Embarked <- factor(titanic.test$Embarked,
                                levels = c("S","C","Q"),
                                labels = c("Southampton", "Cherbourg", "Queenstown"))

###############################################################
## Numerical variables with a small number of missing values
###############################################################

# test the Fare variable for normality
shapiro.test(titanic.test$Fare)

# get the class of the observation with missing Fare variable
missing.fare.pclass <- titanic.test$Pclass[is.na(titanic.test$Fare)]

# calculate the median value for the Fare variable of all passengers from the 3rd class
median.fare <- median(x = titanic.test$Fare[titanic.test$Pclass == missing.fare.pclass], 
                      na.rm = T) # we have to set this to true as Fare has one NA value

# set the median value to the Fare variable of the passenger with a missing Fare
titanic.test$Fare[is.na(titanic.test$Fare)] <- median.fare

# print the summary of the test set
summary(titanic.test$Fare)

####################
# Feature selection
####################

#################################################################
## Examining the predictive power of variables from the data set
#################################################################

# transform the Sex variable into factor
titanic.train$Sex <- factor(titanic.train$Sex)

# get the summary of the Sex variable
summary(titanic.train$Sex)

# compute the proportions table of the Sex variable
prop.table(summary( titanic.train$Sex ))

# create a contingency table for Sex vs. Survived
sex.survived.counts <- xtabs(~Sex + Survived, data = titanic.train)
sex.survived.counts

# transform the Survived variable into factor
titanic.train$Survived <- factor(titanic.train$Survived, 
                                 levels = c(0,1), labels = c('No','Yes'))

# create the table again, now labels for Survived will be available
sex.survived.counts <- xtabs(~Sex + Survived, data = titanic.train)
sex.survived.counts

# compute the proportions for Sex vs. Survived
sex.surv.tbl <- prop.table(sex.survived.counts, 
                           margin = 1) # proportions are computed at the row level (each row sums to 1)
sex.surv.tbl

# transform the Pclass variable into factor
titanic.train$Pclass <- factor(titanic.train$Pclass, 
                               levels = c(1,2,3),
                               labels = c("1st", "2nd", "3rd"))

# plot the number of passengers for different classes and Survived values
library(ggplot2)

gp1 <- ggplot(titanic.train, aes(x = Pclass, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.4) +
  labs(x="Passenger class", y="Number of passengers") + 
  theme_bw()
gp1

# add the Sex facet to the plot
gp2 <- gp1 + facet_wrap(~Sex, nrow=2)
gp2

# Instead of counts, plot the proportions
gp3 <- ggplot(titanic.train, aes(x = Pclass, fill=Survived)) +
  geom_bar(position = "fill", width = 0.4) + # note that 'fill' is used instead of 'dodge'
  labs(x="Passenger class", y="Proportion of passengers") + 
  theme_bw()
gp3

gp4 <- gp3 + facet_wrap(~Sex, nrow=2)
gp4


# plot the number of passengers for different ports and Survived values
ggplot(titanic.train, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge", width = 0.5) +
  labs(x="Place of embarkment", y="Number of passengers") +
  theme_bw()


# examine the relation between Embarked and Survived, but with proportions
ggplot(data = titanic.train,
       mapping = aes(x = Embarked, fill=Survived)) +
  geom_bar(position = "fill", width = 0.5) +
  labs(x="\nPlace of embarkment", y="Proportion of passengers\n") +
  theme_minimal()


# examine the relation between Fare and Survived
ggplot(data = titanic.train,
       mapping = aes(x = Fare, fill=Survived)) +
  geom_density(alpha = 0.5) +
  theme_minimal()


######################
# Feature engineering
######################

# add the Survived variable to the test set
titanic.test$Survived <- NA

# transform the Pclass variable into factor (in the test set)
titanic.test$Pclass <- factor(x = titanic.test$Pclass, 
                              levels = c(1,2,3),
                              labels = c("1st", "2nd", "3rd"))

# transform the Sex variable into factor (in the test set)
titanic.test$Sex <- factor(titanic.test$Sex)

# merge train and test sets
titanic.all <- rbind(titanic.train, titanic.test)

###################################
## Creating the FamilySize variable
###################################

# examine the values of the SibSp variable
summary(titanic.all$SibSp)

table(titanic.all$SibSp)

# examine the values of the Parch variable
summary(titanic.all$Parch)

table(titanic.all$Parch)

# create a new variable FamilySize based on the SibSp and Parch values
titanic.all$FamilySize <- titanic.all$SibSp + titanic.all$Parch
summary(titanic.all$FamilySize)

# print the contingency table for the FamilySize
table(titanic.all$FamilySize)

# compute the proportion of FamilySize >= 3 in all passangers
sum(titanic.all$FamilySize>=3)/length(titanic.all$FamilySize)

# set the FamilySize to 3 to all observations where FamilySize > 3
titanic.all$FamilySize[titanic.all$FamilySize > 3] <- 3

# transform FamilySize into factor
titanic.all$FamilySize <- factor(titanic.all$FamilySize, 
                                 levels = 0:3, 
                                 labels = c(0:2, "3+"))
table(titanic.all$FamilySize)

# plot the FamilySize vs. Survived
ggplot(titanic.all[is.na(titanic.all$Survived) == FALSE,], 
       aes(x = FamilySize, fill = Survived)) + 
  geom_bar(position = "dodge", width = 0.5) + 
  theme_light()