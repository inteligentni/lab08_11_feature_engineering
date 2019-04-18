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
table(titanic.all$AgeGender)

# plot the distribution of Girls for different Age values 
ggplot(titanic.all[titanic.all$AgeGender=="Girls",], aes(x = Age)) + 
  geom_density() + 
  theme_bw()

# plot the distribution of AdultMen for different Age values 
ggplot(titanic.all[titanic.all$AgeGender=="AdultMen", ], aes(x = Age)) + 
  geom_density() + 
  scale_x_continuous(breaks = seq(5,80,5)) +
  theme_bw()

# print the number of Girls who has the Age value set
nrow(titanic.all[titanic.all$AgeGender=="Girls" & !is.na(titanic.all$Age),])

# set the AgeGender to 'AdultWomen' for all 'girls' with age over 18
titanic.all$AgeGender[titanic.all$AgeGender=="Girls" & 
                        !is.na(titanic.all$Age) & 
                        titanic.all$Age >= 18] <- "AdultWomen"

# print the number of AdultMen who has the Age value set
nrow(titanic.all[titanic.all$AgeGender=="AdultMen" & !is.na(titanic.all$Age),])

# set the AgeGender to 'Boys' for all 'AdultMen' with age under 18
titanic.all$AgeGender[titanic.all$AgeGender=="AdultMen" &
                        !is.na(titanic.all$Age) & 
                        titanic.all$Age < 18] <- "Boys"

# print the contingency table for the AgeGender variable
table(titanic.all$AgeGender)

# print the proportions table for the AgeGender variable
round(prop.table(table(titanic.all$AgeGender)), digits = 2)

# transform the AgeGender to factor
titanic.all$AgeGender <- factor(titanic.all$AgeGender)
summary(titanic.all$AgeGender)

# plot the AgeGender against Survived attribute
ggplot(titanic.all[1:891,], aes(x = AgeGender, fill=Survived)) +
  geom_bar(position = "dodge") + 
  theme_bw()

# calculate the proportions for AgeGender and Survived values
age.gen.surv.tbl <- prop.table(table(AgeGender = titanic.all$AgeGender[1:891],
                                     Survived = titanic.all$Survived[1:891]), 
                               margin = 1)
age.gen.surv.tbl

# transform the proportions table in a dataframe
age.gen.surv.df <- as.data.frame(age.gen.surv.tbl)
age.gen.surv.df

# plot the AgeGender vs. Freq vs. Survived
ggplot(age.gen.surv.df, aes(x = AgeGender, y = Freq, fill=Survived)) +
  geom_col(position = "dodge", width = 0.5) + 
  ylab("Proportion") +
  theme_bw()

##################################
## Creating FamilySize variable
##################################

# print the summary of the SibSp variable
summary(titanic.all$SibSp)

# print the summary of the SibSp Parch
summary(titanic.all$Parch)

# create a new variable FamilySize based on the SibSp and Parch values
titanic.all$FamilySize <- titanic.all$SibSp + titanic.all$Parch
summary(titanic.all$FamilySize)

# print the contingency table for the FamilySize
table(titanic.all$FamilySize)

# print the proportion of FamilySize >= 3 in all passangers
length(which(titanic.all$FamilySize>=3))/length(titanic.all$FamilySize)

# set the FamilySize to 3 to all observations where FamilySize > 3
titanic.all$FamilySize[titanic.all$FamilySize > 3] <- 3

# transform FamilySize into factor
titanic.all$FamilySize <- factor(titanic.all$FamilySize, 
                                 levels = c(0,1,2,3), 
                                 labels = c("0", "1", "2", "3+"))
table(titanic.all$FamilySize)

# plot the FamilySize vs. Survived
ggplot(titanic.all[1:891,], aes(x = FamilySize, fill = Survived)) + 
  geom_bar(position = "dodge", width = 0.5) + 
  theme_light()

#####################################
## Making use of the Ticket variable
#####################################

# print the sample for the Ticket variable
titanic.all$Ticket[1:20]

# print the unique values of the Ticket variable
length(unique(titanic.all$Ticket))

# use tapply to compute the number of occurrences of each unique Ticket value 
ticket.count <- tapply(titanic.all$Ticket,
                       INDEX = titanic.all$Ticket,
                       FUN = function(x) sum( !is.na(x) ))

# create a data frame with ticket name and ticket count as variable
ticket.count.df <- data.frame(ticket=names(ticket.count), 
                              count=as.integer(ticket.count))

# print a sample of the values from the new data frame
head(ticket.count.df)

# print the contingency table of the count variable
table(ticket.count.df$count)

# merge titanic.all and ticket.count.df datasets on the Ticket variable
titanic.all <- merge(x = titanic.all, y = ticket.count.df,
                     by.x = "Ticket", by.y = "ticket",
                     all.x = TRUE, all.y = TRUE)

# change the name of the newly added column to PersonPerTicket
colnames(titanic.all)[16] <- "PersonPerTicket"

# set the PersonPerTicket to 3 to all observations where PersonPerTicket > 3
titanic.all$PersonPerTicket[titanic.all$PersonPerTicket > 3] <- 3

# convert PersonPerTicket to factor
titanic.all$PersonPerTicket <- factor(titanic.all$PersonPerTicket, 
                                      levels = c(1,2,3), 
                                      labels = c("1", "2", "3+"))

# print the contingency table for the PersonPerTicket
table(titanic.all$PersonPerTicket)

# print the contingency table for the PersonPerTicket vs. FamilySize
xtabs(~ PersonPerTicket + FamilySize, data = titanic.all)

# plot all survived passangers (without NAs)
ggplot(titanic.all[!is.na(titanic.all$Survived),], aes(x = PersonPerTicket, fill=Survived)) + 
  geom_bar(position = "dodge", width = 0.5) + 
  theme_light()

# calculate the proportions of the PersonPerTicket vs. Survived table
tcount.surv.tbl <- prop.table(table(PersonPerTicket = titanic.all$PersonPerTicket,
                                    Survived = titanic.all$Survived, 
                                    useNA = "no"), 
                              margin = 1)
tcount.surv.tbl

# convert the table into a data frame
tcount.surv.df <- as.data.frame(tcount.surv.tbl)
tcount.surv.df

# plot the PersonPerTicket vs. Freq barchart, split based on the Survived attribute
ggplot(tcount.surv.df, aes(x = PersonPerTicket, y = Freq*100, fill=Survived)) +
  geom_col(width = 0.5, position = "dodge") + 
  theme_light() + 
  ylab("Percentage")

##################################
# Save the augmented data set
##################################

# split into train and test set based on whether the Survived is present
ttrain.new <- titanic.all[!is.na(titanic.all$Survived),]
ttest.new <- titanic.all[is.na(titanic.all$Survived),]

# save both data sets to a file
saveRDS(ttrain.new, file = "data/train_new.RData")
saveRDS(ttest.new, file = "data/test_new.RData")
