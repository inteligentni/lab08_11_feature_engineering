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

# get indices of observations with no Cabin value from the first class, in the train set
train.class1.no.cabin <- which(titanic.train$Pclass==1 & titanic.train$Cabin=="")
length(train.class1.no.cabin)

# get indices of observations with no Cabin value from the first class, in the test set
test.class1.no.cabin <- which(titanic.test$Pclass==1 & titanic.test$Cabin=="")
length(test.class1.no.cabin)

# set the Cabin value for identified passangers to NA in the train and test sets
titanic.train$Cabin[train.class1.no.cabin] <- NA
titanic.test$Cabin[test.class1.no.cabin] <- NA

# print the number of missing Cabin values in the train and test sets
length(which(is.na(titanic.train$Cabin)))
length(which(is.na(titanic.test$Cabin)))

# checking the presence of empty strings in character variables in the train set
apply(X = titanic.train[,c("Name","Sex","Ticket","Embarked")],
      MARGIN = 2,
      FUN = function(x) length(which(x=="")))

# do the same in the test set 
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

# # use the missmap f. to visualise the missing data in the train set
missmap(obj = titanic.train, main = "Training set", legend = FALSE)

# use the missmap f. to visualise the missing data in the test set
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

# create the contingency table for the values of the Embarked variable
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

# get the summary of the Sex variable
summary(titanic.train$Sex)

# compute the proportions table of the Sex variable
prop.table(summary( titanic.train$Sex ))

# create a contingency table for Sex vs. Survived
sex.survived.counts <- xtabs(~Sex + Survived, data = titanic.train)
sex.survived.counts

# compute the proportions for Sex vs. Survived
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


# plot the number of passengers for different ports and Survived values
gp3 <- ggplot(titanic.train, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "dodge", width = 0.45) +
  ylab("Number of passengers") + 
  xlab("Place of embarkment") +
  theme_bw()
gp3


# examine the relation between Embarked and Survived, but with proportions
emb_prop <- prop.table(table(titanic.train$Embarked, titanic.train$Survived), margin = 1)
emb_prop
emb_prop_df <- as.data.frame(emb_prop)
colnames(emb_prop_df) <- c("Embarked", "Survived", "Proportion")

ggplot(data = emb_prop_df,
       mapping = aes(x = Embarked, y = Proportion, fill=Survived)) +
  geom_col(position = "dodge", width = 0.45) +
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
strsplit(x = titanic.all$Name[1], split = ",|\\.")

# split the name of the first observation on , or . characters and unlist
unlist(strsplit(x = titanic.all$Name[1], split = ",|\\."))

# split the name of the first observation on , or . characters, unlist and take the 2nd elem.
unlist(strsplit(x = titanic.all$Name[1], split = ",|\\."))[2]

# create a variable Title based on the value of the Name variable 
titanic.all$Title <- sapply(titanic.all$Name, 
                            FUN = function(x) unlist(strsplit(x, split = ",|\\."))[2] )

# remove the leading space character from the Title
titanic.all$Title <- trimws(titanic.all$Title, which = "left")

# print the contingency table for the Title values
table(titanic.all$Title)

# create a vector of all women (adult female) titles
adult.women <- c("Dona", "Lady", "Mme", "Mrs", "the Countess")

# create a vector of all girl (young female) titles
girls <- c("Ms", "Mlle", "Miss")

# create a vector of all men (adult male) titles
adult.men <- c("Capt", "Col", "Don", "Dr", "Major", "Mr", "Rev", "Sir")

# create a vector of all boy (young male) titles
boys <- c("Master", "Jonkheer")

# introduce a new character variable AgeGender
titanic.all$AgeGender <- NA

# set the AgeGender value based on the vector the Title value belongs to
titanic.all$AgeGender[ titanic.all$Title %in% adult.women ] <- "Adult_Female"
titanic.all$AgeGender[ titanic.all$Title %in% adult.men ] <- "Adult_Male" 
titanic.all$AgeGender[ titanic.all$Title %in% girls ] <- "Young_Female" 
titanic.all$AgeGender[ titanic.all$Title %in% boys ] <- "Young_Male" 

# print the contingency table for the AgeGender values
table(titanic.all$AgeGender)

# plot the distribution of the Age attribute in the Young_Female group
ggplot(titanic.all[titanic.all$AgeGender=="Young_Female",], aes(x = Age)) + 
  geom_density() + 
  theme_bw()

# plot the distribution of the Age attribute in the Adult_Male group
ggplot(titanic.all[titanic.all$AgeGender=="Adult_Male", ], aes(x = Age)) + 
  geom_density() + 
  scale_x_continuous(breaks = seq(5,80,5)) +
  theme_bw()

# print the number of young females who has the Age value set
nrow(titanic.all[titanic.all$AgeGender=="Young_Female" & !is.na(titanic.all$Age),])

# set the AgeGender to 'Adult_Female' for all 'girls' with age over 18
titanic.all$AgeGender[titanic.all$AgeGender=="Young_Female" & 
                        !is.na(titanic.all$Age) & 
                        titanic.all$Age >= 18] <- "Adult_Female"

# print the number of adult males who has the Age value set
nrow(titanic.all[titanic.all$AgeGender=="Adult_Male" & !is.na(titanic.all$Age),])

# set the AgeGender to 'Young_Male' for all 'Adult_Male' with age under 18
titanic.all$AgeGender[titanic.all$AgeGender=="Adult_Male" &
                        !is.na(titanic.all$Age) & 
                        titanic.all$Age < 18] <- "Young_Male"

# print the contingency table for the AgeGender variable
table(titanic.all$AgeGender)

# print the proportions table for the AgeGender variable
round(prop.table(table(titanic.all$AgeGender)), digits = 2)

# transform the AgeGender to factor
titanic.all$AgeGender <- factor(titanic.all$AgeGender)
summary(titanic.all$AgeGender)

# plot the AgeGender against Survived attribute
ggplot(titanic.all[is.na(titanic.all$Survived) == FALSE,], 
       aes(x = AgeGender, fill=Survived)) +
  geom_bar(position = "dodge", width = 0.65) +
  theme_bw()

# calculate the proportions for AgeGender and Survived values
age.gen.surv.tbl <- prop.table(table(titanic.all$AgeGender,
                                     titanic.all$Survived,
                                     useNA = 'no'), 
                               margin = 1)
age.gen.surv.tbl

# transform the proportions table in a dataframe
age.gen.surv.df <- as.data.frame(age.gen.surv.tbl)
age.gen.surv.df

# change the name of the columns to better reflect their meaning
colnames(age.gen.surv.df) <- c("AgeGender", "Survived", "Proportion")

# plot the AgeGender vs. Proportion vs. Survived
ggplot(age.gen.surv.df, aes(x = AgeGender, y = Proportion, fill=Survived)) +
  geom_col(position = "dodge", width = 0.5) + 
  theme_bw()

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

#####################################
## Making use of the Ticket variable
#####################################

# print a sample of Ticket values
titanic.all$Ticket[1:20]

# compute the number of distinct values of the Ticket variable
length(unique(titanic.all$Ticket))

# use tapply to compute the number of occurrences of each unique Ticket value 
ticket.count <- tapply(titanic.all$Ticket,
                       INDEX = titanic.all$Ticket,
                       FUN = length)

# create a data frame with ticket name and ticket count as variables
ticket.count.df <- data.frame(ticket=names(ticket.count), 
                              count=as.integer(ticket.count))

# print first few rows of the new data frame
head(ticket.count.df)

# print the contingency table of the count variable
table(ticket.count.df$count)

# merge titanic.all and ticket.count.df datasets on the Ticket variable
titanic.all <- merge(x = titanic.all, y = ticket.count.df,
                     by.x = "Ticket", by.y = "ticket")

# change the name of the newly added column to PersonPerTicket
colnames(titanic.all)[16] <- "PersonPerTicket"

# print the contingency table of the PersonPerTicket variable
table(titanic.all$PersonPerTicket)

# set the PersonPerTicket to 4 to all observations where PersonPerTicket > 4
titanic.all$PersonPerTicket[titanic.all$PersonPerTicket > 4] <- 4

# convert PersonPerTicket to factor
titanic.all$PersonPerTicket <- factor(titanic.all$PersonPerTicket, 
                                      levels = 1:4, 
                                      labels = c(1:3, "4+"))

# print the contingency table for the PersonPerTicket
table(titanic.all$PersonPerTicket)

# print the contingency table for the PersonPerTicket vs. FamilySize
xtabs(~ PersonPerTicket + FamilySize, data = titanic.all)

# plot all survived passangers (without NAs)
ggplot(titanic.all[!is.na(titanic.all$Survived),], 
       aes(x = PersonPerTicket, fill=Survived)) + 
  geom_bar(position = "dodge", width = 0.5) + 
  theme_minimal()

# calculate the proportions of the PersonPerTicket vs. Survived table
tcount.surv.tbl <- prop.table(table(PersonPerTicket = titanic.all$PersonPerTicket,
                                    Survived = titanic.all$Survived, 
                                    useNA = "no"), 
                              margin = 1)
tcount.surv.tbl

# convert the table into a data frame
tcount.surv.df <- as.data.frame(tcount.surv.tbl)
tcount.surv.df

# change the name of the last column to better reflect its meaning
colnames(tcount.surv.df)[3] <- "Proportion"

# plot the PersonPerTicket vs. Proportion barchart, split based on the Survived attribute
ggplot(tcount.surv.df, aes(x = PersonPerTicket, y = Proportion, fill=Survived)) +
  geom_col(width = 0.5, position = "dodge") + 
  theme_minimal() 

##################################
# Save the augmented data set
##################################

# split into train and test set based on whether the Survived is present
ttrain.new <- titanic.all[!is.na(titanic.all$Survived),]
ttest.new <- titanic.all[is.na(titanic.all$Survived),]

# save both data sets to a file
saveRDS(ttrain.new, file = "data/train_new.RData")
saveRDS(ttest.new, file = "data/test_new.RData")
