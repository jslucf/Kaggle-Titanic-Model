# Jason Laso                      #
# 1/3/17                          #
# Kaggle Titanic Data Analysis    #
###################################


#read in full train and test datasets
test = read.csv(file="C:/Users/Jason/Documents/R/Titantic/test.csv", header=T, sep=",")
train = read.csv(file="C:/Users/Jason/Documents/R/Titantic/train.csv", header=T, sep=",")

#examine sets
str(test)
str(train)

#add Survived column (blank) to testing set
test$Survived = NA

#combine sets for data cleanup
train$istrain = 1 #add train/test identifier
test$istrain = 0
titanic.full = rbind(train, test) 


                                            #### Data Cleanup ####

summary(titanic.full)
#NAs for Age and Fare, blank spaces for Embarked
table(titanic.full$Embarked)

# Two missing values as blank spaces. Replace with the mode.
titanic.full[titanic.full$Embarked == "", "Embarked"] = modalvalue(titanic.full$Embarked)

# Using LM to predict NAS, it's best to remove outliers before prediction. Subset all rows below the upper whisker.
boxplot(titanic.full$Fare)
upper.whisker = boxplot.stats(titanic.full$Fare)$stats[5]
full.non.fare.outlier = titanic.full[titanic.full$Fare <= upper.whisker,]

# Replace NA for Fare using linear regression to predict
fare.na = titanic.full[is.na(titanic.full$Fare) == TRUE, ]
fare.lm = lm(Fare ~ Embarked+Pclass+SibSp+Parch, data= full.non.fare.outlier)
summary(fare.lm)
titanic.full[is.na(titanic.full$Fare) == TRUE, "Fare"] = round(predict(fare.lm, fare.na),2)
table(is.na(titanic.full$Fare))

boxplot(titanic.full$Age)
upper.whisker = boxplot.stats(titanic.full$Age)$stats[5]
full.non.age.outlier = titanic.full[titanic.full$Age <= upper.whisker,]

# Do the same for the 263 NAs for Age
age.na =  titanic.full[is.na(titanic.full$Age) == TRUE, ]
age.lm = lm(Age ~ Fare+Pclass+Sex+Parch, data= full.non.age.outlier)
summary(age.lm)
titanic.full[is.na(titanic.full$Age) == TRUE, "Age"] = round(predict(age.lm, age.na),0)
table(is.na(titanic.full$Age))

#remove useless objects
rm(age.na, fare.na, age.lm, fare.lm)

# create categorical variables
titanic.full$Sex = as.factor(titanic.full$Sex)
titanic.full$SibSp = as.factor(titanic.full$Embarked)
titanic.full$Pclass = as.factor(titanic.full$Pclass)

# Split data back up
train = titanic.full[titanic.full$istrain == 1, ]
test = titanic.full[titanic.full$istrain == 0, ]
train$istrain = NULL
test$istrain = NULL

# Turn survived in train set into a factor
train$Survived = as.factor(train$Survived)


                                                ##### Modeling #####
#random forest
library(randomForest)
titanic.rf = randomForest(Survived ~ Pclass + Sex + Age + Fare + SibSp + Parch, data = train, 
             ntree = 500, mtry = 3, nodesize=.01*nrow(train))
test$Survived = predict(titanic.rf, test)

kaggle = data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(kaggle, file = "C:/Users/Jason/Documents/R/Titantic/kaggle_rf.csv", row.names=F)
# 77.0%