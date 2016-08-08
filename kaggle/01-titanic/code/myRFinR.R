library(dplyr) # for data munging
library(randomForest)

library(rpart) #for decsion tree and pretty plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)


#create submit func to save typing
submit = function(filename){
  submit <- data.frame(PassengerId = test_data$PassengerId, Survived = test_data$Survived)
  write.csv(submit, file = filename, row.names = FALSE)
}


# LOADING THE DATA
#load train and test datasets
# stringsAsFactors=TRUE by deafault
train_data <- read.csv("../data/train.csv") 
test_data <- read.csv("../data/test.csv")

# 1. EXPLORING THE DATA
#count survived and perished
t<-table(train_data$Survived)
print(t)
#get proportion of those
prop.table(t)

str(train_data)
ls.str(train_data)
tbl_df(train_data)

#add our “everyone dies” to test data to BENCHMARK
test_data$Survived <- rep(0,count(test_data))
# create SUBMIT to kaggle
submit("../answer/theyallperish.csv")

#get the recall value from kaggle
#0.62679 we have 62% of our predictions correct
#compare with prop.table() result
#0.6161616
#now try to improve accuracy, make it better...much better

#make two-way comparison on rows
prop.table(table(train_data$Sex, train_data$Survived),1)
#majority of females aboard survived

#let's update our first hypothesis with the new knowledge
test_data$Survived <- 0
test_data$Survived[test_data$Sex == 'female'] <- 1
submit("../answer/menallperish.csv")
# we get 0.76555
# improved on your best score by 0.13876

## 2. DATA MUNGING
# digging into Age column
summary(train_data$Age)

# 2 issues:
# 1) it is continious and thus is not useful as it is, need to make it discreet
# 2) it has NA values, that we can replace with average age
#let's make a new feature isChild and update NA fields
train_data$Age <- train_data$Age[is.na(train_data$Age)] == mean(train_data$Age,na.rm = TRUE)
train_data$Child <- 0
train_data$Child[train_data$Age < 18] <- 1

# now let’s try to find the number of survivors for the different subsets:
#The aggregate command takes a formula with the target variable on the left hand side of 
#the tilde symbol and the variables to subset over on the right. 
#survived:
aggregate(Survived ~ Child + Sex, data=train_data, FUN=sum)
#totals:
aggregate(Survived ~ Child + Sex, data=train_data, FUN=length)
#proportion:
aggregate(Survived ~ Child + Sex, data=train_data, FUN=function(x) {sum(x)/length(x)})
# females have more chances to survive, predictions are correct so far

summary(train_data$Fare)
#fare price is continious as well, let's reduce it to 3 categories
train_data$Fare2 <- '30+'
train_data$Fare2[train_data$Fare < 30 & train_data$Fare >= 20] <- '20-30'
train_data$Fare2[train_data$Fare < 20 & train_data$Fare >= 10] <- '10-20'
train_data$Fare2[train_data$Fare < 10] <- '<10'
#now aggregate to see anything interesting here
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train_data, FUN=function(x) {sum(x)/length(x)})
#8  20-30      3 female 0.3333333
#9    30+      3 female 0.1250000
#notice that most of the class 3 women who paid more than $20 for their ticket 
#actually also miss out on a lifeboat
#let’s make a new prediction based on the new insights
test_data$Survived <- 0
test_data$Survived[test_data$Sex == 'female'] <- 1
test_data$Survived[test_data$Sex == 'female' & test_data$Pclass == 3 & test_data$Fare >= 20] <- 0
submit("../answer/nowomanperish_ex3cl.csv")
#0.7799. You improved on your best score by 0.01435. 

#lets have a look at decision tree
# rpart: Recursive Partitioning and Regression Trees
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train_data,
             method="class") #method="anova" for continious with automatic quantifiers
# examine the tree
plot(fit)
text(fit)
#not nice, how about now
fancyRpartPlot(fit)

# here we have an insight on port of embarkation
# and younger children had better chance of survival
Prediction <- predict(fit, test_data, type = "class")

submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "../answer/myfirstdtree.csv", row.names = FALSE)
#You improved on your best score by 0.00478. 
# Score: 0.78469
# let's tune the parameters now
# ?rpart.control
# cp = 0.01 - complexity
# minsplit - how many passengers in the bucket
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train_data,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
# this results in overfitting and kaggle won't give a score better than before

# now try interactive version of decision trees, kill the node as you click
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train_data,
             method="class",
             control=rpart.control(minsplit=1, cp=0.01))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)


# 3. FETURE ENGINEERING
train_data$Name[1]
#let's try to extract gender and nobility from the name
#need to do it on both train and test datasets
#bind them together and fill NA to labels column in test

test_data$Fare2 <- '30+'
test_data$Fare2[test_data$Fare < 30 & test_data$Fare >= 20] <- '20-30'
test_data$Fare2[test_data$Fare < 20 & test_data$Fare >= 10] <- '10-20'
test_data$Fare2[test_data$Fare < 10] <- '<10'

test_data$Child <- 0
test_data$Child[test_data$Age < 18] <- 1

test_data$Survived <- NA
combi <- rbind(train_data, test_data)

#remove factor() type from name column
combi$Name <- as.character(combi$Name)

# split string example, regex is used as a split boundary
strsplit(combi$Name[1], split='[,.]')

#String split uses a doubly stacked matrix because 
#it can never be sure that a given regex will have 
#the same number of pieces. If there were more 
#commas or periods in the name, it would create 
#more segments, so it hides them a level deeper 
#to maintain the rectangular types of containers 
#that we are used to in things like spreadsheets, 
#or now dataframes! 
strsplit(combi$Name[1], split='[,.]')[[1]][2]

#apply function to all rows
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
#strip spaces with sub() or gsub() for all spaces
combi$Title <- sub(' ', '', combi$Title)

# look at the titles
table(combi$Title)

#we can combine Mademoiselle and Madame as similar
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# convert back to factor()
combi$Title <- factor(combi$Title)

# take a look at the family size, combine them together
# as large families might have trouble sticking together in the panic
combi$FamilySize <- combi$SibSp + combi$Parch + 1
#just add the number of siblings, spouses, parents and children the passenger had with them, 
#and plus one for their own existence of course


#determine family by a surname and the size, group them together
#first extract surname
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#combine familysize with surname to get new familyID
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
# prevent people with the same surname travelling by their own be mapped as family
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)

#need to clean up small families or families with several surnames
famIDs <- data.frame(table(combi$FamilyID))
#filter small
famIDs <- famIDs[famIDs$Freq <= 2,]
#overwrite
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
#convert to factor
combi$FamilyID <- factor(combi$FamilyID)
#it is important to stack datasets before we build
#factors as they may exist in one DS only

#now we can split train and test datasets back
#Because we built the factors on a single dataframe, 
#and then split it apart after we built them, R will 
#give all factor levels to both new dataframes, even 
#if the factor doesn’t exist in one. It will still 
#have the factor level, but no actual observations 
#of it in the set. 
train_data <- combi[1:891,]
test_data <- combi[892:1309,]


# grow a new tree
# rpart has a great advantage in that it can use 
# surrogate variables when it encounters an NA value.
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train_data, 
             method="class")
fancyRpartPlot(fit)
#issue: new features governing our tree
#they are biased to favour factors with many levels
#try to submit now
Prediction <- predict(fit, test_data, type = "class")

submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "../answer/tree_w_newfeat.csv", row.names = FALSE)
#You improved on your best score by 0.00957. 
# Score: 0.79426

#Go ahead and try and create some more engineered variables! 
#As before, I also really encourage you to play around with 
#the complexity parameters and maybe try trimming some deeper 
#trees to see if it helps or hinders your rank. You may even 
#consider excluding some variables from the tree to see if 
#that changes anything too.
#In most cases though, the title or gender variables will 
#govern the first decision due to the greedy nature of 
#decision trees. The bias towards many-levelled factors 
#won’t go away either, and the overfitting problem can 
#be difficult to gauge without actually sending in submissions


# 4. TRY DIFFERENT MODEL (Random Forest)
# trees must use a bit of randomeness to differ from each other
# we use bagging, for bootstrap aggregating
# random number of rows with replacement
# every time we take a row we return it back before next random take
# it turns out that we will get approx 63% of the rows and 37% is left out,
# called “out-of-bag” (OOB) observations
# method is called bootstrap 
# if variable is strong it will prob. dominate in all trees
# The second source of randomness gets past this limitation though. 
# Instead of looking at the entire pool of available variables, 
# Random Forests take only a subset of them, typically 
# the square root of the number available. i.e. 3 for 10 variables
# The selection of available variables is changed for each 
# and every node in the decision trees.
# Trees mostly all overfit but in different way. But the mistakes 
# one makes will be averaged out over them all.

#issue RF doesn't work with NA values
summary(combi$Age)
# we'll use decision trees on continious value (ANOVA) to predict the missin Age
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

#Embarked contains empty values
#get index of those
which(combi$Embarked == "")
#set to majority port S
combi$Embarked[which(combi$Embarked == "")] <- "S"
#refactor it
combi$Embarked <- factor(combi$Embarked)

#fare has only one NA value
combi$Fare[which(is.na(combi$Fare))] <- median(combi$Fare, na.rm=TRUE)

#Random Forests in R can only digest factors with up to 32 levels. There is more with FamilyID
#We could take two paths forward here, 
# either change these levels to their underlying integers (using the unclass() function) 
# and having the tree treat them as continuous variables
# or manually reduce the number of levels to keep it under the threshold.
# we'll go the second way

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)
table(combi$FamilyID2)

#as we have two sources of randomness let's set the seed manually
#  This makes your results reproducible 
set.seed(415)

train_data <- combi[1:891,]
test_data <- combi[892:1309,]

# Instead of specifying method="class" as with rpart, 
# we force the model to predict our classification by 
# temporarily changing our target variable to a factor 
# with only two levels using as.factor(). 
# The importance=TRUE argument allows us to inspect 
# variable importance as we’ll see, and the ntree 
# argument specifies how many trees we want to grow.

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                    data=train_data, 
                    importance=TRUE, 
                    ntree=2000)
# you were working with a larger dataset you may want 
# to reduce the number of trees, at least for initial 
# exploration, or restrict the complexity of each tree
# using nodesize as well as reduce the number of rows 
# sampled with sampsize. You can also override the 
# default number of variables to choose from with mtry,
# but the default is the square root of the total number
# available and that should work just fine.
# plot variable importance by RF and Gini index (total decrease in node impurities from splitting on the variable)
varImpPlot(fit)
# RF uses "out-of-bag" (OOB) observations to see how well each tree performs on unseen data
# There’s two types of importance measures shown above. 
# The accuracy one tests to see how worse the model performs without each variable,
# so a high decrease in accuracy would be expected for very predictive variables. 
# The Gini one digs into the mathematics behind decision trees, but essentially 
# measures how pure the nodes are at the end of the tree. Again it tests to 
# see the result if each variable is taken out and a high score means the variable was important.

#make a final submission
Prediction <- predict(fit, test_data)
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "../answer/firstRforest.csv", row.names = FALSE)
#Your submission scored 0.77512, which is not an improvement of your best score.(

# Don't give up. There’s more than one ensemble model. 
# Let’s try a forest of conditional inference trees. 
# They make their decisions in slightly different ways, 
# using a statistical test rather than a purity measure, 
# but the basic construction of each tree is fairly similar.

# Conditional inference trees.
#
#install.packages('party')
library(party)
# Conditional inference trees are able to handle factors with 
# more levels than Random Forests can, so let’s go back to 
# out original version of FamilyID. 
# We also have to manually set the number of variables to 
# sample at each node as the default of 5 is pretty high for our dataset.

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                 data = train_data, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test_data, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = Prediction)
write.csv(submit, file = "../answer/condInfTree.csv", row.names = FALSE)
#You improved on your best score by 0.01914. Score 0.81340

