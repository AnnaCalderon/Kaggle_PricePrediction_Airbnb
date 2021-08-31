install.packages('ggthemes')
install.packages('dlookr')
install.packages('openxlsx')
install.packages('randomForest')
install.packages('psych')
install.packages('corrplot')
install.packages('glmnet')
install.packages('skimr')

library(ggplot2)
library(tidyr)
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
library(ggthemes)
library(leaps)
library(glmnet)
library(dlookr)
library(openxlsx)
library(randomForest)
library(psych)
library(corrplot)
library(glmnet)
library(stats)
library(skimr)


setwd("Downloads")
analysisData = read.csv(file = 'analysisData.csv', stringsAsFactors = F)

scoringData = read.csv(file = 'scoringData.csv', stringsAsFactors = F)
scoringData$zipcode <- as.character(scoringData$zipcode)

set.seed(5656)
ksplit <- createDataPartition(y = analysisData$price, p=.7, list=F, groups=50)
train <- analysisData[ksplit,]
test <- analysisData[-ksplit,]

train$train_test_score <- "train"
test$train_test_score <- "test"
scoringData$train_test_score <- "score"
baseData <- bind_rows(train,test,scoringData)


str(baseData)

# Begin Data Wrangling Process
###### Exploratory data analysis ######

# VARIABLE: review_scores_rating - strong 
baseData %>% ggplot(aes(x = review_scores_rating, y = price)) + geom_smooth()
cor(baseData$price, baseData$review_scores_rating)


# VARIABLE:bathrooms - weak 
baseData = baseData[complete.cases(baseData),]
cor(baseData$price, baseData$bathrooms)
baseData <- bind_rows(train,test,scoringData)
baseData %>% ggplot(aes(x = bathrooms, y = price)) + geom_point(size = 0.4)
# weak correlation 


# VARIABLE: bedrooms - mean price
baseData %>% group_by(bedrooms) %>% 
  summarise(Mean_price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = bedrooms, y = Mean_price)) + geom_bar(stat = "identity")


# VARIABLE: neighbourhood group 
length(unique(baseData$neighbourhood_group_cleansed))
baseData %>% ggplot(aes(x = accommodates, y = price, color = neighbourhood_group_cleansed)) + geom_point()
# not sure about this one 

# VARIABLE: accommodates and room type 
baseData %>% ggplot(aes(x = accommodates, y = price, color = room_type)) + geom_point()


# VARIABLE: number of reviews 
baseData = baseData[complete.cases(baseData),]
cor(baseData$price, baseData$number_of_reviews)
# weak correlation with number_of_reviews 
# -0.2285071

# VARIABLE: property type
baseData <- bind_rows(train,test,scoringData)
length(unique(baseData$property_type))


#Graph for property type - shows strong significance 
baseData %>% group_by(property_type) %>% 
  summarise(Mean_price = mean(price, na.rm = T)) %>% 
  ggplot(aes(x = Mean_price, y = reorder(property_type, Mean_price))) + 
  geom_bar(stat = "identity")


baseData <- bind_rows(train,test,scoringData)
#checking levels 
unique(baseData$city)
unique(baseData$beds)
unique(baseData$bed_type)
unique(baseData$review_scores_rating)
unique(baseData$instant_bookable)
unique(baseData$cancellation_policy)
unique(baseData$cleaning_fee)
unique(baseData$bed_type)
unique(baseData$neighbourhood_group_cleansed)

#checking NAs
sum(is.na(baseData$beds))
sum(is.na(baseData$bathrooms))
sum(is.na(baseData$review_scores_rating))
sum(is.na(baseData$instant_bookable))
sum(is.na(baseData$cancellation_policy))
sum(is.na(baseData$cleaning_fee))
sum(is.na(baseData$accommodates))

############ Beds ##############
# Imputing Missing values for Beds variable
hist(baseData$beds) #observing variable to see how to impute
table(baseData$beds) #observing variable to see how to impute


# using Accomodate variable to predict - checking correlation of data without na rows
plot(baseData$accommodates, baseData$beds) #scatterplot
cor(baseData$ accommodates[!is.na(baseData$beds)], baseData$beds[!is.na(baseData$beds)])
#0.764 correlation 

not_Missing_index = !is.na(baseData$beds) #all observations that do not have missing values

# Make dataframe
df <- data.frame(x = baseData$accommodates[not_Missing_index], y = baseData$beds[not_Missing_index] )

# Fitting model 
model_missing <- lm(y~x, data = df) 

summary (model_missing)
#adjusted r-sqaured value 0.58 

#now we will predict and round the values 
newdata = data.frame(x = baseData$accommodates[!not_Missing_index])

predicted_missing <- predict.lm(model_missing, newdata = newdata)

predicted_missing <- round(predicted_missing, 0)


# Putting missing values in base data set

baseData$beds[!not_Missing_index] = predicted_missing

#checking NAs in beds again 
sum(is.na(baseData$beds))

################ Zip code ################# also imputation 
#Checking number of missing values
sum(is.na(baseData$zipcode))

# number of unique zip codes in the data
length(unique(baseData$zipcode))


# frequency of of the zipcode that repeats the most from the 200 

table(baseData$zipcode) #number times each zipcode repeats itself
which.max(table(baseData$zipcode)) #finding the more that repeats the most 

# finding the zipcode with a frequency of 114
table(baseData$zipcode)[114]

# replacing NAs with zipcode 11211, which is the most frequent one
baseData$zipcode[is.na(baseData$zipcode)] = 11211

#Cross check is there are missing vales in Zip code variable
sum(is.na(baseData$zipcode))


########################### Ameneties ####################### 
#variable creation 

sum(is.na(baseData$amenities)) #checking for NAs

t <- baseData %>% separate_rows(amenities, sep = ",") %>% count(id, amenities) %>% group_by(id) %>% summarise(count_amenties = n())

t #number of amenities 

baseData <- baseData %>% right_join(t) # adding number of amenities as a new variable 




# Begin Data Wrangling Process
baseData$bed_type <- factor(baseData$bed_type)
baseData$property_type <- factor(baseData$property_type)
baseData$neighbourhood_group_cleansed <- factor(baseData$neighbourhood_group_cleansed)
baseData$room_type <- factor(baseData$room_type)
baseData$cancellation_policy <- factor(baseData$cancellation_policy)
baseData$host_is_superhost <- factor(baseData$host_is_superhost)



# Continue with other Data Wrangling, Imputation tasks, dropping rows that are pesky.....

train <- baseData  %>% 
  filter(train_test_score == "train")
test <- baseData  %>% 
  filter(train_test_score == "test")
score <- baseData  %>% 
  filter(train_test_score == "score")

# Test to ensure our datasets match in terms of the number of rows
nrow(analysisData); nrow(train); nrow(test); nrow(score);

#Random Forest

#12 variables number_of_reviews - deleted bed_type 
rf_11 <- randomForest(price~host_is_superhost+beds+count_amenties+
                        neighbourhood_group_cleansed+room_type+accommodates+
                        bathrooms+bedrooms+cancellation_policy+review_scores_rating+
                        number_of_reviews+property_type, ntrees = 1000 ,data = train)
str(rf_11)

class(importance(rf_11))

order(importance(rf_11))

importance(rf_11)

varImpPlot(rf_11) #graph showing importance - deleted bed_type



predrfTrain <- predict(rf_11, newdata=train)
caret::postResample(pred = predrfTrain, train$price) 

predrfTest <- predict(rf_11, newdata=test)
caret::postResample(pred = predrfTest, test$price)

predrfScore = predict(rf_11, newdata=score)
submissionFile = data.frame(id = score$id, price = predrfScore)

setwd('/Users/anarosacalderon/Desktop')
write.csv(submissionFile, 'my_submission11.csv',row.names = F)

