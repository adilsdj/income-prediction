library(ggplot2)
library(caTools)
library(Amelia)
library(corrplot)
library(locfit)
library(dplyr)
library(ggthemes)
library(ROCR)
options(stringsAsFactors = TRUE)

## IMPORTING DATA
adult <- read.csv('income.csv')

## INSPECTING DATA
print(head(adult))
print (str(adult))
print(summary(adult))

## CLEANING DATA
# Cleaning workclass column
print(table(adult$workclass))

job.cleaning <- function(job){
  job <- as.character(job)
  if (job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else if (job=='Local-gov' | job=='State-gov'){
    return("SL-gov")
  }else if (job=='Self-emp-inc' | job=='Self-emp-not-inc'){
    return("self-emp")
  }else{
    return(job)
  }
}

adult$workclass <- sapply(adult$workclass,job.cleaning)

# Cleaning marital.status column
print(table(adult$marital.status))

group_marital <- function(mar){
  mar <- as.character(mar)
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
  }else if(mar=='Never-married'){
    return(mar)
  }else{
    return('Married')
  }
}
adult$marital.status <- sapply(adult$marital.status,group_marital)

# Cleaning native.country column
print(levels(adult$native.country))

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

adult$native.country <- sapply(adult$native.country,group_country)
names(adult)[names(adult)=="native.country"] <- "region"


# Dealing with missing data
adult[adult=="?"] <- NA
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
adult <- na.omit(adult)

# Putting factor levels on the columns we changed
adult$workclass <- sapply(adult$workclass,factor)
adult$region <- sapply(adult$region,factor)
adult$marital.status <- sapply(adult$marital.status,factor)
adult$occupation <- sapply(adult$occupation,factor)

## DATA EXPLORATION
plot(ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1))
dev.off()
plot(ggplot(adult,aes(hours.per.week)) + geom_histogram())
dev.off()
plot(ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black')+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1)))

## MODEL BUILDING
# First Model
sample <- sample.split(adult$income, SplitRatio = 0.8421)
train <- subset(adult, sample == TRUE)
test <- subset(adult, sample == FALSE)
model <-  glm(income ~ ., family = binomial(logit), data = train)
print(summary(model))

# New Model
new.step.model <- step(model)
print(summary(new.step.model))
test$predicted.income = predict(new.step.model, newdata=test, type="response")
print(table(test$income, test$predicted.income > 0.5))
predicted <- ifelse(test$predicted.income > 0.5,1,0)
actual <- ifelse(test$income == ">50K",1,0)
misClasificError <- mean(predicted != actual)
print(paste('Accuracy',1-misClasificError))

## ROC
data <- data.frame(predicted,actual)
pred <- prediction(data$predicted,data$actual)
perf <- performance(pred, "sens", "fpr")
plot(perf)
