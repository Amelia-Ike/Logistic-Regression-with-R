###Logistic Regression
#EX1-Read in the adult_sal.csv file and set it to a data frame called adult.
setwd("C:/Users/Amelia/Documents/R/R  CLASS/First Class/Assignment/LOGISTIC REGRESSION")
adult <- read.csv("adult_sal.csv", stringsAsFactors = TRUE)

#EX2-Check the head of adult
head(adult)

#Ex3-You should notice the index has been repeated. Drop this column.
library(dplyr)
adult <- select(adult,-X)

#EX4-Check the head,str, and summary of the data now.
head(adult)
str(adult)
summary(adult)

#EX5-type_employer column Use table() to check out the frequency of the type_employer column.
table(adult$type_employer)

#EX6-How many Null values are there for type_employer? What are the two smallest groups?
any(is.null(adult$type_employer))

#EX7- Combine these two smallest groups into a single group called "Unemployed". 
#There are lots of ways to do this, so feel free to get creative. Hint: It may 
#be helpful to convert these objects into character data types (as.character() 
#and then use sapply with a custom function)

Not_employed <-function(unemployed){
  unemployed<-as.character(unemployed)
  if(unemployed=='Never-worked'|unemployed=='Without-pay'){
    return('Unemployed')
  }
  else{
    return(unemployed)
  }
}
adult$type_employer<-sapply(adult$type_employer,Not_employed)
table(adult$type_employer)

#EX8-What other columns are suitable for combining? 
#Combine State and Local gov jobs into a category called SL-gov and 
Govt_employed<-function(employed){
  employed<-as.character(employed)
  if(employed=='State-gov'|employed=='Local-gov'){
    return('SL-gov')
  }
  else{
    return(employed)
  }
}

adult$type_employer<-sapply(adult$type_employer,Govt_employed)
table(adult$type_employer)

#combine self-employed jobs into a category called self-emp.
Self_employed<-function(employed){
  employed<-as.character(employed)
  if(employed=='Self-emp-inc'|employed =='Self-emp-not-inc'){
    return('self_emp')
  }
  else{
    return(employed)
  }
}
adult$type_employer<-sapply(adult$type_employer,Self_employed)
table(adult$type_employer)

#EX9-Marital Column
#Use table() to look at the marital column
table(adult$marital)

#Reduce this to three groups:
#Married
#Not-Married
#Never-Married

rel_status<-function(status){
  status<-as.character(status)
  if(status=='Separated'|status=='Widowed'|status=='Divorced'){
    return('Not-Married')
  }
  else if (status== 'Married-AF-spouse'|status=='Married-civ-spouse'| status=='Married-spouse-absent'){
    return('Married')
    
  }
  else{
    return('Never-married')
  }
}

adult$marital<-sapply(adult$marital,rel_status)
table(adult$marital)

##EX10-Country Column
#Check the country column using table()

table(adult$country)

##EX11-Group these countries together however you see fit. You have flexibility 
#here because there is no right/wrong way to do this, possibly group by 
#continents. You should be able to reduce the number of groups here significantly 
#though.

Asia<-c('China','Hong','India','Iran','Cambodia','Japan', 'Laos',
        'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')
North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c()

by_continents <- function(countries){
  if (countries %in% Asia){
    return('Asia')
  }else if (countries %in% North.America){
    return('North.America')
  }else if (countries %in% Europe){
    return('Europe')
  }else if (countries %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
adult$country <- sapply(adult$country,by_continents)

##EX12-Use table() to confirm the groupings
table(adult$country)

##EX13-Check the str() of adult again. Make sure any of the columns 
#we changed have factor levels with factor()

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

str(adult)
##EX14--Install and load the Amelia package.
library(Amelia)

##EX15--Convert any cell with a '?' or a ' ?' value to a NA value. 
#Hint: is.na() may be useful here or you can also use brackets with a 
#conditional statement.

adult[adult == '?'] <- NA

table(adult$type_employer)

##EX16-Play around with the missmap function from the Amelia package. 
#Can you figure out what its doing and how to use it?
missmap(adult, y.cex = 0.1, col=c('white','darkred'))

#EX17-You should have noticed that using missmap(adult) is bascially a heatmap
#pointing out missing values (NA). This gives you a quick glance at how much data
#is missing, in this case, not a whole lot (relatively speaking). You probably 
#also noticed that there is a bunch of y labels, get rid of them by running the 
#command below. What is col=c('yellow','black') doing?
#??missmap
missmap(adult, y.at=c(1), y.labels = c(''), col=c('yellow','black'))

#EX18-Use na.omit() to omit NA data from the adult data frame. Note, 
#it really depends on the situation and your data to judge whether or 
#not this is a good decision. 

#"YOU SHOULDN'T ALWAYS JUST DROP NA VALUES".

adult <- na.omit(adult)

#EX19-Use missmap() to check that all the NA values were in fact dropped.
missmap(adult, y.at=c(1), y.labels = c(''), col=c('yellow','black'))

#EDA
#Although we've cleaned the data, we still have explored it using visualization.

#EX20-Check the str() of the data.
str(adult)

#EX21-Use ggplot2 to create a histogram of ages, colored by income.
library(ggplot2)
library(ggthemes)

pl <- ggplot(adult, aes(x=age))
pl+geom_histogram(aes(fill=income),color='black',binwidth =2)+theme_bw()

#EX22-Plot a histogram of hours worked per week
pl <- ggplot(adult, aes(x=hr_per_week))
pl+geom_histogram()

#EX23-Rename the country column to region column to better reflect 
#the factor levels.
View(adult)
names(adult)[14] <- paste('region')

#EX24-Create a barplot of region with the fill color defined by income class. 
#Optional: Figure out how to rotate the x axis text for readability

pl <- ggplot(adult, aes(x=region))
pl+geom_bar(aes(fill=income))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Building a Model
#Now it's time to build a model to classify people into two groups: Above or Below 50k in Salary.

#Logistic Regression
#EX25- Take a quick look at the head() of adult to make sure we have a good overview before going into building the model!
head(adult)

#EX26-Split the data into a train and test set using the caTools library as done in previous lectures. 
#Reference previous solutions notebooks if you need a refresher.

library(caTools)
set.seed(101)

split = sample.split(adult$income, SplitRatio = 0.70)

final.train = subset(adult, split==TRUE)
final.test = subset(adult, split==FALSE)

#EX27-Use all the features to train a glm() model on the training data set, 
#pass the argument family=binomial(logit) into the glm function.

# Now let's return our model on only our final training set:
final.log.model <- glm(formula=income ~. , family= binomial(link='logit'),
                       data=final.train)
summary(final.log.model)

help(step)

#EX28-Use new.model <- step(your.model.name) to use the step() function
#to create a new model.

new.model <- step(final.log.model)
warnings()

#EX29- You should get a bunch of messages informing you of the process. 
#Check the new.model by using summary()
summary(new.model)

#EX30- Create a confusion matrix using the predict function with 
#type='response' as an argument inside of that function.

fitted.probabilities <- predict(new.model, newdata = final.test, type='response')
#calculate from the predicted values
#fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)

#confusion Matrix
con_matrix <- table(final.test$income, fitted.probabilities > 0.5)
con_matrix

#EX31- What was the accuracy of our model?
#misClassificError <- mean(fitted.probabilities != final.test$income)
#print(paste('Accuracy', 1- misClassificError))
#misClassificError

accuracy_test <- sum(diag(con_matrix))/sum(con_matrix)
print(paste("Accuracy", accuracy_test))

#EX32-Calculate other measures of performance like, recall or precision.

#Recall: formular - TP/TP+FN

recall <- 1423/ (1423+ 872)
recall

#Precision: formular = TP/TP+ FP
precision <- 1423 / (1423 + 548)
precision
