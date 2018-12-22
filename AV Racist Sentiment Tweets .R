

## Analytics Vidhya Tweeter Sentiment Analysis


#============##########================##################====================================

# Improting data files 

train<- read.csv("train_tweets.csv", stringsAsFactors = FALSE)
test<- read.csv("test_tweets.csv",stringsAsFactors = FALSE)

# combining the tweet columns

combi1<-train[3]
combi2<-test[2]

combi<-rbind(combi1,combi2)

# cleaning the tweet column

library(tm)
library(SnowballC)

combi$tweet <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", combi$tweet) # keep only aphanumeric & few sepcial charecters
corpus<-VCorpus(VectorSource(combi$tweet)) # creating a corpus of tweet text

corpus<-tm_map(corpus,content_transformer(tolower))
corpus<-tm_map(corpus,removeNumbers)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeWords,stopwords(kind="en"))
corpus<-tm_map(corpus,stemDocument)
corpus<-tm_map(corpus,stripWhitespace)


as.character(corpus[[1]])

as.character(corpus[[10]])

# creating bag of words model

dtm<-DocumentTermMatrix(corpus) # making document term matrix
dtm<-removeSparseTerms(dtm,0.99) # removing sparse term
dtm # to view

main<-as.data.frame(as.matrix(dtm)) # converting matrix into data frame

# seperate the train and test rows now

train1<-main[1:31962,] # original train data rows

test1<-main[31963:49159,] # original test data rows


train1$label<-train$label # attaching the lables of train data into the clean tweet

train1$label<-as.factor(train1$label) # converting lables as factors


# splitting train data for model building and testing 

library(caTools)
set.seed(123)

split<- sample.split(train1$label,SplitRatio = 0.75)

train_set<-subset(train1, split == TRUE)
test_set<-subset(train1, split == FALSE)

# building RandomForest model 

library(randomForest)
classifier<-randomForest(x=train_set[-98],
                         y= train_set$label,
                         ntree= 100)

label_pred<-predict(classifier,newdata = test_set[-98])

# validating results using confusion matrix

cm<-table(test_set[,98],label_pred)

cm

# model accuracy of 93.3 % 

# checking with other classifier - Naive Bayes and comparing with Random Forest classifier accuracy

library(e1071)

classifier1<-naiveBayes(x=train_set[-98],
                        y= train_set$label)

predNB<-predict(classifier1,newdata = test_set[-98])

# validating results using confusion matrix

cm1<-table(test_set[,98],predNB)

cm1

#  RandomForest gives better accuracy (93%) as compare to NaiveBayes 
#==========#===================================================================
# apply the codes on test set to predict the labels on original Test data


label_pred1<-predict(classifier,newdata = test1)

label_pred1<-as.data.frame(label_pred1)

test<-data.frame(test$tweet,label_pred1)

write.csv(test,'test.csv')







