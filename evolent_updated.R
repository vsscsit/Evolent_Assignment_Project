library(data.table)
library(sqldf)

#Question 1.	Rank top 3 Breweries which produce the strongest beers

#Read Data
original_data <- read.csv('C:/Users/Kanchan/Desktop/Evolent_Assignment/BeerDataScienceProject.csv',header=TRUE, stringsAsFactors=FALSE)
View(original_data)

beer_Strong <- data.frame(original_data$beer_ABV,original_data$beer_brewerId,stringsAsFactors = T)
colnames(beer_Strong) <- c('beer_ABV','beer_brewerID')


query = sqldf("select * from beer_Strong where beer_ABV >= 15")

write.csv(query,file = 'C:/Users/Kanchan/Desktop/Evolent_Assignment/query_ques1.csv.csv',row.names=F)

quer_data <- read.csv("C:/Users/Kanchan/Desktop/Evolent_Assignment/query_ques1.csv.csv",header=TRUE, stringsAsFactors=FALSE)
frame <-data.frame(quer_data$beer_ABV,quer_data$beer_brewerID)
colnames(frame) <- c('beer_ABV','beer_brewerID')
b <- setDT(frame)[, mean(beer_ABV), by = beer_brewerID]

barplot(height = b$V1,names.arg = b$beer_brewerID,
        ylim = c(0,35),
        xlab = 'brewer_ID', 
        ylab = 'ABV Rating', 
        main = 'Top 3 Breweries', col = rainbow(20))

#Question 2: Which year did beers enjoy the highest ratings?

library(lubridate)

datetime <- as_datetime(original_data$review_time)

#convert review_time into Year (ex.2019)
date_convert_in_year <- as.POSIXct(datetime,format="%m-%d-%Y %H:%M")
frame1_datetime<- as.data.frame(format(date_convert_in_year,format="%Y"))

#create data frame to read required column
beer_rating_year <- data.frame(beer_review_mean,original_data$review_time,frame1_datetime)
View(beer_rating_year)

#calculate mean of same year
beer_rating_year_ave <- setDT(beer_rating_year)[,mean(beer_review_mean),by=frame1_datetime]


#barplot of beer rating year average
barplot(height = beer_rating_year_ave$V1,
        names.arg = beer_rating_year_ave$`format(date_convert_in_year, format = "%Y")`,
        xlab = 'Year', 
        ylab = 'Customer_Rating', 
        main = 'Highest Ratings Year', col = "blue")


#Question 3. Based on the user's ratings which factors are important among taste, aroma, appearance, and palette

important_factor <- data.frame(original_data$review_taste,original_data$review_aroma,original_data$review_appearance,
                          original_data$review_palette)
factor_mean <- colMeans(important_factor)
write.csv(factor_mean,file='C:/Users/Kanchan/Desktop/Evolent_Assignment/factorMean.csv',row.names = TRUE)

#calculate mean of column
mean_all_factor <- colMeans(important_factor)
View(mean_all_factor)

#Read created mean file
a <- read.csv('C:/Users/Kanchan/Desktop/Evolent_Assignment/factorMean.csv',header=TRUE, stringsAsFactors = TRUE)

#barplot of beer rating year average
barplot(height = a$x, 
       names.arg = c("Taste","Aroma","Appearance","Palette"),
       ylim = c(0,4),
       xlab = 'Important Factor',
       ylab = 'Customer_Rating', 
       main = 'User's Ratings Factors', col = rainbow(4))

text(a$Review, labels=a$Review, cex=0.2, pos=3, col="black")


#Question 4: recommend 3 beers to your friends based on the data

data_4 = data.frame(original_data$beer_name,SA)
a = sqldf("select * from data_4 where score>=100")
View(a)

#barplot of beer rating year average
barplot(height = a$score, 
        names.arg = c('Sierra Nevada','Summerfest Lager','Blithering ldiot'),
        xlab = 'Beers Name',
        ylab = 'Review_Polarity',
        ylim = c(0,120),
        main = 'Recommended 3 beers', font.lab=4, font.sub=4, col = rainbow(4))

#Question 5: 5.	Which Beer style seems to be the favorite based on reviews written by users? 

library(tm)
library(dplyr)
library(textstem)

#creating corpus file of 'review_text' column
corpcp = Corpus(VectorSource(original_data$review_text))


#Preprocessing data
pre_number = tm_map(corpcp, removeNumbers)
Pre_pun = tm_map(pre_number, removePunctuation)
pre_stripW = tm_map(Pre_pun, stripWhitespace)
pre_lower = tm_map(pre_stripW,content_transformer(tolower))
pre_stopW = tm_map(pre_lower, removeWords, stopwords("english"))
com_lemma<- tm_map(pre_stopW, lemmatize_strings)
#pre_stem = tm_map(pre_stopW,stemDocument)


frame <- data.frame(review_text = sapply(com_lemma, as.character), stringsAsFactors = FALSE)

#Sentiment Analysis of data

hu.liu.pos = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AAA_Go_Y3kJxQACFaVBem__ea/positive-words.txt?dl=1');
hu.liu.neg = readLines('https://www.dropbox.com/sh/3xctszdxx4n00xq/AABTGWHitlRZcddq1pPXOSqca/negative-words.txt?dl=1');

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}

SA=score.sentiment(frame$review_text,hu.liu.pos,hu.liu.neg)
write.csv(SA,file='C:/Users/Kanchan/Desktop/Evolent_Assignment/Beer_Style_fav__SentimentAnalysis.csv', row.names=FALSE)

beer_style <- data.frame(original_data$beer_beerId,original_data$beer_style,SA)
View(beer_style)

#Query
a = sqldf("select * from beer_style where score>=100")

#Barplot of results
barplot(height = a$score, 
        names.arg = a$original_data.beer_style,
        xlab = 'Beer Style',
        ylab = 'Review_Polarity',
        ylim = c(0,120),
        main = 'Favorite Beers Style based on Review', font.lab=4, font.sub=4, col = rainbow(4))


#Question 6:review compare to overall review score for the beer styles 

#Reading required columns from original data and objects (SA)
beer_style_overall <- data.frame(original_data,SA)
View(beer_style_overall)

#Query
query = sqldf("select beer_style,review_overall,score from beer_style_overall where score>=80")

#saved results in csv file
write.csv(query,file ='C:/Users/Kanchan/Desktop/Evolent_Assignment/compareReview_with_overallreview.csv', row.names=FALSE)


#Question 7: find similar beer drinkers by using written reviews only (Not completed need some more time)

beer_drinkers<-data.frame(original_data$beer_name,original_data$review_profileName,SA)
View(beer_drinkers)
