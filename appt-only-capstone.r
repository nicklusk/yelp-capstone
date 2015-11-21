

## read-in business json file
install.packages("jsonlite")
library("jsonlite")
busJsonOriginal <- stream_in(file("yelp_academic_dataset_business.json"))

## create rds backup of original business json variable
saveRDS(busJsonOriginal, "busJsonOriginal.rds")

## flatten business json data and convert categories to string
install.packages("dplyr")
library(dplyr)
busJsonOriginal.flat <- flatten(busJsonOriginal)
busJsonOriginal.cat <- mutate(busJsonOriginal.flat, cat2=toString(categories[[1]]))

## filter out only businesses that are appointment only and create barplot by stars
appointmentBIZ <- filter(busJsonOriginal.cat, `attributes.By Appointment Only` == TRUE)
stars.BIZ.appt.TRUE <- table(appointmentBIZ$stars)
barplot(stars.BIZ.appt.TRUE, main="By Appointment Only = TRUE")

## convert NA to false in appointment only category and filter all other businesses into seperate dataset and create barplot by stars
BIZ.appt.na.to.false <- busJsonOriginal.cat
BIZ.appt.na.to.false$`attributes.By Appointment Only`[is.na(BIZ.appt.na.to.false$`attributes.By Appointment Only`)] <- 'FALSE'
appt.BIZ.false <- filter(BIZ.appt.na.to.false, `attributes.By Appointment Only` != TRUE)
stars.BIZ.appt.FALSE <- table(appt.BIZ.false$stars)
barplot(stars.BIZ.appt.FALSE, main="By Appointment Only = FALSE/NA")

## create a subset of review dataset containing only reviews of appointment only businesses
apptBusinessIDs <- appointmentBIZ$business_id
revJsonOriginal <- stream_in(file("yelp_academic_dataset_review.json"))
saveRDS(revJsonOriginal, "revJsonOriginal.rds")
apptReviews <- subset(revJsonOriginal, business_id %in% apptBusinessIDs)
saveRDS(apptReviews, "apptReviews.rds")
apptReviewsText <- apptReviews$text
saveRDS(apptReviewsText, "apptReviewsText.rds")

## begin data transformation and mining of appointment only review text
install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
apptReviewsTextCorpus <- Corpus(VectorSource(apptReviewsText))
saveRDS(apptReviewsTextCorpus, "apptReviewsTextCorpus.rds")
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, removePunctuation)
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, removeNumbers)
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, tolower)
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, removeWords, stopwords("english"))
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, stemDocument)
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, stripWhitespace)
apptReviewsTextCorpus <- tm_map(apptReviewsTextCorpus, PlainTextDocument)
saveRDS(apptReviewsTextCorpus, "apptReviewsTextCorpusProcessed.rds")

## Create document term matrix, sort frequent terms and write output to csv file
dtmReviews <- DocumentTermMatrix(apptReviewsTextCorpus)
saveRDS(dtmReviews, "dtmReviews.rds")
freqReviewTerms <- sort(colSums(as.matrix(dtmReviews)), decreasing=TRUE)
write.csv(freqReviewTerms, file = "freqReviewTerms.csv")
saveRDS(freqReviewTerms, "freqReviewTerms.rds")

## repeat freq terms process for tips data
## read-in tips json file
tipsJsonOriginal <- stream_in(file("yelp_academic_dataset_tip.json"))
## create rds backup of original tips json variable
saveRDS(tipsJsonOriginal, "tipsJsonOriginal.rds")

## create a subset of tips dataset containing only tips of appointment only businesses
apptTips <- subset(tipsJsonOriginal, business_id %in% apptBusinessIDs)
saveRDS(apptTips, "apptTips.rds")
apptTipsText <- apptTips$text
saveRDS(apptTipsText, "apptTipsText.rds")

## begin data transformation and mining of appointment only tips text
apptTipsTextCorpus <- Corpus(VectorSource(apptTipsText))
saveRDS(apptTipsTextCorpus, "apptTipsTextCorpus.rds")
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, removePunctuation)
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, removeNumbers)
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, content_transformer(tolower))
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, removeWords, stopwords("english"))
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, stemDocument, language = "english")
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, stripWhitespace)
apptTipsTextCorpus <- tm_map(apptTipsTextCorpus, PlainTextDocument)

## Create document term matrix, sort frequent terms and write output to csv file
dtmTips <- DocumentTermMatrix(apptTipsTextCorpus)
saveRDS(dtmTips, "dtmTips.rds")
freqTipsTerms <- sort(colSums(as.matrix(dtmTips)), decreasing=TRUE)
saveRDS(freqTipsTerms, "freqTipsTerms.rds")
write.csv(freqTipsTerms, file = "freqTipsTerms.csv")

## create subsets of all appointment only businesses by star rating
appointmentBIZoneStar <- appointmentBIZ[appointmentBIZ$stars == 1.0,]
appointmentBIZoneHalfStar <- appointmentBIZ[appointmentBIZ$stars == 1.5,]
appointmentBIZtwoStar <- appointmentBIZ[appointmentBIZ$stars == 2.0,]
appointmentBIZtwoHalfStar <- appointmentBIZ[appointmentBIZ$stars == 2.5,]
appointmentBIZthreeStar <- appointmentBIZ[appointmentBIZ$stars == 3.0,]
appointmentBIZthreeHalfStar <- appointmentBIZ[appointmentBIZ$stars == 3.5,]
appointmentBIZfourStar <- appointmentBIZ[appointmentBIZ$stars == 4.0,]
appointmentBIZfourHalfStar <- appointmentBIZ[appointmentBIZ$stars == 4.5,]
appointmentBIZfiveStar <- appointmentBIZ[appointmentBIZ$stars == 5.0,]

## repeat frequent term process this time subsetting each result by star rating
## one star frequent terms
appointmentBIZOneStarIDs <- appointmentBIZoneStar$business_id
apptReviewsOneStar <- subset(revJsonOriginal, business_id %in% appointmentBIZOneStarIDs)
saveRDS(apptReviewsOneStar, "apptReviewsOneStar.rds")
apptReviewsTextOneStar <- apptReviewsOneStar$text
saveRDS(apptReviewsTextOneStar, "apptReviewsTextOneStar.rds")
apptReviewsTextCorpusOneStar <- Corpus(VectorSource(apptReviewsTextOneStar))
saveRDS(apptReviewsTextCorpusOneStar, "apptReviewsTextCorpusOneStar.rds")
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, removePunctuation)
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, removeNumbers)
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, content_transformer(tolower))
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, removeWords, stopwords("english"))
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, stemDocument, language = "english")
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, stripWhitespace)
apptReviewsTextCorpusOneStar <- tm_map(apptReviewsTextCorpusOneStar, PlainTextDocument)
dtmOneStar <- DocumentTermMatrix(apptReviewsTextCorpusOneStar)
saveRDS(dtmOneStar, "dtmOneStar.rds")
freqOneStar <- sort(colSums(as.matrix(dtmOneStar)), decreasing=TRUE)
saveRDS(freqOneStar, "freqOneStar.rds")
write.csv(freqOneStar, file = "freqOneStar.csv")

## one half star frequent terms
appointmentBIZOneHalfStarIDs <- appointmentBIZOneHalfStar$business_id
apptReviewsOneHalfStar <- subset(revJsonOriginal, business_id %in% appointmentBIZOneHalfStarIDs)
saveRDS(apptReviewsOneHalfStar, "apptReviewsOneHalfStar.rds")
apptReviewsTextOneHalfStar <- apptReviewsOneHalfStar$text
saveRDS(apptReviewsTextOneHalfStar, "apptReviewsTextOneHalfStar.rds")
apptReviewsTextCorpusOneHalfStar <- Corpus(VectorSource(apptReviewsTextOneHalfStar))
saveRDS(apptReviewsTextCorpusOneHalfStar, "apptReviewsTextCorpusOneHalfStar.rds")
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, removePunctuation)
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, removeNumbers)
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, content_transformer(tolower))
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, removeWords, stopwords("english"))
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, stemDocument, language = "english")
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, stripWhitespace)
apptReviewsTextCorpusOneHalfStar <- tm_map(apptReviewsTextCorpusOneHalfStar, PlainTextDocument)
dtmOneHalfStar <- DocumentTermMatrix(apptReviewsTextCorpusOneHalfStar)
saveRDS(dtmOneHalfStar, "dtmOneHalfStar.rds")
freqOneHalfStar <- sort(colSums(as.matrix(dtmOneHalfStar)), decreasing=TRUE)
saveRDS(freqOneHalfStar, "freqOneHalfStar.rds")
write.csv(freqOneHalfStar, file = "freqOneHalfStar.csv")

## two star frequent terms
appointmentBIZTwoStarIDs <- appointmentBIZTwoStar$business_id
apptReviewsTwoStar <- subset(revJsonOriginal, business_id %in% appointmentBIZTwoStarIDs)
saveRDS(apptReviewsTwoStar, "apptReviewsTwoStar.rds")
apptReviewsTextTwoStar <- apptReviewsTwoStar$text
saveRDS(apptReviewsTextTwoStar, "apptReviewsTextTwoStar.rds")
apptReviewsTextCorpusTwoStar <- Corpus(VectorSource(apptReviewsTextTwoStar))
saveRDS(apptReviewsTextCorpusTwoStar, "apptReviewsTextCorpusTwoStar.rds")
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, removePunctuation)
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, removeNumbers)
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, content_transformer(tolower))
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, removeWords, stopwords("english"))
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, stemDocument, language = "english")
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, stripWhitespace)
apptReviewsTextCorpusTwoStar <- tm_map(apptReviewsTextCorpusTwoStar, PlainTextDocument)
dtmTwoStar <- DocumentTermMatrix(apptReviewsTextCorpusTwoStar)
saveRDS(dtmTwoStar, "dtmTwoStar.rds")
freqTwoStar <- sort(colSums(as.matrix(dtmTwoStar)), decreasing=TRUE)
saveRDS(freqTwoStar, "freqTwoStar.rds")
write.csv(freqTwoStar, file = "freqTwoStar.csv")

## two half star frequent terms
appointmentBIZTwoHalfStarIDs <- appointmentBIZTwoHalfStar$business_id
apptReviewsTwoHalfStar <- subset(revJsonOriginal, business_id %in% appointmentBIZTwoHalfStarIDs)
saveRDS(apptReviewsTwoHalfStar, "apptReviewsTwoHalfStar.rds")
apptReviewsTextTwoHalfStar <- apptReviewsTwoHalfStar$text
saveRDS(apptReviewsTextTwoHalfStar, "apptReviewsTextTwoHalfStar.rds")
apptReviewsTextCorpusTwoHalfStar <- Corpus(VectorSource(apptReviewsTextTwoHalfStar))
saveRDS(apptReviewsTextCorpusTwoHalfStar, "apptReviewsTextCorpusTwoHalfStar.rds")
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, removePunctuation)
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, removeNumbers)
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, content_transformer(tolower))
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, removeWords, stopwords("english"))
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, stemDocument, language = "english")
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, stripWhitespace)
apptReviewsTextCorpusTwoHalfStar <- tm_map(apptReviewsTextCorpusTwoHalfStar, PlainTextDocument)
dtmTwoHalfStar <- DocumentTermMatrix(apptReviewsTextCorpusTwoHalfStar)
saveRDS(dtmTwoHalfStar, "dtmTwoHalfStar.rds")
freqTwoHalfStar <- sort(colSums(as.matrix(dtmTwoHalfStar)), decreasing=TRUE)
saveRDS(freqTwoHalfStar, "freqTwoHalfStar.rds")
write.csv(freqTwoHalfStar, file = "freqTwoHalfStar.csv")

## three star frequent terms
appointmentBIZThreeStarIDs <- appointmentBIZThreeStar$business_id
apptReviewsThreeStar <- subset(revJsonOriginal, business_id %in% appointmentBIZThreeStarIDs)
saveRDS(apptReviewsThreeStar, "apptReviewsThreeStar.rds")
apptReviewsTextThreeStar <- apptReviewsThreeStar$text
saveRDS(apptReviewsTextThreeStar, "apptReviewsTextThreeStar.rds")
apptReviewsTextCorpusThreeStar <- Corpus(VectorSource(apptReviewsTextThreeStar))
saveRDS(apptReviewsTextCorpusThreeStar, "apptReviewsTextCorpusThreeStar.rds")
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, removePunctuation)
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, removeNumbers)
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, content_transformer(tolower))
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, removeWords, stopwords("english"))
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, stemDocument, language = "english")
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, stripWhitespace)
apptReviewsTextCorpusThreeStar <- tm_map(apptReviewsTextCorpusThreeStar, PlainTextDocument)
dtmThreeStar <- DocumentTermMatrix(apptReviewsTextCorpusThreeStar)
saveRDS(dtmThreeStar, "dtmThreeStar.rds")
freqThreeStar <- sort(colSums(as.matrix(dtmThreeStar)), decreasing=TRUE)
saveRDS(freqThreeStar, "freqThreeStar.rds")
write.csv(freqThreeStar, file = "freqThreeStar.csv")

## three half star frequent terms
appointmentBIZThreeHalfStarIDs <- appointmentBIZThreeHalfStar$business_id
apptReviewsThreeHalfStar <- subset(revJsonOriginal, business_id %in% appointmentBIZThreeHalfStarIDs)
saveRDS(apptReviewsThreeHalfStar, "apptReviewsThreeHalfStar.rds")
apptReviewsTextThreeHalfStar <- apptReviewsThreeHalfStar$text
saveRDS(apptReviewsTextThreeHalfStar, "apptReviewsTextThreeHalfStar.rds")
apptReviewsTextCorpusThreeHalfStar <- Corpus(VectorSource(apptReviewsTextThreeHalfStar))
saveRDS(apptReviewsTextCorpusThreeHalfStar, "apptReviewsTextCorpusThreeHalfStar.rds")
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, removePunctuation)
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, removeNumbers)
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, content_transformer(tolower))
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, removeWords, stopwords("english"))
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, stemDocument, language = "english")
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, stripWhitespace)
apptReviewsTextCorpusThreeHalfStar <- tm_map(apptReviewsTextCorpusThreeHalfStar, PlainTextDocument)
dtmThreeHalfStar <- DocumentTermMatrix(apptReviewsTextCorpusThreeHalfStar)
saveRDS(dtmThreeHalfStar, "dtmThreeHalfStar.rds")
freqThreeHalfStar <- sort(colSums(as.matrix(dtmThreeHalfStar)), decreasing=TRUE)
saveRDS(freqThreeHalfStar, "freqThreeHalfStar.rds")
write.csv(freqThreeHalfStar, file = "freqThreeHalfStar.csv")

## four star frequent terms
appointmentBIZFourStarIDs <- appointmentBIZFourStar$business_id
apptReviewsFourStar <- subset(revJsonOriginal, business_id %in% appointmentBIZFourStarIDs)
saveRDS(apptReviewsFourStar, "apptReviewsFourStar.rds")
apptReviewsTextFourStar <- apptReviewsFourStar$text
saveRDS(apptReviewsTextFourStar, "apptReviewsTextFourStar.rds")
apptReviewsTextCorpusFourStar <- Corpus(VectorSource(apptReviewsTextFourStar))
saveRDS(apptReviewsTextCorpusFourStar, "apptReviewsTextCorpusFourStar.rds")
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, removePunctuation)
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, removeNumbers)
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, content_transformer(tolower))
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, removeWords, stopwords("english"))
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, stemDocument, language = "english")
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, stripWhitespace)
apptReviewsTextCorpusFourStar <- tm_map(apptReviewsTextCorpusFourStar, PlainTextDocument)
dtmFourStar <- DocumentTermMatrix(apptReviewsTextCorpusFourStar)
saveRDS(dtmFourStar, "dtmFourStar.rds")
freqFourStar <- sort(colSums(as.matrix(dtmFourStar)), decreasing=TRUE)
saveRDS(freqFourStar, "freqFourStar.rds")
write.csv(freqFourStar, file = "freqFourStar.csv")

## four half star frequent terms
appointmentBIZFourHalfStarIDs <- appointmentBIZFourHalfStar$business_id
apptReviewsFourHalfStar <- subset(revJsonOriginal, business_id %in% appointmentBIZFourHalfStarIDs)
saveRDS(apptReviewsFourHalfStar, "apptReviewsFourHalfStar.rds")
apptReviewsTextFourHalfStar <- apptReviewsFourHalfStar$text
saveRDS(apptReviewsTextFourHalfStar, "apptReviewsTextFourHalfStar.rds")
apptReviewsTextCorpusFourHalfStar <- Corpus(VectorSource(apptReviewsTextFourHalfStar))
saveRDS(apptReviewsTextCorpusFourHalfStar, "apptReviewsTextCorpusFourHalfStar.rds")
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, removePunctuation)
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, removeNumbers)
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, content_transformer(tolower))
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, removeWords, stopwords("english"))
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, stemDocument, language = "english")
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, stripWhitespace)
apptReviewsTextCorpusFourHalfStar <- tm_map(apptReviewsTextCorpusFourHalfStar, PlainTextDocument)
dtmFourHalfStar <- DocumentTermMatrix(apptReviewsTextCorpusFourHalfStar)
saveRDS(dtmFourHalfStar, "dtmFourHalfStar.rds")
freqFourHalfStar <- sort(colSums(as.matrix(dtmFourHalfStar)), decreasing=TRUE)
saveRDS(freqFourHalfStar, "freqFourHalfStar.rds")
write.csv(freqFourHalfStar, file = "freqFourHalfStar.csv")

## five star frequent terms
appointmentBIZFiveStarIDs <- appointmentBIZFiveStar$business_id
apptReviewsFiveStar <- subset(revJsonOriginal, business_id %in% appointmentBIZFiveStarIDs)
saveRDS(apptReviewsFiveStar, "apptReviewsFiveStar.rds")
apptReviewsTextFiveStar <- apptReviewsFiveStar$text
saveRDS(apptReviewsTextFiveStar, "apptReviewsTextFiveStar.rds")
apptReviewsTextCorpusFiveStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
saveRDS(apptReviewsTextCorpusFiveStar, "apptReviewsTextCorpusFiveStar.rds")
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, removePunctuation)
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, removeNumbers)
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, content_transformer(tolower))
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, removeWords, stopwords("english"))
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, stemDocument, language = "english")
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, stripWhitespace)
apptReviewsTextCorpusFiveStar <- tm_map(apptReviewsTextCorpusFiveStar, PlainTextDocument)
dtmFiveStar <- DocumentTermMatrix(apptReviewsTextCorpusFiveStar)
saveRDS(dtmFiveStar, "dtmFiveStar.rds")
freqFiveStar <- sort(colSums(as.matrix(dtmFiveStar)), decreasing=TRUE)
saveRDS(freqFiveStar, "freqFiveStar.rds")
write.csv(freqFiveStar, file = "freqFiveStar.csv")

## identify frequent trigrams of the review text subsets
install.packages("RWeka")
library(RWeka)
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) # 3 word ngrams

## one star business reviews frequent trigrams
apptReviewsTextOneStar <- readRDS("apptReviewsTextOneStar.rds")
apptReviewsTextOneStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, removePunctuation)
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, removeNumbers)
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, content_transformer(tolower))
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, removeWords, stopwords("english"))
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, stemDocument, language = "english")
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, stripWhitespace)
apptReviewsTextOneStar <- tm_map(apptReviewsTextOneStar, PlainTextDocument)
dtmRevTrigramsOneStar <- DocumentTermMatrix(apptReviewsTextOneStar, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsOneStar, "dtmRevTrigramsOneStar.rds")
freqDtmRevTrigramsOneStar <- sort(colSums(as.matrix(dtmRevTrigramsOneStar)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsOneStar, "freqDtmRevTrigramsOneStar.rds")
write.csv(freqDtmRevTrigramsOneStar, file = "freqDtmRevTrigramsOneStar.csv")

## one half star business reviews frequent trigrams
apptReviewsTextOneHalfStar <- readRDS("apptReviewsTextOneHalfStar.rds")
apptReviewsTextOneHalfStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, removePunctuation)
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, removeNumbers)
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, content_transformer(tolower))
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, removeWords, stopwords("english"))
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, stemDocument, language = "english")
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, stripWhitespace)
apptReviewsTextOneHalfStar <- tm_map(apptReviewsTextOneHalfStar, PlainTextDocument)
dtmRevTrigramsOneHalfStar <- DocumentTermMatrix(apptReviewsTextOneHalfStar, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsOneHalfStar, "dtmRevTrigramsOneHalfStar.rds")
freqDtmRevTrigramsOneHalfStar <- sort(colSums(as.matrix(dtmRevTrigramsOneHalfStar)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsOneHalfStar, "freqDtmRevTrigramsOneHalfStar.rds")
write.csv(freqDtmRevTrigramsOneHalfStar, file = "freqDtmRevTrigramsOneHalfStar.csv")

## two star business reviews frequent trigrams
apptReviewsTextTwoStar <- readRDS("apptReviewsTextTwoStar.rds")
apptReviewsTextTwoStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, removePunctuation)
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, removeNumbers)
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, content_transformer(tolower))
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, removeWords, stopwords("english"))
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, stemDocument, language = "english")
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, stripWhitespace)
apptReviewsTextTwoStar <- tm_map(apptReviewsTextTwoStar, PlainTextDocument)
dtmRevTrigramsTwoStar <- DocumentTermMatrix(apptReviewsTextTwoStar, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsTwoStar, "dtmRevTrigramsTwoStar.rds")
freqDtmRevTrigramsTwoStar <- sort(colSums(as.matrix(dtmRevTrigramsTwoStar)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsTwoStar, "freqDtmRevTrigramsTwoStar.rds")
write.csv(freqDtmRevTrigramsTwoStar, file = "freqDtmRevTrigramsTwoStar.csv")

## two half star business reviews frequent trigrams
apptReviewsTextTwoHalfStar <- readRDS("apptReviewsTextTwoHalfStar.rds")
apptReviewsTextTwoHalfStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, removePunctuation)
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, removeNumbers)
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, content_transformer(tolower))
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, removeWords, stopwords("english"))
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, stemDocument, language = "english")
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, stripWhitespace)
apptReviewsTextTwoHalfStar <- tm_map(apptReviewsTextTwoHalfStar, PlainTextDocument)
dtmRevTrigramsTwoHalfStar <- DocumentTermMatrix(apptReviewsTextTwoHalfStar, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsTwoHalfStar, "dtmRevTrigramsTwoHalfStar.rds")
freqDtmRevTrigramsTwoHalfStar <- sort(colSums(as.matrix(dtmRevTrigramsTwoHalfStar)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsTwoHalfStar, "freqDtmRevTrigramsTwoHalfStar.rds")
write.csv(freqDtmRevTrigramsTwoHalfStar, file = "freqDtmRevTrigramsTwoHalfStar.csv")

## three star business reviews frequent trigrams
apptReviewsTextThreeStar <- readRDS("apptReviewsTextThreeStar.rds")
apptReviewsTextThreeStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, removePunctuation)
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, removeNumbers)
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, content_transformer(tolower))
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, removeWords, stopwords("english"))
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, stemDocument, language = "english")
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, stripWhitespace)
apptReviewsTextThreeStar <- tm_map(apptReviewsTextThreeStar, PlainTextDocument)
dtmRevTrigramsThreeStar <- DocumentTermMatrix(apptReviewsTextThreeStar, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsThreeStar, "dtmRevTrigramsThreeStar.rds")
freqDtmRevTrigramsThreeStar <- sort(colSums(as.matrix(dtmRevTrigramsThreeStar)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsThreeStar, "freqDtmRevTrigramsThreeStar.rds")
write.csv(freqDtmRevTrigramsThreeStar, file = "freqDtmRevTrigramsThreeStar.csv")

## three half star business reviews frequent trigrams
apptReviewsTextThreeHalfStar <- readRDS("apptReviewsTextThreeHalfStar.rds")
apptReviewsTextThreeHalfStar <- Corpus(VectorSource(apptReviewsTextFiveStar))
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, removePunctuation)
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, removeNumbers)
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, content_transformer(tolower))
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, removeWords, stopwords("english"))
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, stemDocument, language = "english")
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, stripWhitespace)
apptReviewsTextThreeHalfStar <- tm_map(apptReviewsTextThreeHalfStar, PlainTextDocument)
dtmRevTrigramsThreeHalfStar <- DocumentTermMatrix(apptReviewsTextThreeHalfStar, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsThreeHalfStar, "dtmRevTrigramsThreeHalfStar.rds")
freqDtmRevTrigramsThreeHalfStar <- sort(colSums(as.matrix(dtmRevTrigramsThreeHalfStar)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsThreeHalfStar, "freqDtmRevTrigramsThreeHalfStar.rds")
write.csv(freqDtmRevTrigramsThreeHalfStar, file = "freqDtmRevTrigramsThreeHalfStar.csv")

## four star business reviews frequent trigrams, broken into two sets due to memory constraints
apptReviewsTextFourStar <- readRDS("apptReviewsTextFourStar.rds")
apptReviewsTextFourStar1 <- Corpus(VectorSource(apptReviewsTextFourStar[1:3500]))
apptReviewsTextFourStar2 <- Corpus(VectorSource(apptReviewsTextFourStar[3501:6965]))
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, removePunctuation)
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, removeNumbers)
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, content_transformer(tolower))
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, removeWords, stopwords("english"))
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, stemDocument, language = "english")
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, stripWhitespace)
apptReviewsTextFourStar1 <- tm_map(apptReviewsTextFourStar1, PlainTextDocument)
dtmRevTrigramsFourStar1 <- DocumentTermMatrix(apptReviewsTextFourStar1, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsFourStar1, "dtmRevTrigramsFourStar1.rds")
freqDtmRevTrigramsFourStar1 <- sort(colSums(as.matrix(dtmRevTrigramsFourStar1)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsFourStar1, "freqDtmRevTrigramsFourStar1.rds")
write.csv(freqDtmRevTrigramsFourStar1, file = "freqDtmRevTrigramsFourStar1.csv")
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, removePunctuation)
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, removeNumbers)
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, content_transformer(tolower))
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, removeWords, stopwords("english"))
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, stemDocument, language = "english")
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, stripWhitespace)
apptReviewsTextFourStar2 <- tm_map(apptReviewsTextFourStar2, PlainTextDocument)
dtmRevTrigramsFourStar2 <- DocumentTermMatrix(apptReviewsTextFourStar2, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsFourStar2, "dtmRevTrigramsFourStar2.rds")
freqDtmRevTrigramsFourStar2 <- sort(colSums(as.matrix(dtmRevTrigramsFourStar2)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsFourStar2, "freqDtmRevTrigramsFourStar2.rds")
write.csv(freqDtmRevTrigramsFourStar2, file = "freqDtmRevTrigramsFourStar2.csv")

## four half star business reviews frequent trigrams, broken into two sets due to memory constraints
apptReviewsTextFourHalfStar <- readRDS("apptReviewsTextFourHalfStar.rds")
apptReviewsTextFourHalfStar1 <- Corpus(VectorSource(apptReviewsTextFourHalfStar[1:4250]))
apptReviewsTextFourHalfStar2 <- Corpus(VectorSource(apptReviewsTextFourHalfStar[4251:8526]))
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, removePunctuation)
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, removeNumbers)
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, content_transformer(tolower))
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, removeWords, stopwords("english"))
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, stemDocument, language = "english")
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, stripWhitespace)
apptReviewsTextFourHalfStar1 <- tm_map(apptReviewsTextFourHalfStar1, PlainTextDocument)
dtmRevTrigramsFourHalfStar1 <- DocumentTermMatrix(apptReviewsTextFourHalfStar1, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsFourHalfStar1, "dtmRevTrigramsFourHalfStar1.rds")
freqDtmRevTrigramsFourHalfStar1 <- sort(colSums(as.matrix(dtmRevTrigramsFourHalfStar1)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsFourHalfStar1, "freqDtmRevTrigramsFourHalfStar1.rds")
write.csv(freqDtmRevTrigramsFourHalfStar1, file = "freqDtmRevTrigramsFourHalfStar1.csv")
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, removePunctuation)
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, removeNumbers)
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, content_transformer(tolower))
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, removeWords, stopwords("english"))
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, stemDocument, language = "english")
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, stripWhitespace)
apptReviewsTextFourHalfStar2 <- tm_map(apptReviewsTextFourHalfStar2, PlainTextDocument)
dtmRevTrigramsFourHalfStar2 <- DocumentTermMatrix(apptReviewsTextFourHalfStar2, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsFourHalfStar2, "dtmRevTrigramsFourHalfStar2.rds")
freqDtmRevTrigramsFourHalfStar2 <- sort(colSums(as.matrix(dtmRevTrigramsFourHalfStar2)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsFourHalfStar2, "freqDtmRevTrigramsFourHalfStar2.rds")
write.csv(freqDtmRevTrigramsFourHalfStar2, file = "freqDtmRevTrigramsFourHalfStar2.csv")

## five star business reviews frequent trigrams, broken into two sets due to memory constraints
apptReviewsTextFiveStar <- readRDS("apptReviewsTextFiveStar.rds")
apptReviewsTextFiveStar1 <- Corpus(VectorSource(apptReviewsTextFiveStar[1:4500]))
apptReviewsTextFiveStar2 <- Corpus(VectorSource(apptReviewsTextFiveStar[4501:9997]))
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, removePunctuation)
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, removeNumbers)
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, content_transformer(tolower))
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, removeWords, stopwords("english"))
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, stemDocument, language = "english")
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, stripWhitespace)
apptReviewsTextFiveStar1 <- tm_map(apptReviewsTextFiveStar1, PlainTextDocument)
dtmRevTrigramsFiveStar1 <- DocumentTermMatrix(apptReviewsTextFiveStar1, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsFiveStar1, "dtmRevTrigramsFiveStar1.rds")
freqDtmRevTrigramsFiveStar1 <- sort(colSums(as.matrix(dtmRevTrigramsFiveStar1)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsFiveStar1, "freqDtmRevTrigramsFiveStar1.rds")
write.csv(freqDtmRevTrigramsFiveStar1, file = "freqDtmRevTrigramsFiveStar1.csv")
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, removePunctuation)
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, removeNumbers)
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, content_transformer(tolower))
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, removeWords, stopwords("english"))
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, stemDocument, language = "english")
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, stripWhitespace)
apptReviewsTextFiveStar2 <- tm_map(apptReviewsTextFiveStar2, PlainTextDocument)
dtmRevTrigramsFiveStar2 <- DocumentTermMatrix(apptReviewsTextFiveStar2, control = list(tokenize = TrigramTokenizer))
saveRDS(dtmRevTrigramsFiveStar2, "dtmRevTrigramsFiveStar2.rds")
freqDtmRevTrigramsFiveStar2 <- sort(colSums(as.matrix(dtmRevTrigramsFiveStar2)), decreasing=TRUE)
saveRDS(freqDtmRevTrigramsFiveStar2, "freqDtmRevTrigramsFiveStar2.rds")
write.csv(freqDtmRevTrigramsFiveStar2, file = "freqDtmRevTrigramsFiveStar2.csv")





