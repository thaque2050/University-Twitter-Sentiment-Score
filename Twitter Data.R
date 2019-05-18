library(dplyr)
library(purrr)
library(rtweet)
library(tidytext)
library(httpuv)
library(sentimentr)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

token<-create_token(app="ResearchProject2050",
                    consumer_key="w8jN35MpspikHiPhzcJxDNj0M",
                    consumer_secret="vJxJMf8bqBqs5m3qDrOpTCyrmjn1rJl4BAn66myfGzOeRsX2ry",
                    access_token="146052720-yq4fgOk9IEUjujUbbtIsQFYqJ1TaWnBFIpoRuLsu",
                    access_secret="s2Cz3LVFtjvamd47Cl1nJl1DEyFEki2kEZeyGErzK1Tg2")


srch_tweet<- search_tweets2("University of Maryland",n=10000,retryonratelimit=TRUE)
srch_tweet2<- search_tweets2("University of Chicago",n=50000,retryonratelimit=TRUE)


sentiment1<-get_sentences(srch_tweet$text)
sentiment2<-sentiment_by(sentiment1)
sentiment2<-sentimen(sentiment1)


#Term Document Matrix
docs <- Corpus(VectorSource(srch_tweet2$text))

#Text Transpormation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("university", "chicago", "https","tco")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=5000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))