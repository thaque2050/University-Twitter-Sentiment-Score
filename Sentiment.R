library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(ggrepel)



#COUNT WORDS FOR SENTIMENT BY DICTIONARIES
corpus<-Corpus(VectorSource(reviews_data2$value))
corpus <- tm_map(x = corpus,removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
myStopwords <- c(stopwords(kind = 'en'),"students","school","university","college")
corpus <- tm_map(corpus, removeWords, myStopwords) 
df<-data.frame(text = sapply(corpus, as.character), stringsAsFactors = FALSE)
reviews_data3<-cbind(reviews_data2,df)


tokens <- data_frame(text = reviews_data3$text) %>% unnest_tokens(word, text)
tokens %>%inner_join(get_sentiments("nrc")) %>% count(sentiment)
tokens %>%inner_join(get_sentiments("bing")) %>% count(sentiment)




#ANALYSIS USING ADVANCED SENTIMENTR
#df<-get_sentences2(reviews_data2$value)
#ll2<-as.data.frame(stri_list2matrix(read_news_headline, byrow=TRUE))

word_df<-extract_sentiment_terms(reviews_data2$value)
positive_df<-as.data.frame(stri_list2matrix(word_df$positive, byrow=TRUE))

positive_df$text<- do.call(paste0, positive_df[1:15]) 




