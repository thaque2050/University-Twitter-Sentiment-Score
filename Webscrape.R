library(rvest)
library(dplyr)
library(stringi)
library(tidyr)

URLS<-c('https://www.unigo.com/colleges/purdue-university-main-campus/',
'https://www.unigo.com/colleges/university-of-maryland-college-park/',
'https://www.unigo.com/colleges/university-of-iowa/',
'https://www.unigo.com/colleges/indiana-university-bloomington/',
'https://www.unigo.com/colleges/university-of-minnesota-twin-cities/',
'https://www.unigo.com/colleges/university-of-illinois-at-urbana-champaign/',
'https://www.unigo.com/colleges/university-of-michigan-ann-arbor/',
'https://www.unigo.com/colleges/michigan-state-university/',
'https://www.unigo.com/colleges/university-of-nebraska-lincoln/',
'https://www.unigo.com/colleges/northwestern-university/',
'https://www.unigo.com/colleges/ohio-state-university-main-campus/',
'https://www.unigo.com/colleges/pennsylvania-state-university-main-campus/',
'https://www.unigo.com/colleges/rutgers-university-new-brunswick/',
'https://www.unigo.com/colleges/university-of-wisconsin-madison/')

#Specifying the url for desired website to be scrapped
scrap_reviews<-function(url_input,n){
number_of_elements<-length(url_input)
url<-0
url2<-list()
set.seed(123)
for (j in 1:number_of_elements) {
url<-0
for (i in 1:n){
  url[i] <- paste(url_input[j],i,sep = '')
}
  url<-as.data.frame(url)
  url2[[j]]<-url
}

#Reading data from the website and storing information in a list
review_data_html<-list()
reviews_data2<-list()
for (k in 1:length(url2)) {
  temp_url=url2[[k]]
for (i in 1:n){
  derby<-read_html(as.character(temp_url[i,1]))
  review_data_html[[i]] <- derby%>%html_nodes('#reviewAnswerList p')%>%html_text()
}
  reviews_data<-as.data.frame(stri_list2matrix(review_data_html, byrow=TRUE))
  reviews_data<-reviews_data%>%gather(na.rm=TRUE)%>%unique()
  reviews_data2[[k]]<-reviews_data
}
  reviews_data2
}
