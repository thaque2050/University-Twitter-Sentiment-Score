library(tidytext)
library(dplyr)
library(stringr)
library(sentimentr)
library(ggplot2)
library(ggrepel)
library(reshape2)
library(gridExtra)

sentiment_score<-function(list_reviews){
sentiment_df2<-data.frame()
sentiment_analysis<-data.frame()
for (t in 1:length(list_reviews)) {

sentiment_df<-0
sentiment_df<-list_reviews[[t]]$value%>%sentiment()

set.seed(123)

for (i in 1:nrow(sentiment_df)){
  if(sentiment_df$sentiment[i]>.1){
    sentiment_df$sentiment_final[i]="Positive"
  }
  else{
    sentiment_df$sentiment_final[i]="Negative"
  }
}
  number_of_positive<-nrow(sentiment_df[which(sentiment_df$sentiment_final=="Positive"),])
  number_of_negative<-nrow(sentiment_df[which(sentiment_df$sentiment_final=="Negative"),])
  sentiment_analysis[t,1]<-t
  sentiment_analysis[t,2]<-number_of_positive
  sentiment_analysis[t,3]<-number_of_negative
  number_of_positive<-NULL
  number_of_negative<-NULL
}

colnames(sentiment_analysis)<-c("University_Name","Positive Reviews","Negative_Reviews")
sentiment_analysis2 <- melt(sentiment_analysis, id=c("University_Name"))
colnames(sentiment_analysis2)<-c("University_Name","Review_Type","Score")
sentiment_analysis2$University_Name<-as.factor(sentiment_analysis2$University_Name)
sentiment_analysis2$Review_Type<-as.factor(sentiment_analysis2$Review_Type)
sentiment_analysis2
}



#FINAL ANALYSIS
University_Name<-c('purdue', 'UMD','iowa','indiana','minnesota','illinois','michigan','michigan state',
                   'nebraska','northwestern','ohio state','penn state','rutgers','wisconsin')
sentiment_data$University_Name<-University_Name

total_data<-sentiment_data%>%group_by(University_Name)%>%summarise(total=sum(Score))

sentiment_data<-merge(x = sentiment_data,y = total_data,by = "University_Name")
sentiment_data<-sentiment_data%>%mutate(Percentage_Reviews=round((Score/total)*100,1))

c1<-sentiment_data%>%ggplot()+geom_col(aes(x=University_Name,y=Score,fill=Review_Type))+
  theme(axis.text.x = element_text(angle = 90))
  
c2<-sentiment_data%>%ggplot()+geom_col(aes(x=University_Name,y=Percentage_Reviews,fill=Review_Type))+
  theme(axis.text.x = element_text(angle = 90))

grid.arrange(c1,c2,nrow=2)
