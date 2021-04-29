
library(tidyverse)
library(tidytext)
library(textdata)
library(udpipe)
library(ggraph)
library(igraph)
library(SnowballC)
library(tm)
library(gridExtra)
library(rvest)
library(xml2)
library(readtext)
library(docxtractr)
library(sentimentr)

setwd("C:/Users/pranj/Documents/GitHub/final-project-pranjal/")
path<-getwd()

dfs<- c(2010,2012,2014,2015,2016,2017,2018,2019)

dfs <- as.Date(as.character(dfs), format = "%Y")
dfs <- year(dfs)

comp_sentiment<- c()

for (i in dfs){
  
  temp=readtext(paste0(path, "/IMF Reports/imf_article_4_",i,".docx"))
  
  text_df=tibble(doc=i,text=temp$text)
  
  temp_sentiment<- unnest_tokens(text_df, sent_tokens, text, token="sentences")%>%
    get_sentences()%>%
    sentiment()
  
  comp_sentiment<- rbind(temp_sentiment,comp_sentiment)
  
}
#https://medium.com/@ODSC/an-introduction-to-sentence-level-sentiment-analysis-with-sentimentr-ac556bd7f75a


write.csv(comp_sentiment,paste0(path,"/comp_sentiment.csv"))

comp_text<- c()

for (i in dfs){
  
  temp=readtext(paste0(getwd(), "/IMF Reports/imf_article_4_",i,".docx"))
  
  temp$doc_id=i
  
  comp_text<-rbind(temp, comp_text)
}

df_udp <- udpipe(comp_text, "english")

df_udp$stem<- wordStem(df_udp$token, language = "porter")


frequency<-df_udp%>%  
  filter(!lemma %in% stop_words,
         !upos  %in% c("PUNCT", "CCONJ","NUM","SYM")) %>%
  #mutate_if(is.character, str_to_lower) %>% 
  document_term_frequencies(term = "token")%>%
  arrange(desc(freq))

