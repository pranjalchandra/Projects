
library(rvest)
library(dplyr)
library(stringr)
library(stringi)
library(tidyverse)

#Create a list of All The States

all_states<-list("up2007","up2012","uttarpradesh2017")

#Save all the links

scrapped_list<- lapply(all_states, function(w) {
  
  state_details<-read_html(paste0("http://myneta.info/",w,"/"))
  print(state_details)
  data.frame(id=state_details %>%
               rvest::html_nodes("a") %>%
               rvest::html_attr("href"),
             state=w)
  
})
#Step2: save the list in form of a dataframe

all_links_df <- do.call(rbind, scrapped_list)

#Scrape the constituencies

constituency_id<-all_links_df%>%
  filter(str_detect(all_links_df$id,"state_id")==TRUE)


#Code for extracting state name
#state=str_extract(all_links_df$state[2], "[a-zA-Z]{2}"))

df_candidate_id<-c("id","state")

for (i in 1:nrow(constituency_id)) {
  
  url<- toString (constituency_id[i,"id"])
  
  constituency<- read_html(paste0("http://myneta.info/",constituency_id[i,2],"/",url))
  
  in_progress_id<- data.frame(id=constituency %>%
                                rvest::html_nodes("div") %>%
                                rvest::html_nodes("table") %>%
                                rvest::html_nodes("tr") %>%
                                rvest::html_nodes(xpath="//td/a")%>%
                                html_attr("href"), 
                              state=constituency_id[i,2])
  
  df_candidate_id<- rbind(in_progress_id, df_candidate_id)
  
}

df_candidate_links<-  df_candidate_id%>%
  filter(str_detect(df_candidate_id$id, "candidate_id")==TRUE)


write.csv(df_candidate_links, paste0(getwd(),"\\df_candidate_links.csv"))
```

#Scrapping the Candidates
```{r}
#df_candidate_links<- read.csv("C:/Users/pranj/Documents/GitHub/final-project-pranjal/df_candidate_links.csv")

df_cand_l_07<-df_candidate_links %>% filter(state=="up2007")

all_cand_list<- c()

f_mv_a<-c()
f_immv_a<-c()

f_liab<-c()

f_crime_cases<-c()

#"main_title", "constituency", "party","so_do_Wo","age", "address", "address","ipc_details", "cases"


for (i in 1:nrow(df_cand_l_07)) {
  
  url<- toString (df_cand_l_07[i,"id"])
  
  my_neta<- read_html(paste0("http://myneta.info/","up2007","/",url))
  
  in_progress_candidates=data.frame(
    name= my_neta %>%  
      html_node('body') %>% 
      xml2::xml_find_all("//h2[contains(@class, 
                           'main-title')]") %>%
      rvest::html_text(),
    
    constituency= my_neta %>% 
      html_node('body') %>% 
      xml2::xml_find_all("//h5[contains(@class, '')]") %>% 
      rvest::html_text(),
    
    
    party= (my_neta %>% 
              html_node('body') %>% 
              xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
              rvest::html_text())[1],
    
    so_do_wo=(my_neta %>% 
                html_node('body') %>% 
                xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
                rvest::html_text())[2],
    
    age=(my_neta %>% 
           html_node('body') %>% 
           xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
           rvest::html_text())[3],
    
    address=(my_neta %>% 
               html_node('body') %>% 
               xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
               rvest::html_text())[4],
    
    year=df_cand_l_07[i,"state"]
    
  )
  
  
  all_cand_list<- rbind(in_progress_candidates, all_cand_list)
  
  #Criminal Cases
  
  ipc_details=data.frame(ipc_details=tail((my_neta %>% 
                                             html_node('body') %>% 
                                             html_node('tr')%>%
                                             rvest::html_nodes("td")%>%
                                             html_text()),1))
  
  
  cases=data.frame(cases=my_neta %>% 
                     html_node('body') %>% 
                     html_node('tr')%>%
                     rvest::html_nodes("td")%>%
                     rvest::html_nodes("tr")%>%
                     html_text())
  
  crime_cases=merge(in_progress_candidates$name, ipc_details)
  
  crime_cases=merge(cases,crime_cases)
  
  crime_cases=merge(crime_cases, in_progress_candidates$year)
  
  f_crime_cases=rbind(f_crime_cases, crime_cases)
  
  #assets and liabilities
  tables<-html_table(my_neta, fill=TRUE)
  
  n<- length(tables)
  n_3<- n-2
  for (i in n:n_3) {
    
    if(i==n-2) {
      movable_assets<-data.frame(tables[i])
      
      colnames(movable_assets)<- movable_assets[1,]
      
      movable_assets<- movable_assets[-1,-1]
    } else if(i==n-1){
      
      immovable_assets<-data.frame(tables[3])
      
      colnames(immovable_assets)<- immovable_assets[1,]
      
      immovable_assets<- immovable_assets[-1,-1]    
      
    } else if(i==n){
      
      liab<-data.frame(tables[4])
      
      colnames(liab)<- liab[1,]
      
      liab<- liab[-1,-1]    
      
    }
  }
  
  
  
  movable_assets<- merge(in_progress_candidates$name, movable_assets)
  
  movable_assets<- merge(in_progress_candidates$year, movable_assets)
  
  f_mv_a<-bind_rows(movable_assets, f_mv_a)
  
  immovable_assets<- merge(in_progress_candidates$name, immovable_assets)
  
  immovable_assets<- merge(in_progress_candidates$year, immovable_assets)
  
  f_immv_a<-bind_rows(immovable_assets, f_immv_a)
  
  liab<-merge(in_progress_candidates$name, liab)
  
  liab<-merge(in_progress_candidates$year, liab)
  
  f_liab<-bind_rows(liab,f_liab)
  
}

write.csv(all_cand_list, paste0(getwd(), "/2007_cand_list.csv"))
write.csv(f_crime_cases, paste0(getwd(), "/2007_crime_cases.csv"))
write.csv(f_mv_a, paste0(getwd(), "/2007_mv_a.csv"))
write.csv(f_immv_a, paste0(getwd(), "/2007_immv_a.csv"))
write.csv(f_liab, paste0(getwd(), "/2007_liab.csv"))

#2012 dataframe

df_cand_l_12<-df_candidate_links %>% filter(state=="up2012")


#"main_title", "constituency", "party","so_do_Wo","age", "address", "address","ipc_details", "cases"

#done till 4217

for (i in 4218:nrow(df_cand_l_12)) {
  
  url<- toString (df_cand_l_12[i,"id"])
  
  my_neta<- read_html(paste0("http://myneta.info/","up2012","/",url))
  
  in_progress_candidates=data.frame(
    name= my_neta %>%  
      html_node('body') %>% 
      xml2::xml_find_all("//h2[contains(@class, 
                           'main-title')]") %>%
      rvest::html_text(),
    
    constituency= my_neta %>% 
      html_node('body') %>% 
      xml2::xml_find_all("//h5[contains(@class, '')]") %>% 
      rvest::html_text(),
    
    
    party= (my_neta %>% 
              html_node('body') %>% 
              xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
              rvest::html_text())[1],
    
    so_do_wo=(my_neta %>% 
                html_node('body') %>% 
                xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
                rvest::html_text())[2],
    
    age=(my_neta %>% 
           html_node('body') %>% 
           xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
           rvest::html_text())[3],
    
    address=(my_neta %>% 
               html_node('body') %>% 
               xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
               rvest::html_text())[4],
    

    year=df_cand_l_12[i,"state"]
    
  )
  
  
  all_cand_list<- rbind(in_progress_candidates, all_cand_list)
  
  #Criminal Cases
  ipc_details=data.frame(ipc_details=tail((my_neta %>% 
                                             html_node('body') %>% 
                                             html_node('tr')%>%
                                             rvest::html_nodes("td")%>%
                                             html_text()),1))
  
  cases=data.frame(cases=my_neta %>% 
                     html_node('body') %>% 
                     html_node('tr')%>%
                     rvest::html_nodes("td")%>%
                     rvest::html_nodes("tr")%>%
                     html_text())
  
  crime_cases=merge(in_progress_candidates$name, ipc_details)
  
  crime_cases=merge(cases,crime_cases)
  
  crime_cases=merge(crime_cases, in_progress_candidates$year)
  
  f_crime_cases=rbind(f_crime_cases, crime_cases)
}

write.csv(all_cand_list, paste0(getwd(), "/2007_cand_list.csv"))
write.csv(f_crime_cases, paste0(getwd(), "/2007_crime_cases.csv"))


#2017 dataframe

df_cand_l_17<-df_candidate_links %>% filter(state=="uttarpradesh2017")

for (i in 1:nrow(df_cand_l_17)) {
  
  url<- toString (df_cand_l_17[i,"id"])
  
  my_neta<- read_html(paste0("http://myneta.info/","uttarpradesh2017","/",url))
  
  in_progress_candidates=data.frame(
    name= my_neta %>%  
      html_node('body') %>% 
      xml2::xml_find_all("//h2[contains(@class, 
                           'main-title')]") %>%
      rvest::html_text(),
    
    constituency= my_neta %>% 
      html_node('body') %>% 
      xml2::xml_find_all("//h5[contains(@class, '')]") %>% 
      rvest::html_text(),
    
    
    party= (my_neta %>% 
              html_node('body') %>% 
              xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
              rvest::html_text())[1],
    
    so_do_wo=(my_neta %>% 
                html_node('body') %>% 
                xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
                rvest::html_text())[2],
    
    age=(my_neta %>% 
           html_node('body') %>% 
           xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
           rvest::html_text())[3],
    
    address=(my_neta %>% 
               html_node('body') %>% 
               xml2::xml_find_all("//div[contains(@class, 
                           'grid_2 alpha')]")%>% 
               rvest::html_text())[4],
    
    year=df_cand_l_12[i,"state"]
    
  )
  
  
  all_cand_list<- rbind(in_progress_candidates, all_cand_list)
  
  #Criminal Cases
  ipc_details=data.frame(ipc_details=tail((my_neta %>% 
                                             html_node('body') %>% 
                                             html_node('tr')%>%
                                             rvest::html_nodes("td")%>%
                                             html_text()),1))
  
  cases=data.frame(cases=my_neta %>% 
                     html_node('body') %>% 
                     html_node('tr')%>%
                     rvest::html_nodes("td")%>%
                     rvest::html_nodes("tr")%>%
                     html_text())
  
  crime_cases=merge(in_progress_candidates$name, ipc_details)
  
  crime_cases=merge(cases,crime_cases)
  
  crime_cases=merge(crime_cases, in_progress_candidates$year)
  
  f_crime_cases=rbind(f_crime_cases, crime_cases)
}

write.csv(all_cand_list, paste0(getwd(), "/2007_cand_list.csv"))
write.csv(f_crime_cases, paste0(getwd(), "/2007_crime_cases.csv"))

