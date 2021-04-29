
library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(RCurl)
library(readxl)
library(data.table)
  
url<- read_html("https://www.rbi.org.in/Scripts/AnnualPublications.aspx?head=Handbook+of+Statistics+on+Indian+States")

links<-data.frame(link=url %>%
                    rvest::html_nodes("div") %>%
                    rvest::html_nodes("table") %>%
                    rvest::html_nodes("tr") %>%
                    rvest::html_nodes(xpath="//td/a")%>%
                    html_attr("href"))%>%  
  filter(str_detect(link,"XLSX"))

write.csv(links, paste0(getwd(),"//links.csv"))

for (i in 1:nrow(links)) {
  temp = tempfile(fileext = ".xlsx")
  
  dataURL <- links$link[i]
  download.file(dataURL, destfile=temp, mode='wb')
  #https://stackoverflow.com/questions/42028408/read-in-online-xlsx-sheet-in-r
  
  sheetnames <- excel_sheets(temp)
  
  #save sheets in different lists
  mylist <- lapply(excel_sheets(temp), read_xlsx, path = temp)
  #https://stackoverflow.com/questions/49359587/import-excel-workbook-with-multiple-sheets
  
  # name the dataframes
  names(mylist) <- sheetnames
  
  #list2env(mylist,.GlobalEnv)
  
  for (w in seq_along(mylist))
  { 
    
    assign(colnames(data.frame(mylist[w]))[1], data.frame(mylist[w]))
    
    unit_df<-  (colnames(data.frame(mylist[w]))[1])[1]
  }
  
}

#finding all the different units across the dataframe
dfs<-mget(ls())

names(dfs)

#dfs<- dfs[-c(1,2,3,4,5,383,384,385,386)]

dfs<- dfs[-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,394,395,396,397,398,399,400,401,402,403)] #remove this

key<- c()

for (i in dfs){
  t<-data.frame(i)
  
  units<-data.frame(unit=t[1,1], unit_2=t[2,1],unit_3=t[3,1],df=colnames(t)[1] )
  
  key<- rbind(units, key)
  
}

key<-key[-c(1,2,3),]

write.csv(key,paste0(getwd(), "/key_gd.csv"))

#Create the final data frame to store values

state_ut=c("Andaman and Nicobar Islands","Andhra Pradesh","Arunachal Pradesh","Assam","Bihar","Chandigarh","Chhattisgarh","Dadra and Nagar Haveli","Daman and Diu","Delhi","Goa","Gujarat","Haryana","Himachal Pradesh","Jammu and Kashmir","Jharkhand","Karnataka","Kerala","Lakshadweep","Madhya Pradesh","Maharashtra","Manipur","Meghalaya","Mizoram","Nagaland","Odisha","Puducherry","Punjab","Rajasthan","Sikkim","Tamil Nadu","Tripura","Telangana","Uttar Pradesh","Uttarakhand","West Bengal","INDIA")

Year<- rep(c(1947:2020), length(state_ut))

final<- data.frame(cbind(state_ut, Year))

final$Year<-as.numeric(final$Year)


#cleaning of years-as-row dataframes

#Create dataframes that has year as rows
y_df<-key%>%
  filter(unit_2=="Year")%>%
  select(df)

#Subset the dataframes that have year as rows

y_list_df<-dfs[match(y_df[,1],names(dfs))]


for (i in y_list_df){
  
  t<-data.frame(i)
  
  name_df<-data.frame(str_split_fixed(str_to_lower(colnames(t)), "\\.",20))[,5:20]
  
  variable_name<- apply(name_df[1,], 1, 
                        function(x) paste(x[x!=""& x!="table" &x!="concld"& is.na(as.numeric(x)) &
                                              str_detect(x,"\\d")==FALSE], collapse = "_"))
  
  #https://stackoverflow.com/questions/47763452/r-collapsing-data-from-multiple-columns-into-one  
  
  colnames(t)<- t[2,]
  
  t<-t[-c(1,2),]
  
  #Find if the states are added over each other  
  
  if(any('Year'==t[1])==TRUE){
    
    #remove na row
    t<- t[-(which(is.na(t[1])==TRUE)),]
    
    #Find which row it starts getting repeated
    rep_r<-which(t[1]=="Year")
    
    #Find total rows
    t_rows<-nrow(t)
    
    #create new datframe
    n_t<-t[c(rep_r:t_rows),]
    
    #remove na column
    n_t<- n_t[,colSums(is.na(n_t)) !=nrow(n_t)]
    
    colnames(n_t)<-n_t[1,]
    
    n_t<- n_t[-1,]
    
    #remove rows from old_dataframe
    t<- t[-c(rep_r:t_rows),]
    
    t<- merge(n_t,t,by="Year")
    
  }
  
  #choose the base year and convert to numeric
  
  if(str_length(t[1,1])>4){
    
    t[,1]  <-str_sub(t[,1],1,4)
    
    t[,1]<-as.numeric(t[,1])
    
    t[,1]<-t[,1]+1
    
  }else{
    
    t[,1]<-as.numeric(t[,1])
    
    t[,1]<-t[,1]+1    
  }
  
  
  cl_names=colnames(t)
  
  n=length(cl_names)
  
  updated_t<-t%>%
    pivot_longer(cols=cl_names[2]:cl_names[n],
                 names_to='state_ut',
                 values_to=variable_name)
  
  #Align the names with the main dataframe names

    updated_t<-updated_t%>%
    mutate(state_ut=ifelse(state_ut=="All India"|
                             state_ut=="ALL INDIA"|
                             state_ut=="ALL INDIA (Average)"|
                             state_ut=="All States and UTs"|
                             state_ut=="All-India"|
                             state_ut=="INDIA"|
                             state_ut=="ALL INDIA","India",
                           ifelse(state_ut=="Uttar Pradesh-(E&W)","Uttar Pradesh",
                                  ifelse(state_ut=="Andaman & Nicobar"|
                                           state_ut=="Andaman & Nicobar Islands"|
                                           state_ut=="Andaman and Nicobar Islands"|
                                           state_ut=="Andaman & Nicobar Islands"|
                                           state_ut=="Andaman and Nicobar"|
                                           state_ut=="Andaman & nicobar Islands",
                                         "Andaman and Nicobar",
                                         ifelse(state_ut=="Andhra Pradesh"|
                                                  state_ut=="Andhra Pradesh#"|
                                                  state_ut=="Andhra Pradhesh","Andhra Pradesh",
                                                ifelse(state_ut=="Chattisgarh"|
                                                         state_ut=="Chhattisgarh"|
                                                         state_ut=="Chhattisgarh**","Chhattisgarh",
                                                       ifelse(state_ut=="Dadra & Nagar Haveli"|
                                                                state_ut=="Dadra & Nagar Haveli",
                                                              "Dadra and Nagar Haveli",
                                                              ifelse(state_ut=="Daman & Diu"|
                                                                       state_ut=="Daman & Diu","Daman and Diu",
                                                                     ifelse(state_ut=="Jammu & Kashmir",
                                                                            "Jammu and Kashmir", 
                                                                            ifelse(state_ut=="Jharkhand**",
                                                                                   "Jharkhand",
                                                                                   ifelse(state_ut=="Orissa",
                                                                                          "Odisha",
                                                                                          ifelse(state_ut=="Uttarakhand"|
                                                                                                   state_ut=="Uttarakhand**"|state_ut=="Uttarakhand",
                                                                                                 "Uttaranchal",state_ut))))))))))))
  
  
  
  final<-full_join(final, updated_t, by=c("Year","state_ut"))
  
}



#Select names of the dataframes that have state as the column

st_df<-key%>%
  filter((unit=="State/Union Territory"|(unit_2=="State/Union Territory")|
            (unit_2=="State/Union Territory/Zone")|
            (unit_2=="Region/State/Union Territory")|
            (unit_3=="State/Union Territory")|
            (unit_3=="Region/State/Union Territory"))&
           (str_detect(key$df,"T_88")==FALSE)&
           (str_detect(key$df,"T_130")==FALSE)&
           (str_detect(key$df,"T_11.")==FALSE)&
           (str_detect(key$df,"T153")==FALSE)&
           (str_detect(key$df,"T_14.")==FALSE)&
           (str_detect(key$df,"T_14C")==FALSE)&
           (str_detect(key$df,"T_127")==FALSE))%>%
  select(df)


#subset the dataframes that have state as the column
st_list_df<-dfs[match(st_df[,1],names(dfs))]



#Changing the column names of the files selected

for (j in st_list_df){
  
  trial<-data.frame(j)
  
  name_df<-data.frame(str_split_fixed(str_to_lower(colnames(trial)), "\\.",20))[,5:20]
  
  variable_name<- apply(name_df[1,], 1, 
                        function(x) paste(x[x!=""& x!="table" &x!="concld"& is.na(as.numeric(x)) &
                                              str_detect(x,"\\d")==FALSE], collapse = "_"))
  
  if(trial[1,1]=="State/Union Territory"){
    
    if(str_length(trial[1,2])>4){ #check if the length of year variable is greater than 4 digits
      
      
      n_cl=ncol(trial)
      
      colnames(trial)[1:n_cl]<-str_sub(trial[1,(2:n_cl)],1,4) #save the first 4 digits of the year
      
      colnames(trial)[1]<-trial[1,1] #save the first cell as the column name 
      
      
      trial<-trial[-1,]
      
    }else {
      
      
      colnames(trial)<-trial[1,] 
      trial<-trial[-1,]
      
    }
    
  }else if((trial[2,1]=="State/Union Territory")|
           (trial[2,1]=="State/Union Territory/Zone")|
           (trial[2,1]=="Region/State/Union Territory")){
    
    if(str_length(trial[2,2])>4){
      
      n_cl=ncol(trial)
      
      colnames(trial)[2:n_cl]<-str_sub(trial[2,(2:n_cl)],1,4)#save the last 4 digits of the year
      
      colnames(trial)[1]<-trial[2,1] #save the second cell of the first column as the column name
      
      trial<-trial[-c(1,2),]
      
    } else {
      colnames(trial)<- (trial[-1,])[1,]
      
      trial<-trial[-c(1,2),]
    }
    
  }else if((trial[3,1]=="State/Union Territory")|
           (trial[3,1]=="Region/State/Union Territory")) {
    
    if(str_length(trial[3,2])>4){
      
      n_cl=ncol(trial)
      
      colnames(trial)[2:n_cl]<-str_sub(trial[3,(2:n_cl)],1,4)#save the last 4 digits of the year
      
      colnames(trial)[1]<-trial[3,1] #save the third cell of the first column as the column name
      
      trial<-trial[-c(1,2,3),]
      
    } else {
      colnames(trial)<- (trial[-c(1,2),])[1,]
      trial<-trial[-c(1,2,3),] #remove the first three rows
      
    }
  }
  
  
  if(is.na(trial[1,1])==TRUE|
     trial[1,1]==" (Kg. Per Hectare)"|
     trial[1,1]=="(Kg. Per Hectare)"|
     trial[1,1]=="(Mega Watt)"|
     trial[1,1]=="(Thousand Kgs)"|
     trial[1,1]=="(Thousand Hectares)"|
     trial[1,1]=="(Thousand Bales)"|
     trial[1,1]==" (Thousand Tonnes)"|
     trial[1,1]=="(Kilowatt-Hour)"){
    
    colnames(trial)<- str_sub(trial[1,], 1,4)
    
    trial<-trial[-1,]
    
    colnames(trial)[1]<- "state"
    
  } else{
    
    trial=trial
  }
  
  
  cl_names<-colnames(trial)
  
  n=length(cl_names)
  
  
  trial[,2:n]<-lapply(trial[,2:n], as.numeric)
  
  updated_trial<-trial%>%
    pivot_longer(cols=cl_names[2]:cl_names[n],
                 names_to='Year',
                 values_to=variable_name)
  
  updated_trial<-updated_trial%>%
    rename("state_ut"=colnames(updated_trial)[1])
  
  updated_trial<-updated_trial%>%
    mutate(state_ut=ifelse(state_ut=="All India"|
                             state_ut=="ALL INDIA"|
                             state_ut=="ALL INDIA (Average)"|
                             state_ut=="All States and UTs"|
                             state_ut=="All-India"|
                             state_ut=="INDIA"|
                             state_ut=="ALL INDIA","India",
                           ifelse(state_ut=="Uttar Pradesh-(E&W)","Uttar Pradesh",
                                  ifelse(state_ut=="Andaman & Nicobar"|
                                           state_ut=="Andaman & Nicobar Islands"|
                                           state_ut=="Andaman and Nicobar Islands"|
                                           state_ut=="Andaman & Nicobar Islands"|
                                           state_ut=="Andaman and Nicobar"|
                                           state_ut=="Andaman & nicobar Islands",
                                         "Andaman and Nicobar",
                                         ifelse(state_ut=="Andhra Pradesh"|
                                                  state_ut=="Andhra Pradesh#"|
                                                  state_ut=="Andhra Pradhesh","Andhra Pradesh",
                                                ifelse(state_ut=="Chattisgarh"|
                                                         state_ut=="Chhattisgarh"|
                                                         state_ut=="Chhattisgarh**","Chhattisgarh",
                                                       ifelse(state_ut=="Dadra & Nagar Haveli"|
                                                                state_ut=="Dadra & Nagar Haveli",
                                                              "Dadra and Nagar Haveli",
                                                              ifelse(state_ut=="Daman & Diu"|
                                                                       state_ut=="Daman & Diu","Daman and Diu",
                                                                     ifelse(state_ut=="Jammu & Kashmir",
                                                                            "Jammu and Kashmir", 
                                                                            ifelse(state_ut=="Jharkhand**",
                                                                                   "Jharkhand",
                                                                                   ifelse(state_ut=="Orissa",
                                                                                          "Odisha",
                                                                                          ifelse(state_ut=="Uttarakhand"|
                                                                                                   state_ut=="Uttarakhand**"|state_ut=="Uttarakhand",
                                                                                                 "Uttaranchal",state_ut))))))))))))%>%
    filter(str_detect(state_ut,"Source")==FALSE&
             str_detect(state_ut,"Note")==FALSE&
             str_detect(state_ut,"Population")==FALSE&
             str_detect(state_ut,"Efforts")==FALSE&
             str_detect(state_ut,"Capital")==FALSE&
             str_detect(state_ut,"Figures")==FALSE&
             str_detect(state_ut,"figures")==FALSE&
             str_detect(state_ut,"Census")==FALSE&
             str_detect(state_ut,"inclusive")==FALSE&
             str_detect(state_ut,"Not Applicable")==FALSE&
             str_detect(state_ut,"September")==FALSE&
             str_detect(state_ut,"Date")==FALSE&
             str_detect(state_ut,"Cumulative")==FALSE&
             str_detect(state_ut,"Including")==FALSE&
             str_detect(state_ut,"Revised")==FALSE&
             str_detect(state_ut,"Missing")==FALSE&
             str_detect(state_ut,"Not Available")==FALSE&
             str_detect(state_ut,"Include")==FALSE&
             str_detect(state_ut,"census")==FALSE& 
             str_detect(state_ut,"included")==FALSE&
             str_detect(state_ut,"revised")==FALSE&
             str_detect(state_ut,"average")==FALSE&
             str_detect(state_ut,"inflation")==FALSE&
             str_detect(state_ut,"Nitrogen")==FALSE)
  
  
  updated_trial[,2]<- as.numeric(unlist(updated_trial[,2]))+1
  
  
  final<-full_join(final, updated_trial, by=c("state_ut","Year"))
  
up_data<- final%>%
  filter(state_ut=="Uttar Pradesh")

up_data<-up_data[,colSums(is.na(up_data)) !=nrow(up_data)]

up_data_final<-up_data%>%
  filter(Year!=1980 & Year!=2017)%>%
  rbind((up_data%>%
           filter(Year==1980|Year==2017))[1:2,])


write.csv(up_data_final, paste0(getwd(), "/up_final.csv"))

ind_shape1<- st_read(paste0(getwd(),"/Ind shape/IND_adm1.shp" ))

state_data_sf<- (st_sf(inner_join(ind_shape1,final, 
                                  by=c("NAME_1"="state_ut"))))


cl_names<- data.frame(column_n=unique(colnames(state_data_sf)))

cl_names<- cl_names%>%
  mutate(indicator=ifelse(
    str_detect(column_n,"bank")==TRUE|
      str_detect(column_n,"product")==TRUE|
      str_detect(column_n,"output")==TRUE|
      str_detect(column_n,"value")==TRUE|
      str_detect(column_n,"activity")==TRUE,"State Domestic Product",
    ifelse(str_detect(column_n,"emoluments")==TRUE|
             str_detect(column_n,"factories")==TRUE|
             str_detect(column_n,"employees")==TRUE|
             str_detect(column_n, "workers")==TRUE|
             str_detect(column_n, "inputs")==TRUE|
             str_detect(column_n, "persons")==TRUE,"Industry",
           ifelse(str_detect(column_n,"yield")==TRUE|
                    str_detect(column_n,"production")==TRUE|
                    str_detect(column_n,"food")==TRUE|
                    str_detect(column_n,"fertiliser")==TRUE|
                    str_detect(column_n,"vegetables")==TRUE|
                    str_detect(column_n,"fruits")==TRUE|
                    str_detect(column_n,"sown")==TRUE,"Agriculture and Allied",
                  ifelse(str_detect(column_n,"wage")==TRUE|
                           str_detect(column_n,"inflation")==TRUE, "Price and Wages",
                         ifelse(str_detect(column_n, "roads")==TRUE|
                                  str_detect(column_n, "highways")==TRUE|
                                  str_detect(column_n, "power")==TRUE|
                                  str_detect(column_n, "telephones")==TRUE|
                                  str_detect(column_n, "electricity")==TRUE|
                                  str_detect(column_n, "roads")==TRUE|
                                  str_detect(column_n, "irrigated")==TRUE|
                                  str_detect(column_n, "roads")==TRUE|
                                  str_detect(column_n, "railway")==TRUE|
                                  str_detect(column_n, "roads")==TRUE,"Infrastructure",
                                ifelse(str_detect(column_n, "social")==TRUE|
                                         str_detect(column_n,"capital")==TRUE,"Fiscal",
                                       ifelse(column_n=="state_ut"|column_n=="year"|column_n=="ID_0"|
                                                column_n=="ISO"|
                                                column_n=="NAME_0"|
                                                column_n=="ID_1"|
                                                column_n=="NAME_1"|
                                                column_n=="TYPE_1"|
                                                column_n=="ENGTYPE_1"|
                                                column_n=="NL_NAME_1"|
                                                column_n=="VARNAME_1"," ",
                                              "Demographic"))))))))

write.csv(cl_names,paste0(getwd(),"/variable_names.csv"))


#----Regression----------------------------------------------

#create an empty dataframe
up_data_infra<-data.frame("Year"=c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2017))

#I created a function to ease the process, but wasnt able to figure out how to change variable name.
#So, had to copy-paste. here is the function:

#chng_variable_name<-function(variable){
  
 # name_df<-data.frame(str_split_fixed(variable, "_",10))
  
  #t_var<-apply(name_df[1,],1, 
   #            function(x) paste(x[x!="state"&x!="wise"  &x!="net"& x!="x"& x!="y"], collapse = " "))
  
  #var=str_split(t_var,"\\.")[[1]][1]
  
#  var
#}





#installed capacity of power
temp<-up_data%>%
  select(state_wise_installed_capacity_of_power.x, Year)%>%
  filter(!is.na(state_wise_installed_capacity_of_power.x))%>%
  rename("installed_capacity_of_power"=state_wise_installed_capacity_of_power.x)

temp_1<-up_data%>%
  select(state_wise_installed_capacity_of_power.y, Year)%>%
  filter(!is.na(state_wise_installed_capacity_of_power.y))%>%
  rename("installed_capacity_of_power"=state_wise_installed_capacity_of_power.y)

up_data_power<-rbind(temp, temp_1)


#state_wise_availability_of_power.x
temp_2<-up_data%>%
  select(state_wise_availability_of_power.x, Year)%>%
  filter(!is.na(state_wise_availability_of_power.x))%>%
  rename("availability_of_power"=state_wise_availability_of_power.x)

temp_3<-up_data%>%
  select(state_wise_availability_of_power.y, Year)%>%
  filter(!is.na(state_wise_availability_of_power.y))%>%
  rename("availability_of_power"=state_wise_availability_of_power.y)  

up_data_avail_power<-rbind(temp_2, temp_3)

up_infra<-left_join(up_data_avail_power, up_data_power,by="Year")


#state_wise_power_requirement.x

temp_4<-up_data%>%
  select(state_wise_power_requirement.x, Year)%>%
  filter(!is.na(state_wise_power_requirement.x))%>%
  rename("power_requirement"=state_wise_power_requirement.x)

temp_5<-up_data%>%
  select(state_wise_power_requirement.y, Year)%>%
  filter(!is.na(state_wise_power_requirement.y))%>%
  rename("power_requirement"=state_wise_power_requirement.y)  

up_data_power_req<-rbind(temp_4, temp_5)

up_infra<-left_join(up_data_power_req, up_infra,by="Year")

#state_wise_length_of_state_highways.x

temp_6<-up_data%>%
  select(state_wise_length_of_state_highways.x, Year)%>%
  filter(!is.na(state_wise_length_of_state_highways.x))%>%
  rename("state_highway_length"=state_wise_length_of_state_highways.x)

temp_7<-up_data%>%
  select(state_wise_length_of_state_highways.y, Year)%>%
  filter(!is.na(state_wise_length_of_state_highways.y))%>%
  rename("state_highway_length"=state_wise_length_of_state_highways.y)  

state_high<-rbind(temp_6, temp_7)


up_infra<-left_join(state_high,up_infra,,by="Year")

temp_8<-up_data%>%
  select(net_state_value_added_by_economic_activity_manufacturing.x, Year)%>%
  filter(!is.na(net_state_value_added_by_economic_activity_manufacturing.x))%>%
  rename("value_added_manufacturing"=net_state_value_added_by_economic_activity_manufacturing.x)

temp_9<-up_data%>%
  select(net_state_value_added_by_economic_activity_manufacturing.y, Year)%>%
  filter(!is.na(net_state_value_added_by_economic_activity_manufacturing.y))%>%
  rename("value_added_manufacturing"=net_state_value_added_by_economic_activity_manufacturing.y)  

temp_10<-up_data%>%
  select(net_state_value_added_by_economic_activity_manufacturing.x.x, Year)%>%
  filter(!is.na(net_state_value_added_by_economic_activity_manufacturing.x.x))%>%
  rename("value_added_manufacturing"=net_state_value_added_by_economic_activity_manufacturing.x.x)  

temp_11<-up_data%>%
  select(net_state_value_added_by_economic_activity_manufacturing.y.y, Year)%>%
  filter(!is.na(net_state_value_added_by_economic_activity_manufacturing.y.y))%>%
  rename("value_added_manufacturing"=net_state_value_added_by_economic_activity_manufacturing.y.y)

temp_12<-up_data%>%
  select(net_state_value_added_by_economic_activity_manufacturing.y.y.y, Year)%>%
  filter(!is.na(net_state_value_added_by_economic_activity_manufacturing.y.y.y))%>%
  rename("value_added_manufacturing"=net_state_value_added_by_economic_activity_manufacturing.y.y.y)

temp_13<-up_data%>%
  select(net_state_value_added_by_economic_activity_manufacturing.x.x.x, Year)%>%
  filter(!is.na(net_state_value_added_by_economic_activity_manufacturing.x.x.x))%>%
  rename("value_added_manufacturing"=net_state_value_added_by_economic_activity_manufacturing.x.x.x)

manuf<-rbind(temp_8,temp_9,temp_10, temp_11,temp_12,temp_13)

up_infra<-left_join(manuf,up_infra,by="Year")

write.csv(up_infra,paste0(getwd(),"/up_infra.csv"))

#creating file for shiny
ggplot()+
  geom_sf(data=subset(state_data_sf,year==2017& !is.na(state.wise.installed.capacity.of.power)), aes(fill=state.wise.installed.capacity.of.power))+
  geom_sf(data=subset(state_data_sf,year==2017& !is.na(state.wise.installed.capacity.of.power.1)), aes(fill=state.wise.installed.capacity.of.power.1))+
  scale_size_continuous()+
  labs(fill="Installed Capacity of Power")+
  theme_minimal()

ggsave(paste0(getwd(),"/cap_power.png"))
