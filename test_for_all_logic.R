library(tidyverse)
require(data.table)
getwd()#Data analysis
#Analysis of data base on age>=25 ,state=us  

csv_2017_data=fread("2017_cdc.csv", header = T)
#read csv for 2014
csv_2014_data=fread("Nat2014us.csv", header = T)
csv_2014_data

csv_2017_data$ME_ROUT

csv_2017_data
dim(csv_2017_data)

#select for variable for 2017
select_data_2017=select(csv_2017_data,DOB_YY,MEDUC,MAGER,MBSTATE_REC)
select_data_2017
dim(select_data_2017)
#select varible for 2014

select_data_2014=na.omit(select(csv_2014_data,DOB_YY,MEDUC,MAGER,MBSTATE_REC))
dim(select_data_2014)

#combine for 2017 and 2014

combine_2017_2014 <- rbind(select_data_2017, select_data_2014)

dim(combine_2017_2014)


filtered_data_2017=filter(combine_2017_2014,MAGER>=25 & MBSTATE_REC==1)



dim(filtered_data_2017)
grouped_data=filtered_data_2017%>%group_by(MEDUC,DOB_YY)%>%summarise(total=n(),percent=n()/100)

grouped_data

mother_ed <- function(val){
  if (val==1) {
    "8th grade"
  } else if ( val==2) {
    "9th grade"
  } else if ( val==3) {
    "High school grade"
  } else if( val==4) {
   "Some college "
  }else if( val==5) {
    "Associate degree"
  }else if( val==6) {
    "Bachelor’s degree "
  }else if( val==7) {
    "Master’s degree"
  }else if( val==8) {
    "Doctorate (PhD, EdD)"
  }else{
    "Unknown"
  }
}


marital_status <- function(val){
  if (val==1) {
    "married"
  }else{
    "single"
  }
}

cesarean_mother <- function(val){
  if (val==4) {
    "cesarean"
  }else{
    "Non-cesarean"
  }
}

delivery_mode <- function(val){
  if (val==1) {
    "Spontaneous"
  }else if(val==2){
    "Forceps"
  }else if(val==3){
    "Vacuum"
  }else if(val==4){
    "cesarean"
  }else{
    "Unkown"
  }
    
}



summay_data=grouped_data%>%mutate(motherEducation=sapply(MEDUC, mother_ed))
summay_data

write.csv(summay_data,"mother_birth_education_age.csv", row.names = FALSE)

ggplot(summay_data, aes(motherEducation, total, colour = MEDUC)) + geom_point()



#check which age of mother have high infection of disease


#select for variable for 2017
select_data_2017_infection=na.omit(select(csv_2017_data,DOB_YY,DOB_MM,MEDUC,MAGER,MBSTATE_REC,IP_GON,IP_SYPH,IP_CHLAM,IP_HEPB,IP_HEPC,SEX,DMAR,ME_ROUT))
select_data_2014_infection=na.omit(select(csv_2014_data,DOB_YY,DOB_MM,MEDUC,MAGER,MBSTATE_REC,IP_GON,IP_SYPH,IP_CHLAM,IP_HEPB,IP_HEPC,SEX,DMAR,ME_ROUT))
dim(select_data_2017)

all_data=fread("katisho.csv", header = T) 
all_data

combine_2017_2014_infection <- rbind(select_data_2017_infection, select_data_2014_infection)

combine_2017_2014_infection

dim(combine_2017_2014_infection)

select_data_2017_all_infection= combine_2017_2014_infection%>%
  mutate(motherEducation=sapply(MEDUC, mother_ed),status=sapply(DMAR, marital_status),devery_method=sapply(ME_ROUT, delivery_mode),cesarean_method=sapply(ME_ROUT, cesarean_mother))%>% 
  filter(MBSTATE_REC==1)
select_data_2017_all_infection


#check final csv
#status=sapply(DMAR, marital_status)

final_data= na.omit(all_data)%>%
  mutate(motherEducation=sapply(MEDUC, mother_ed),devery_method=sapply(ME_ROUT, delivery_mode),cesarean_method=sapply(ME_ROUT, cesarean_mother),status=sapply(DMAR, marital_status)) 
write.csv(final_data,"final_data.csv", row.names = FALSE)
#end check final csv

write.csv(select_data_2017_all_infection,"infection.csv", row.names = FALSE)
#select varible for 2014 infected_mother






check_diseas <- function(val){
  if (val=="Syphilis") {
    "8IP_SYPH"
  } else if ( val=="Gonorrhea") {
    "IP_GON"
  } else if ( val=="Chlamydia") {
    "IP_CHLAM"
  } else if( val=="Hepatitis B") {
    "IP_HEPB"
  }else {
    "IP_HEPC"
  }
}
  
check_diseas("Gonorrhea")



#Difference btn male and female  birth on the year

#select for variable for 2017
select_data_2017_gender=select(csv_2017_data,SEX)%>%group_by(SEX)%>%summarise(n=n())%>%mutate(ratio=n/sum(n),percent=ratio*100,DOB_YY=2017)
select_data_2017_gender
dim(select_data_2017_gender)
#select varible for 2014

select_data_2014_gender=na.omit(select(csv_2014_data,SEX,DOB_YY))%>%group_by(SEX)%>%summarise(n=n())%>%mutate(ratio=n/sum(n),percent=ratio*100,DOB_YY=2014)
dim(select_data_2014_gender)
select_data_2014_gender

#combine for 2017 and 2014

combine_2017_2014_gender <- rbind(select_data_2017_gender, select_data_2014_gender)
combine_2017_2014_gender
dim(combine_2017_2014_gender)

#combine_2017_2014_gender=combine_2017_2014_gender%>%group_by(SEX,DOB_YY)%>%summarise(total=n())%>%mutate(ratio=total/sum(total),percent=ratio*100)

"start"
1952273/sum(combine_2017_2014_gender$total)
1887091/sum(combine_2017_2014_gender$total)
2045902/sum(combine_2017_2014_gender$total)
1977663/sum(combine_2017_2014_gender$total)
write.csv(combine_2017_2014_gender,"different_btn_m_f_birth.csv", row.names = FALSE)



#im

data=infected_mother%>% select(DOB_YY, MEDUC, MAGER,motherEducation, MBSTATE_REC,!!sym(check_diseas(input$infection)))%>%
  filter(DOB_YY==input$year & !!sym(check_diseas(input$infection))==c('Y','N') )

data%>%group_by(DOB_YY,motherEducation,!!sym(check_diseas(input$infection)))%>%
  summarise(total=n(),percent=total/nrow(data))%>%mutate(val=!sym(check_diseas(input$infection)))
ggplot(aes(x = motherEducation, y = percent))+
  geom_bar(position = "dodge",stat = "identity",aes(fill = val))+xlab("Mother level of education") + ylab("Total") +
  ggtitle("")+
  theme_bw()


