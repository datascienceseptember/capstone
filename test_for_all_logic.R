library(tidyverse)
require(data.table)
getwd()
#Data analysis
#Analysis of data base on age>=25 ,state=us  

csv_2017_data=fread("2017_cdc.csv", header = T)
#read csv for 2014
csv_2014_data=fread("Nat2014us.csv", header = T)
csv_2014_data

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
    "8th grade or less"
  } else if ( val==2) {
    "9th through 12th grade with no diploma"
  } else if ( val==3) {
    "High school graduate or GED completed"
  } else if( val==4) {
   "Some college credit, but not a degree"
  }else if( val==5) {
    "Associate degree (AA,AS)"
  }else if( val==6) {
    "Bachelor’s degree (BA, AB, BS)"
  }else if( val==7) {
    "Master’s degree (MA, MS, MEng, MEd, MSW, MBA)"
  }else if( val==8) {
    "Doctorate (PhD, EdD) or Professional Degree (MD, DDS,
      DVM, LLB, JD)"
  }else{
    "Unknown"
  }
}





summay_data=grouped_data%>%mutate(motherEducation=sapply(MEDUC, mother_ed))
summay_data

write.csv(summay_data,"mother_birth_education_age.csv", row.names = FALSE)

ggplot(summay_data, aes(motherEducation, total, colour = MEDUC)) + geom_point()



#check which age of mother have high infection of disease


#select for variable for 2017
select_data_2017_infection=select(csv_2017_data,DOB_YY,DOB_MM,MEDUC,MAGER,MBSTATE_REC,IP_GON,IP_SYPH,IP_CHLAM,IP_HEPB,IP_HEPC,SEX,DMAR)
select_data_2014_infection=na.omit(select(csv_2014_data,DOB_YY,DOB_MM,MEDUC,MAGER,MBSTATE_REC,IP_GON,IP_SYPH,IP_CHLAM,IP_HEPB,IP_HEPC,SEX,DMAR))
dim(select_data_2017)


combine_2017_2014_infection <- rbind(select_data_2017_infection, select_data_2014_infection)

combine_2017_2014_infection

dim(combine_2017_2014_infection)

select_data_2017_all_infection= combine_2017_2014_infection%>%mutate(motherEducation=sapply(MEDUC, mother_ed))%>% filter(MBSTATE_REC==1)
select_data_2017_all_infection

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
select_data_2017_gender=select(csv_2017_data,SEX,DOB_YY)
select_data_2017_gender
dim(select_data_2017_gender)
#select varible for 2014

select_data_2014_gender=na.omit(select(csv_2014_data,SEX,DOB_YY))
dim(select_data_2014_gender)
select_data_2014_gender
#combine for 2017 and 2014

combine_2017_2014_gender <- rbind(select_data_2017_gender, select_data_2014_gender)

dim(combine_2017_2014_gender)

combine_2017_2014_gender=combine_2017_2014_gender%>%group_by(SEX,DOB_YY)%>%summarise(total=n())
combine_2017_2014_gender

write.csv(combine_2017_2014_gender,"different_btn_m_f_birth.csv", row.names = FALSE)


