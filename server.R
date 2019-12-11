library(shiny)
library(tidyverse)
require(data.table)

infected_mother=fread("final_data.csv", header = T)
infected_mother=subset(infected_mother, motherEducation !="Unknown")
unique(infected_mother$motherEducation)
#gender=fread("different_btn_m_f_birth.csv",header = T)
#mother_birth_education_age=fread("mother_birth_education_age.csv", header = T)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  #Reactive method which show which mother age  have high rate of of infection of sexual transtmited diseas 
  mother_infection <- reactive({
    
    check_diseas <- function(val){
      if (val=="Syphilis") {
        "IP_SYPH"
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
    
    
    group_age <- function(val){
      if (between(val, 1, 20)) {
        "10-20 age"
      } else if(between(val, 21, 30)){
        "21-30 age"
      }else if(between(val,31,40)){
        "31-40 age"
      }else if(between(val,41,50)){
        "41-50 age"
      }else if(between(val,51,60)){
        "51-60 age "
      }else{
        "61 and above"
      }
    }
    
    
    
    
    temp =   infected_mother%>%filter(!!sym(check_diseas(input$infection))==c("Y","N"))%>%
            select(!!sym(check_diseas(input$infection)),MAGER)%>%mutate(age_of_group=sapply(MAGER, group_age))
    temp1 =  temp%>%group_by(age_of_group,!!sym(check_diseas(input$infection)))%>%summarise(total=n())
    
    
    check <- function(val){
      if (val=="10-20 age") {
        temp3=temp1%>%filter(total,age_of_group == "10-20 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="21-30 age") {
        temp3=temp1%>%filter(total,age_of_group == "21-30 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="31-40 age") {
        temp3=temp1%>%filter(total,age_of_group == "31-40 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if( val=="41-50 age") {
        temp3=temp1%>%filter(total,age_of_group == "41-50 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="51-60 age ") {
        temp3=temp1%>%filter(total,age_of_group == "51-60 age ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="61 and above") {
        temp3=temp1%>%filter(total,age_of_group == "61 and above")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else{
        temp3=temp1%>%filter(total,age_of_group == "61 and above")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
    }
   
    temp2= temp1%>%mutate(grade_total=sapply(age_of_group, check))%>%
           mutate(percent=(total/grade_total)*100)%>%
           ggplot( aes(x=age_of_group, y=percent, fill=!!sym(check_diseas(input$infection)))) +
           geom_bar(stat="identity", position=position_dodge())+
           geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.9), vjust=-0.25)+
           labs(title="Graph show infected mother base on the age ",
           x ="Mother Age", y = "Percent")
           temp2
  
  })
  
  
  
  #Reactive method which show which mother of what education have high rate of of infection of sexual transtmited diseas
  
   mother_infection_base_on_education <- reactive({
    
    check_diseas <- function(val){
      if (val=="Syphilis") {
        "IP_SYPH"
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
    
    
    
    
    
    
    temp1= infected_mother%>%select(DOB_YY, MEDUC, MAGER,motherEducation,!!sym(check_diseas(input$infection)))%>%
           filter(!!sym(check_diseas(input$infection))==c('Y','N'))%>%
           group_by(motherEducation,!!sym(check_diseas(input$infection)))%>%
           summarise(total=n()) 
    
    
    
    check <- function(val){
      if (val=="8th grade") {
        temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="9th grade") {
        temp3=temp1%>%filter(total,motherEducation == "9th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="High school grade") {
        temp3=temp1%>%filter(total,motherEducation == "High school grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if( val=="Some college ") {
        temp3=temp1%>%filter(total,motherEducation == "Some college ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Associate degree") {
        temp3=temp1%>%filter(total,motherEducation == "Associate degree")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Bachelor’s degree ") {
        temp3=temp1%>%filter(total,motherEducation == "Bachelor’s degree ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Master’s degree") {
        temp3=temp1%>%filter(total,motherEducation == "Master’s degree")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Doctorate (PhD, EdD)") {
        temp3=temp1%>%filter(total,motherEducation == "Doctorate (PhD, EdD)")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else{
        temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
    }
    
    temp2=infected_mother%>%select( MEDUC, MAGER,motherEducation,!!sym(check_diseas(input$infection)))%>%filter(!!sym(check_diseas(input$infection))==c('Y','N'))%>%
           group_by(motherEducation,!!sym(check_diseas(input$infection)))%>%
          summarise(total=n())%>%mutate(grade_total=sapply(motherEducation, check))%>%mutate(percent=(total/grade_total*100)) %>%
          ggplot(aes(x = motherEducation, y = percent))+
          geom_bar(position = "dodge",stat = "identity",aes(fill = !!sym(check_diseas(input$infection))))+
          geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.8), vjust=-0.25)+
          labs(title="Graph show mother infection based on education ",x ="Mother Education", y = "Percent")
          theme_bw()
          temp2  
          
          
  })
  
  
  
  
  
  
 
  #Reactive method which show mother birth base on age and  delivery method 
  
  mother_education <- reactive({
    
    mother_delivery_cleaned=subset(infected_mother, devery_method !="Unkown")
    
    
    group_age <- function(val){
      
      if (between(val, 1, 20)) {
        "10-20 age"
      } else if(between(val, 21, 30)){
        "21-30 age"
      }else if(between(val,31,40)){
        "31-40 age"
      }else if(between(val,41,50)){
        "41-50 age"
      }else if(between(val,51,60)){
        "51-60 age "
      }else{
        "61 and above"
      }
    }
    
    
    
    
    temp =   mother_delivery_cleaned%>%select( DOB_YY,MAGER,ME_ROUT,devery_method)%>%mutate(age_of_group=sapply(MAGER, group_age))
    temp1 =  temp %>%group_by(age_of_group,devery_method)%>%summarise(total=n())
    
    check <- function(val){
      if (val=="10-20 age") {
        temp3=temp1%>%filter(total,age_of_group == "10-20 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="21-30 age") {
        temp3=temp1%>%filter(total,age_of_group == "21-30 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="31-40 age") {
        temp3=temp1%>%filter(total,age_of_group == "31-40 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if( val=="41-50 age") {
        temp3=temp1%>%filter(total,age_of_group == "41-50 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="51-60 age ") {
        temp3=temp1%>%filter(total,age_of_group == "51-60 age ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="61 and above") {
        temp3=temp1%>%filter(total,age_of_group == "61 and above")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else{
        temp3=temp1%>%filter(total,age_of_group == "61 and above")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
    }
    
    temp2=temp1%>%mutate(grade_total=sapply(age_of_group, check))%>%mutate(percent=(total/grade_total)*100)%>%
          ggplot( aes(x=age_of_group, y=percent, fill=devery_method)) +
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.8), vjust=-0.25)+
          labs(title="Graph show mother birth base on Delivery method ",
          x ="Mother Age", y = "Percent")
          temp2
    
    
  })
  
  
  #Reactive method which show mother birth base on age and  with caresearen delivery method 
  av_mother_education <- reactive({
    cesarean_method_cleaned=subset(infected_mother, devery_method !="Unkown")
    cesarean_method_cleaned
    
    group_age <- function(val){
      
      if (between(val, 1, 20)) {
        "10-20 age"
      } else if(between(val, 21, 30)){
        "21-30 age"
      }else if(between(val,31,40)){
        "31-40 age"
      }else if(between(val,41,50)){
        "41-50 age"
      }else if(between(val,51,60)){
        "51-60 age "
      }else{
        "61 and above"
      }
    }
    
    
    
    
    temp =    cesarean_method_cleaned%>%select( MAGER,cesarean_method)%>%mutate(age_of_group=sapply(MAGER, group_age))
    
    temp1 =  temp %>%group_by(age_of_group,cesarean_method)%>%summarise(total=n())
    
    check <- function(val){
      if (val=="10-20 age") {
        temp3=temp1%>%filter(total,age_of_group == "10-20 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="21-30 age") {
        temp3=temp1%>%filter(total,age_of_group == "21-30 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="31-40 age") {
        temp3=temp1%>%filter(total,age_of_group == "31-40 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if( val=="41-50 age") {
        temp3=temp1%>%filter(total,age_of_group == "41-50 age")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="51-60 age ") {
        temp3=temp1%>%filter(total,age_of_group == "51-60 age ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="61 and above") {
        temp3=temp1%>%filter(total,age_of_group == "61 and above")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else{
        temp3=temp1%>%filter(total,age_of_group == "61 and above")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
    }
    
    
    temp2=temp1%>%mutate(grade_total=sapply(age_of_group, check))%>%mutate(percent=(total/grade_total)*100)%>%
          ggplot( aes(x=age_of_group, y=percent, fill=cesarean_method)) +
          geom_bar(stat="identity", position=position_dodge())+
          geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.8), vjust=-0.25)+
          labs(title="Graph show mother birth base on Delivery method ",
          x ="Mother Age", y = "Percent")
          temp2

  })
  
  
  
  #Rective method of mother of different delivery method and education
  
  mother_education_delivery_method <- reactive({
    
          mother_delivery_cleaned=subset(infected_mother, devery_method !="Unkown")
          unique(mother_delivery_cleaned$motherEducation)
    
         temp1 = mother_delivery_cleaned%>%select( DOB_YY,motherEducation,ME_ROUT,devery_method)%>%
                 group_by(motherEducation)%>%summarise(total=n())
      
      
      check <- function(val){
        if (val=="8th grade") {
          temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%select(total)%>%sum()
          temp3
        } else if ( val=="9th grade") {
          temp3=temp1%>%filter(total,motherEducation == "9th grade")%>%select(total)%>%sum()
          temp3
        } else if ( val=="High school grade") {
          temp3=temp1%>%filter(total,motherEducation == "High school grade")%>%select(total)%>%sum()
          temp3
        } else if( val=="Some college ") {
          temp3=temp1%>%filter(total,motherEducation == "Some college ")%>%select(total)%>%sum()
          temp3
        }else if( val=="Associate degree") {
          temp3=temp1%>%filter(total,motherEducation == "Associate degree")%>%select(total)%>%sum()
          temp3
        }else if( val=="Bachelor’s degree ") {
          temp3=temp1%>%filter(total,motherEducation == "Bachelor’s degree ")%>%select(total)%>%sum()
          temp3
        }else if( val=="Master’s degree") {
          temp3=temp1%>%filter(total,motherEducation == "Master’s degree")%>%select(total)%>%sum()
          temp3
        }else if( val=="Doctorate (PhD, EdD)") {
          temp3=temp1%>%filter(total,motherEducation == "Doctorate (PhD, EdD)")%>%select(total)%>%sum()
          temp3
        }else{
          temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%select(total)%>%sum()
          temp3
        }
      }
      
      #end of the function to check
      
      temp2 = mother_delivery_cleaned%>%select( DOB_YY,motherEducation,ME_ROUT,devery_method)%>%
              group_by(motherEducation,devery_method)%>%summarise(total=n())%>%
              mutate(grade_total=sapply(motherEducation, check))%>%
              mutate(percent=(total/grade_total)*100)%>%
              ggplot( aes(x=motherEducation, y=percent, fill=devery_method)) +
              geom_bar(stat="identity", position=position_dodge())+geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.9), vjust=-0.25)+
              labs(title="Graph show mother birth base on Delivery method ",
             x ="Mother Age", y = "Percent")
             temp2
      
    
  })
  
  
  #Rective method mother delivery method of ceaserean and education
  av_mother_education_delivery_method <- reactive({
       cesarean_method_cleaned=subset(infected_mother, devery_method !="Unkown")
      temp1= cesarean_method_cleaned%>%select(motherEducation,cesarean_method)%>%
               group_by(motherEducation,cesarean_method)%>%summarise(total=n())
        
        
        checki <- function(val){
          if (val=="8th grade") {
            temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          } else if ( val=="9th grade") {
            temp3=temp1%>%filter(total,motherEducation == "9th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          } else if ( val=="High school grade") {
            temp3=temp1%>%filter(total,motherEducation == "High school grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          } else if( val=="Some college ") {
            temp3=temp1%>%filter(total,motherEducation == "Some college ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          }else if( val=="Associate degree") {
            temp3=temp1%>%filter(total,motherEducation == "Associate degree")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          }else if( val=="Bachelor’s degree ") {
            temp3=temp1%>%filter(total,motherEducation == "Bachelor’s degree ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          }else if( val=="Master’s degree") {
            temp3=temp1%>%filter(total,motherEducation == "Master’s degree")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          }else if( val=="Doctorate (PhD, EdD)") {
            temp3=temp1%>%filter(total,motherEducation == "Doctorate (PhD, EdD)")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          }else{
            temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
            temp3
          }
        }
        
        
        temp2 = cesarean_method_cleaned%>%select( motherEducation,cesarean_method)%>%group_by(motherEducation,cesarean_method)%>%summarise(total=n())%>%
                mutate(n=motherEducation)%>%mutate(grade_total=sapply(motherEducation, checki))%>%mutate(percent=(total/grade_total)*100)%>%     
                ggplot( aes(x=motherEducation, y=percent, fill=cesarean_method)) +
                geom_bar(stat="identity", position=position_dodge())+
                geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.9), vjust=-0.25)+
                labs(title="Graph show mother birth with Cesarean or Not ",
               
               x ="Mother Age", y = "Percent")
               temp2 
  })
  #end mother delivery method and education
  
  
  #Reactive method which show Different btn male and  female in 2017
  gender_number_2017<- reactive({
    mycols <- c("#0073C2FF", "#EFC000FF")
    gender_2017=gender%>%filter(DOB_YY==2017)
   
    pie = ggplot(gender_2017, aes(x="", y=percent, fill=SEX)) + geom_bar(stat="identity", width=1)
    
    # Convert to pie (polar coordinates) and add labels
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent,2),'%')), position = position_stack(vjust = 0.5))
    
    # Add color scale (hex colors)
    pie = pie + scale_fill_manual(values=mycols) 
    
    # Remove labels and add title
    pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "2017")
    
    # Tidy up the theme
    pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                        axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        plot.title = element_text(hjust = 0.5, color = "#666666"))
    pie
  })
  
  
  
  #Reactive method which show Different btn male and  female in 2014
  
  gender_number_2014<- reactive({
    mycols <- c("#868686FF", "#CD534CFF")
    gender
    gender_2014=gender%>%filter(DOB_YY==2014)
    gender_2014
    pie = ggplot(gender_2014, aes(x="", y=percent, fill=SEX)) + geom_bar(stat="identity", width=1)
    
    # Convert to pie (polar coordinates) and add labels
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent,2),"%")), position = position_stack(vjust = 0.5))
    
    # Add color scale (hex colors)
    pie = pie + scale_fill_manual(values=mycols) 
    
    # Remove labels and add title
    pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "2014")
    
    # Tidy up the theme
    pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                        axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        plot.title = element_text(hjust = 0.5, color = "#666666"))
    pie
  })
  
 
  #Reactive method which number of infection btn male and female
  infection_btn_male_female <- reactive({
    
    check_diseas <- function(val){
      if (val=="Syphilis") {
        "IP_SYPH"
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
    
    
     temp1=  infected_mother%>%select(DOB_YY,SEX,!!sym(check_diseas(input$male_female_infec)))%>%
            filter( !!sym(check_diseas(input$male_female_infec))=='Y')%>%
            group_by(DOB_YY,SEX)%>%summarise(total=n())
     
    
    
    check <- function(val){
      if (val==2014) {
        temp3=temp1%>%filter(total,DOB_YY == 2014)%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val==2015) {
        temp3=temp1%>%filter(total,DOB_YY == 2015)%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val==2016) {
        temp3=temp1%>%filter(total,DOB_YY == 2016)%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if( val==2017) {
        temp3=temp1%>%filter(total,DOB_YY == 2017)%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      
      }else if(val==2018){
        temp3=temp1%>%filter(total,DOB_YY == 2018)%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
      else{
        temp3=temp1%>%filter(total,DOB_YY == 2019)%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
    }
    
    
    temp2=temp1%>%mutate(grade_total=sapply(DOB_YY, check))%>%mutate(percent=(total/grade_total)*100)
          temp2%>%ggplot(aes(fill=SEX, y=percent, x=DOB_YY)) + 
          geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.9), vjust=-0.25)+
          geom_bar(position="dodge", stat="identity")+xlab("YEAR")
    
  })
  
  #Reactive method which number of overall infection btn male and female
  overall_infection_btn_male_female <- reactive({
    
    check_diseas <- function(val){
      if (val=="Syphilis") {
        "IP_SYPH"
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
    
    mycols <- c("#868686FF", "#CD534CFF")
    
    
    temp1=  infected_mother%>%select(SEX,!!sym(check_diseas(input$male_female_infec)))%>%
            filter(!!sym(check_diseas(input$male_female_infec))=='Y')%>%
            group_by(SEX)%>%summarise(total=n())
    
    
    temp2=temp1%>%mutate(percent=(total/sum(temp1$total)*100))
    
    
    
    #overall_infection=infected_mother%>% select(SEX,!!sym(check_diseas(input$male_female_infec)))%>%
     # filter( !!sym(check_diseas(input$male_female_infec))=='Y')%>%
     # group_by(SEX)%>%summarise(total=n())
    
    pie = ggplot(temp2, aes(x="", y=percent, fill=SEX)) + geom_bar(stat="identity", width=1)
    
    # Convert to pie (polar coordinates) and add labels
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent,2),"%")), position = position_stack(vjust = 0.5))
    
    # Add color scale (hex colors)
    pie = pie + scale_fill_manual(values=mycols) 
    
    # Remove labels and add title
    pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Total Infection")
    
    # Tidy up the theme
    pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                        axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        plot.title = element_text(hjust = 0.5, color = "#666666"))
    pie
  })
  
  
  
  
  
  
  
  #Reactive method of mother  base on marriage and education
  
  marrige_education <- reactive({
       
    data_marriage=infected_mother%>% select(MEDUC,status,motherEducation)
    data_with_no_null=na.omit(data_marriage)
    
    temp1=data_with_no_null%>%group_by(motherEducation)%>%summarise(total=n())
    
    
    
    
    check <- function(val){
      if (val=="8th grade") {
        temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="9th grade") {
        temp3=temp1%>%filter(total,motherEducation == "9th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if ( val=="High school grade") {
        temp3=temp1%>%filter(total,motherEducation == "High school grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      } else if( val=="Some college ") {
        temp3=temp1%>%filter(total,motherEducation == "Some college ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Associate degree") {
        temp3=temp1%>%filter(total,motherEducation == "Associate degree")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Bachelor’s degree ") {
        temp3=temp1%>%filter(total,motherEducation == "Bachelor’s degree ")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Master’s degree") {
        temp3=temp1%>%filter(total,motherEducation == "Master’s degree")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else if( val=="Doctorate (PhD, EdD)") {
        temp3=temp1%>%filter(total,motherEducation == "Doctorate (PhD, EdD)")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }else{
        temp3=temp1%>%filter(total,motherEducation == "8th grade")%>%summarise(total=sum(total))%>%select(total)%>%sum()
        temp3
      }
    }
    
    
    
    temp2=data_with_no_null%>%group_by(motherEducation,MEDUC,status)%>%summarise(total=n())%>%
      mutate(grade_total=sapply(motherEducation, check))%>%mutate(percent=(total/grade_total)*100)%>%
      ggplot( aes(x=motherEducation, y=percent, fill=status)) +
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_manual(values=c('#E69F00','#009e28'))+
      geom_text(aes(label=paste0(round(percent,2),"%")),position=position_dodge(width=0.9), vjust=-0.25)+
      scale_fill_brewer(palette="Blues")+labs(title="Graph show mother Marriage base on education ",
      x ="Mother education", y = "Percent")
      temp2
    
  })
  
 
  
  
  
  #render graph for find 1
  
  output$infection <- renderPlot(
    
    mother_infection()
    )
  
  
  
  
  #render graph for find 1 base on education
  
  output$infection_base_on_education <- renderPlot(
    
    mother_infection_base_on_education()
  )
  
  #render graph for finding 2 which check total
  
  output$mother_education <- renderPlot(
    
    mother_education()
    
     )
  
  #render graph for finding 2 which check avarage
  output$av_mother_education <- renderPlot(
    
    av_mother_education()
    
  )
  
  
  #start mother delivery method and education
  
  

  
  output$mother_delivery_method_education <- renderPlot(
    
    mother_education_delivery_method()
    
  )
  
 
 output$av_mother_delivery_method_education <- renderPlot(
    
   av_mother_education_delivery_method()
    
  )
  
  #end of mother delivery method and education
  
  
  
  
  #render graph for gender difference btn male and female 2017
  
  output$gendertotal_2017 <- renderPlot(
    
    gender_number_2017()
    
  )
  
  
  #render graph for gender difference btn male and female 2014
  output$gendertotal_2014 <- renderPlot(
    
    gender_number_2014()
    
  )
  
  
  #render graph for male and female infection 
  
  output$male_female_infection  <- renderPlot(
    
    infection_btn_male_female()
  )

 
  #render graph for overal infection btn male and female infection 
  
  output$total_male_female_infection  <- renderPlot(
    
    overall_infection_btn_male_female()
    
  )
  
  
  #render graph for marriage base on education 
  
  output$marriage_educ<- renderPlot(
    
    marrige_education()
    
  )
  
  
  output$table <- DT::renderDataTable({
    datatable(select(infected_mother,-V1), rownames=FALSE, options = list(scrollX = TRUE))
     # formatStyle(select(infected_mother,-V1),height = 894)
  })
  
})
