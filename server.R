library(shiny)
library(tidyverse)
require(data.table)

#infected_mother=fread("infection.csv", header = T)



gender=fread("different_btn_m_f_birth.csv",header = T)

mother_birth_education_age=fread("mother_birth_education_age.csv", header = T)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
 #Reactive method which show which mother age  have high rate of of infection of sexual transtmited diseas 
 #finding 1  
  
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
    
   
    
    
    infected_mother%>% select(DOB_YY, MEDUC, MAGER, MBSTATE_REC,!!sym(check_diseas(input$infection)))%>%
      filter( !!sym(check_diseas(input$infection))=='Y' & DOB_YY==input$year)%>%
        group_by(MAGER,DOB_YY)%>%summarise(total=n())
  })
  
  
  
  #Reactive method which show which mother of what education   have high rate of of infection of sexual transtmited diseas
  
  
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
    
    
    
    
    infected_mother%>% select(DOB_YY, MEDUC, MAGER,motherEducation, MBSTATE_REC,!!sym(check_diseas(input$infection)))%>%
      filter( !!sym(check_diseas(input$infection))=='Y' & DOB_YY==input$year)%>%
      group_by(DOB_YY,motherEducation,MEDUC)%>%summarise(total=n())
  })
  
  
  
  
  
  
 
  #Reactive method which show relationship  between total number of birth rate  and education for the mother with age above 25 
  # finding 2 (which check total)
  
  mother_education <- reactive({
    
    ggplot(mother_birth_education_age %>%filter(DOB_YY==input$yearage), aes( MEDUC, total)) +
      geom_bar(stat = "identity",aes(fill = motherEducation))+xlab("Mother level of education") + ylab("Total") +
      ggtitle("Total number of mother birth with age of greater than 25 based on Education") +
      theme_bw()
  })
  
  
  #Reactive method which show avarage relationship  between   birth rate  and education for the mother with age above 25
  # finding 2 (which check avarage) 
  av_mother_education <- reactive({
    
    ggplot(mother_birth_education_age %>%filter(DOB_YY==input$yearage), aes( MEDUC, percent)) +
      geom_bar(stat = "identity",aes(fill = motherEducation))+xlab("Mother level of education") + ylab("Percentage") +
      ggtitle("Percentange number of mother birth with age of greater than 25 based on Education")+
      theme_bw()
  })
  
  
  #Reactive method which show Different btn male and  female in 2017
  gender_number_2017<- reactive({
    mycols <- c("#0073C2FF", "#EFC000FF")
    gender=gender%>%filter(DOB_YY==2017)
    gender
    pie = ggplot(gender, aes(x="", y=total, fill=SEX)) + geom_bar(stat="identity", width=1)
    
    # Convert to pie (polar coordinates) and add labels
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(total))), position = position_stack(vjust = 0.5))
    
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
    gender_2014=gender%>%filter(DOB_YY==2014)
    
    pie = ggplot(gender_2014, aes(x="", y=total, fill=SEX)) + geom_bar(stat="identity", width=1)
    
    # Convert to pie (polar coordinates) and add labels
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(total))), position = position_stack(vjust = 0.5))
    
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
    
    
    
    
    infected_mother%>% select(DOB_YY,SEX,!!sym(check_diseas(input$male_female_infec)))%>%
      filter( !!sym(check_diseas(input$male_female_infec))=='Y')%>%
      group_by(SEX,DOB_YY)%>%summarise(total=n())
    
    
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
    
    overall_infection=infected_mother%>% select(SEX,!!sym(check_diseas(input$male_female_infec)))%>%
      filter( !!sym(check_diseas(input$male_female_infec))=='Y')%>%
      group_by(SEX)%>%summarise(total=n())
    
    pie = ggplot(overall_infection, aes(x="", y=total, fill=SEX)) + geom_bar(stat="identity", width=1)
    
    # Convert to pie (polar coordinates) and add labels
    pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(total))), position = position_stack(vjust = 0.5))
    
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
  
  
  #Reactive metthod base on marriage and education
  
  #Reactive method which number of infection btn male and female
  marrige_education <- reactive({
    
    
    
    
    
    
    infected_mother%>% select(MEDUC,DMAR,motherEducation)%>%
      filter(DMAR==1)%>%
      group_by(motherEducation,MEDUC)%>%summarise(total=n())%>%ggplot(aes(fill=motherEducation, y=total, x=MEDUC)) + 
      geom_bar(stat="identity")+xlab("Mother Education")
    
    
  })
  
  
  
  #render graph for find 1
  
  output$infection <- renderPlot(
    
    mother_infection()%>%ggplot(aes(x = MAGER, y = total,colour = MAGER))+geom_point() +
      labs(colour = "Mother Age")+labs(x = "Mother Age")
    )
  
  
  
  
  #render graph for find 1 base on education
  
  output$infection_base_on_education <- renderPlot(
    
    mother_infection_base_on_education()%>%ggplot(aes(x = MEDUC, y = total))+
      geom_bar(stat = "identity",aes(fill = motherEducation))+xlab("Mother level of education") + ylab("Total") +
      ggtitle("")+
      theme_bw()
  )
  
  #render graph for finding 2 which check total
  
  output$mother_education <- renderPlot(
    
    mother_education()
    
     )
  
  #render graph for finding 2 which check avarage
  output$av_mother_education <- renderPlot(
    
    av_mother_education()
    
  )
  
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
    
    infection_btn_male_female()%>% ggplot(aes(fill=SEX, y=total, x=DOB_YY)) + 
      geom_bar(position="dodge", stat="identity")+xlab("YEAR")
    
  )

 
  #render graph for overal infection btn male and female infection 
  
  output$total_male_female_infection  <- renderPlot(
    
    overall_infection_btn_male_female()
    
  )
  
  
  #render graph for marriage base on education 
  
  output$marriage_educ<- renderPlot(
    
    marrige_education()
    
  )
  
})
