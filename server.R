library(shiny)
library(tidyverse)
require(data.table)

infected_mother=fread("infected_mother.csv", header = T)

mother_birth_education_age=fread("mother_birth_education_age.csv", header = T)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
 
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
  
  
  
 
  
  mother_education <- reactive({
    
    ggplot(mother_birth_education_age %>%filter(DOB_YY==input$yearage), aes( MEDUC, total)) +
      geom_bar(stat = "identity",aes(fill = motherEducation))+xlab("Mother level of education") + ylab("Total") +
      ggtitle("Total number of mother birth with age of greater than 25 based on Education") +
      theme_bw()
  })
  
  
  av_mother_education <- reactive({
    
    ggplot(mother_birth_education_age %>%filter(DOB_YY==input$yearage), aes( MEDUC, percent)) +
      geom_bar(stat = "identity",aes(fill = motherEducation))+xlab("Mother level of education") + ylab("Percentage") +
      ggtitle("Percentange number of mother birth with age of greater than 25 based on Education")+
      theme_bw()
  })
  
  
 
  output$infection <- renderPlot(
    
    mother_infection()%>%ggplot(aes(x = MAGER, y = total,colour = MAGER))+geom_point() +
      labs(colour = "Mother Age")+labs(x = "Mother Age")
    
    
  )
  
  
  output$mother_education <- renderPlot(
    
    mother_education()
    
     )
  
  
  output$av_mother_education <- renderPlot(
    
    av_mother_education()
    
  )
  
  

  
})
