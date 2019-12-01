library(tidyverse)
library(shinydashboard)
library(DT)
require(data.table)

#csv_2017_data=fread("2017_cdc.csv", header = T)
#read csv for 2014
#csv_2014_data=fread("Nat2014us.csv", header = T)
infected_mother=fread("infected_mother.csv", header = T)

mother_birth_education_age=fread("mother_birth_education_age.csv", header = T)
mother_birth_education_age



year<- c(2017,2016,2015,2014)
infection<- c("Gonorrhea","Syphilis","Chlamydia","Hepatitis B","Hepatitis C")
state= c("Us")


data = read.csv('uefa.csv', stringsAsFactors=FALSE)
team =data%>%select(.,HomeTeam,AwayTeam)
action=data%>%select(.,-HomeTeam,-AwayTeam,-X)





dashboardPage(
  dashboardHeader(
    
    title = "CDC",
    
    
    dropdownMenu(
      
      
      
      notificationItem("check on github", icon = icon("users"),
                       href = "https://github.com/datascienceseptember/shiny_project_on_uefa")
    )),
  
  #side bar
  dashboardSidebar(
    sidebarUserPanel(image ="uefalogo.png",name = "" ),
    
    
    
    
    sidebarMenu(
      hr(),
      menuItem("Infection", tabName = "infection", icon = icon("database")),
      hr(),
      menuItem("Mother Eduction", tabName = "med", icon = icon("home")),
      hr()
      
      
      
      
    )
  ),
  #end of side bar
  
  #body
  dashboardBody
  (
    
    
    tabItems(
      #tabItem(tabName = "map",
      #       fluidRow(infoBoxOutput("maxgoal"),
      #    infoBoxOutput("minBox"),
      #  infoBoxOutput("avgBox")),"Gonorrhea","Syphilis","Chlamydia","Hepatitis B","Hepatitis C" tags$div(
     # HTML("<strong>Raw HTML!</strong>")
    #)
      
     
      
      
      tabItem(tabName = "infection",
              br(),
              fluidRow(column(12,tags$div(HTML(toupper("<h4><strong>Graph show which age of mother have  high  change of being affected with Gonorrhea,Syphilis,Chlamydia,Hepatitis B, Hepatitis C</strong></h4>"))),offset =0)),
              br(),
              br(),
              br(),
              fluidRow(
                column(4,
                       selectInput( inputId= "year", 
                                    label =  "Year:", 
                                    choices =  year)
                ),
                
                column(4,
                       selectInput( inputId= "infection", 
                                    label =  "Infection:", 
                                    choices =  infection)
                ),
                
                column(4,
                       selectInput( inputId= "state", 
                                    label =  "State:", 
                                    choices =  state)
                )
                
              ),
              
              fluidRow(
                column(12, plotOutput("infection"))
              )
      ),
      
      
      tabItem(tabName = "med",
              br(),
             
              
              fluidRow(
                column(4,
                       selectInput( inputId= "yearage", 
                                    label =  "Year", 
                                    choices =  year),offset = 4
                )
                
               
                
              ),
              br(),
              br(),
              
              fluidRow(
                column(12, plotOutput("mother_education"))
              ),
              br(),
              br(),
              br(),
              fluidRow(
                column(12, plotOutput("av_mother_education"))
              )
              
      ) 
           
       
      )
      
      
    ),
    
    
    
    
    
  )
  
  
  
  #end of body

