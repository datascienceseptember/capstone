library(tidyverse)
library(shinydashboard)
library(DT)
require(data.table)

#infected_mother=fread("final_data.csv", header = T)


#mother_birth_education_age=fread("mother_birth_education_age.csv", header = T)


#gender=fread("different_btn_m_f_birth.csv",header = T)


year<- c(2017,2016,2015,2014)
infection<- c("Gonorrhea","Syphilis","Chlamydia","Hepatitis B","Hepatitis C")
state= c("Us")


dashboardPage(
  dashboardHeader(
    
    title = "Center for disease Control and Prevention",
   
    
    
    dropdownMenu(
      
      
      
      notificationItem("check on github", icon = icon("users"),
                       href = "https://github.com/datascienceseptember/capstone")
    )),
  
  #side bar
  dashboardSidebar(
    sidebarUserPanel(image ="cdc_logo_1.jpeg",name = "CDC" ),
    
    
   
    
    
    #Infection Base on Age & Education
    sidebarMenu(
      
      hr(),
      menuItem( tags$div(HTML("<h4>Dataset Used"), 
                         tags$br(),
                         HTML("<h4></h4>")), tabName = "dataset", icon = icon("")),
      hr(),
      menuItem( tags$div(HTML("<h4>Mother Infection Base on"), 
                              tags$br(),
                              HTML("<h4>Age & Education</h4>")), tabName = "infection", icon = icon("")),
      hr(),
      
      menuItem(tags$div(HTML("<h4>Infection btn "), 
                        tags$br(),
                        HTML("<h4>Male and Female</h4>")), tabName = "infection_male_female", icon = icon("")),
      hr(),
      menuItem(tags$div(HTML("<h4>Mother Age &"), 
                        tags$br(),
                        HTML("<h4>Delivery Method </h4>")), tabName = "med", icon = icon("")),
      hr(),
      
      menuItem(tags$div(HTML("<h4>Mother Delivery Method "), 
                        tags$br(),
                        HTML("<h4>   & Education</h4>")), tabName = "mothde", icon = icon("")),
      hr(),
      #menuItem(tags$div(HTML("<h4>Birth Gender</h4>")), tabName = "gender", icon = icon("")),
      #hr(),
      
      
      menuItem(tags$div(HTML("<h4>Mother marrige "), 
                        tags$br(),
                        HTML("<h4>Base on Education</h4>")), tabName = "marriage", icon = icon("")),
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
              #fluidRow(column(12,tags$div(HTML(toupper("<h4><strong>Graph show which age of mother have  high  change of being affected with Gonorrhea,Syphilis,Chlamydia,Hepatitis B, Hepatitis C</strong></h4>"))),offset =0)),
              #br(),
              #br(),
              br(),
              fluidRow(
                
                column(4,
                       selectInput( inputId= "infection", 
                                    label =  "Infection:", 
                                    choices =  infection)
                ),
                
                br(),
                br()
                
              
                
              ),
              
              fluidRow(
                column(12, plotOutput("infection"))
              ),
              br(),
              br(),
              br(),
              br(),
              br(),
              
              fluidRow(
                column(12, plotOutput("infection_base_on_education"))
              )
      ),
      
      
      tabItem(tabName = "med",
              br(),
             
              
            
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
              
      ),
      #mother delivery and education
      tabItem(tabName = "mothde",
              br(),
              
              
              
              br(),
              br(),
              
              fluidRow(
                column(12, plotOutput("mother_delivery_method_education"))
              ),
              br(),
              br(),
              br(),
              fluidRow(
                column(12, plotOutput("av_mother_delivery_method_education"))
              )
              
      ),
      #end of delivery and education
      
      
      #gender
      
      
      #tabItem(tabName = "gender",
          #    br(),
              
              
             
             # br(),
              #br(),
              
              #fluidRow(
               # column(6, plotOutput("gendertotal_2017")),
                #column(6, plotOutput("gendertotal_2014"))
              #)
              
              # ),
      
      
      
      
      
      #infection male and female
      
      tabItem(tabName = "infection_male_female",
              br(),
              fluidRow(column(12,tags$div(HTML(toupper("<h4><strong>Graph show the difference btn male and female affected with Diseases</strong></h4>"))),offset =0)),
              br(),
              br(),
              br(),
              fluidRow(
               
                column(4,
                       selectInput( inputId= "male_female_infec", 
                                    label =  "Infection:", 
                                    choices =  infection)
                ),
                ),
              
              fluidRow(
                column(12, plotOutput("male_female_infection"))
              ),
              br(),
              br(),
              br(),
              
              fluidRow(
                column(12, plotOutput("total_male_female_infection"))
              )
      ),
      
      
      #end of infection of male and female 
      
      #marriage base on education
      
      tabItem(tabName = "marriage",
              br(),
              fluidRow(column(12,tags$div(HTML(toupper("<h4><strong>Graph show mother of what education are more Married</strong></h4>"))),offset =0)),
              br(),
              
              
              fluidRow(
                column(12, plotOutput("marriage_educ"))
              )
              
              
              
      )
      ,
      #end of marriage base on education
      
      #start dataset
      
      tabItem(tabName = "dataset",
              
              fluidRow(
                column(12, DT::dataTableOutput("table"))
              )
              
              
              
      )
      
      #end dataset
       
      )
      
      
    ),
    
    
    
    
    
  )
  
  
  
  #end of body

