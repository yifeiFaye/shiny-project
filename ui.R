#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
library(plotly)
library(shinydashboard)
library(shiny)
dashboardPage(
  # Application title
  dashboardHeader(title = "HSYLC Applicatoin Data Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  dashboardSidebar(
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = '2016-01-01',
                   end = Sys.Date()),
    
    selectInput(inputId = "demo",
                label = "Select Demographic Input",
                choices = c("gender", "user_status", "age", "user_student_type", "user_attent", "user_ad_status"),
                selectize = FALSE),

    checkboxGroupInput(inputId = 'active.time.var',
                       label = 'Select Time Stamp Variable to View Activity',
                       choices = c("user_insertedtime", "user_timestamp_1", "user_timestamp_2", 
                                   "user_timestamp_3", "user_timestamp_4", "user_pay_app_time",
                                   "user_ad_time", "user_pay_confirm_time"),
                       selected = c("user_insertedtime", "user_timestamp_1", "user_timestamp_2", 
                                    "user_timestamp_3", "user_timestamp_4", "user_pay_app_time",
                                    "user_ad_time", "user_pay_confirm_time")),
    
    checkboxGroupInput(inputId = 'active.time.weekday',
                       label = 'Select Weekday to View Activity',
                       choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                       selected = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
    
    checkboxGroupInput(inputId = "application.stage",
                       label = "Select the Application Stage to View Time Spent per Stage",
                       choices = c('register_step1.time', 'step1_step2.time', 'step2_step3.time', 'step3_step4.time', 'step4_payapp.time',
                                   'payapp_ad.time', 'ad_payconfirm.time'),
                       selected = c('register_step1.time', 'step1_step2.time', 'step2_step3.time', 'step3_step4.time', 'step4_payapp.time',
                                    'payapp_ad.time', 'ad_payconfirm.time')),
    
    checkboxGroupInput(inputId = 'payment',
                       label = 'Select the Payment Type',
                       choices = c('Application Payment', 'Tuition Payment'),
                       selected = c('Application Payment', 'Tuition Payment')),
    
    checkboxInput(inputId = 'multiple.payment',
                  label = 'Only Show Payment Platform with Multiple Payments Made by Applicants',
                  value = FALSE)
  ),
    
  dashboardBody(
    fluidRow(
      box(width = 4, plotlyOutput("demoplot")),
      box(width = 4, plotlyOutput("promoplot")),
      box(width = 4, plotlyOutput("activetime")),
      box(width = 4, plotlyOutput("new.user")),
      box(width = 4, plotlyOutput("steptime")),
      box(width = 4, plotlyOutput("campusplot")),
      box(width = 4, plotlyOutput("payment.platform")),
      box(width = 4, plotlyOutput("essay")),
      box(width = 4, plotlyOutput("activity.hour")),
      box(width = 8,
          title = "Application Material Summary",
          tableOutput("material.summary")),
      box(width = 4, 
          title = "Importance of Application Material",
          tableOutput("importance"))
    )
  )
)

