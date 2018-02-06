#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/


# shiny dashboard!!!
shinyServer(function(input, output) {
  library(plyr)
  library(ggplot2)
  library(reshape2)
  library(lubridate)
  library(tree)
  library(randomForest)
  library(scales)
  library(plotly)
  library(RColorBrewer)
  wd <- setwd("C:/Users/yifei.liu/Documents/hsylc")
  db <- readxl::read_excel("hsylc raw data.xlsx", 1, col_names = T)
  
  ############################## function ##########################################
  generatesummary <- function(db, grp.var){
    require(dplyr)
    require(lazyeval)
    db %>% 
      mutate_(weight = 1) %>%
      select(grp.var, 'weight') %>%
      group_by_(grp.var) %>%
      summarise_(value = interp(~sum(v, na.rm = TRUE), v=as.name('weight'))) 
  }
  
  # check duplicate applicants record
  # count chinese names and email address see if there are duplicate records
  # could add in more vairables in checkup.varlist, variables included in list will
  # be used as criteria to define duplicate recard
  clean.duplicate.record <- function(db, checkdup.varlist, delete.crit){
    
    # initialize the list need to delete and the list need to keep
    # at the end of function, remove user_id of del list from db
    del<- NULL
    keep<- NULL
    
    # loop through each variable in checkup.varlist to grab duplicate recard
    for (m in 1:length(checkdup.varlist)){ 
      # grab duplicate record based on current checkup.varlist variable
      dup.var <- as.data.frame(count(db, vars = checkdup.varlist[m]))
      dup.var <- dup.var[dup.var$freq>1,]
      # if the duplicate record list is not empty
      if(nrow(dup.var) != 0){
        # loop through all duplicated record
        for (i in 1 : nrow(dup.var)){
          # dup is a temperary list to put duplicated ppl, grab everyone who has same
          # duplicated variable record
          dup<- db[which(db[checkdup.varlist[m]] == dup.var[i,1]), ] 
          # add in some function here to delete duplicate case
          for (n in 1 : length(delete.crit)){
            max.var<- max((dup[delete.crit[n]]))
            del <- rbind(del, dup[which(dup[delete.crit[n]] < max.var), ])
            dup <- dup[which(dup[delete.crit[n]] == max.var), ]
            if (nrow(dup) == 1) break
          }
          keep<- rbind(keep, dup)
        }
      }
      rm(dup.var)
    }
    
    duplicate<- NULL
    for (i in 1:nrow(db)){
      if (db$user_id[i] %in% del$user_id){
        duplicate[i] = T
      } else {
        duplicate[i] = F
      }
    }
    
    db<- cbind(db, duplicate)
    
    outputset <- subset(db, db$duplicate==F)
    
    return(list(outputset, del))
  }
  
  # close look at promotion channel
  # make sure the dataset passed to function promotion.channel includes user_id and
  # info_resource variables
  # the function will return a promotion channel reach frequency table.
  promotion.channel <- function(db){
    meltlist <- colnames(db)
    meltlist <- meltlist[meltlist!="info_resource"]
    totaln <- nrow(db)
    db <- cbind(db[,meltlist], colsplit(db$info_resource, "," , 
                                        c("promo1", "promo2", "promo3", "promo4", "promo5")))
    promotion <- melt(db, id=meltlist, value.name = "channel")
    promotion <- subset(promotion, promotion$channel!="")
    return(promotion)
  }
  
  # clean up the active time and extract the weekday and hour of active time
  # transform the time data into long format
  active.time <- function(db, var, times){
    active.times <- melt(db[,c("user_id","user_insertedtime", "user_timestamp_1", "user_timestamp_2", 
                               "user_timestamp_3", "user_timestamp_4", "user_pay_app_time",
                               "user_ad_time", "user_pay_confirm_time")]
                         , id="user_id", na.rm = TRUE)
    active.times$wkday <- weekdays(active.times$value)
    active.times$hour <- hour(active.times$value)
    active.times <- active.times[active.times$variable %in% var, ]
    active.times <- active.times[active.times$wkday %in% times, ]
    return(active.times)
  }

  insert.time <- function(db){
    db$user_insertedtime <- as.Date(db$user_insertedtime)
    return(db)
  }
    
  # campus preference, clean the campus first
  cleancampus <- function(db, falsevalue, correctvalue){
    campus <- db[, c("user_id", "campus_preference", "weight")]
    if (is.null(falsevalue) == FALSE){
      campus1 <- campus[which(campus$campus_preference != falsevalue),]
      campus2 <- campus[which(campus$campus_preference == falsevalue),]
      campus2$campus_preference <- correctvalue
      campus<- rbind(campus1, campus2)}
      return (campus)
  }
  
  # clean campus and split column, format the splited campus preference into long format  
  campus <- function(db){
    db <- cleancampus(cleancampus(db, "21571", "21,571"), "10571", "10,571")
    db <- cbind(db[,c('user_id', 'weight')], colsplit(db$campus_preference, "," ,
                                                            c("campus1","campus2", "campus3", "campus4")))
    db <- melt(db, id = "user_id", na.rm = TRUE)
    db$campus <- as.factor(db$value)
    return(db)
  }
  
  
  # payment platform,
  payment.platform <- function(db, 
                                payment.var = c('Application Payment', 'Tuition Payment'),
                                multiple = TRUE){
    payment_app <- db[is.na(db$app_pay_platform) == FALSE, c("user_id","app_pay_platform", "pay_order_num1")]
    colnames(payment_app) <- c("user_id", "platform", "order_number")
    payment_app$variable <- 'Application Payment'
    payment_tui <- db[is.na(db$tui_pay_platform) == FALSE, c("user_id","tui_pay_platform", "pay_order_num2")]
    colnames(payment_tui) <- c("user_id", "platform", "order_number")
    payment_tui$variable <- 'Tuition Payment'
    payment <- rbind(payment_app, payment_tui)
    payment$weight <- 1
    
    if(is.null(payment.var) == TRUE){payment <- NULL
        }else if(multiple == TRUE){
          payment <- payment[payment$variable %in% payment.var & payment$order_number > 1,]
        }else{
          payment <- payment[payment$variable %in% payment.var, ]
        }
  return(payment)
  }

    ############################ Clean Original Data Set ###########################
  clean.dup.list1 <- c("user_chinaname","email") # this list is the list to define duplication
  clean.dup.list2 <- c("user_status", "last_login_time") # this list is the list to decide which duplicated case to keep which to remove
  
  db <- db %>%
    mutate(weight = 1) %>%
    mutate(register_step1.time = as.numeric(difftime(user_timestamp_1, user_insertedtime, units = "days"))) %>%
    mutate(step1_step2.time = as.numeric(difftime(user_timestamp_2, user_timestamp_1, units = "days"))) %>%
    mutate(step2_step3.time = as.numeric(difftime(user_timestamp_3, user_timestamp_2, units = "days"))) %>%
    mutate(step3_step4.time = as.numeric(difftime(user_timestamp_4, user_timestamp_3, units = "days"))) %>%
    mutate(step4_payapp.time = as.numeric(difftime(user_pay_app_time, user_timestamp_4, units = "days"))) %>%
    mutate(payapp_ad.time = as.numeric(difftime(user_ad_time, user_pay_app_time, units = "days"))) %>%
    mutate(ad_payconfirm.time = as.numeric(difftime(user_pay_confirm_time, user_ad_time, units = "days"))) %>%
    mutate(ad = as.factor(ifelse(user_ad_status>=3,1,0)))
  
  clean <- clean.duplicate.record(db, clean.dup.list1, clean.dup.list2)[[1]]
  ds <- reactive({
    subset(clean, user_insertedtime >= input$dateRange[1] & user_insertedtime <= input$dateRange[2])
  })
  
  set.seed(1)
  ad <- clean[, c('ad', "essay_word_num1", "essay_word_num2", "essay_word_num3", "essay_word_num4","other_material_num", "activity_num", "honor_num")]
  tree.rf<- randomForest(ad~., data = ad ,
                         mtry=2, ntree= 500, nodesize = 20, keep.forest = T, importance=T, na.action = na.omit, 
                         type = classfication)
  imp <- as.data.frame(importance(tree.rf))
  imp <- imp[order(imp$MeanDecreaseGini, decreasing = TRUE), ]
  imp$variable <- rownames(imp)
  imp <- imp[,c('variable', 'MeanDecreaseGini')]
  
  ############################### data set list for plot ##########################
  dl1 <- reactive({generatesummary(ds(), input$demo)})
  dl2 <- reactive({generatesummary(promotion.channel(ds()), 'channel')})
  dl3 <- reactive({active.time(ds(), input$active.time.var, input$active.time.weekday)})
  dl4 <- reactive({generatesummary(insert.time(ds()[, c("user_id", "user_insertedtime")]), 'user_insertedtime')})
  dl5 <- reactive({melt(ds()[, c('ad', input$application.stage)], id = 'ad', na.rm = TRUE)})
  dl6 <- reactive({campus(ds())}) 
  dl7 <- reactive({payment.platform(ds(), input$payment, input$multiple.payment)})
  dl8 <- reactive({melt(ds()[, c('ad', "essay_word_num1", "essay_word_num2", "essay_word_num3", "essay_word_num4","other_material_num")],id = 'ad', na.rm = TRUE)})
  dl9 <- reactive({melt(ds()[, c('ad', "activity_num", "honor_num")],id= "ad", na.rm = TRUE)})

  #################################### Output ######################################
  #1, chart for demo variables
  output$demoplot <- renderPlotly({
    plot_ly(dl1(), values = ~value, type = 'pie', textposition = 'outside', textinfo = 'label+percent',
            marker = list(color = brewer.pal(2, 'Set2')))%>%
      layout(title = 'Demographic Information',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  #2, chart for promotion channels
  output$promoplot <- renderPlotly({
    plot_ly(dl2(), x= ~channel, y=~value, type = 'bar', textposition = 'outside',
            marker = list(color = brewer.pal(6, 'Paired'))) %>%
      layout(title = 'Number of Application by Promotion Channel',
             xaxis = list(title = 'promotion channel', showgrid = FALSE), 
             yaxis = list(title = 'number of applicant', showgrid = FALSE))
  })
  
  #3, chart for application process variables activity time,
  output$activetime <- renderPlotly({
    data <- generatesummary(dl3(), 'hour')   
    plot_ly(data, x=~hour, y=~value, type = 'scatter', mode = 'lines') %>%
      layout(title = 'User Activity by Time',
             xaxis = list(title = 'Hour', showgrid = FALSE),
             yaxis = list(title = 'Active User Count', showgrid = FALSE))
  })
  
  #4, plot the trend for new user count
  output$new.user <- renderPlotly({
    plot_ly(dl4(), x= ~user_insertedtime, y= ~value, type = 'bar', textposition = 'outside') %>%
      layout(title = 'Daily New Applicant Number',
             xaxis = list(title = 'Date', showgrid = FALSE),
             yaxis = list(title = 'New Applicant Number', showgrid = FALSE))
  })
  
  #5, step time
  output$steptime <- renderPlotly({
    plot_ly(dl5(), x = ~variable, y = ~value, color = ~ad, type = 'box')%>%
      layout(boxmode = 'group',
             title = "Time Spent per Application Step",
             yaxis = list(title = 'days spent', showgrid = FALSE))
  })
  
  #6, campus preference,
  output$campusplot <- renderPlotly({
    campus <- generatesummary(dl6(), 'campus')
    plot_ly(campus, x=~campus, y=~value, type = 'bar', textposition = 'outside',
            marker = list(color = brewer.pal(6, 'Paired'))) %>%
      layout(title = 'Campus Preference',
             xaxis = list(title = 'campus', showgrid = FALSE), 
             yaxis = list(title = 'number of applicant', showgrid = FALSE))
  })
  
  #7, payment platform,
  output$payment.platform <- renderPlotly({
    payment <- generatesummary(dl7(), 'platform')
    plot_ly(payment, x = ~platform, y = ~value, type = 'bar', textposition = 'outside',
            marker = list(color = brewer.pal(11, 'Spectral'))) %>%
      layout(title = 'Payment Platform Summary',
             xaxis = list(title = 'platform', showgrid = FALSE),
             yaxis = list(title = 'payment count', showgrid = FALSE))
  })
  
  #8, essay admission decision
  output$essay <- renderPlotly({
    plot_ly(dl8(), x = ~variable, y = ~value, color = ~ad, type = 'box')%>%
      layout(boxmode = 'group',
             title = "Essay Word Number by Admission Result",
             yaxis = list(title = 'Number of word', showgrid = FALSE))
  })
  
  #9, hour activity admission decision
  output$activity.hour <- renderPlotly({
    plot_ly(dl9(), x = ~variable, y = ~value, color = ~ad, type = 'box')%>%
      layout(boxmode = 'group',
             title = "Activity and Honor Number by Admission Result",
             yaxis = list(title = 'Number of Activity/Honor', showgrid = FALSE))
  })
  
  output$material.summary <- renderTable({
    toy <-as.data.frame(summary(ds()[, c("essay_word_num1", "essay_word_num2", "essay_word_num3", 
                        "essay_word_num4", "honor_num", "activity_num", "other_material_num")]))
    toy <- cbind(toy$Var2, colsplit(toy$Freq, ":", c("statistics", "value")))
    colnames(toy)<- c("variable", "statistics", "value")
    toy <- reshape(toy, idvar = "variable", timevar = "statistics", direction = "wide")
    toy
  })
  
  #10 summary on admission variable importance
  output$importance <- renderTable({ imp })
  
})
