#########Libraries##########
library(shiny)
library(data.table)
library(shinydashboard)
library(jsonlite)
library(httr)
library(rCharts)
library(plyr)
library(highcharter)
library(forecast)
library(ShinyRatingInput)
library(psych)


`%then%` <- shiny:::`%OR%`

##########Shiny server.R##########
shinyServer(function(input, output, session) { 
  
  observeEvent(input$activetab, {
    updatemenu()
  })
  output$homehelp <- renderMenu({
    updatemenu()
  })
  updatemenu<-reactive({
    if (input$activetab=="home"){
      dropdownMenu(type = "message", icon=icon("question-circle"), badgeStatus="primary",
                   messageItem(
                     from='',
                     message = tags$div(HTML('<h4> Connect</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:70px;">
                                             &nbsp; Please, provide your <br>
                                             &nbsp;  API credentials to  <br> 
                                             &nbsp;  connect with the EuroBanker<br>
                                             &nbsp;  service! <br> 
                                             You can proceed to set your goals<br>
                                             or use the Bank Service Facilitator<br>
                                             
                                             <br>  '), style = "display: inline-block; vertical-align: left;"), icon("calendar-minus-o")
                     ))
    }
    else if (input$activetab=="overview"){
      dropdownMenu(type = "message", icon=icon("question-circle"), badgeStatus="primary",
                   messageItem(
                     from='',
                     message = tags$div(HTML('<h4> View Progress</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:70px;">
                                             &nbsp; See your business <br>
                                             &nbsp; progress visualized  <br> 
                                             &nbsp; and make according <br>
                                             &nbsp; decisions! <br> 
                                             You can also compare your current<br>
                                             progress with previous months.<br>
                                             
                                             <br>  '), style = "display: inline-block; vertical-align: left;"), icon("calendar-minus-o")
                     ))
    }
    else if (input$activetab=="faq"){
      dropdownMenu(type = "message", icon=icon("question-circle"), badgeStatus="primary",
                   messageItem(
                     from='',
                     message = tags$div(HTML('<h4> FAQ</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:70px;">
                                             &nbsp; Find what you need <br>
                                             &nbsp; for your next visit  <br> 
                                             &nbsp; and never go to the <br>
                                             &nbsp; bank just to wait ever! <br> 
                                             again! Obtain the correct papers<br>
                                             based on our database.<br>
                                             
                                             <br>  '), style = "display: inline-block; vertical-align: left;"), icon("calendar-minus-o")
                     ))
    }
    else if (input$activetab=="achievements"){
      dropdownMenu(type = "message", icon=icon("question-circle"), badgeStatus="primary",
                   messageItem(
                     from='',
                     message = tags$div(HTML('<h4> View Progress</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:70px;">
                                             &nbsp; See your business <br>
                                             &nbsp; progress and at what  <br> 
                                             &nbsp; percentage you <br>
                                             &nbsp; achieved your goals! <br> 
                                             Gain badges and ranks compared<br>
                                             to similar businesses.<br>
                                             
                                             <br>  '), style = "display: inline-block; vertical-align: left;"), icon("calendar-minus-o")
                     ))
    }
    else if (input$activetab=="Tips"){
      dropdownMenu(type = "message", icon=icon("question-circle"), badgeStatus="primary",
                   messageItem(
                     from='',
                     message = tags$div(HTML('<h4> FAQ</h4><p><img src="fileformat.png" alt="csvfileformat" align="left" style="width:50px;height:70px;">
                                             &nbsp; Find the tip you need <br>
                                             &nbsp; from the most suitable<br> 
                                             &nbsp; person, a fellow colleague<br>
                                             &nbsp; who was in your place! <br> 
                                             '), style = "display: inline-block; vertical-align: left;"), icon("calendar-minus-o")
                     ))
    }	    
    })
  output$selectbr<-renderUI({
    selectInput( 'addrselect', 'select branch', choices=branchaddress() )
  })
  
  branchaddress<-reactive({
    url<-"http://api.beyondhackathon.com/info/branches"
    branches <- fromJSON(url)
    addr<-branches$address1
    addr
  })
  observeEvent(input$auth,{
    client_id	<-input$clientid
    client_secret	<- input$clientsecret
    grant_type<-'password'
    username<-input$username
    password<-input$password
    #Use basic auth
    req <- httr::POST("http://api.beyondhackathon.com/authorization/token",
                      httr::add_headers(
                        "Content-Type"= "application/x-www-form-urlencoded",
                        "Accept" = 'application/json'
                      ),
                      # body = paste0(
                      #   "client_id=",input$client_id,"&",
                      #   "client_secret=",input$client_secret,"&",
                      #   "grant_type=",'password',"&",
                      #   "username=",input$username,"&",
                      #   "password=",input$password
                      # )
                      body = paste0(
                        "client_id=",client_id,"&",
                        "client_secret=",client_secret,"&",
                        "grant_type=",grant_type,"&",
                        "username=",username,"&",
                        "password=",password
                      )
    )
    #Extract the access token
    cred$token<-paste("Bearer", content(req)$access_token)
    if (length(content(req)$access_token)>0) {
      authmsg$text<-"Success!"
      cred$token <- paste("Bearer", content(req)$access_token)
    } else
      authmsg$text<-"Authorization failed!"
  })
  authmsg<-reactiveValues(text='')
  cred<-reactiveValues(token='')

  output$authenticationmessage<-renderText({
    authmsg$text
  })
  # output$tokenmessage<-renderText({
  #   cred$token
  # })
  clientinfo<-reactive({ 
    
    req<-httr::GET('http://api.beyondhackathon.com/customers/me/info',
                                         httr::add_headers(
                                           "Accept" = 'application/json',
                                           "Authorization" = cred$token
                                         ))
    content(req)
    
      
  })
  output$clientinfoui<-renderUI({
    box(title="Client Information",status="primary",width=12,
        h3("Client ID"), p(paste0(clientinfo()$id)),
        h3("Name"), p(paste0(clientinfo()$first_name," ",clientinfo()$last_name)),
        h3("Email"), p(paste0(unlist(clientinfo()$email_addresses)))
        )
  })
  accountdata<-reactive({
    url<-'http://api.beyondhackathon.com/customers/me/products'
    req <- httr::GET(url,
                     httr::add_headers(
                       
                       "Accept" = 'application/json',
                       "Authorization" = cred$token
                     )
    )
    ad<-1:length(content(req))
    for (i in 1:length(content(req))){
      ad[i]<-paste0(content(req)[[i]]$type,"-",as.character(content(req)[[i]]$contract_number))
    }
    ad
    

  })

  findlasttransactions<-reactive({
    input$activetab
    enddate<-format(Sys.Date(),"%Y-%m-%d")
    startdate<-seq(as.Date(enddate), length = 2, by = "-4 months")[2]
    accountid<- strsplit (accountdata(), "-")[[1]][2]

    url<-paste0('http://api.beyondhackathon.com/accounts/',accountid,"/statement?date_from=",startdate,"&date_to=",enddate)
    req <- httr::GET(url,
                     httr::add_headers(
                       "Accept" = 'application/json',
                       "Authorization" = cred$token
                     )
    );
    balance<-1:length(content(req))
    balance$Balance<-1:length(content(req))
    balance$Date<-1:length(content(req))
    for (i in 1:length(content(req))){
      balance$Balance[i]<-content(req)[[i]]$new_balance
      balance$Date[i]<-content(req)[[i]]$transaction_date
    }
    balance
  })
  
  
  findlasttransactions2<-reactive({
    input$activetab
    enddate<-format(Sys.Date(),"%Y-%m-%d")
    startdate<-seq(as.Date(enddate), length = 2, by = "-4 months")[2]
    accountid<- strsplit (accountdata(), "-")[[1]][2]
    

    url<-paste0('http://api.beyondhackathon.com/accounts/',accountid,"/statement?date_from=",startdate,"&date_to=",enddate)
    req <- httr::GET(url,
                     httr::add_headers(
                       "Accept" = 'application/json',
                       "Authorization" = cred$token
                     )
    )
    balance<-1:length(content(req))
    balance$Balance<-1:length(content(req))
    balance$Date<-1:length(content(req))
    for (i in 1:length(content(req))){
      balance$Balance[i]<-content(req)[[i]]$new_balance
      balance$Date[i]<-content(req)[[i]]$transaction_date
    }
    balance$Balance<-rev(balance$Balance)
    balance
  })
  output$plot2 <- renderChart2({
    hPlot(x = "Date", y = "Balance",  data = findlasttransactions2(), type = "line")
  })
  output$plot <- renderChart2({
    hPlot(x = "Date", y = "Balance",  data = findlasttransactions(), type = "line")
  })
  numberOfDays <- function(date) {
    m <- format(date, format="%m")
    
    while (format(date, format="%m") == m) {
      date <- date + 1
    }
    
    return(as.integer(format(date - 1, format="%d")))
  }
  output$diagrammachart <- renderHighchart({
    test1<-as.data.frame(findlasttransactions()$Date)
    test2<-as.data.frame(findlasttransactions()$Balance)
    cb<-cbind(test1,test2)
    colnames(cb)<-c("Date","Balance")
    cb[order(as.Date(cb$Date, format="%Y-%m-%d")),]
    
    diagrammaBAU <-cb
    maxdate<-max(as.Date(cb$Date, format="%Y-%m-%d"))
    maxdate<-as.Date(maxdate,origin="1960-10-01")
    maxdays<-numberOfDays(maxdate)
                            
    curr<-as.integer(format(as.Date(maxdate,format="%Y-%m-%d"), "%d"))
    horizon<-maxdays-curr
    b<-as.data.frame(forecast(as.numeric(as.character(diagrammaBAU$Balance)),h=horizon)$fitted)

    Date<-as.character(as.Date(seq(as.Date(maxdate), by = "day", length.out = horizon)))

    diagrammaBAUf<- cbind(head(b,horizon),Date)

    colnames(diagrammaBAUf)<-c("Balance","Date")
    
    if(mean(head(diagrammaBAUf$Balance),horizon/2)<mean(tail(diagrammaBAUf$Balance),horizon/2)) 
     showNotification("Warning","Your Cards Sales are falling compared to historical data... Consult the available hints to improve your performance",type="warning")
    diagrammaBAU<-rbind(diagrammaBAU,diagrammaBAUf)
    diagrammaBAU<-tail(diagrammaBAU,maxdays)
    
    hc <- highchart() %>% 
      hc_colors("#025664") %>%
      hc_xAxis(categories = diagrammaBAU$Date) %>%
      hc_tooltip(borderColor = "#025664") %>%
      hc_series(list(name = "Expected Achievements",
                     data = diagrammaBAU$Balance,
                     zoneAxis="x",
                     zones=list(list(value=length(diagrammaBAU$Date)-horizon, dashStyle="Solid"),
                                list(value=length(diagrammaBAU$Date), dashStyle="Dot")
                     )
      )
      )
    hc
  })
  rv<-reactiveValues(v=0)

  
  output$diagrammachart2 <- renderHighchart({
    test1<-as.data.frame(findlasttransactions2()$Date)
    test2<-as.data.frame(findlasttransactions2()$Balance)
    cb<-cbind(test1,test2)
    colnames(cb)<-c("Date","Balance")
    cb[order(as.Date(cb$Date, format="%Y-%m-%d")),]
    
    diagrammaBAU <-cb
    maxdate<-max(as.Date(cb$Date, format="%Y-%m-%d"))
    maxdate<-as.Date(maxdate,origin="1960-10-01")
    maxdays<-numberOfDays(maxdate)
    
    curr<-as.integer(format(as.Date(maxdate,format="%Y-%m-%d"), "%d"))
    horizon<-maxdays-curr
    b<-as.data.frame(forecast(as.numeric(as.character(diagrammaBAU$Balance)),h=horizon)$fitted)
    
    Date<-as.character(as.Date(seq(as.Date(maxdate), by = "day", length.out = horizon)))
    
    diagrammaBAUf<- cbind(head(b,horizon),Date)
    

    colnames(diagrammaBAUf)<-c("Balance","Date")

    if(mean(head(diagrammaBAUf$Balance),horizon/2)<mean(tail(diagrammaBAUf$Balance),horizon/2)) 
      showNotification("Warning!","Your Deposits are falling compared to historical data... Consult the available hints to improve your performance",type="Warning")
    
    
    diagrammaBAU<-rbind(diagrammaBAU,diagrammaBAUf)
    diagrammaBAU<-tail(diagrammaBAU,maxdays)
    
    hc <- highchart() %>% 
      hc_colors("#025664") %>%
      hc_xAxis(categories = diagrammaBAU$Date) %>%
      hc_tooltip(borderColor = "#025664") %>%
      hc_series(list(name = "Expected Achievements",
                     data = diagrammaBAU$Balance,
                     zoneAxis="x",
                     zones=list(list(value=length(diagrammaBAU$Date)-horizon, dashStyle="Solid"),
                                list(value=length(diagrammaBAU$Date), dashStyle="Dot")
                     )
      )
      )
    hc
  })
  
  
  
  observeEvent(input$setplanedgoal,{
    username<-input$username
    
    goals.file <- 'goals.rda'
    
    if(!file.exists(goals.file)) {
      # Create initial, empty file, otherwise errors will occur below
      GOALS <- data.frame(UserId = character(),
                          GoalCat=character(),
                          GoalVal=numeric(),
                          DeadLine = character(),
                          Achieved = character(),
                          stringsAsFactors = FALSE)
      save(GOALS, file=goals.file)
      load("goals.rda")
      
    }else{
      load("goals.rda")
      
    }
    
    
    newgoal<-data.frame(UserId = input$username,
                        GoalCat=input$plangoal,
                        GoalVal=input$plangoalval,
                        DeadLine = as.character(input$plangoalexp),
                        Achieved = "False",
                        stringsAsFactors = FALSE)
      
    GOALS<-rbind(GOALS,newgoal)
    save(GOALS, file=goals.file)
    
  })

lastmonthcards<-reactive({
  test1<-as.data.frame(findlasttransactions()$Date)
  test2<-as.data.frame(findlasttransactions()$Balance)
  cb<-cbind(test1,test2)
  colnames(cb)<-c("Date","Balance")
  
  cb$Date1<-1:nrow(cb)
  cb$Date2<-1:nrow(cb)
  
  for (i in 1:nrow(cb)){
  
  cb$Date1[i]<-strsplit(as.character(cb$Date[i]),"-")[[1]][1]
  
  cb$Date2[i]<-strsplit(as.character(cb$Date[i]),"-")[[1]][2]
  }

  cb$dt<-paste0(cb$Date1,cb$Date2)
  cb$dt<-as.integer(cb$dt)

  curr<-max(cb$dt)
  cb$Balance<-as.numeric(as.character(cb$Balance))
  newdata <- subset(cb, dt==curr)
  
  finalval<-round(sum(newdata$Balance)/10,0)
  as.character(finalval)
})

lastmonthsales<-reactive({
  test1<-as.data.frame(findlasttransactions2()$Date)
  test2<-as.data.frame(findlasttransactions2()$Balance)
  cb<-cbind(test1,test2)
  colnames(cb)<-c("Date","Balance")
  
  cb$Date1<-1:nrow(cb)
  cb$Date2<-1:nrow(cb)
  
  for (i in 1:nrow(cb)){
    
    cb$Date1[i]<-strsplit(as.character(cb$Date[i]),"-")[[1]][1]
    
    cb$Date2[i]<-strsplit(as.character(cb$Date[i]),"-")[[1]][2]
  }
  
  cb$dt<-paste0(cb$Date1,cb$Date2)
  cb$dt<-as.integer(cb$dt)
  
  curr<-max(cb$dt)
  cb$Balance<-as.numeric(as.character(cb$Balance))
  newdata <- subset(cb, dt==curr)
  
  finalval<-round(sum(newdata$Balance)/10,0)
  as.character(finalval)
})
output$infosales<-renderUI({
  infoBox("Deposits", paste0(as.character(lastmonthsales()),'/1000'), icon = icon("envelope-o"))
})

 output$infocards<-renderUI({
   infoBox("Cards Sold", paste0(as.character(lastmonthcards()),'/1000'), icon = icon("credit-card"))
 })
topofthetop<-reactive({
  leading<-leads()[,c(4,5,8,9)]
  c<-dfOrder(leading,c(3))
  res<-cbind(as.character(head(rev(c[,1]),n=10L)),(as.character(head(rev(c[,2]),10L))),(as.numeric(head(rev(c[,3]),10L))),(as.integer(head(rev(c[,4]),10L))))
  colnames(res)<-c("First Name","Last Name","GoalsAchieved","BranchName")
  res
})
topbranch<-reactive({
  c<-dfOrder(ddply(leads(), .(BranchName), summarise, GoalsAchieved=sum(GoalsAchieved)),c(2))
  res<-cbind(as.character(head(rev(c$BranchName),n=10L)),(as.numeric(head(rev(c$GoalsAchieved),10L))))
  colnames(res)<-c("BranchCode","GoalsAchieved")
  res
  })
output$testleads<-renderTable({
  topbranch()
})
output$branchleads<-renderUI({
  tableOutput("testleads")
})
output$toptop<-renderTable({
  topofthetop()
})
output$topofthetop<-renderUI({
  tableOutput("toptop")
})

# ll the way up -> kept rising for x consecutive months
# plantin the seeds -> went through the first steps to success
# top of the hill -> reached #1 position 
# i am the champion -> reached #1 position (ατομικο) 
# bullseye -> achieved target/goal
# fly me to the moon -> moved >5 spots up at once
# teamwork makes the dream work ->  have a >4.5 star rating on your tip
# strategist -> post 10 hints & tips
output$plantinseeds<-renderUI({
   div( h5("Went through the first steps to success!"),imageOutput("plantintheseeds",height='100px'))
})
output$plantintheseeds<- renderImage({
  return(list(
    src = "www/seeds.png",
    filetype = "png",
    alt = " ",
    width = 50
  ))
}, deleteFile = FALSE)
output$topofthehill<-renderUI({
  div(
    h5("You are the top of the top!"),imageOutput("ctopofthetopbadge",height='100px')
  )
})
output$iamthechampion<-renderUI({
  div(
    h5("Top ranked person!"),imageOutput("champion",height='100px')
  )
})
output$cbranchleadsbadgeboc<-renderUI({
  div(
    h5("Achieved Goal!"),imageOutput("cbranchleadsbadge",height='100px')
  )
})
output$cbranchleadsbadge<-renderImage({
  return(list(
    src = "www/png/Bullseye.png",
    filetype = "png",
    alt = " ",
    width = 50
  ))}
  , deleteFile = FALSE)
  output$ctopofthetopbadge<- renderImage({
    return(list(
      src = "www/png/top of the hill.png",
      filetype = "png",
      alt = " ",
      width = 50
    ))
}, deleteFile = FALSE)
  output$champion<- renderImage({
    return(list(
      src = "www/png/i am the champion.png",
      filetype = "png",
      alt = " ",
      width = 50
    ))
  }, deleteFile = FALSE)
leads<-reactive({
  leads<-read.csv("employees.csv")
})

output$message6<-renderText({
  "Bankerwoman"
})






output$messageMenu <- renderMenu({
  dropdownMenu(type = "message",
               messageItem(
                 from='Makis Dimakis',
                 message = tags$div(HTML("Otan to anoikseis tha deis
                                         anafores")),
                 time = "11:59"
                 
                 ),
               messageItem(
                 from='Director',
                 message = tags$div(HTML("Congratulations for your level up!")),
                 time = "17/3/2017"
               )
  )
})
output$notificationsMenu <- renderMenu({
  dropdownMenu(type = "notifications",
               notificationItem(
                 text = "Fly me to the moon",
                 icon("rocket"),
                 status = "success"
               ),
               notificationItem(
                 text = "I am the champion",
                 icon("trophy"),
                 status = "success"
               ),
               notificationItem(
                 text = "All the way up",
                 icon = icon("paper-plane"),
                 status = "success"
               )
  )
})
#if (TRUE) showNotification("Warning!","Sales not looking ok, find the tip of the day! ;)", duration=10, closeButton = TRUE, type = "warning")


output$image1 <- renderImage({
  return(list(
    src = "www/elec.png",
    filetype = "png",
    alt = " ",
    width = 85
  ))
  
}, deleteFile = FALSE)
output$image2 <- renderImage({
  return(list(
    src = "www/axilleas_raptis.png",
    filetype = "png",
    alt = " ",
    width = 85
  ))
  
}, deleteFile = FALSE)
output$image3 <- renderImage({
  return(list(
    src = "www/vspil.jpg",
    filetype = "jpg",
    alt = " ",
    width = 85
  ))
  
}, deleteFile = FALSE)
output$image8 <- renderImage({
  return(list(
    src = "www/elec.png",
    filetype = "png",
    alt = " ",
    width = 85
  ))
  
}, deleteFile = FALSE)
output$image9 <- renderImage({
  return(list(
    src = "www/vspil.jpg",
    filetype = "jpg",
    alt = " ",
    width = 85
  ))
  
}, deleteFile = FALSE)
output$image4 <- renderImage({
  return(list(
    src = "www/vspil.jpg",
    filetype = "jpg",
    alt = " ",
    width = 85
  ))
  
}, deleteFile = FALSE)


output$message1<-renderText({
  "Ha! Got you!"
  
  
})
output$message2<-renderText({
  "Got my first badge!"
})
output$message5<-renderText({
  "Wizard of EurOz"
  
  
  
  
})
output$message4<-renderText({input$message3})
output$message7<-renderText({"Next CEO"})


output$progressBar <- renderMenu({
  dropdownMenu(type = "tasks", badgeStatus = "success",
               taskItem(value = 86.5, color = "green",
                        "Deposits"
               ),
               taskItem(value = 17, color = "aqua",
                        "POS"
               ),
               taskItem(value = 82.5, color = "yellow",
                        "Debit Cards"
               ),
               taskItem(value = 70, color = "blue",
                        "Group Service Accounts"
               ),
               taskItem(value = 65, color = "red",
                        "Overall project"
               )
  )
})

observeEvent(input$button3,{
  output$invisible<-renderUI({
    
    column(width = 12, 
           fluidRow(column(width=7,
                           box(title=paste0(clientinfo()$first_name," ",clientinfo()$last_name),status="primary",width=12,
                               textOutput('message4'))),column(width=1,
                                                               imageOutput('image4', height = "100px"),
                                                               
                                                               textOutput('message7'))))
    
  })
  
    })

#tips
output$badge1<-renderText({
  "Bankerwoman"
  
  
  
  
})
output$tip1<-renderText({
  "#POS Post videos using Eurobank POS on tweeter"
})
output$badge2<-renderText({
  "Wizard of EurOz"
})
output$tip2<-renderText({
  "#Deposits Share on Facebook programms for new accounts for young people"
})





hch<-reactive({
  r<- NULL
  h<-as.data.frame(matrix(data=NA,nrow=2,ncol=2))
  h[1,]<-c(2,"#Deposits")
  h[2,]<-c(1,"#POS")
  colnames(h)<-c("tip","hashtag")
  
  #h$e<-subset(h, hashtag==paste0("#",keyword))
  hn<-h[h$hashtag %in% input$keyword,]
  paste0('tip',hn$tip)
})

output$uichoice<-renderUI({
  if (any(c("tip1") %in% hch()) && any(c("tip2") %in% hch())){
    div(textOutput('badge1'),
        fluidRow(column(width=1,
                        imageOutput('image8', height = "100px")),column(width=7,
                                                                        box(title="Electra",status="primary",width=12,
                                                                            textOutput('tip1'),htmlOutput("Rating11"))),column(width=2,
                                                                                                                               ratingInput("Rating1", label="Rate this tip", dataStop=5, dataFractions=2))),
        
        fluidRow(column(width=1,
                        imageOutput('image9', height = "100px")),column(width=7,
                                                                        box(title="Axilleas",status="primary",width=12,
                                                                            textOutput('tip2'),htmlOutput("Rating12"))),column(width=2,
                                                                                                                               ratingInput("Rating2", label="Rate this tip", dataStop=5, dataFractions=2))))
    
  }
  else if(any(c("tip1") %in% hch())){
    div(textOutput('badge1'),
        fluidRow(column(width=1,
                        imageOutput('image8', height = "100px")),column(width=7,
                                                                        box(title="Electra",status="primary",width=12,
                                                                            textOutput('tip1'),htmlOutput("Rating13"))),column(width=2,
                                                                                                                               ratingInput("Rating3", label="Rate this tip", dataStop=5, dataFractions=2))))
    
  }
  else if(any(c("tip2") %in% hch()))
  {
    div(textOutput('badge2'),
        fluidRow(column(width=1,
                        imageOutput('image9', height = "100px")),column(width=7,
                                                                        box(title="Axilleas",status="primary",width=12,
                                                                            textOutput('tip2'),htmlOutput("Rating14"))),column(width=2,
                                                                                                                               ratingInput("Rating4", label="Rate this tip", dataStop=5, dataFractions=2))))
    
  }
  else{
    div(textOutput('badge1'),
        fluidRow(column(width=1,
                        imageOutput('image8', height = "100px")),column(width=7,
                                                                        box(title="Electra",status="primary",width=12,
                                                                            textOutput('tip1'),htmlOutput("Rating15"))),column(width=2,
                                                                                                                               ratingInput("Rating5", label="Rate this tip", dataStop=5, dataFractions=2))),
        textOutput('badge2'),
        fluidRow(column(width=1,
                        imageOutput('image9', height = "100px")),column(width=7,
                                                                        box(title="Axilleas",status="primary",width=12,
                                                                            textOutput('tip2'),htmlOutput("Rating16"))),column(width=2,
                                                                                                                               ratingInput("Rating6", label="Rate this tip", dataStop=5, dataFractions=2))))
    
  }
  
  
})
output$Rating11 <- renderText({
  paste("Rating: ",input$Rating1)
})
output$Rating12 <- renderText({
  paste("Rating: ",input$Rating2)
})
output$Rating13 <- renderText({
  paste("Rating: ",input$Rating3)
})
output$Rating14 <- renderText({
  paste("Rating: ",input$Rating4)
})
output$Rating15 <- renderText({
  paste("Rating: ",input$Rating5)
})
output$Rating16 <- renderText({
  paste("Rating: ",input$Rating6)
})
output$trs<-renderText({
  hch()
}) 




})