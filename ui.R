library(shinydashboard)
library(rCharts)
library(highcharter)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title= "GoalKeeper",
      dropdownMenuOutput(outputId="AboutMenu"),
      dropdownMenuOutput(outputId="homehelp"),
      dropdownMenuOutput("messageMenu"),
      dropdownMenuOutput("notificationsMenu"),
      dropdownMenuOutput("progressBar")
      
    ),
    dashboardSidebar( 
      sidebarMenu(id ="activetab",
                  menuItem("Home", tabName = "home", icon = icon("home")),
                  menuItem("Overview", tabName = "overview", icon = icon("gears")),
                  menuItem("Achievements", tabName = "achievements", icon = icon("signal")),
                  menuItem("Tips", tabName = "tips", icon = icon("pencil-square-o")),
                  menuItem("LeaderBoard", tabName = "lb", icon = icon("pencil-square-o")),
                  menuItem("The Wall", tabName = "wall", icon = icon("pencil-square-o")) #icon 
                  
      )
    ),
    dashboardBody(  
      
      # tags$style(type="text/css",
      #            ".shiny-output-error { visibility: hidden; }",
      #            ".shiny-output-error:before { visibility: visible; content: 'An error occurred. Please contact the admin.'; }"
      # ),
      includeCSS("custom.css"),
      tags$head(
        tags$style(HTML('#button1{background-color: transparent;
                                color: DarkRed;border-color: transparent}',
                        '#button2{background-color: transparent;
                        color: DarkRed;border-color: transparent}',
                        '#message7{color : SlateBlue}'
        )),
        
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
      tabItems(
        tabItem(tabName="home",
                column(width=7,
                       
                       # "client_id=",input$client_id,"&",
                       # "client_secret=",input$client_secret,"&",
                       # "grant_type=",input$grant_type,"&",
                       # "username=",input$username,"&",
                       # "password=",input$password
                       fluidRow(
                         box(title="Connect with your API credentials",status="primary",width=12,
                             textInput("clientid", label= "Eurobank API Key", value="contestant"),
                             textInput("clientsecret", label= "Eurobank API Secret",value="secret"),
                             textInput("username", label= "Username",value="cray"),
                             passwordInput("password", label= "Password",value="addodsnjej"),
                             actionButton('auth','Authenticate'),
                             textOutput('authenticationmessage'),
                             textOutput('tokenmessage')
                             
                             
                         )
                       ),
                       fluidRow(
                         box(title="Set Your Goals",status="primary",width=12,
                             selectInput("goalcat1", label= "Set Goal Category",choices=c("Personal","Group Project","Branch")),
                             numericInput("goalval1", label= "Set Goal Value", value= ""),
                             dateInput("goaldl1",label="Set Goal Deadline")
                         )
                       )
                       
                       
                ),
                column(width=5,
                       fluidRow(
                         uiOutput("clientinfoui")
                       ))
        ),
        tabItem(tabName="overview",
                column(width=12,
                       fluidRow(
                         uiOutput("infocards"),uiOutput("infosales"),
                         column(width=6,box(title="Card Progress",status="primary",width=12,
                                            tableOutput("test"),
                                            highchartOutput("diagrammachart")
                                            
                         )),
                         
                         column(width=6,box(title="Deposits Progress",status="primary",width=12,
                                            highchartOutput("diagrammachart2")
                                          #  showOutput("plot2","highcharts"),HTML('<style>.rChart {width: 100%; height: 600px}</style>')
                                            )
                                )

                         )
                       ,
                       fluidRow(
                         box(title="Add Planned Goal",status="primary",width=12,
                             selectInput("plangoal", label= "Set Goal Category",choices=c("Cards","POS","Deposits")),
                             numericInput("plangoalval", label= "Set Goal Value", value= ""),
                             dateInput("plangoalexp",label="Set Expected Complete date"),
                             actionButton("setplanedgoal","Save Changes")
                         )
                       )
                       
                       
                       
                ),
                column(width=5,
                       fluidRow(
                         uiOutput("userinfo")
                       )),
                fluidRow()
        ),
        tabItem(tabName="achievements",
                column(width=12,
                       fluidRow(
                         box(title="Current Month Competition",status="primary",width=12,
                             uiOutput("cbranchleadsbadgeboc"),uiOutput("plantinseeds")),
                         box(title="Past Competitions",status="primary",width=12,
                             uiOutput("iamthechampion"),uiOutput("topofthehill"))
                         
                       )
                )
        ),
        tabItem(tabName="tips",
                selectizeInput("keyword",label="Choose #Hashtag",choices=c("#Deposits","#POS","#Debit_Cards","#Group_Service_Accounts"),multiple=TRUE),
                uiOutput('uichoice')
                # column(width=12,
                #        textOutput('badge1'),
                #        fluidRow(column(width=1,
                #                        imageOutput('image8', height = "100px")),column(width=7,
                #                                                      box(title="Electra",status="primary",width=12,
                #                                                      textOutput('tip1'))),column(width=2,
                #                                                      br(),
                #                                                      actionButton('button8', "Like", icon('heart-o')))),
                #        textOutput('badge2'),
                #        fluidRow(column(width=1,
                #                        imageOutput('image9', height = "100px")),column(width=7,
                #                                                         box(title="Axilleas",status="primary",width=12,
                #                                                         textOutput('tip2'))),column(width=2,
                #                                                         br(),
                #                                                         actionButton('button9', "Like",icon('heart-o'))))
                #        
                # )
        ),
        tabItem(tabName="lb",
                column(width=12,
                       fluidRow(
                         
                         box(title="Branch Leaderboard",status="primary",width=4,
                             uiOutput('branchleads')
                         ),
                         box(title="Top of the Top",status="primary",width=8,
                             uiOutput('topofthetop')
                         )
                         
                       ),
                       fluidRow()
                       
                ) 
        ),
        tabItem(tabName="wall",
                column(width=12,
                       textOutput('message6'),
                       fluidRow(column(width=1,
                                       imageOutput('image1', height = "100px")),column(width=7,
                                                                                       box(title="Electra",status="primary",width=12,
                                                                                           textOutput('message1'))),column(width=2,
                                                                                                                           br(),
                                                                                                                           actionButton('button1', "Like", icon('heart-o')))),
                       textOutput('message5'),
                       fluidRow(column(width=1,
                                       imageOutput('image2', height = "100px")),column(width=7,
                                                                                       box(title="Axilleas",status="primary",width=12,
                                                                                           textOutput('message2'))),column(width=2,
                                                                                                                           br(),
                                                                                                                           actionButton('button2', "Like",icon('heart-o')))),
                       uiOutput('invisible'),
                       fluidRow(column(width=1,
                                       imageOutput('image3', height = "100px")),column(width=7,
                                                                                       box(title="Post your message..",status="primary",width=12,
                                                                                           textInput('message3', " ", " "))),column(width=2,
                                                                                                                                    br(),
                                                                                                                                    br(),
                                                                                                                                    actionButton('button3', "Post", icon = NULL, 
                                                                                                                                                 width = '58%'))
                       )
                       
                ))
      )
    )
  )
)
