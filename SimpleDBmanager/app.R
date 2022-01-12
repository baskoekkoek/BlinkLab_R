# New subject creator
library(signal)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyFeedback)
library(RPostgres)
library(DBI)
library(DT)


df <- 'empty'

Connect2BlinkLabDB <- function() {
  con <- dbConnect(RPostgres::Postgres(),dbname = 'blinklabdb', 
                   host = 'blinklabdb.caknbcvpnqnz.eu-central-1.rds.amazonaws.com', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                   port = 5432, # or any other port specified by your DBA
                   user = 'dbadmin',
                   password = '1970b.k.')
  return(con)
  
}

insertEntry <- function(con,query){
  rs <- dbSendQuery(con,query)
  dbClearResult(rs)
}

getNewID <- function(con,table) {
  query = paste("select max(id) from ",table)
  print(query)
  newID=dbGetQuery(con,query)$max +1
  return(newID)
}

checkUserAndPW <- function(con,user,pw){
  query=paste("SELECT (password = crypt('",pw,"', password)) AS match FROM users where user_name='",user,"';",sep="")
  OK=dbGetQuery(con,query)
  return(OK)
}

GetColorsAndCallsings <- function(con) {
  query = paste("select callsign from callsigns")
  callsigns <- dbGetQuery(con,query)
  query = paste("select color from callsign_colors")
  colors <- dbGetQuery(con,query)
  query = paste("select gender_identity from gender_identities")
  gender_identities <- dbGetQuery(con,query)
  return(list(colors,callsigns,gender_identities))
}

getUniquesFromColumn <- function(con,table,field){
  query=paste("select distinct ",field," from ",table,sep="")
  uniques=dbGetQuery(con,query)
  return(na.omit(uniques))
}

getSubjectsByFilter <- function(con,DOB=NULL,Callsign=NULL,Color=NULL,Exp=NULL) {
  query="select id,color,callsign,first_name,last_name,sex,gender,date_of_birth from subject "
  if (is.null(DOB)){
    args=0
  } else {
    args=1
    query=paste(query,"where date_of_birth='",DOB,"'",sep="")
  }
  
  if (!is.null(Callsign)){
    args=args+1
    if (args==1){
      query=paste(query,"where ")
    } else {
      query=paste(query,"AND ")
    }
    query=paste(query,"Callsign='",Callsign,"'",sep="")
  }
  
  if (!is.null(Color)){
    args=args+1
    if (args==1){
      query=paste(query,"where ")
    } else {
      query=paste(query,"AND ")
    }
    query=paste(query,"Color='",Color,"'",sep="")
  }
  
  subjects=dbGetQuery(con,query)
  return(subjects)
}

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  fluidRow(
    img(src = "logo.png", height = 80, width = 317)
  ),
  fluidRow(
    tags$head(
      tags$style(type="text/css", "#inline label{ display: table-cell; text-align: center; vertical-align: middle; } 
                #inline .form-group { display: table-row;}")
    ),
    column(2,
           tags$div(id = "inline", textInput("user",label="Username :",value="")),
           tags$div(id = "inline",  passwordInput("pw",label="Password :",value="",width=NULL,placeholder=NULL))
    ),
    column(2, actionButton("submit",label="validate"))
  ),
  fluidRow(
    column(6, DT::dataTableOutput("SubjectTable"))
  ),
  fluidRow(
    column(width = 1, uiOutput("color", offset = 0 )),
    column(width = 1, uiOutput("callsign", offset = 0)),
    column(width = 1, uiOutput("firstname", offset = 0)),
    column(width = 1, uiOutput("lastname", offset = 0)),
    column(width = 1, uiOutput("sex", offset = 0)),
    column(width = 1, uiOutput("gender", offset = 0)),
    column(width = 1, uiOutput("dob", offset = 0))
  ),
  fluidRow(
    column(width = 1, uiOutput("addSubject", offset = 0))
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  rv <- reactiveValues()
  db <- Connect2BlinkLabDB()
  rv$db <- db
  callsignoptions = GetColorsAndCallsings(db)
  rv$callsigns <- setNames(callsignoptions[2],'callsign')
  rv$callsigncolors <- setNames(callsignoptions[1],'color')
  rv$gender_identities <- setNames(callsignoptions[3],'gender_identities')
  rv$sexe <- c('Male','Female')
  
  observeEvent(
    input$submit,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      print
      if (input$user!="" && input$pw!=""){
        ok=checkUserAndPW(rv$db,input$user,input$pw)
        if (ok==TRUE){
          rv$pw=TRUE
          print("yes")
          removeUI(selector = "div:has(> #user)")
          removeUI(selector = "div:has(> #pw)")
          removeUI(selector = "div:has(> #submit)")
          subjects=getSubjectsByFilter(db)
          output$SubjectTable<-renderDataTable({
            DT::datatable(subjects, selection='single', options=list(pagelength=12))
          })
          output$color <- renderUI({
            selectInput(inputId='color', label="color", choices = c("",rv$callsigncolors$color), selected=NULL, width = NULL)
          })
          output$callsign <- renderUI({
            selectInput(inputId='callsign', label="Callsign", choices = c("",rv$callsigns$callsign), selected=NULL, width = NULL)
          })
          output$firstname <- renderUI({
            textInput(inputId='firstname', label="First name", value="")
          })
          output$lastname <- renderUI({
            textInput(inputId='lastname', label="Last name", value="")
          })
          output$sex <- renderUI({
            selectInput(inputId='sex', label="sex", choices = c("",rv$sexe), selected=NULL, width = NULL)
          })
          output$gender <- renderUI({
            selectInput(inputId='gender', label="gender", choices = c("",rv$gender_identities$gender_identities), selected=NULL, width = NULL)
          })
          output$dob <- renderUI({
            dateInput(inputId="dob", label="date of birth", value = "", width = NULL, format="mm-dd-yyyy",startview="decade",weekstart=0,language="en",autoclose=TRUE,datesdisabled=NULL,daysofweekdisabled = NULL)
          })
          output$addSubject <- renderUI({
            actionButton(inputId='addSubject', label="Add new subject", width=150)
          })
        }
      }
    }  
  )
  
  observeEvent(
    input$addSubject,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,

    {
      newid=getNewID(rv$db,'subject')
      query=paste("insert into subject (id,callsign,first_name,gender,sex,last_name,color,date_of_birth) 
                   VALUES (",newid,",'",input$callsign,"','",input$firstname,"','",input$gender,"','",input$sex,"','",input$lastname,"','",input$color,"','",input$dob,"');",sep="")
      insertEntry(rv$db,query)
      subjects=getSubjectsByFilter(db)
      output$SubjectTable<-renderDataTable({
        DT::datatable(subjects, selection='single', options=list(pagelength=12))
      })
    }
  )
}

shinyApp(ui = ui, server = server)