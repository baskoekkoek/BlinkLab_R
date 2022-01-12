#CMS importer
library(signal)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyFeedback)
library(RPostgres)
library(DBI)
library(imputeTS)

# wd <- setwd("G:\\My Drive\\BlinkLab smartphone\\Data analysis\\R scripts\\ShinyBlinkLabImport")
# get helper functions
#source("BlinkLabHelperFunctions.R")

#rm(wd)
#wd <- setwd("G:\\My Drive\\BlinkLab smartphone\\Data analysis\\Measurement files")
#print(wd)

df <- 'empty'


Connect2BlinkLabDB <- function() {
  con <- dbConnect(RPostgres::Postgres(),dbname = 'blinklabdb', 
                   host = 'blinklabdb.caknbcvpnqnz.eu-central-1.rds.amazonaws.com', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                   port = 5432, # or any other port specified by your DBA
                   user = 'dbadmin',
                   password = '1970b.k.',
                   bigint="integer")
  return(con)
  
}

GetStimulationsByExpAndTrialName <- function(con,expname,trialname) {
  
  trialident=gsub("[0-9]*_trial-[0-9]*-","",trialname)
  
  query = paste("select  t.stimuli from trialtypes t inner join experiments e on e.id = t.experiment_id where e.experiment_name = '",expname,"' and t.trial_identifier = '",trialident,"'", sep='')
  stims = dbGetQuery(con,query)
  query = paste("select  stimulus_type, stimulus_onset, stimulus_duration, stimulus_amplitude, frequency from stimulations where id in (",stims[1,1],")", sep='')
  stimuli = dbGetQuery(con,query)  
  return(stimuli)
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

CheckNewImportForExistByNumber <- function(con,number) {
  query = paste("select id from session where cms_number=",number,"",sep="") 
  exist = dbGetQuery(con,query)
  return(nrow(exist)>0)
}

checkNewImportForExistByCallsign <- function(con,callsign,col,dob) {
  query = paste("select id from subject where callsign='",callsign,"' and color='",col,"' and date_of_birth='",dob,"'",sep="")
  id = dbGetQuery(con,query)$id
  return(id)
}

insertEntry <- function(con,query){
  rs <- dbSendQuery(con,query)
  dbClearResult(rs)
}

getExperimentId <- function(con,exp) {
  query = paste("select id from experiments where experiment_name='",exp,"'",sep="")
  expid = dbGetQuery(con,query)$id
  return(expid)
}

getNewID <- function(con,table) {
  query = paste("select max(id) from ",table)
  print(query)
  newID=dbGetQuery(con,query)$max +1
  return(newID)
}

getExistingresultsForSubjectAndExperiment <- function(con,exp,sub) {
  query = paste("select max(session_nr) from session where experiment_id=",exp," and subject_id=",sub,sep="")
  lastsession = dbGetQuery(con,query)$max
  return(lastsession)
}

checkUserAndPW <- function(con,user,pw){
  query=paste("SELECT (password = crypt('",pw,"', password)) AS match FROM users where user_name='",user,"';",sep="")
  OK=dbGetQuery(con,query)
  return(OK)
}

DisconnectBlinkLabDB <- function(con) {
  dbDisconnect(con)
}

GetDeviceTypes <- function(con) {
  query = paste("select device_type from device_type")
  devices=dbGetQuery(con,query)
  return(devices)
}

GetEarTypes <- function(con) {
  query = paste("select earphone_type from earphone_type")
  devices=dbGetQuery(con,query)
  return(devices)
}

GetEarTypeIDbyType <- function(con,type) {
  query = paste("select id from earphone_type where earphone_type='",type,"'",sep="")
  id=dbGetQuery(con,query)
  return(id)
}

GetDeviceIDs <- function(con) {
  query = paste("select id from device")
  devices=dbGetQuery(con,query)
  return(devices)
}

GetDeviceTypeIDbyType <- function(con,type) {
  query = paste("select id from device_type where device_type='",type,"'",sep="")
  id=dbGetQuery(con,query)
  return(id)
}

getDeviceTypeByID <- function(con,id){
  query = paste("select t.device_type,t.id from device d inner join device_type t on d.device_type_id=t.id where d.id=",id,sep="")
  type=dbGetQuery(con,query)
  return(type)
}

GetDataFrameFromCSV <- function(csv) {
  message(csv)
  df <- read.table(csv, sep = '\t', header = TRUE)
  df_split <- split(df, df$Trial.Name)
  return(df_split)
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
           tags$div(id = "inline", textInput("user",label="Username",value="")),
           tags$div(id = "inline",  passwordInput("pw",label="Password",value="",width=NULL,placeholder=NULL))
    ),
    column(2, actionButton("submit",label="validate"))
  ),
  fluidRow(
    column(8,uiOutput("Info"))
  ),
  fluidRow(
    column(8,uiOutput("filename"))
  ),
  fluidRow(
    column(4,textOutput("Message"))
  ),  
  fluidRow(
    column(3,uiOutput("CMS")),
    column(3,uiOutput("Device_type")),
    column(1,uiOutput("Device_id"))
  ),
  fluidRow(
    column(3,offset=3,uiOutput("ear_type")),
    column(3,uiOutput("nc"))
  ),
  fluidRow(
    column(3, uiOutput("Date")),
    column(4,uiOutput("Age"))
  ),
  fluidRow(
    column(3, uiOutput("Sexe")),
    column(4,uiOutput("Gender_identity"))
  ),
  fluidRow(
    column(3,uiOutput("Colors")),
    column(4,uiOutput("Callsigns"))
  ),
  fluidRow(
    column(4, offset=3,uiOutput("Go"))  
  ),
  fluidRow(
    column(3, progressBar(id="Progress",value = 0, total = 1,title = "Uploaded 0 trials",display_pct = FALSE))  
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
  rv$devicetypes <- GetDeviceTypes(db)
  rv$deviceIDs <- GetDeviceIDs(db)
  rv$eartypes <- GetEarTypes(db)
  rv$ncoptions <- c("Yes","No")
  rv$sexe <- c('Male','Female')
  rv$warning <- "no warning"
 
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
          removeUI(selector = "div:has(> #user)")
          removeUI(selector = "div:has(> #pw)")
          removeUI(selector = "div:has(> #submit)")
          rv$pw=TRUE
          output$Info <- renderText({
            HTML(paste0("Hi there,<br>This program imports data from the CMS into a convenient postgress database for easier analysis.  
                         Select a CSV measurement file downloaded from the CMS, then enter the <b>recording date</b>, 
                         the subjects <b>callsign</b> and original <b>CMS number</b>.<br>Please have these ready (check CMS).<br> If you experience a disconnect, 
                         the CSV might be corrupt.<br>"))
          })
          output$filename <- renderUI({
            fileInput(inputId='csv_filename', label="Choose a csv measurement file", accept='.csv', width='800px', multiple=FALSE, placeholder = "no file selected")
          })
        }
      }
    }  
  )
#  event handling of updating the csv filepath
  observeEvent(
    input$csv_filename,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    { 
      rv$expname=gsub('_measurements.csv',"",input$csv_filename$name)
      rv$exp_id = getExperimentId(rv$db,rv$expname)
      if (rv$exp_id>0){
        rv$df <- read.table(toString(input$csv_filename$datapath), sep = '\t', header = TRUE)
        rv$df_split <- split(rv$df, rv$df$Trial.Name)
        # define butterworth filter
        filt <- function(n) {
          if (is.na (n$Left.Eye.FEC [1]) == TRUE) { # if first elem is zero, replace it with average of whole trace
            n$Left.Eye.FEC [1] <- mean(n$Left.Eye.FEC)
          }
          if (is.na (n$Right.Eye.FEC [1]) == TRUE) { # if first elem is zero, replace it with average of whole trace
            n$Right.Eye.FEC [1] <- mean(n$Right.Eye.FEC)
          }
          if (is.na (n$Left.Eye.FEC [nrow(n)]) == TRUE) { # if last elem is zero, replace it with average of whole trace
            n$Left.Eye.FEC [nrow(n)] <- mean(n$Left.Eye.FEC)
          }
          if (is.na (n$Right.Eye.FEC [nrow(n)]) == TRUE) { # if last elem is zero, replace it with average of whole trace
            n$Right.Eye.FEC [nrow(n)] <- mean(n$Right.Eye.FEC)
          }
          
          n$Left.Eye.FEC <- imputeTS::na_interpolation(n$Left.Eye.FEC) # NA interpolate
          n$Right.Eye.FEC <- imputeTS::na_interpolation(n$Right.Eye.FEC)
          
          
          bf <- butter(2, 1/3, type="low") # filter
          n$Left.Eye.FEC <- filtfilt(bf, n$Left.Eye.FEC)
          
          bf <- butter(2, 1/3, type="low") # filter
          n$Right.Eye.FEC <- filtfilt(bf, n$Right.Eye.FEC)
          n$max_frameNr <- max(n$Frame.number)
          return(n)
        }
        # apply function on all splits
        rv$df_list <- lapply(rv$df_split, filt)
        # make new dataset1 with new columns added
        rv$df <- do.call(rbind, rv$df_list)
        # back to normal rownames
        rownames(rv$df) <- 1:nrow(rv$df)
        # go to single value per timepoint (average over multiple cases)
        rv$df_aggr <- rv$df %>% group_by (Trial.Name) %>% summarise_at (c("max_frameNr"), funs( mean (., na.rm=T)))
        rv$df <- rv$df_aggr
        #View(rv$df)
        
        output$Message <- renderText({
          paste("Loaded a dataset with ",nrow(rv$df)," trials")
        })
        output$Colors <- renderUI({
          selectInput(inputId='Color', label="Subject's color", choices = c("",rv$callsigncolors$color), selected=NULL, width = NULL)
        })
        output$Callsigns <- renderUI({
          selectInput(inputId='Callsign', label="Subject's Callsign", choices = c("",rv$callsigns$callsign), selected=NULL, width = NULL)
        })
        output$Sexe <- renderUI({
          selectInput(inputId='Sexe', label="Subject's sex at birth", choices = c("",rv$sexe), selected=NULL, width = NULL)
        })
        output$Gender_identity <- renderUI({
          selectInput(inputId='Gender_identity', label="Gender subject identifies with the most", choices = c("",rv$gender_identities$gender_identities), selected="Cis", width = NULL)
        })
        output$CMS <- renderUI({
          numericInput(inputId="CMS_number", label="CMS number", min = 1, value = "", width = NULL)
        })
        output$Device_type <- renderUI({
          selectInput(inputId='Device_type', label="recording device", choices = c("",rv$devicetypes$device_type), selected="unknown", width = NULL)
        })
        output$ear_type <- renderUI({
          selectInput(inputId='ear_type', label="Earphones", choices = c("",rv$eartypes$earphone_type), selected="unknown", width = NULL)
        })
        output$nc <- renderUI({
          selectInput(inputId='nc', label="noise cancelation", choices = c("",rv$ncoptions), selected="unknown", width = NULL)
        })
        output$Device_id <- renderUI({
          selectInput(inputId='Device_id', label="device id", choices = rv$deviceIDs$id, selected="1", width = NULL)
        })
        output$Date <- renderUI({
          dateInput(inputId='Date', label="Recording date:", value = "", width = NULL, format="mm-dd-yyyy",startview="month",weekstart=0,language="en",autoclose=TRUE,datesdisabled=NULL,daysofweekdisabled = NULL)
        })
        output$Age <- renderUI({
          dateInput(inputId="Age", label="Subject's date of birth", value = "", width = NULL, format="mm-dd-yyyy",startview="decade",weekstart=0,language="en",autoclose=TRUE,datesdisabled=NULL,daysofweekdisabled = NULL)
        })
      } else {
        shinyFeedback::feedbackWarning("csv_filename", TRUE, "Experiment does not exist in database")
      }
    }
  )
  
  observeEvent(
    input$CMS_number,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      message(toString(input$CMS_number))
      if (is.numeric(input$CMS_number)){
        exist=CheckNewImportForExistByNumber(rv$db,input$CMS_number)
        shinyFeedback::feedbackWarning("CMS_number", exist, "Dataset exists in database")
      }
    }
  )

  observeEvent(
    input$Device_id,
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    {
      type=getDeviceTypeByID(rv$db,input$Device_id)
      updateSelectInput(session, "Device_type", selected=type$type)
    }
  )
  
  observeEvent(
    input$Callsign,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      if(input$Color!=""){
        output$Go <- renderUI({
          buttonlabel=paste("Upload data to subject: '",input$Color," ",input$Callsign,"'", sep="")
          actionButton(inputId='Upload', label=buttonlabel)
        })
      }
      if (rv$warning!="no warning"){
        shinyFeedback::feedbackWarning("Color", FALSE, "")
        shinyFeedback::feedbackWarning("Callsign", FALSE, "")
        rv$warning="no warning"
      }
    }
  )
  
  observeEvent(
    input$Color,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      if(input$Callsign!=""){
        output$Go <- renderUI({
          buttonlabel=paste("Upload data to subject: '",input$Color," ",input$Callsign,"'", sep="")
          actionButton(inputId='Upload', label=buttonlabel)
        })
      }  
      if (rv$warning!="no warning"){
        shinyFeedback::feedbackWarning("Color", FALSE, "")
        shinyFeedback::feedbackWarning("Callsign", FALSE, "")
        rv$warning="no warning"
      }
    }
  )
  
  observeEvent(
    input$Sexe,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    { 
      if (is.null(input$Sexe)){
        updateSelectInput(session, "Gender_identity", selected= "Cis")
      }
      if (input$Sexe=="Male"){
        updateSelectInput(session, "Gender_identity", selected= "Cis Male")
      } else if (input$Sexe=="Female") {
        updateSelectInput(session, "Gender_identity", selected= "Cis Female")
      }  
    }  
  )
  
  observeEvent(
    input$Upload,
    {
      rv$subject_id=checkNewImportForExistByCallsign(rv$db,input$Callsign,input$Color,input$Age)
      exist=length(rv$subject_id)>0
      if (exist==FALSE){
        rv$warning=paste("subject: '",input$Color," ",input$Callsign,"; ",input$Age,"' does not exist in Database, please adjust dob, color or callsign and try again", sep="")
        shinyFeedback::feedbackWarning("Callsign", TRUE, rv$warning)
        shinyFeedback::feedbackWarning("Color", TRUE, "")
      } else {
# Here we update the database
        
   # write result entry (basicly the 'old' session table)
        lastsession=getExistingresultsForSubjectAndExperiment(rv$db,rv$exp_id,rv$subject_id)
        if (is.null(lastsession) || is.na(lastsession)){
          newsession=1
        } else {
          newsession=lastsession+1
        }
        result_id=getNewID(rv$db,'session')
        DevicetypeID=GetDeviceTypeIDbyType(rv$db,input$Device_type)
        EartypeID=GetEarTypeIDbyType(rv$db,input$ear_type)
        
        query=paste("insert into session (id,CMS_number,session_nr,subject_id,experiment_id,date_of_session,device_type_id,earphone_type_id,noise_cancelation) values (",result_id,",",input$CMS_number,",",newsession,",",rv$subject_id,",",rv$exp_id,",'",input$Date,"',",DevicetypeID,",",EartypeID,",",as.numeric(input$nc=='Yes'),");", sep="")
        insertEntry(rv$db,query)
        
        extract <- function(n,i){
          split=strsplit(n,split=",")
          n = as.numeric(split[[1]][i])
        }
        # iterate over the trial names in result set  
        for (i in 1:nrow(rv$df_aggr)){
          updateProgressBar(session,id = "Progress", value = i, total = nrow(rv$df_aggr),title = paste("uploading trial",i,"of",nrow(rv$df_aggr)))
          trialname=rv$df_aggr$Trial.Name[i]
          trialnr=gsub("_trial-[0-9]*-.*","",trialname)
          trialtype=gsub("[0-9]*_trial-[0-9]*-","",trialname)
          stims=GetStimulationsByExpAndTrialName(rv$db,rv$expname,trialname)
          
        # write trial entry (info per trial (not raw data))
          trial_id=getNewID(rv$db,'trial')
          trial_query=paste("insert into trial (id,session_id,trial_name,trial_number,trial_type) values (",trial_id,",",result_id,",'",trialname,"',",trialnr,",'",trialtype,"');",sep="")
          insertEntry(rv$db,trial_query)
        # write stimulations per trial (trial_stimulations); can be more entries per trial  
          trial_stimulations_id=getNewID(rv$db,'trial_stimulations')
          stim_query=paste("insert into trial_stimulations (id,trial_id,stimulus_type,stimulus_onset,stimulus_duration,stimulus_amplitude,frequency) values ")
          for (j in 1:nrow(stims)){
            stim_query=paste(stim_query,"(",trial_stimulations_id,",",trial_id,",'",stims$stimulus_type[j],"',",stims$stimulus_onset[j],",",stims$stimulus_duration[j],",",stims$stimulus_amplitude[j],",",stims$frequency[j],"),", sep="")
            trial_stimulations_id=trial_stimulations_id+1
          }
          stim_query=gsub('.{1}$', '', stim_query)
          stim_query=gsub('NA','null',stim_query)
          insertEntry(rv$db,stim_query)
          
        # write trial_data entries (raw data in multiple measures per trial)
          # fec entries
          
          trial_data_query=paste("insert into trial_data (id,trial_id,measurement,side,data,time_axis) values ")
          trial_data_id=getNewID(rv$db,'trial_data')
          # get values from DF
          leftFEC=eval(parse(text=paste0("rv$df_list$'",trialname,"'$Left.Eye.FEC",sep="")))
          rightFEC=eval(parse(text=paste0("rv$df_list$'",trialname,"'$Right.Eye.FEC",sep="")))
          # upsample
          x=eval(parse(text=paste0("rv$df_list$'",trialname,"'$Frame.millis",sep="")))
          tx=seq(min(x),max(x),by=10) # x in ms so 10 is 100Hz interpolation
          up_leftFEC=approx(x,leftFEC,tx,method='linear')
          up_rightFEC=approx(x,rightFEC,tx,method='linear')
          # make csv string with max 3 digits
          up_leftFECstr=paste(as.character(signif(up_leftFEC$y,digits=3)),collapse=",")
          up_rightFECstr=paste(as.character(signif(up_rightFEC$y,digits=3)),collapse=",")
          tx_str=paste(as.character(tx),collapse=",")
          # add to query
          trial_data_query=paste(trial_data_query,"(",trial_data_id,",",trial_id,",'FEC','left','",up_leftFECstr,"','",tx_str,"'),",sep="") 
          trial_data_query=paste(trial_data_query,"(",trial_data_id+1,",",trial_id,",'FEC','right','",up_rightFECstr,"','",tx_str,"'),",sep="")
          trial_data_id=trial_data_id+2
          
          # extract landmarks (special since "left,right" format per sample)
          landmarks=list("LM37","LM38","LM39","LM40","LM41","LM42","LM43","LM44","LM45","LM46","LM47","LM48")
          for (j in 1:length(landmarks)){
            y=eval(parse(text=paste0("rv$df_list$'",trialname,"'$",landmarks[j],sep="")))
            left=unlist(lapply(y,extract,1)) # extract left into vector
            right=unlist(lapply(y,extract,2)) # extract right into vector
          
            x=eval(parse(text=paste0("rv$df_list$'",trialname,"'$Frame.millis",sep="")))
            
            tx=seq(min(x),max(x),by=10) # x in ms so 10 is 100Hz interpolation
            up_left=approx(x,left,tx,method='linear')
            up_right=approx(x,right,tx,method='linear')
            
            up_leftstr=paste(as.character(signif(up_left$y,digits=3)),collapse=",")
            up_rightstr=paste(as.character(signif(up_right$y,digits=3)),collapse=",")
            tx_str=paste(as.character(tx),collapse=",")
            
            trial_data_query=paste(trial_data_query,"(",trial_data_id,",",trial_id,",'",landmarks[j],"','left','",up_leftstr,"','",tx_str,"'),",sep="") # build up query
            trial_data_query=paste(trial_data_query,"(",trial_data_id+1,",",trial_id,",'",landmarks[j],"','right','",up_rightstr,"','",tx_str,"'),",sep="")
           trial_data_id=trial_data_id+2
          }
          trial_data_query=gsub('.{1}$', '', trial_data_query) # remove last character (comma)
          insertEntry(rv$db,trial_data_query)
        }
      }
    }
  )
}

shinyApp(ui = ui, server = server)