# Quick check of data
library(signal)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyFeedback)
library(RPostgres)
library(DBI)
library(DT)
library(ggplot2)
library(plotly)
library(tidyr)


df <- 'empty'

Connect2BlinkLabDB <- function() {
  con <- dbConnect(RPostgres::Postgres(),dbname = 'blinklabdb', 
                   host = 'blinklabdb.caknbcvpnqnz.eu-central-1.rds.amazonaws.com', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                   port = 5432, # or any other port specified by your DBA
                   user = 'dbadmin',
                   password = '1970b.k.',
                   bigint="integer") # warning, maxes out out some point 
  return(con)
  
}

getNewID <- function(con,table) {
  query = paste("select max(id) from ",table)
  newID=dbGetQuery(con,query)$max +1
  return(newID)
}

getUniquesFromColumn <- function(con,table,field){
  query=paste("select distinct ",field," from ",table,sep="")
  uniques=dbGetQuery(con,query)
  return(na.omit(uniques))
}

getExperiments <- function(con){
  query= paste("select id,experiment_name from experiments")
  explist=dbGetQuery(con,query)
  return(explist)
}

getFactorLevels <- function(con,id){
  query=paste("select factor_levels from experiments where experiment_name='",id,"'",sep="")
  levels=dbGetQuery(con,query)
  return(levels)
}  

checkUserAndPW <- function(con,user,pw){
  query=paste("SELECT (password = crypt('",pw,"', password)) AS match FROM users where user_name='",user,"';",sep="")
  OK=dbGetQuery(con,query)
  return(OK)
}

getSubjectsByFilter <- function(con,DOB=NULL,Callsign=NULL,Color=NULL,Exp=NULL) {
  query="select id,color,callsign,first_name,last_name,sex from subject "
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

getResultsByExpName <- function(con,expname){
  query=paste("select r.id,s.color,s.callsign,s.date_of_birth, r.session_nr, r.date_of_session, dt.device_type, et.earphone_type from session r
               inner join subject s on r.subject_id = s.id
               inner join experiments e on e.id  = r.experiment_id
               inner join device_type dt on dt.id = r.device_type_id
               inner join earphone_type et on et.id = r.earphone_type_id
               where e.experiment_name='",expname,"'",sep="")
  seslist=dbGetQuery(con,query)
  return(seslist)
}

getTrialTypesBySessionID <- function(con,id){
  query=paste("select distinct t.trial_type, ts.stimulus_type, ts.stimulus_onset, ts.stimulus_duration,ts.stimulus_amplitude,ts.frequency from trial t
               inner join trial_stimulations ts on t.id=ts.trial_id where t.session_id=",id,sep="")
  typelist=dbGetQuery(con,query)
  return(typelist)
}

getProbeDataBySessionID <- function(con,id,measurement='FEC'){
  query=paste("select t.session_id,t.trial_number,t.id as trial_id,t.trial_type,d.side,d.measurement,d.data, d.time_axis, ts.stimulus_onset, ts.stimulus_duration from trial_data d 
               inner join trial t on t.id = d.trial_id 
               inner join trial_stimulations ts on t.id=ts.trial_id
               where t.session_id in(",id,") and d.measurement ='",measurement,"'",sep="")
               #where t.session_id =",id," and d.measurement ='",measurement,"'",sep="")
  ProbeData=dbGetQuery(con,query)
  return(ProbeData)
}

ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  
  fluidRow(
    column(12,
      img(src = "logo.png", height = 80, width = 317),
    )
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
    column(12,
      uiOutput("Exp")
    )
  ),
  fluidRow(
    column(12, DT::dataTableOutput("SesTable")),
  ),
  fluidRow(
    column(12, plotlyOutput("plot", height =900)),
  ),
  fluidRow(
    column(12, DT::dataTableOutput("resultTable")),
  )
)


server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  rv <- reactiveValues()
  db <- Connect2BlinkLabDB()
  rv$db <- db
  rv$pw <- FALSE
  rv$max_n_samples <- 300
  rv$single_sample_dur <- 10
  
  cutter <- function(n) {
    vector <- as.double(unlist(strsplit(toString(n$data),",")))[1:rv$max_n_samples]
    vector <- vector * -1 # invert signal
    n$data <- as.character(gsub(" ", "", toString(vector), fixed = TRUE))
    vector <- as.double(unlist(strsplit(toString(n$time_axis),",")))[1:rv$max_n_samples]
    n$time_axis <- as.character(gsub(" ", "", toString(vector), fixed = TRUE))
    return(n)
  }
  
  filt <- function(n) {
    bf <- butter(2, 1/5, type="low") # define butterworth filter
    n$data_filt <- filtfilt(bf, n$data)
    return(n)
  }
  
  velocity <- function(n) {
    last=length(n$data)
    velocity <- diff(n$data_filt)
    velocity[last]=0
    n$velocity <- velocity
    return(n)
  }
 
  normalise <- function(n) {
    n$norm_data <- scales::rescale(n$data, to=c(0,1))
    # change filt function to use norm_data when this function is used
  }
  
  find_peaks <- function (x, m = 3){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
    return(pks)
    
  }
  observeEvent(
    input$submit,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      if (input$user!="" && input$pw!=""){
        ok=checkUserAndPW(rv$db,input$user,input$pw)
        if (ok==TRUE){
          rv$pw=TRUE
          removeUI(selector = "div:has(> #user)")
          removeUI(selector = "div:has(> #pw)")
          removeUI(selector = "div:has(> #submit)")
          id=getUniquesFromColumn(db,'experiments','experiment_name')
          output$Exp <- renderUI({
            selectInput(inputId='Exp', label="Select Experiment", choices = c("",id$experiment_name), selected=NULL, width = 640)
          })
        }
      }
    }  
  )
  
  observeEvent(
    input$Exp,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      rv$sessions=getResultsByExpName(rv$db,input$Exp)
      output$SesTable<-renderDataTable(rv$sessions, selection='multi',filter="top",options=list(pagelength=6))
    }
  )
  observeEvent(input$SesTable_state, {
    test=str(input$SesTable_state)
  })
  
  observeEvent(
    input$SesTable_rows_selected,
    ignoreInit = TRUE,
    ignoreNULL = TRUE,
    {
      selection = input$SesTable_rows_selected
      if (length(selection)) {
        rv$session_id=rv$sessions$id[selection]
        rv$trialtypes=getTrialTypesBySessionID(rv$db,rv$session_id[1])
        output$resultTable<-renderDataTable(rv$trialtypes, selection='multi')
        idlist=paste(rv$session_id, collapse=",")
        df=getProbeDataBySessionID(rv$db,idlist)
        
        df_split <- split(df, list(df$trial_id, df$side))
        df_list <- lapply(df_split, cutter)
        df <- do.call(rbind, df_list)
        
        df_data <- read.table(text = df$data, sep = ",", colClasses = "numeric") 
        df1 <- cbind (df,  df_data)
        df1_long <- gather(df1, sample, value, V1:paste0("V", rv$max_n_samples))
        df1_long$sample <- as.numeric(gsub("V", "", paste(df1_long$sample)))
        df1_long$data<- as.numeric(df1_long$value)
        
        df1_long$time_axis <- as.numeric(df1_long$sample * rv$single_sample_dur) - rv$single_sample_dur
        df1_long$sample <- NULL
        df1_long$value <- NULL
        df <- df1_long
        
        # normalize all Y values between 0 and 1
        session <- 0
        for (session in unique(df$session_id)) {
          df$data[df$session_id==session] <- scales::rescale(df$data[df$session_id==session], to=c(0,1))
        }
        
        # function alternative, does not work yet:
        
        # df_split <- split(df, df$session_id)
        # df_list <- lapply(df_split, normalise)
        # df <- do.call(rbind, df_list)
        
        # low pass filter

        df_split <- split(df, list(df$trial_id, df$side))
        df_list <- lapply(df_split, filt)
        #signal analysis here
        df_list <- lapply(df_list, velocity)
        
        # make new dataset1 with new columns added
        df <- do.call(rbind, df_list)
        # to normal rownames
        rownames(df) <- 1:nrow(df)
        # new time axis, 0 = first stim onset
        df$time_axis_corr <- df$time_axis - 1500
        # to factor
        datalevels=getFactorLevels(rv$db,input$Exp)
        datalevels=read.table(text=datalevels$factor_levels, sep=",")
        df$trial_type_f <- factor(df$trial_type, levels=datalevels) # to factor trial names
        # mean trace
        df_aggr <- df %>% group_by (time_axis_corr, trial_type_f, side, stimulus_onset, stimulus_duration) %>% summarise_at (c("data_filt","velocity"), list(mean= ~mean(., na.rm=T)))
        #df_aggr <- df %>% group_by (time_axis_corr, trial_type_f, side, stimulus_onset, stimulus_duration, session_id) %>% summarise_at (c("data_filt","velocity"), funs( mean (., na.rm=T)))
        print
        output$plot <- renderPlotly(
          {
            ggplot(data = df, aes(x = time_axis_corr, y = data_filt, alpha=0.5)) + 
              geom_line(aes(group = trial_id, color = side)) +
              geom_line(data = df_aggr, aes(x = time_axis_corr, y = data_filt_mean), size = 1, alpha=1) + 
              theme_bw() + 
              scale_x_continuous(limits = c(-500, 1000), name = "Time after noise onset (ms)") +
              scale_y_continuous(name = "Normalized eyelid closure") +
              facet_grid (trial_type_f ~ side) +
              geom_vline(xintercept= unique(df_aggr$stimulus_onset), linetype="dotted", color = "blue", size=1)
          }
        )  
      }
    }
  )  
}

shinyApp(ui = ui, server = server)