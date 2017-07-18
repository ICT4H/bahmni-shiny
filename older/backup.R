## Simple datatable

library("dplyr")
library("tidyr")
library("stringr")
library("lubridate")
library("RMySQL")
library("readr")
library("ggplot2")
library("scales")
library("eeptools")
library(DBI)
library(shiny)
library(ggplot2)
library(DT)
options(shiny.trace=F)

getConceptAnswers <- function(conDplyr, concepts = NULL){
  concept_answers <- conDplyr %>% 
    tbl("concept_answer") %>% 
    select(answer_concept, concept_id) %>% 
    collect(n= Inf)
  
  if(!is.null(concepts)){
    concept_answers <- concept_answers %>% 
      filter(concept_id %in% concepts)
  }
  concept_answers <- concept_answers %>% 
    group_by(answer_concept) %>% 
    summarise(Count = n())
  concept_names <- conDplyr %>% 
    tbl("concept_name") %>% 
    select(concept_id, name, voided, concept_name_type) %>% 
    filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
    collect(n=Inf) %>% 
    filter(concept_id %in% concept_answers$answer_concept)
  dbOutput <- setNames(concept_names$concept_id, concept_names$name)
  return(dbOutput)
}
age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

getConceptDates <- function(){
  conDplyr <- src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")
  
  dbOutput <- list("Obs Date"=1)
  
  concept_data_type <- conDplyr %>% 
    tbl("concept_datatype") %>% 
    select(concept_datatype_id,name, retired) %>% 
    filter(retired == 0, name=="Datetime")  #Get all date type concepts
  concept <- conDplyr %>% 
    tbl("concept") %>% 
    inner_join(concept_data_type, by=c("datatype_id"="concept_datatype_id")) %>% 
    filter(retired.x==0) %>% 
    select(concept_id) %>% 
    rename(conceptid = concept_id)
  concept_names <- conDplyr %>% 
    tbl("concept_name") %>% 
    inner_join(concept, by = c("concept_id"="conceptid")) %>% 
    filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
    select(concept_id, name) %>% 
    collect(n= Inf)
  dbDisconnect(conDplyr$con)
  dbOutput <- append(dbOutput,setNames(concept_names$concept_id, concept_names$name))
  return (dbOutput)
}

ui <- fluidPage(
  tags$head(tags$style(".rightAlign{float:right;}")),
  pageWithSidebar(
    headerPanel("Welcome!"),
    sidebarPanel(
      width = 3,
      actionButton("inSave","Save"),
      p(""),
      checkboxGroupInput('inDocuments', 'Files:',
                         choices=c(""))
    ),
    mainPanel(
      tabsetPanel(
        id="inTabPanel",
        tabPanel('Observations',
                 p(""),
                 fluidRow(
                   column(2,selectInput("inSelect", "",
                                        c(""), selected = 2))
                   ,
                   column(4,selectInput("inCheckboxGroup", "",
                                        c(""), multiple = T))
                   ,
                   conditionalPanel(
                     condition = "input.inSelect==2",
                     column(4,selectInput("inAnswers", "",
                                          c(""), multiple = T))
                   )
                 )
                 ,
                 fluidRow(
                   column(4,selectInput("inDateBy", "Date Filter",
                                        c("")))
                   ,
                   column(4,dateRangeInput('inDateRange',
                                           label = 'Range',
                                           start = Sys.Date() -30, end = Sys.Date() 
                   ))
                 )
                 ,
                 actionButton("inApply","Apply")
                 ,
                 conditionalPanel(
                   condition = "1==0",
                   checkboxInput("incheckbox", label = "Choice A", value = F)
                 )
                 ,
                 p("")
                 ,
                 conditionalPanel(
                   condition = "input.incheckbox==1",
                   sidebarLayout(
                     sidebarPanel(
                       width = 3,
                       checkboxGroupInput('inColumnNames', 'Columns:',
                                          choices=c(""))
                     ),
                     mainPanel(
                       DT::dataTableOutput("obsDT")
                       ,
                       downloadButton('downloadData', 'Download')
                     )
                   )
                 )
        ),
        tabPanel('Bars and Charts',
                 p(""),
                 pageWithSidebar(
                   
                   # Application title
                   headerPanel(""),
                   
                   # Sidebar with a slider input
                   sidebarPanel(
                     checkboxGroupInput('inDimensions', 'Columns:',
                                        choices=c("")),
                     selectInput('inCharts', 'Charts:',
                                 choices=c("Table", "Bar Chart", "Histogram"), multiple = F),
                     actionButton("inShow","Show")
                   ),
                   # Show a plot of the generated distribution
                   mainPanel(
                     DT::dataTableOutput("tableDF")
                     
                   )
                 )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  values <- reactiveValues(df_data = NULL)
  selectChoices <- reactiveValues(data = list("Class" = 1, "Question" = 2, "Answer" = 3))
  observe({
    
    # Can also set the label and select items
    updateSelectInput(session, "inSelect",
                      label = "",
                      choices = selectChoices$data,
                      selected = 2
    )
    updateSelectInput(session, "inDateBy",
                      label = "Date Filter",
                      choices = getConceptDates()
    )
  })
  observeEvent(input$inShow, {
    chartOption <- input$inCharts
    grp_cols <- input$inDimensions
    if(chartOption == 1){
      obs <- values$df_data
      dots <- lapply(grp_cols, as.symbol)
      if(length(grp_cols)==3){
        index<-grep(grp_cols[3], colnames(obs))
        colnames(obs)[index]="Answer_Unique"
        df <- obs %>% 
          group_by_(.dots=dots[1:2]) %>% 
          summarise(Answer_Unique=toString(unique(Answer_Unique)))
        col_df <- colnames(df)
        df <- df %>% spread_(col_df[2],col_df[3])
        tableop <- table(df[names(df)[3:2]])
        tableop.df <- as.data.frame.matrix(tableop)
      }
      else if(length(grp_cols)==2){
        df <- obs %>% 
          group_by_(.dots=dots) %>% 
          summarise(n = n()) %>% select(-n)
        tableop <- table(df[names(df)[2]])
        tableop.df <- as.data.frame(tableop)
      }
      output$tableDF <- DT::renderDataTable(datatable(tableop.df, 
                                                      options = list(paging=T), 
                                                      rownames=T, 
                                                      filter = "none"))
    }
  })
  observeEvent(input$inTabPanel, {
    if(input$inTabPanel=="Bars and Charts"){
      obs <- values$df_data
      updateCheckboxGroupInput(session, 
                               "inDimensions",
                               choices = names(obs),
                               selected = names(obs)[2]
      )
      updateSelectInput(session, 
                        "inCharts",
                        choices = list("Table"=1, "Bar Chart"=2, "Histogram"=3,"Scatter Plot"=4)
      )
      
    }
  })
  observeEvent(input$inSelect, {
    conDplyr <- src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")
    filterBy <- input$inSelect
    dbOutput <- list()
    concept_names <- NULL
    if(filterBy==1){
      concept_classes <- conDplyr %>% 
        tbl("concept_class") %>% 
        select(concept_class_id, name, retired) %>% 
        filter(retired == 0) %>% 
        collect(n=Inf)
      dbOutput <- setNames(concept_classes$concept_class_id, concept_classes$name)
    }else if(filterBy==2){
      concept_answers <- conDplyr %>% 
        tbl("concept_answer") %>% 
        distinct(answer_concept) %>% 
        select(answer_concept) %>% 
        collect(n= Inf)
      concept_names <- conDplyr %>% 
        tbl("concept_name") %>% 
        select(concept_id, name, voided, concept_name_type) %>% 
        filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
        collect(n=Inf) %>% 
        filter(!(concept_id %in% concept_answers$answer_concept))
      dbOutput <- setNames(concept_names$concept_id, concept_names$name)
    }else{
      concept_answers <- conDplyr %>% 
        tbl("concept_answer") %>% 
        distinct(answer_concept) %>% 
        select(answer_concept) %>% 
        collect(n= Inf)
      concept_names <- conDplyr %>% 
        tbl("concept_name") %>% 
        select(concept_id, name, voided, concept_name_type) %>% 
        filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
        collect(n=Inf) %>% 
        filter(concept_id %in% concept_answers$answer_concept)
      dbOutput <- setNames(concept_names$concept_id, concept_names$name)
    }
    updateSelectInput(session, 
                      "inCheckboxGroup",
                      label = "",
                      choices = dbOutput,
                      selected = tail(dbOutput, 1)
    )
    dbDisconnect(conDplyr$con)
  })
  observeEvent(input$inCheckboxGroup,{
    conDplyr <- src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")
    filterBy <- input$inSelect
    concept_names <- input$inCheckboxGroup
    if(filterBy==2){
      dbAnswers <- c("")
      if(!is.null(as.list(concept_names))){
        dbAnswers <- getConceptAnswers(conDplyr,concept_names)
      }
      updateSelectInput(session, 
                        "inAnswers",
                        label = "",
                        choices = dbAnswers,
                        selected = tail(dbAnswers, 1)
      )
    }
    dbDisconnect(conDplyr$con)
  })
  observeEvent(input$inColumnNames, {
    obs <- values$df_data
    obs <- obs[,input$inColumnNames, drop=F]
    output$obsDT <- DT::renderDataTable(obs, 
                                        options = list(paging=T), 
                                        rownames=F, 
                                        filter = "top")
  })
  
  observeEvent(input$inApply, {
    conDplyr <- src_mysql(dbname = "openmrs", user = "root", password = "", host = "localhost")
    filterBy <- input$inSelect
    listBy <- as.list(input$inCheckboxGroup)
    dateBy <- input$inDateBy
    dateRange <- as.character(input$inDateRange)
    conceptDates <- as.list(input$inDateBy)
    
    concepts <- conDplyr %>% 
      tbl("concept") %>% 
      select(concept_id, class_id) %>% 
      rename(conceptid = concept_id)
    
    if(filterBy ==1){#Concept class
      obs <- conDplyr %>% 
        tbl("obs") %>% 
        inner_join(concepts, by=c("concept_id"="conceptid")) %>% 
        filter(voided==0, class_id %in% listBy) %>% 
        select(obs_id,person_id, concept_id, obs_datetime, 
               location_id, value_boolean, value_coded, 
               value_drug, value_datetime, value_numeric,
               value_text, comments) %>% 
        collect(n = Inf) 
    }else if(filterBy==2){#Concept Name
      
      obs <- conDplyr %>% 
        tbl("obs") %>% 
        inner_join(concepts, by=c("concept_id"="conceptid")) %>% 
        filter(voided==0, concept_id %in% listBy) %>% 
        select(obs_id, person_id, concept_id, obs_datetime, 
               location_id, value_boolean, value_coded, 
               value_drug, value_datetime, value_numeric,
               value_text, comments) %>% 
        collect(n = Inf) 
      answers <- as.list(input$inAnswers)
      if(!is.null(answers)){
        if(length(answers) > 0){
          obs <- obs %>% filter(value_coded %in% answers)
        }
      }
    }else{
      
      obs <- conDplyr %>% 
        tbl("obs") %>% 
        inner_join(concepts, by=c("concept_id"="conceptid")) %>% 
        filter(voided==0, value_coded %in% listBy) %>% 
        select(obs_id, person_id, concept_id, obs_datetime, 
               location_id, value_boolean, value_coded, 
               value_drug, value_datetime, value_numeric,
               value_text, comments) %>% 
        collect(n = Inf) 
      
    }
    
    concept_names <- conDplyr %>% 
      tbl("concept_name") %>% 
      select(concept_id, name, voided, concept_name_type) %>% 
      rename(conceptid = concept_id) %>% 
      filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
      collect(n=Inf) 
    
    obs <- obs %>% 
      inner_join(concept_names, by = c("concept_id"="conceptid")) %>% 
      select(-voided, -concept_name_type) %>% 
      rename(concept_name = name) %>% 
      left_join(concept_names, by = c("value_coded"="conceptid")) %>% 
      select(-voided, -concept_name_type, -value_coded) %>% 
      rename(value_coded = name) %>% 
      select(obs_id, person_id, concept_name, obs_datetime, 
             location_id, value_boolean, value_coded, 
             value_drug, value_datetime, value_numeric,
             value_text, comments) %>% 
      mutate(location_id = as.factor(location_id),
             value_coded = as.factor(value_coded),
             concept_name = as.factor(concept_name),
             obs_datetime= ymd_hms(obs_datetime),
             value_datetime = ymd_hms(value_datetime))
    
    if(dateBy == 1){#ObsDate
      obs <- obs %>% 
        filter(obs_datetime >=ymd(dateRange[1]), obs_datetime<=ymd(dateRange[2]))
    }else{#Concept Date
      obs_dt <- conDplyr %>% 
        tbl("obs") %>% 
        filter(voided==0, concept_id %in% conceptDates) %>% 
        select(person_id, value_datetime) %>% 
        rename(personId = person_id) %>% 
        collect(n=Inf) %>% 
        mutate(value_datetime = ymd_hms(value_datetime)) %>% 
        filter(value_datetime>=ymd(dateRange[1]),
               value_datetime<=ymd(dateRange[2])) %>% 
        select(personId)
      obs <- obs %>% 
        inner_join(obs_dt, by = c("person_id"="personId")) 
    }
    pat_identifiers <- conDplyr %>% 
      tbl("patient_identifier") %>% 
      filter(voided==0,identifier_type==3) %>% 
      select(patient_id, identifier) %>% 
      collect(n=Inf)
    person_demographics <- conDplyr %>% 
      tbl("person") %>% 
      filter(voided == 0) %>% 
      select(person_id, gender, birthdate) %>% 
      collect(n=Inf) %>% 
      mutate(birthdate = ymd(birthdate),
             gender = as.factor(gender)
      ) %>% 
      rename(Gender = gender) %>% 
      mutate(Age = age(from=birthdate, to=Sys.Date())
      ) %>% 
      select(-birthdate)
    
    obs <- obs %>% 
      inner_join(person_demographics,by=c("person_id"="person_id")) %>% 
      inner_join(pat_identifiers, by=c("person_id"="patient_id")) %>% 
      mutate(person_id = identifier,
             obs_datetime = ymd(paste(year(obs_datetime),"-",
                                      month(obs_datetime),"-",
                                      day(obs_datetime)
             )
             ),
             value_datetime = ymd(paste(year(value_datetime),"-",
                                        month(value_datetime),"-",
                                        day(value_datetime)
             )
             )
      ) %>% 
      select(-identifier) %>% 
      rename(`Patient Identifier` = person_id,
             `Obs Date` = obs_datetime,
             Question = concept_name,
             ID = obs_id,
             Location = location_id)
    
    obs <- obs %>% 
      gather(Key,Value, starts_with("value_")) %>% 
      filter(!is.na(Value)) %>% 
      select(-Key) %>% 
      rename(Answer = Value, Comments = comments) %>% 
      select(ID, `Patient Identifier`, 
             Gender, Age, Question, 
             `Obs Date`, Location, 
             Answer, Comments)
    
    values$df_data <- obs
    output$obsDT <- DT::renderDataTable(values$df_data, 
                                        options = list(paging=T), 
                                        rownames=F, 
                                        filter = "top")
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste('observations', "_",year(ymd_hms(Sys.time())),
              "_",month(ymd_hms(Sys.time())),
              "_",day(ymd_hms(Sys.time())),
              "_",hour(ymd_hms(Sys.time())),
              "_",minute(ymd_hms(Sys.time())),
              "_",second(ymd_hms(Sys.time())),
              ".csv",sep='') 
      },
      content = function(file) {
        write.csv(obs, file)
      }
    )
    dbDisconnect(conDplyr$con)
    updateCheckboxGroupInput(session, 
                             "inColumnNames",
                             choices = names(obs),
                             selected = names(obs)
    )
    updateCheckboxInput(session,
                        "incheckbox",
                        value = T)
  })
  
}

shinyApp(ui = ui, server = server)