getDateTimeConcepts <- function(input, pool) {
  conceptDates <- eventReactive(input$inTabPanel,{
    if(input$inTabPanel=="Observations"){
      dbOutput <- list("Obs Date"=1)
      #SRC_POOL <- src_pool(pool)
      #SRC_POOL <- my_db
      concept_data_type <- pool %>%
         tbl("concept_datatype") %>%
         select(concept_datatype_id,name, retired) %>%
         filter(retired == 0, name=="Datetime")  #Get all date type concepts
      
      concept <- pool %>%
         tbl("concept") %>%
         inner_join(concept_data_type, by=c("datatype_id"="concept_datatype_id")) %>%
         filter(retired.x==0) %>%
         select(concept_id) %>%
         rename(conceptid = concept_id)
      
       concept_names <- pool %>%
         tbl("concept_name") %>%
         inner_join(concept, by = c("concept_id"="conceptid")) %>%
         filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>%
         select(concept_id, name) %>%
         collect(n= Inf)
  
      dbOutput <- append(dbOutput,setNames(concept_names$concept_id, concept_names$name))
      dbOutput
    }
  })
}

getConceptForSelection <- function(input, pool){
  questionAnswers <- eventReactive(input$inSelect, {
    #conDplyr <- src_pool(pool)
    #conDplyr <- my_db
    filterBy <- input$inSelect
    dbOutput <- list()
    concept_names <- NULL
    if(filterBy==1){ #Class
      concept_classes <- pool %>% 
        tbl("concept_class") %>% 
        select(concept_class_id, name, retired) %>% 
        filter(retired == 0) %>% 
        collect(n=Inf)
      dbOutput <- setNames(concept_classes$concept_class_id, concept_classes$name)
    }else if(filterBy %in% c(2,3)){ #Question and Answer
      concept_answers <- pool %>% 
        tbl("concept_answer") %>% 
        distinct(answer_concept) %>% 
        select(answer_concept) %>% 
        collect(n= Inf)
      concept_names <- pool %>% 
        tbl("concept_name") %>% 
        select(concept_id, name, voided, concept_name_type) %>% 
        filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
        collect(n=Inf)
        if(filterBy==2){
          concept_names<- concept_names %>% filter(!(concept_id %in% concept_answers$answer_concept))
        }else if(filterBy==3){
          concept_names<- concept_names %>% filter(concept_id %in% concept_answers$answer_concept)
        }
      dbOutput <- setNames(concept_names$concept_id, concept_names$name)
    }
  })
}

getConceptAnswers <- function(input, pool){
  answers <- eventReactive(c(input$inCheckboxGroup,input$inSelect),{
    filterBy <- input$inSelect
    concepts <- input$inCheckboxGroup
    if(filterBy==2 && !is.null(concepts)){
      #conDplyr <- src_pool(pool)
      #conDplyr <- my_db
      concept_answers <- pool %>% 
        tbl("concept_answer") %>% 
        select(answer_concept, concept_id) %>% 
        collect(n= Inf)
      
      concept_answers <- concept_answers %>% 
          filter(concept_id %in% concepts)
      
      concept_answers <- concept_answers %>% 
        group_by(answer_concept) %>% 
        summarise(Count = n())
      concept_names <- pool %>% 
        tbl("concept_name") %>% 
        select(concept_id, name, voided, concept_name_type) %>% 
        filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
        collect(n=Inf) %>% 
        filter(concept_id %in% concept_answers$answer_concept)
      dbOutput <- setNames(concept_names$concept_id, concept_names$name)
      dbOutput
    } else{
      dbOutput <- c("")
      dbOutput
    }
  })
}

getConcepts <- function(input, pool){
  concepts <- pool %>% 
  tbl("concept") %>% 
  select(concept_id, class_id) %>% 
  rename(conceptid = concept_id)
}