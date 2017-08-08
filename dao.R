library(rlang)
getDateTimeConcepts <- function() {
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- list("Obs Date"=1)
  concept_data_type <- mysqlPool %>%
     tbl("concept_datatype") %>%
     select(concept_datatype_id,name, retired) %>%
     filter(retired == 0, name=="Datetime")  #Get all date type concepts
  
  concept <- mysqlPool %>%
     tbl("concept") %>%
     inner_join(concept_data_type, by=c("datatype_id"="concept_datatype_id")) %>%
     filter(retired.x==0) %>%
     select(concept_id) %>%
     rename(conceptid = concept_id)
  
   concept_names <- mysqlPool %>%
     tbl("concept_name") %>%
     inner_join(concept, by = c("concept_id"="conceptid")) %>%
     filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>%
     select(concept_id, name) %>%
     collect(n= Inf)

  dbOutput <- append(dbOutput,setNames(concept_names$concept_id, concept_names$name))
  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

getConceptByType <- function(type){
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- list()
  concept_names <- NULL
  if(type==1){ #Class
    concept_classes <- mysqlPool %>% 
      tbl("concept_class") %>% 
      select(concept_class_id, name, retired) %>% 
      filter(retired == 0) %>% 
      collect(n=Inf)
    dbOutput <- setNames(concept_classes$concept_class_id, concept_classes$name)
  }else if(type %in% c(2,3)){ #Question and Answer
    concept_answers <- mysqlPool %>% 
      tbl("concept_answer") %>% 
      distinct(answer_concept) %>% 
      select(answer_concept) %>% 
      collect(n= Inf)
    concept_names <- mysqlPool %>% 
      tbl("concept_name") %>% 
      select(concept_id, name, voided, concept_name_type) %>% 
      filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
      collect(n=Inf)
      if(type==2){
        concept_names<- concept_names %>% filter(!(concept_id %in% concept_answers$answer_concept))
      }else if(type==3){
        concept_names<- concept_names %>% filter(concept_id %in% concept_answers$answer_concept)
      }
    dbOutput <- setNames(concept_names$concept_id, concept_names$name)
  }
  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

getConceptAnswers <- function(concepts, filterBy){
  mysqlPool <- getMysqlConnectionPool()
  if(filterBy==2 && !is.null(concepts)){
    concept_answers <- mysqlPool %>% 
      tbl("concept_answer") %>% 
      select(answer_concept, concept_id) %>% 
      collect(n= Inf)
    
    concept_answers <- concept_answers %>% 
        filter(concept_id %in% concepts)
    
    concept_answers <- concept_answers %>% 
      group_by(answer_concept) %>% 
      summarise(Count = n())
    concept_names <- mysqlPool %>% 
      tbl("concept_name") %>% 
      select(concept_id, name, voided, concept_name_type) %>% 
      filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
      collect(n=Inf) %>% 
      filter(concept_id %in% concept_answers$answer_concept)
    dbOutput <- setNames(concept_names$concept_id, concept_names$name)
  } else{
    dbOutput <- c("")
  }
  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

getConcepts <- function(){
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- mysqlPool %>% 
    tbl("concept") %>% 
    select(concept_id, class_id) %>% 
    rename(conceptid = concept_id)

  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

getObsForSelection <- function(concepts, filterColumn, listBy) {
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- mysqlPool %>% 
    tbl("obs") %>% 
    inner_join(concepts, by=c("concept_id"="conceptid")) %>%  # in case of class the join should be on value_coded
    filter_("voided==0", paste(filterColumn, "%in% listBy")) %>% 
    select(obs_id,person_id, concept_id, obs_datetime, 
           location_id, value_coded, 
           value_drug, value_datetime, value_numeric,
           value_text, comments, obs_group_id) %>% 
    collect(n = Inf) 

  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

getAllConceptNames <- function() {
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- mysqlPool %>% 
    tbl("concept_name") %>% 
    select(concept_id, name, voided, concept_name_type) %>% 
    rename(conceptid = concept_id) %>% 
    filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>% 
    collect(n=Inf) 

  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

mapObsWithConceptNames <- function(obs, conceptNames) {
   obs %>% 
    inner_join(conceptNames, by = c("concept_id"="conceptid")) %>% 
    select(-voided, -concept_name_type) %>% 
    rename(concept_name = name) %>% 
    left_join(conceptNames, by = c("value_coded"="conceptid")) %>% 
    select(-voided, -concept_name_type, -value_coded) %>% 
    rename(value_coded = name) %>% 
    select(obs_id, person_id, concept_name, obs_datetime, 
           location_id, value_coded, 
           value_drug, value_datetime, value_numeric,
           value_text, comments, obs_group_id) %>% 
    mutate(location_id = as.factor(location_id),
           value_coded = as.factor(value_coded),
           concept_name = as.factor(concept_name),
           obs_datetime= ymd_hms(obs_datetime),
           value_datetime = ymd_hms(value_datetime),
           obs_group_id = as.factor(obs_group_id))
}

filterObsByDateConcept <- function(obs, conceptDates, dateRange) {
  mysqlPool <- getMysqlConnectionPool()
  obs_dt <- mysqlPool %>% 
    tbl("obs") %>% 
    filter(voided==0, concept_id %in% conceptDates) %>% 
    select(person_id, value_datetime) %>% 
    rename(personId = person_id) %>% 
    collect(n=Inf) %>% 
    mutate(value_datetime = ymd_hms(value_datetime)) %>% 
    filter(value_datetime>=ymd(dateRange[1]),
           value_datetime<=ymd(dateRange[2])) %>% 
    select(personId)
  dbOutput <- obs %>% 
    inner_join(obs_dt, by = c("person_id"="personId"))

  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

getPatientIdentifiers <- function(){
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- mysqlPool %>% 
    tbl("patient_identifier") %>% 
    filter(voided==0,identifier_type==3) %>% 
    select(patient_id, identifier) %>% 
    collect(n=Inf)

  disconnectFromDb(mysqlPool)
  return (dbOutput)
}

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

getPersonDemographics <- function(){
  mysqlPool <- getMysqlConnectionPool()
  dbOutput <- mysqlPool %>% 
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
    
  disconnectFromDb(mysqlPool)
  return (dbOutput)
}
