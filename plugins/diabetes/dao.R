library("dplyr")

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

fetchData <- function(pool, startDate, endDate) {
  dbOutput <- list()
  variablesToFetch <- list("BMI", "Systolic blood pressure",
                           "Diastolic blood pressure")

  choleraConceptId <- pool %>%
     tbl("concept_name") %>%
     filter(voided == 0, name=="Cholera",
      concept_name_type=="FULLY_SPECIFIED") %>%
     select(concept_id) %>%
     pull(concept_id)

  patientWithObs <- pool %>% 
    tbl("obs") %>% 
    filter(voided==0,
     value_coded == choleraConceptId,
     obs_datetime>=startDate,
     obs_datetime<endDate) %>% 
    select(person_id, concept_id, obs_id, value_numeric) %>% 
    collect(n = Inf)

  personIds <- pull(patientWithObs, person_id)
  patients <- pool %>%
    tbl("person") %>%
    filter(voided==0, person_id %in% personIds) %>%
    select(person_id, gender, birthdate) %>% 
    collect(n=Inf) %>% 
    mutate(birthdate = ymd(birthdate)) %>% 
    rename(Gender = gender) %>% 
    mutate(Age = age(from=birthdate, to=Sys.Date())) %>% 
    select(-birthdate)

  personAddresses <- pool %>%
    tbl("person_address") %>%
    filter(voided==0, person_id %in% personIds) %>%
    select(person_id,county_district,state_province) %>%
    collect(n=Inf)

  patientIdentifiers <- pool %>% 
    tbl("patient_identifier") %>% 
    filter(voided==0,identifier_type==3, patient_id %in% personIds) %>% 
    select(patient_id, identifier) %>% 
    collect(n=Inf)

  #Block Below need to be modified when we have an idea
  #about filters for visit_type and obs_relation 

  patientVisitDates <- pool %>% 
    tbl("visit") %>% 
    filter(voided==0, patient_id %in% personIds) %>% 
    group_by(patient_id) %>%
    summarise(date_started = max(date_started)) %>%
    select(patient_id, date_started) %>%
    rename(visitDate = date_started) %>%
    collect(n=Inf)

  patients <- patients %>%
    inner_join(personAddresses, by = c("person_id"="person_id")) %>%
    rename(District = county_district) %>%
    rename(State = state_province) %>%
    inner_join(patientIdentifiers, by = c("person_id"="patient_id")) %>%
    inner_join(patientVisitDates, by = c("person_id"="patient_id")) %>%
    collect(n=Inf)

  allObsForCholeraPatients <- pool %>%
    tbl("obs") %>%
    filter(voided==0, person_id %in% personIds) %>%
    collect(n=Inf)
    
  conceptNames <- pool %>%
    tbl("concept_name") %>%
    filter(voided == 0, name %in% variablesToFetch,
      concept_name_type=="FULLY_SPECIFIED") %>%
    select(concept_id,name) %>%
    rename(conceptid = concept_id) %>%
    collect(n=Inf) 

  obsForVariables <- allObsForCholeraPatients %>%
    inner_join(conceptNames, by = c("concept_id"="conceptid")) %>%  
    inner_join(patients, by = c("person_id"="person_id")) %>%
    rename(ID=identifier) %>%
    group_by(name,person_id) %>%
    filter(obs_datetime == max(obs_datetime)) %>%
    ungroup() %>%
    select(ID,name,obs_datetime,value_numeric, Age, State, District, Gender, visitDate) %>%
    collect(n = Inf)

  obsForVariables <- obsForVariables %>% 
    gather(Key, Value, starts_with("value_numeric")) %>%
    select(-Key, -obs_datetime) %>%
    spread(name, Value)
  
  obsForVariables
}