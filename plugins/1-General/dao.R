library("dplyr")

age <- function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)
  
  age = to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

fetchData <- function(mysqlPool, psqlPool, shouldFetchAll, startDate, endDate) {
  dbOutput <- list()
  
  codedDiagnosisConceptId <- mysqlPool %>%
     tbl("concept_name") %>%
     filter(voided == 0, name=="Coded Diagnosis",
      concept_name_type=="FULLY_SPECIFIED") %>%
     select(concept_id) %>%
     pull(concept_id)  

  conceptNames <- mysqlPool %>% 
    tbl("concept_name") %>%
    filter(voided == 0, concept_name_type=="FULLY_SPECIFIED") %>%
    select(concept_id, name) %>%
    collect(n=Inf)

  allDiagnoses <- mysqlPool %>% 
    tbl("obs") %>%
    filter(voided == 0, concept_id == codedDiagnosisConceptId) %>%
    select(person_id, value_coded, obs_datetime) %>%
    collect(n=Inf)

  if(!shouldFetchAll){
      allDiagnoses <- allDiagnoses %>% filter(obs_datetime>=startDate,obs_datetime<endDate)
  }
  
  allDiagnoses <- allDiagnoses %>%
    inner_join(conceptNames, by = c("value_coded"="concept_id")) %>%
    collect(n=Inf)

  patients <- mysqlPool %>%
    tbl("person") %>%
    filter(voided==0) %>%
    select(person_id, gender, birthdate) %>% 
    collect(n=Inf) %>% 
    mutate(birthdate = ymd(birthdate)) %>% 
    rename(Gender = gender) %>% 
    mutate(Age = age(from=birthdate, to=Sys.Date())) %>% 
    select(-birthdate)

  personAddresses <- mysqlPool %>%
    tbl("person_address") %>%
    filter(voided==0) %>%
    select(person_id,county_district,state_province) %>%
    collect(n=Inf)

  patientIdentifiers <- mysqlPool %>% 
    tbl("patient_identifier") %>% 
    filter(voided==0,identifier_type==3) %>% 
    select(patient_id, identifier) %>% 
    collect(n=Inf)

  patients <- patients %>%
    inner_join(personAddresses, by = c("person_id"="person_id")) %>%
    rename(District = county_district) %>%
    rename(State = state_province) %>%
    inner_join(patientIdentifiers, by = c("person_id"="patient_id")) %>%
    collect(n=Inf)

  patient <- patients %>%
    inner_join(allDiagnoses, by = c("person_id"="person_id")) %>%
    group_by(person_id, value_coded) %>%
    filter(obs_datetime == max(obs_datetime)) %>%
    ungroup() %>%
    rename(ID=identifier) %>%
    rename(Diagnosis=name) %>%
    rename(`Visit Date`=obs_datetime) %>%
    select(ID, Gender, Age, District, State, `Visit Date`, Diagnosis) %>%
    collect(n=Inf)

  patient
}