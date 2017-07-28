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
  variablesToFetch <- list("BMI", "Systolic",
                           "Diastolic")

  hypertensionConceptId <- pool %>%
     tbl("concept_name") %>%
     filter(voided == 0, name=="Hypertension",
      concept_name_type=="FULLY_SPECIFIED") %>%
     select(concept_id) %>%
     pull(concept_id)

  codedDiagnosisConceptId <- pool %>%
     tbl("concept_name") %>%
     filter(voided == 0, name=="Coded Diagnosis",
      concept_name_type=="FULLY_SPECIFIED") %>%
     select(concept_id) %>%
     pull(concept_id)

  patientWithHypertension <- pool %>% 
    tbl("obs") %>% 
    filter(voided==0,
     value_coded == hypertensionConceptId,
     concept_id == codedDiagnosisConceptId,
     obs_datetime>=startDate,
     obs_datetime<endDate) %>% 
    select(person_id, encounter_id) %>% 
    collect(n = Inf)

  if(nrow(patientWithHypertension) <= 0){
    return (data.frame())
  }

  encIds <- pull(patientWithHypertension, encounter_id)
  personIds <- pull(patientWithHypertension, person_id)

  hypertensionVisits <- pool %>%
    tbl("encounter") %>%
    filter(voided==0,
     encounter_id %in% encIds) %>% 
    select(visit_id, patient_id) %>%
    collect(n=Inf)

  visitIds <- pull(hypertensionVisits, visit_id)
  visitDates <- pool %>%
    tbl("visit") %>%
    filter(voided==0, visit_id %in% visitIds) %>% 
    select(date_started,visit_id) %>%
    collect(n=Inf)

  hypertensionVisits <- hypertensionVisits %>%
    inner_join(visitDates, by=c("visit_id"="visit_id")) %>%
    select(patient_id, date_started) %>%
    rename(visitDate = date_started) %>%
    collect(n=Inf)
  
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

  patients <- patients %>%
    inner_join(personAddresses, by = c("person_id"="person_id")) %>%
    rename(District = county_district) %>%
    rename(State = state_province) %>%
    inner_join(patientIdentifiers, by = c("person_id"="patient_id")) %>%
    inner_join(hypertensionVisits, by = c("person_id"="patient_id")) %>%
    collect(n=Inf)

  allObsForHypertensionPatients <- pool %>%
    tbl("obs") %>%
    filter(voided==0, person_id %in% personIds) %>%
    collect(n=Inf)
    
  conceptNames <- pool %>%
    tbl("concept_name") %>%
    filter(voided == 0, name %in% variablesToFetch,
      concept_name_type=="FULLY_SPECIFIED") %>%
    select(concept_id,name) %>%
    collect(n=Inf) 

  obsForVariables <- allObsForHypertensionPatients %>%
    inner_join(conceptNames, by = c("concept_id"="concept_id")) %>%  
    inner_join(patients, by = c("person_id"="person_id")) %>%
    group_by(person_id, concept_id) %>%
    filter(obs_datetime == max(obs_datetime)) %>%
    ungroup() %>%
    rename(ID=identifier) %>%
    select(ID,name,value_numeric, Age, State, District, Gender, visitDate) %>%
    collect(n = Inf)

    #This is to filter out incorrect data entries.
    #Like Query below should return single row
    #SELECT concept_id,encounter_id,value_numeric,obs_datetime FROM obs WHERE obs_id IN (7211637,7211653);
    #This row says in single encounter same concept has been filled twice at same time
    obsForVariables <- obsForVariables %>% distinct(ID,name, .keep_all = TRUE)

  obsForVariables <- obsForVariables %>% 
    gather(Key, Value, starts_with("value_numeric")) %>%
    select(-Key) %>%
    spread(name, Value)

  obsForVariables
}