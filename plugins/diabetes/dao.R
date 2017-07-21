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
     obs_datetime<=endDate) %>% 
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
    select(person_id,value_numeric, name) %>% 
    collect(n = Inf)

  print(patientWithObs)
  print(conceptNames)
  print(obsForVariables)
    
  columns <- c(pull(patients,person_id), pull(patients,Gender),pull(patients,Age))
  dbOutput <- append(dbOutput, setNames(columns, c("PatientId", "Gender", "Age")))
}