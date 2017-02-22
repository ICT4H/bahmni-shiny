eventReactive(input$inSelect, {
  conDplyr <- src_pool(pool)
  filterBy <- input$inSelect
  dbOutput <- list()
  concept_names <- NULL
  if(filterBy==1){ #Class
    concept_classes <- conDplyr %>% 
      tbl("concept_class") %>% 
      select(concept_class_id, name, retired) %>% 
      filter(retired == 0) %>% 
      collect(n=Inf)
    dbOutput <- setNames(concept_classes$concept_class_id, concept_classes$name)
  }else if(filterBy==2 || filterBy==3){ #Question and Answer
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
      if(filterBy == 2){
        concept_names<- concept_names %>% filter(!(concept_id %in% concept_answers$answer_concept))
      }else if(filterBy == 3){
        concept_names<- concept_names %>% filter(concept_id %in% concept_answers$answer_concept)
      }
    dbOutput <- setNames(concept_names$concept_id, concept_names$name)
  }
})