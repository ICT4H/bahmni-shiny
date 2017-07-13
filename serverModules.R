sideBar <- function(input, output, session) {
  observeEvent(input$inSave, {
    cat("he was callled")
  })
}

observationTab <- function(input, output, session, pool) {
  conceptsForSelection <- getConceptForSelection(input, pool)
  conceptAnswers <- getConceptAnswers(input, pool)

  observeEvent(input$inSelect, {
    updateSelectInput(
      session,
      "inCheckboxGroup",
      label = "",
      choices = conceptsForSelection(),
      selected = tail(conceptsForSelection(), 1)
    )
  })
  
  observeEvent(c(input$inCheckboxGroup, input$inSelect), {
    updateSelectInput(
      session,
      "inAnswers",
      label = "",
      choices = conceptAnswers(),
      selected = tail(conceptAnswers(), 1)
    )
  })
}