#Module functions for indicator visualization app

inputUI <- function(id) {
  ns <- NS(id)
  
  if(id == "Var"){
    selectInput(ns("Var"),
                label = "Variable",
                choices = var_ids,
                selected = var_ids[1],
                selectize = TRUE,
                multiple = TRUE)
  } else if (id == "EPU"){
    selectizeInput("EPU",
                   label = "EPU",
                   choices = epu_ids,
                   selected = epu_ids[1],
                   multiple = TRUE)
  }
  
}