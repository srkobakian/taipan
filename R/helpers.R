buildQuestionTable <- function(QuestionSet){
  QuestionSet %>% imap_dfr(~ .x %>% map_dfr(~ tibble(Title = .x$qtext(), InputType = .x$qtype()) %>%
                                              cbind(tibble(Options = .x$opts() %>% unlist))) %>%
                 mutate(AreaType = .y,
                        Path = "../images"))
}

# find how to give the options as a list for choices
buildQuestionOutputs <- function(data){
  #return a html question object

  switch(data$InputType[1],
         "radio" = radioButtons(inputId = data$inputID[1],
                                label = data$Title[1],
                                choices = as.vector(data$Options)),
         "check" = checkboxGroupInput(inputId = data$inputID[1],
                                      label = data$Title[1],
                                      choices = as.vector(data$Options)))

}
