buildQuestionTable <- function(QuestionSet){
  QuestionSet %>% imap_dfr(~ .x %>% map_dfr(~ tibble(Title = .x$qtext(), InputType = .x$qtype()) %>%
                                              cbind(tibble(Options = .x$opts() %>% unlist))) %>%
                 mutate(AreaType = .y,
                        Path = "../images"))
}

# find how to give the options as a list for choices
#  buildQuestionOutputs <- function(x){
#    #return a html question object
#
#    switch(x$InputType,
#           "radio" =  radioButtons(inputId = paste0(x$inputID),
#                                   label = paste0(x$Title),), selected = 0),
#           "check" = )
#  }
