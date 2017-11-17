buildQuestionTable <- function(QuestionSet){
  QuestionSet %>% imap_dfr(~ .x %>% map_dfr(~ tibble(Title = .x$qtext(), InputType = .x$qtype()) %>%
                                              cbind(tibble(Options = .x$opts() %>% unlist))) %>%
                 mutate(AreaType = .y,
                        Path = "../images"))
}

# find how to give the options as a list for choices

buildQuestionOutputs <- function(args, id){
  #return a html question object

  inputFn <- switch(args$qType,
                    "radio" = "radioButtons",
                    "check" = "checkboxGroupInput",
                    args$qType)

  args$qType <- NULL
  args$inputId <- id

  do.call(inputFn, args)
}
