# buildQuestionTable <- function(QuestionSet) {
#     QuestionSet %>% imap_dfr(~.x %>% map_dfr(~tibble(Title = .x$qtext(),
#         InputType = .x$qtype()) %>% cbind(tibble(Options = .x$opts() %>%
#         unlist))) %>% mutate(AreaType = .y, Path = "../images"))
# }

# find how to give the options as a list for choices

buildQuestionOutputs <- function(args, id) {
  #return a html object for every individual question

  inputFn <-
    switch(args$qType, radio = "radioButtons", check = "checkboxGroupInput",
           args$qType)

  args$qType <- NULL
  args$inputId <- id

  do.call(inputFn, args)
}


#' @importFrom purrr imap_dfr map_dfr
updateAnswers <- function(ansDf, pathid, questionIDs, input) {
  #don't remove rows, check for changes and replace only if changed

  inputAns <- questionIDs %>%
    imap_dfr( ~ .x %>%
                map_dfr( ~ tibble(
                  question = .x,
                  answers = ifelse(is.null(input[[.x]]), "NULL", input[[.x]])
                )) %>%
                mutate(tab = .y)) %>%
    mutate(path = pathid) %>%
    select("path", "tab", "question", "answers") %>%
    as.data.frame

  if(identical(ansDf %>% filter(UQE(as_quosure(sym("path"))) == !!quo(pathid)),
               inputAns)){
    ansDf
  }
  else{
    ansDf %>%
      filter(UQE(as_quosure(sym("path"))) != !!quo(pathid)) %>%
      bind_rows(
        inputAns
      )
  }


}
