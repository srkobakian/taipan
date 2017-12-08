# buildQuestionTable <- function(QuestionSet) {
#     QuestionSet %>% imap_dfr(~.x %>% map_dfr(~tibble(Title = .x$qtext(),
#         InputType = .x$qtype()) %>% cbind(tibble(Options = .x$opts() %>%
#         unlist))) %>% mutate(AreaType = .y, Path = "../images"))
# }

# find how to give the options as a list for choices

build_question_outputs <- function(args, id) {
  #return a html object for every individual question

  inputFn <-
    switch(args$qType, radio = "radioButtons", check = "checkboxGroupInput",
           args$qType)

  args$qType <- NULL
  args$inputId <- id

  do.call(inputFn, args)
}


answers_vec <- function(name, input = input) {
  ans <- c()
  if (length(input[[name]]) > 0) {
    ans <- as.vector(input[[name]])
  } else {
    ans <- "NULL"
  }

  return(ans)
}


#' @importFrom purrr imap_dfr map_dfr
update_answers <- function(ansDf = v$ansDf,
                          pathId = images[v$imageNum],
                          questionIDs = questionIDs,
                          input = input) {
  #don't remove rows, check for changes and replace only if changed
  browser()
  inputAns <- questionIDs$scene %>%
    map_dfr(~ tibble(
      question = .x,
      #write a function to check for null answers and still produce multiple responses for check boxes
      answers = answers_vec(name = .x, input = input)
    )) %>%
    group_by(question) %>%
    nest() %>%
    mutate(path = pathId) %>%
    select("path", "question", "data") %>%
    as.data.frame

  if (identical(ansDf %>% filter(UQE(as_quosure(sym(
    "path"
  ))) == !!quo(pathId)),
  inputAns)) {
    ansDf
  }
  else{
    ansDf %>%
      filter(UQE(as_quosure(sym("path"))) != !!quo(pathId)) %>%
      bind_rows(inputAns)
  }

}

#' @importFrom tidyr spread nest

update_selection_answers <-
  function(selAnsDf = v$selAnsDf,
           pathId = images[v$imageNum],
           selNum = v$selectionNum,
           questionIDs = questionIDs,
           input = input) {
    if (is.null(input$plot_brush)) {
      shiny::showNotification("No area selected", type = "warning")
    } else {
      #browser()
      #data frame for individual selection
      inputAns <- questionIDs$selection %>%
        map_dfr( ~ tibble(
          question = .x,
          #write a function to check for null answers and still produce multiple responses for check boxes
          answers = answers_vec(name = .x, input = input)
        )) %>%
        group_by(question) %>%
        nest() %>%
        mutate(
          path = pathId,
          selectionNum = selNum,
          xmin = input$plot_brush$xmin,
          xmax = input$plot_brush$xmax,
          ymin = input$plot_brush$ymin,
          ymax = input$plot_brush$ymax
        ) %>%
        select("path",
               "selectionNum",
               "question",
               "data",
               "xmin",
               "xmax",
               "ymin",
               "ymax") %>%
        as.data.frame

      # add data frame for new selection to previous selections
      selAnsDf <- selAnsDf %>%
        bind_rows(inputAns)

      return(selAnsDf)
    }

  }

combine_data <- function(selAnsDf = v$selAnsDf, ansOut = v$ansOut) {

  ansOut %>% spread(question, data) -> scenes
  selAnsDf %>% spread(question, data) -> selections
  allData <- full_join(scenes, selections, by = "path", all=TRUE)

  return(allData)
}

