# buildQuestionTable <- function(QuestionSet) {
#     QuestionSet %>% imap_dfr(~.x %>% map_dfr(~tibble(Title = .x$qtext(),
#         InputType = .x$qtype()) %>% cbind(tibble(Options = .x$opts() %>%
#         unlist))) %>% mutate(AreaType = .y, Path = "../images"))
# }

# find how to give the options as a list for choices

update_questions <- function(questions, imgData){
  renderUI({
    tabs <- questions %>%
      imap(~ map2(.x, paste0(.y, "_", names(.x)),
                  ~ build_question_outputs(.x, .y, possibly(~ imgData %>%
                                                              filter(question == !!quo(.x)) %>%
                                                              pull(data) %>%
                                                              unlist, NULL)(.y)
                  )
      ) %>%
        wellPanel %>%
        tabPanel(tolower(.y), .))

    names(tabs) <- NULL
    tabs$id <- "areaQuestions"
    do.call(tabsetPanel, tabs)
  })
}

build_question_outputs <- function(args, id, prevInput = NULL) {
  #return a html object for every individual question
  inputFn <-
    switch(args$qType, radio = "radioButtons", check = "checkboxGroupInput",
           args$qType)

  args$qType <- NULL
  args$inputId <- id

  if(!is.null(prevInput)){
    args[[names(formals(inputFn))[na.omit(match(c("selected", "value"), names(formals(inputFn))))]]] <- prevInput
  }

  do.call(inputFn, args)
}


answers_vec <- function(name, input = input) {
  ans <- c()
  if (length(input[[name]]) > 0) {
    ans <- as.vector(input[[name]]) %>% paste(collapse = ",")
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
  inputAns <- questionIDs$scene %>%
    map_dfr(~ tibble(
      question = .x,
      #write a function to check for null answers and still produce multiple responses for check boxes
      answers = answers_vec(name = .x, input = input)
    )) %>%
    #group_by(question) %>%
    #nest() %>%
    mutate(path = pathId) %>%
    select("path", "question", "answers") %>%
    as.data.frame

  if (identical(ansDf %>% filter(path == !!quo(pathId)),
                inputAns)) {
    ansDf
  }
  else{
    ansDf %>%
      filter(path != !!quo(pathId)) %>%
      bind_rows(inputAns)
  }

}

#' @importFrom tidyr spread nest

update_selection_answers <-
  function(selAnsDf = v$selAnsDf,
           pathId = images[v$imageNum],
           selNum = v$selectionNum,
           questionIDs = questionIDs,
           input = input,
           editing = v$editing,
           range = areaSelected()) {

    if (is.null(input$plot_brush) & editing) {
      shiny::showNotification("No area selected", type = "warning")
    } else {
      #data frame for individual selection
      inputAns <- questionIDs$selection %>%
        map_dfr(~ tibble(
          question = .x,
          answers = answers_vec(name = .x, input = input)
        )) %>%
        #group_by(question) %>%
        #nest() %>%
        mutate(
          path = pathId,
          selectionNum = selNum,
          xmin = range$xmin,
          xmax = range$xmax,
          ymin = range$ymin,
          ymax = range$ymax
        ) %>%
        select("path",
               "selectionNum",
               "question",
               "answers",
               "xmin",
               "xmax",
               "ymin",
               "ymax") %>%
        as.data.frame

      if (selNum %in% selAnsDf$selectionNum) {
        #remove previous answers

        selAnsDf <- selAnsDf %>% filter(!(path == pathId &
                                            selectionNum == selNum))
      }


      # add data frame for new selection to previous selections

      selAnsDf <- selAnsDf %>%
        bind_rows(inputAns)
    }

    return(selAnsDf)
  }

combine_data <- function(selAnsDf = v$selAnsDf, ansOut = v$ansOut) {

  ansOut %>% spread(question, answers) -> scenes
  selAnsDf %>% spread(question, answers) -> selections
  allData <- full_join(scenes, selections, by = "path", all=TRUE)

  return(allData)
}

