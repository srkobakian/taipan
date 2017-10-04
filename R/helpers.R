buildQuestionTable <- function(QuestionSet){
  QuestionSet %>% imap_dfr(~ .x %>% map_dfr(~ data.frame(Title = .x$qtext(), InputType = .x$qtype()) %>%
                                  cbind(Options = .x$opts() %>% unlist)) %>%
                 mutate(AreaType = .y))
}
