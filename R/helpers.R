buildQuestionTable <- function(QuestionSet){
  QuestionSet %>% imap_dfr(~ .x %>% map_dfr(~ tibble(Title = .x$qtext(), InputType = .x$qtype()) %>%
                                              cbind(tibble(Options = .x$opts() %>% unlist))) %>%
                 mutate(AreaType = .y))
}
