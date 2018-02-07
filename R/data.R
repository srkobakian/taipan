
#' Nested list of a sample of Questions
#'
#' A nested list containing two sets of questions and the information needed
#' to create shiny objects for each question.
#'
#' @docType data
#' @name sampleQuestions
#' @format
#' \itemize{
#'   \item{scene: located in the top level list, contains a list of questions
#'    to be asked about the entire image.}
#'   \item{selection: located in the top level list, contains a list of
#'   questions regarding selected areas within an image.}
#'   \item{qType: A character string of the function name of the shiny question
#'    object to be produced.}
#'    \item{label: The question that will be displayed in the shiny app.}
#'    \item{choices: A character vector of options to be chosen.}
#'    \item{selected: An optional argument, when a particular answer should be a
#'    default selection.}
#' }
#'
NULL

#' Nested list of questions for manual annotation of Australian Open images
#'
#' A nested list structure containing questions regarding the images
#' provided by Tennis Australia of the Australian Open 2016:
#'
#' @docType data
#' @name tennisQuestions
#' @format
#' \itemize{
#'   \item{scene: A list of questions regarding the background, angle of the
#'   shot and the situation occuring.}
#'   \item{selection: A list of questions regarding the face captured. Whether
#'   it is completely visible, the angle of the face, how the face is lit, and
#'   the accessories being worn.}
#'   \item{qType: A character string of the function name of the shiny question
#'    object to be produced.}
#'    \item{label: The question that will be displayed in the shiny app.}
#'    \item{choices: A character vector of options to be chosen.}
#'    \item{selected: An option argument, when a particular answer should be a
#'    default selection.}
#' }
#'
NULL
