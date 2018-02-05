
#' Nested list of a sample of Questions
#'
#' A nested list containing two sets of questions and the information needed
#' to create shiny objects for each question.
#'
#' @docType data
#' @name sampleQuestions
#' @format A nested list with two lists, both containing information for
#' questions to be displayed:
#' \itemize{
#'   \item{scene}{located in the top level list, contains a list of questions
#'    to be asked about the entire image.}
#'   \item{selection}{located in the top level list, contains a list of
#'   questions regarding selected areas within an image.}
#'   \item{qType}{A character string of the function name of the shiny question
#'    object to be produced.}
#'    \item{label}{The question that will be displayed in the shiny app.}
#'    \item{choices}{A character vector of options to be chosen.}
#'    \item{selected}{An option argument, when a particular answer should be a
#'    default selection.}
#'   ...
#' }
#'



sampleQuestions <- list(scene = list(Q1 = list(qType = "radio",
                                               label = "My First Question",
                                               choices = c("a", "b", "c"),
                                               selected = c("b")),
                                     Q2 = list(qType = "check",
                                               label = "My Second Question",
                                               choices = c("1", "2", "3"))),
                        selection = list(Q1 = list(qType = "check",
                                                   label = "My First Question",
                                                   choices = c("a", "b", "c")),
                                         Q2 = list(qType = "radio",
                                                   label = "My Second Question",
                                                   choices = c("1", "2", "3"))))



utils::globalVariables(c("v", "question", "data", ".",
  "path","xmin","xmax","ymin","ymax","selectionNum"))


#' Nested list of questions for manual annotation of Australian Open images
#'
#' A nested list containing two sets of questions and the information needed
#' to create shiny objects for each question.
#'
#' @docType data
#' @name tennisQuestions
#' @format A nested list structure containing questions regarding the images
#' provided by Tennia Australia of the Australian Open 2016:
#' \itemize{
#'   \item{scene}{A list of questions regarding the background, angle of the
#'   shot and the situation occuring.}
#'   \item{selection}{A list of questions regarding the face captured. Whether
#'   it is completely visible, the angle of the face, how the face is lit, and
#'   the accessories being worn.}
#'   \item{qType}{A character string of the function name of the shiny question
#'    object to be produced.}
#'    \item{label}{The question that will be displayed in the shiny app.}
#'    \item{choices}{A character vector of options to be chosen.}
#'    \item{selected}{An option argument, when a particular answer should be a
#'    default selection.}
#'   ...
#' }
#'


tennisQuestions <- list(scene = list(graphic = list(qType = "radio",
                                              label = "Is it a graphic or image of live play?",
                                              choices = c("Live Image", "2d Grpahic")),
                                     bg = list(qType = "radio",
                                               label = "What is the background?",
                                               choices = c("Crowd", "Court", "Logo wall", "Not applicable")),
                                     person = list(qType = "radio",
                                               label = "Is there a person?",
                                               choices = c("No","Yes")),
                                     shotangle = list(qType = "radio",
                                               label = "What angle was the image taken from?",
                                               choices = c("Level with players","Birds eye", "Upward angle")),
                                     situation = list(qType = "radio",
                                               label = "What is the siutation occurring?",
                                               choices = c("Court in play", "Court player close-up","Court close-up not player",
                                                           "Crowd", "Off court close up of player","Transition"))),

                        selection = list(detect = list(qType = "radio",
                                                   label = "Who is the person selected?",
                                                   choices = c("Player","Other staff on court", "Fan")),
                                         obscured = list(qType = "radio",
                                                   label = "Is the face obscured or partially blocked?",
                                                   choices = c("No", "Yes")),
                                         lighting = list(qType = "radio",
                                                         label = "How is the face lit?",
                                                         choices = c("Direct sunlight", "Shaded", "Partially shaded")),
                                         headangle = list(qType = "radio",
                                                         label = "What is the angle of the face relative to the camera?",
                                                         choices = c("Front on", "Back of head",
                                                                     "Profile", "Other")),
                                         glasses = list(qType = "radio",
                                                         label = "Is the person wearing glasses?",
                                                         choices = c("No", "Yes")),
                                         visorhat = list(qType = "radio",
                                                         label = "Is the person wearing a visor or hat?",
                                                         choices = c("No", "Yes"))))

#' Nested list of a sample of Questions
#'
#' A nested list containing two sets of questions and the information needed
#' to create shiny objects for each question.
#'
#' @docType data
#' @name sampleQuestions
#' @format A nested list with two lists, both containing information for
#' questions to be displayed:
#' \itemize{
#'   \item{scene}{located in the top level list, contains a list of questions
#'    to be asked about the entire image.}
#'   \item{selection}{located in the top level list, contains a list of
#'   questions regarding selected areas within an image.}
#'   \item{qType}{A character string of the function name of the shiny question
#'    object to be produced.}
#'    \item{label}{The question that will be displayed in the shiny app.}
#'    \item{choices}{A character vector of options to be chosen.}
#'    \item{selected}{An option argument, when a particular answer should be a
#'    default selection.}
#'   ...
#' }
#'



tennisQuestionsSmall <- list(
  scene = list(
    bg = list(
      qType = "radio",
      label = "What is the background?",
      choices = c("Crowd", "Court", "Logo wall", "Not applicable")
    ),
    shotangle = list(
      qType = "radio",
      label = "What angle was the image taken from?",
      choices = c("Level with players", "Birds eye", "Upward angle")
    ),
    situation = list(
      qType = "radio",
      label = "What is the siutation occurring?",
      choices = c(
        "Court in play",
        "Court player close-up",
        "Court close-up not player",
        "Crowd",
        "Off court close up of player",
        "Transition"
      )
    )
  ),

  selection = list(
    detect = list(
      qType = "radio",
      label = "Who is the person selected?",
      choices = c("Player", "Other staff on court", "Fan")
    ),
    glasses = list(
      qType = "radio",
      label = "Is the person wearing glasses?",
      choices = c("No", "Yes")
    ),
    visorhat = list(
      qType = "radio",
      label = "Is the person wearing a visor or hat?",
      choices = c("No", "Yes")
    )
  )
)

