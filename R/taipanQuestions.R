#' Prepare a set of questions for taipan
#'
#' Produces a set of scene and selection questions suitable for use in
#' `buildTaipan`
#'
#' @param scene a shiny UI object containing shiny inputs and web elements.
#' These questions relate to the entire image.
#' @param selection a shiny UI object containing shiny inputs and web elements. These questions relate to each selected area of an image. There is
#' no limit on how many times these questions will be asked.
#'
#' @examples
#' \dontrun{
#'
#' questions <- taipanQuestions(
#'   scene = div(radioButtons("graphic", label = "2D Graphic",
#'                            choices = list("Live image", "2D Graphic")),
#'               radioButtons("bg", label = "Background",
#'                            choices = list("Crowd",
#'                                           "Court", "Logo wall", "Not applicable")),
#'               radioButtons("person", label = "Detectable Person",
#'                            choices = list("Yes", "No")),
#'               radioButtons("shotangle", label = "Shot angle",
#'                            choices = list("Level with players",
#'                                           "Birds eye",
#'                                           "Upward angle")),
#'               radioButtons("situation", label = "Situation",
#'                            choices = list("Court in play",
#'                                           "Court player close-up",
#'                                           "Court close-up not player",
#'                                           "Crowd",
#'                                           "Off court close up of player",
#'                                           "Transition"))),
#'   selection = div(radioButtons("detect", label = "Detect Face",
#'                                choices = list("Player" ,
#'                                               "Other staff on court", "Fan", "None")),
#'                   radioButtons("obscured", label = "Face obscured",
#'                                choices = list("Yes", "No")),
#'                   radioButtons("lighting", label = "Lighting",
#'                                choices = list("Direct sunlight", "Shaded", "Partially shaded")),
#'                   radioButtons("headangle", label = "Head angle",
#'                                choices = list("Front on", "Back of head",
#'                                               "Profile", "Other")),
#'                   radioButtons("glasses", label = "Glasses",
#'                                choices = list("Yes", "No")),
#'                   radioButtons("visorhat", label = "Visor/hat",
#'                                choices = list("Yes", "No")))
#' )
#'}
#'
#' @export
taipanQuestions <- function(scene, selection){
  structure(
    list(scene = scene, selection = selection),
    class = "taipanQuestions"
  )
}
