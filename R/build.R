#' buildTaipan
#'
#' This function produces all necessary files for a shiny app.
#' It requires a list of questions to ask users, the location of the images to
#' display and the directory for the folder of shiny app files.
#' Changes can be made to the appearance by altering the css in the folder.
#'
#' @param questions a taipan Questions list of scene and selection questions
#' @param images a vector of image locations, can be local or URLs
#' @param appdir location to export the completed app
#' @param launch launch the app from the new directory after build is completed
#' @param overwrite replace the contents of the supplied location with the completed app
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(taipan)
#'
#' questions <- taipanQuestions(
#'   scene = div(radioButtons("graphic", label = ("2D Graphic"),
#'                            choices = list("Live image", "2D Graphic")),
#'               radioButtons("bg", label = ("Background"),
#'                            choices = list("Crowd",
#'                                           "Court", "Logo wall", "Not applicable")),
#'               radioButtons("person", label = ("Detectable Person"),
#'                            choices = list("Yes", "No")),
#'               radioButtons("shotangle", label = ("Shot angle"),
#'                            choices = list("Level with players",
#'                                           "Birds eye",
#'                                           "Upward angle")),
#'               radioButtons("situation", label = ("Situation"),
#'                            choices = list("Court in play",
#'                                           "Court player close-up",
#'                                           "Court close-up not player",
#'                                           "Crowd",
#'                                           "Off court close up of player",
#'                                           "Transition"))),
#'   selection = div(radioButtons("detect", label = ("Detect Face"),
#'                                choices = list("Player" ,
#'                                               "Other staff on court", "Fan", "None")),
#'                   radioButtons("obscured", label = ("Face obscured"),
#'                                choices = list("Yes", "No")),
#'                   radioButtons("lighting", label = ("Lighting"),
#'                                choices = list("Direct sunlight", "Shaded", "Partially shaded")),
#'                   radioButtons("headangle", label = ("Head angle"),
#'                                choices = list("Front on", "Back of head",
#'                                               "Profile", "Other")),
#'                   radioButtons("glasses", label = ("Glasses"),
#'                                choices = list("Yes", "No")),
#'                   radioButtons("visorhat", label = ("Visor/hat"),
#'                                choices = list("Yes", "No")))
#' )
#'
#' buildTaipan(
#'   questions = questions,
#'   images =
#'   c("https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/1first_image.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/2second_image.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/3third_image.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/4fourth_image.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/5fifth_image.png"),
#'      file.path(tempdir(), "taipan")
#' )
#'
#' }
#'
#' @importFrom shiny runApp
#' @importFrom downloader download
#'
#' @export

buildTaipan <- function(questions, images, appdir, launch = TRUE, overwrite = FALSE){
  # images <- tools::file_path_as_absolute(images)
  if(!inherits(questions, "taipanQuestions")){
    stop("Questions must be created using the taipanQuestions() function.")
  }
  if(overwrite){
    message(paste0("Are you sure you want to overwrite '", appdir, "'? All files in this folder will be deleted!\nYes: Delete ALL of these files!\nNo: Keep it the way it is!"))
    auth <- readline()
    if(toupper(auth)!="YES"){
      message("Aborted building of taipan app.")
      return(invisible(NULL))
    }
    unlink(appdir, recursive = TRUE)
  }
  if(dir.exists(appdir)){
    if(length(list.files(appdir))>0){
      stop(sprintf('Output appdir "%s" already exists, please provide a different location to save app',
                   appdir))
    }
  }
  else{
    dir.create(appdir)
  }
  appdir <- tools::file_path_as_absolute(appdir)

  # WRITE APPDIR
  app_files <- list.files(file.path(system.file(package="taipan"), "app"))
  file.copy(file.path(system.file(package="taipan"), "app", app_files), appdir, recursive = TRUE)

  # SAVE QUESTIONS
  saveRDS(questions, file = file.path(appdir, "data", "questions.rds"))

  # CONSTRUCT IMAGE DIR
  if(dir.exists(images)){
    images <- list.files(images, full.names = TRUE)
  }
  img_success <- file.copy(images, file.path(appdir, "www", "app_images", basename(images)))
  if(any(!img_success)){
    lapply(images[!img_success], download, mode = "wb", destfile = file.path(appdir, "www", "app_images", basename(images)))
  }

  # LAUNCH APP
  if(launch){
    runApp(appdir)
  }
  else {
    cat(paste("The app has been saved in", appdir))
  }
}
