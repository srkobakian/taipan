#' buildTaipan
#'
#' This function produces all necessary files for a shiny app.
#' It requires a list of questions to ask users, the location of the images to
#' display and the directory for the folder of shiny app files.
#' Changes can be made to the appearance by altering the css in the folder.
#'
#' @param questions a `taipanQuestions` list of scene and selection questions
#' @param images a vector of image locations, can be local or URLs
#' @param appdir location to export the completed app
#' @param launch launch the app from the new directory after build is completed
#' @param overwrite replace the contents of the supplied location with the completed app
#' @param skip_check if TRUE, the requirement for user input to overwrite an
#' existing app is removed
#' @param ext_restricted if TRUE only JPEG (JPG), PNG, GIF, SVG are acceptable image
#' formats, all other types will be removed
#'
#' @examples
#' \dontrun{
#' library(taipan)
#' library(shiny)
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
#'
#'
#' images =
#'   c("https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/blue_player.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/pink_player.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/red_player.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/yellow_player.png",
#'   "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/disgruntled_player.png")
#'
#'
#' buildTaipan(
#'   questions = questions,
#'   images = images,
#'   appdir = file.path(tempdir(), "taipan")
#' )
#'
#' }
#'
#' @importFrom shiny runApp
#' @importFrom utils download.file head
#' @importFrom tools file_ext
#'
#' @export
buildTaipan <- function(questions, images, appdir, launch = TRUE, overwrite = FALSE, skip_check = FALSE, ext_restricted = TRUE){
  # images <- tools::file_path_as_absolute(images)
  if(!inherits(questions, "taipanQuestions")){
    stop("Questions must be created using the taipanQuestions() function.")
  }
  if(overwrite){
    message(paste0("Are you sure you want to overwrite '", appdir, "'? All files in this folder will be deleted!\nYes: Delete ALL of these files!\nNo: Keep it the way it is!"))
    if(!skip_check){
      auth <- readline()
      if(toupper(auth)!="YES"){
        message("Aborted building of taipan app.")
        return(invisible(NULL))
      }
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
  dir.create(file.path(appdir, "data"))
  saveRDS(questions, file = file.path(appdir, "data", "questions.Rds"))


  # DELETE UNSUPPORTED IMAGES
  if (ext_restricted){
    valid_ext <- file_ext(images) %in% c("png", "jpeg", "jpg", "svg", "gif")
    if(any(!valid_ext)){
      message <- "Images have been removed due to extension type:\n"
      message <- paste0(message, paste(head(images[!valid_ext]), collapse = ", \n"))
      if (length(images) > 6) {
        extra <- length(images)-6
        message <- paste0(message,", \n and ", extra, " other images.")
      }
      message(message)
      images  <- images[valid_ext] #only keep valid image extensions
    }
  }

  # CONSTRUCT IMAGE DIR
  dir.create(file.path(appdir, "www", "app_images"))
  if(any(dirs <- dir.exists(images))){
    images <- c(list.files(images[dirs], full.names = TRUE, recursive = TRUE), images[!dirs])
  }
  img_success <- file.copy(images, file.path(appdir, "www", "app_images", basename(images)))
  if(any(!img_success)){
    # check download method to use
    if (.Platform$OS.type == "windows") {
      method <- "wininet"
    }
    else{
      method <- "auto"
    }
    Map(download.file, url = images[!img_success], mode = "wb", method = method, destfile = file.path(appdir, "www", "app_images", basename(images[!img_success])))
  }

  cat(paste("The app has been saved in", appdir))
  # LAUNCH APP
  if (launch) {
    runApp(appdir)
  }

}
