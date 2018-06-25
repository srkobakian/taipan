#' @importFrom shiny runApp
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
    stop(sprintf('Output appdir "%s" already exists, please provide a different location to save app',
                 appdir))
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
  img_success <- file.copy(images, file.path(appdir, "www", "app_images", basename(images)))
  if(any(!img_success)){
    download.file(images[!img_success], file.path(appdir, "www", "app_images", basename(images)))
  }

  # LAUNCH APP
  if(launch){
    shiny::runApp(appdir)
  }
}
