buildTaipan <- function(questions, images, appdir, launch = TRUE, overwrite = FALSE){
  # images <- tools::file_path_as_absolute(images)
  if(!inherits(questions, "taipanQuestions")){
    stop("Questions must be created using the taipanQuestions() function.")
  }
  if(overwrite){
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
  file.copy(images, file.path(appdir, "www", "app_images", basename(images)))

  # LAUNCH APP
  if(launch){
    shiny::runApp(appdir)
  }
}
