buildTaipan <- function(questions, images, appdir, launch = TRUE, overwrite = FALSE){
  # images <- tools::file_path_as_absolute(images)
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
  app_files <- list.files(system.file("app", package="taipan"))
  file.copy(file.path(system.file(package="taipan"), "app", app_files), file.path(appdir, app_files))

  # SAVE QUESTIONS
  dir.create(file.path(appdir, "data"))
  saveRDS(questions, file = file.path(appdir, "data", "questions.rds"))

  # CONSTRUCT IMAGE DIR
  dir.create(file.path(appdir, "www", "app_images"), recursive = TRUE)
  file.copy(images, file.path(appdir, "www", "app_images", basename(images)))

  # LAUNCH APP
  if(launch){
    shiny::runApp(appdir)
  }
}
