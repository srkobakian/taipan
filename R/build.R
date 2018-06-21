buildTaipan <- function(questions, images, appdir, launch = TRUE){
  if(dir.exists(appdir)){
    stop(sprintf('Output appdir "%s" already exists, please provide a different location to save app',
                 appdir))
  }
  else{
    dir.create(appdir)
  }

  # WRITE APPDIR
  app_files <- list.files(system.file("app", package="taipan"))
  file.copy(file.path(system.file(package="taipan"), "app", app_files), file.path(appdir, app_files))

  # SAVE QUESTIONS
  dir.create(file.path(appdir, "data"))
  saveRDS(questions, file = file.path(appdir, "data", "questions.rds"))

  # CONSTRUCT IMAGE DIR
  file.copy(images, file.path(appdir, "www", "images", basename(images)))

  # LAUNCH APP
  if(launch){
    runApp(appdir)
  }
}
