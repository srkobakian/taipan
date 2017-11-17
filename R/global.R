### produce the list of the images within the "images" folder


#if there are no images in the folder in the folder path
# stop and give a message to reset folder path
# run from beginning
# if("images" %in% list.files()){
#   img_folders <- c()
#   for (path in list.dirs("images")) {
#     exts <- c("jpg", "png")
#     if (length(list_files_with_exts(file.path(path), exts))>0){
#       img_folders <- c(img_folders, path)
#     }
#   }
# }

imglist <- tools::list_files_with_exts(system.file("images", package="taipan"), c("jpg","jpeg", "png", "bmp"))


### produce the pixel size of the first image

img1 <- imager::load.image(imglist[2])
hwratio <- imager::height(img1)/imager::width(img1)

### create sample questions list

sampleQuestions <- list(selection = list(Q1 = list(QuestionText = "My First Question",
                                                   choices = c("a", "b", "c"),
                                                   multipleAnswers = FALSE),
                                         Q2 = list(QuestionText = "My Second Question",
                                                   choices = c("1", "2", "3"),
                                                   multipleAnswers = FALSE)),
                       scene = list(Q1 = list(QuestionText = "My First Question",
                                                    choices = c("a", "b", "c"),
                                                    multipleAnswers = TRUE),
                                          Q2 = list(QuestionText = "My Second Question",
                                                    choices = c("1", "2", "3"),
                                                    multipleAnswers = TRUE)))

