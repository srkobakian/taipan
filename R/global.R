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
sampleQuestions <- list(selection = list(Q1 = list(QuestionText = "My First Question",
                                                   choices = c("a", "b", "c"),
                                                   qType = "check"),
                                         Q2 = list(QuestionText = "My Second Question",
                                                   choices = c("1", "2", "3"),
                                                   qType = "radio")),
                       scene = list(Q1 = list(QuestionText = "My First Question",
                                              choices = c("a", "b", "c"),
                                              qType = "radio"),
                                    Q2 = list(QuestionText = "My Second Question",
                                              choices = c("1", "2", "3"),
                                              qType = "check")))

