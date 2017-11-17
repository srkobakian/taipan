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

sampleQuestions <- list(scene = list(Q1 = list(qType = "radio",
                                              label = "My First Question",
                                              choices = c("a", "b", "c")),
                                    Q2 = list(qType = "check",
                                              label = "My Second Question",
                                              choices = c("1", "2", "3"))),
                        selection = list(Q1 = list(qType = "check",
                                                   label = "My First Question",
                                                   choices = c("a", "b", "c")),
                                         Q2 = list(qType = "radio",
                                                   label = "My Second Question",
                                                   choices = c("1", "2", "3"))))


