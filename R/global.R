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

utils::globalVariables(c("v", "question", "data", ".",
  "path","xmin","xmax","ymin","ymax","selectionNum"))

tennisQuestions <- list(scene = list(graphic = list(qType = "radio",
                                              label = "Is it a graphic or image of live play?",
                                              choices = c("Live Image", "2d Grpahic")),
                                     bg = list(qType = "radio",
                                               label = "What is the background?",
                                               choices = c("Crowd", "Court", "Logo wall", "Not applicable")),
                                     person = list(qType = "radio",
                                               label = "Is there a person?",
                                               choices = c("No","Yes")),
                                     shotangle = list(qType = "radio",
                                               label = "What angle was the image taken from?",
                                               choices = c("Level with players","Birds eye", "Upward angle")),
                                     situation = list(qType = "radio",
                                               label = "What is the siutation occurring?",
                                               choices = c("Court in play", "Court player close-up","Court close-up not player",
                                                           "Crowd", "Off court close up of player","Transition"))),

                        selection = list(detect = list(qType = "radio",
                                                   label = "Who is the person selected?",
                                                   choices = c("Player","Other staff on court", "Fan")),
                                         obscured = list(qType = "radio",
                                                   label = "Is the face obscured or partially blocked?",
                                                   choices = c("No", "Yes")),
                                         lighting = list(qType = "radio",
                                                         label = "How is the face lit?",
                                                         choices = c("Direct sunlight", "Shaded", "Partially shaded")),
                                         headangle = list(qType = "radio",
                                                         label = "What is the angle of the face relative to the camera?",
                                                         choices = c("Front on", "Back of head",
                                                                     "Profile", "Other")),
                                         glasses = list(qType = "radio",
                                                         label = "Is the person wearing glasses?",
                                                         choices = c("No", "Yes")),
                                         visorhat = list(qType = "radio",
                                                         label = "Is the person wearing a visor or hat?",
                                                         choices = c("No", "Yes"))))


sampleQuestions <- list(scene = list(Q1 = list(qType = "radio",
                                               label = "My First Question",
                                               choices = c("a", "b", "c"),
                                               selected = c("b")),
                                     Q2 = list(qType = "check",
                                               label = "My Second Question",
                                               choices = c("1", "2", "3"))),
                        selection = list(Q1 = list(qType = "check",
                                                   label = "My First Question",
                                                   choices = c("a", "b", "c")),
                                         Q2 = list(qType = "radio",
                                                   label = "My Second Question",
                                                   choices = c("1", "2", "3"))))
