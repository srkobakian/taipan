
utils::globalVariables(c("v", "question", "data", ".",
                         "path","xmin","xmax","ymin","ymax","selectionNum",
                         "images", "answers", "sampleQuestions", "img_folder"))

# Reactive Variables
areaSelected <- function() NULL

data("sampleQuestions")
data("tennisQuestions")
data("tennisQuestionsSmall")
