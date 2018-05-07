## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "centre")
library(taipan)

## ---- eval = FALSE-------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_github("srkob1/taipan")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  require(taipan)
#  launchTaipan()

## ---- echo=FALSE, dpi=200, fig.cap="Taipan initial display"--------------
knitr::include_graphics("Taipan1imageInformation.png")

## ---- echo=FALSE, dpi=200, fig.cap="Taipan application selection questions displayed when area is selected by brush."----
knitr::include_graphics("Taipan2selectionInformation.png")


## ---- echo=FALSE, dpi=200,fig.cap="Taipan application previously annotated face highlighted by green box."----
knitr::include_graphics("Taipan3selectionInformation.png")

## ---- echo=FALSE, dpi=280, fig.cap="Taipan application editing previously annotated face, highlighted by red box on face and around image."----
knitr::include_graphics("Taipan4editings.png")

## ---- echo=TRUE, warning=FALSE, message=FALSE----------------------------
scene_answers <- read_csv("data/scene_answers.csv")
selection_answers <- read_csv("data/selection_answers.csv")

## ---- echo=FALSE---------------------------------------------------------
scene_answers[1:3,-1] %>% as.data.frame()

## ---- eval=FALSE---------------------------------------------------------
#  scene_answers %>% spread(question, answers)

## ---- eval=FALSE---------------------------------------------------------
#  selection_answers %>% spread(question, answers)

## ---- echo = FALSE, dpi=180----------------------------------------------
selection_answers %>% spread(question,answers) -> faces

# library(ggplot2)
# ggplot(data=faces) +
#    geom_bar(aes(x=selection_visorhat, fill = selection_detect)) +
#   coord_flip() +
#   guides(fill = guide_legend(title="Person Captured")) +
#   theme(legend.position="bottom") +
#   labs(caption="") +
#   ylab("Amount of Faces") + xlab("Hat or Visor worn on face")

knitr::include_graphics("Taipan5plotting.png")

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  sampleQuestions <- list(scene = list(Q1 = list(qType = "radio",
#                                                 label = "My First Question",
#                                                 choices = c("a", "b", "c"),
#                                                 selected = c("b")),
#                                       Q2 = list(qType = "check",
#                                                 label = "My Second Question",
#                                                 choices = c("1", "2", "3"))),
#                          selection = list(Q1 = list(qType = "check",
#                                                     label = "My First Question",
#                                                     choices = c("a", "b", "c")),
#                                           Q2 = list(qType = "radio",
#                                                     label = "My Second Question",
#                                                     choices = c("1", "2", "3"))))

