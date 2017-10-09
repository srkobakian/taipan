#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @import shiny
#' @import shinythemes

library(shiny)
library(shinythemes)
library(shinyWidgets)
# not necessary if importing pipe function
library(tidyverse)
library(tools) #file extension

ui <- navbarPage(theme = shinytheme("spacelab"), "QuestionInput",
                tabPanel("Edit", icon = icon("pencil"),
                         fluidRow(
                           column(3, "File Path"),
                           column(5, offset = 2, selectInput("folderSelect", "Select Folder Path", choices = img_folders))),

                         actionButton("debug", "debug"),


                         #conditional panel
                         #fluidRow(plotOutput("plotE")), add preview when folder path is selected?

                         wellPanel(
                           fluidRow(
                             tabsetPanel(
                               tabPanel("Scene",
                                        wellPanel(style = "overflow-y:scroll; max-height: 600px;",
                                                  fluidRow(column(2, offset=1, actionButton("scene_addq", "Add a Question", icon("plus")))),
                                                  div(id = "scene_questions"))),

                               tabPanel("Selection",
                                        wellPanel(style = "overflow-y:scroll; max-height: 600px;",
                                                  fluidRow(column(2, offset=1, actionButton("selection_addq", "Add a Question", icon("plus")))),
                                                  div(id = "selection_questions")))

                           ))
                         ),

                           fluidRow(column(1),
                                    column(3, actionButton("folderl", "Previous Folder", icon("arrow-left"))),
                                    column(4, offset=4, downloadButton("saveq", "Save Questions", icon("check")),
                                              actionButton("folderr","Next Folder",icon("arrow-right"))))

                           ),


                tabPanel("Preview", icon = icon("search"),

                         tabPanel("Plot", plotOutput("plotP")),
                         wellPanel(
                           fluidRow(
                             tabsetPanel(
                               tabPanel("Scene",
                                        wellPanel(
                                          fluidRow("Questions regarding the entire image")
                                          )),

                               tabPanel("Selection",
                                        wellPanel(
                                          fluidRow("Questions regarding a selected area"
                                                   )
                                          ))
                             ))),

                         fluidRow(column(1),
                                  column(6, actionButton("imagel", "Previous Image", icon("arrow-left"))),
                                  column(5, actionButton("savei", "Save Answers", icon("check")),
                                         actionButton("imager","Next Image",icon("arrow-right")))),

                         fluidRow(column(1),
                                    column(6, actionButton("folderl", "Previous Folder", icon("arrow-left"))),
                                    column(5, actionButton("savef", "Save Answers", icon("check")),
                                           actionButton("folderr","Next Folder",icon("arrow-right"))))
                         )

)



server <-function(input, output, session) {
  source("modules.R") # Temporary - encorporate into the package
  source("helpers.R") # Temporary - encorporate into the package

  #if there are no images in the folder in the folder path
  # stop and give a message to reset folder path
# run from beginning
  if("images" %in% list.files()){
    img_folders <- c()
    for (path in list.dirs("images")) {
      exts <- c("jpg", "png")
      if (length(list_files_with_exts(file.path(path), exts))>0){
        img_folders <- c(img_folders, path)
      }
    }
  }


  observeEvent(input$folderSelect, {
    currentFilePath <- input$folderSelect
    #populate questions with previous question data

  })


  #debug
  observeEvent(input$debug, {
    browser()
  })

  v <- try(reactiveValues(imgcount = dir(),
                          questions = read.csv("questions.csv"),
                          qdata = rep(NA,6))
           )

  if(class(v)=="try-error"){
    message("Previous question set not found")
  }

  v2 <- reactiveValues(img_questions = list(selection = list(),
                                            scene = list())
                       )

  output$plotE <- renderPlot({
    plot(cars, type=input$plotType)
  })

  output$plotP <- renderPlot({
    plot(cars, type=input$plotType)
  })

  observeEvent(input$scene_addq, {
    insertUI("#scene_questions", ui = questionInputUI(paste0("question", input$scene_addq)))
    v2$img_questions$scene[[paste0("question", input$scene_addq)]] <- callModule(questionInput, paste0("question", input$scene_addq))
  })

  observeEvent(input$selection_addq, {
    insertUI("#selection_questions", ui = questionInputUI(paste0("selection", input$selection_addq)))
    v2$img_questions$selection[[paste0("selection", input$selection_addq)]] <- callModule(questionInput, paste0("selection", input$selection_addq))
  })


  output$saveq <- downloadHandler(
    filename = function(currentFilePath){
      paste0("taipan",currentFilePath, "-qs.csv")},

    content = function(con) {

      out <- buildQuestionTable(v2$img_questions)
      write.csv(out, con, row.names = FALSE)
    },
    contentType = "text/csv"
  )



}


shinyApp(ui, server)

