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
library(imager)



ui <- navbarPage(theme = shinytheme("spacelab"), "QuestionInput",
                tabPanel("Edit", icon = icon("pencil"),

                         # allow for selection between folders
                         #fluidRow(column(3, "File Path"),
                         #  column(5, offset = 2, selectInput("folderSelect", "Select Folder Path", choices = img_folders))),

                        actionButton("debug", "debug"),


                         #conditional panel
                        #conditionalPanel(length(imglist)>0,
                          #               div(id = "plotE", )),


                        uiOutput("plotUI"),
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

                           ))),

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


server <- function(input, output, session) {
  source("modules.R")
  source("helpers.R")
  source("global.R")

  #debug
  observeEvent(input$debug, {
    browser()
  })


  v2 <- reactiveValues(img_questions = list(selection = list(),
                                            scene = list()))

  height <- function() {
    session$clientData$output_plotE_width*(450/800)
  }


  output$plotUI <- renderUI({
    plotOutput(
      "plotE",
      #write a function to automatically find these depending on p ratio of the chosen image
      width = "80vw", height = "56vw"
    )
  })

  output$plotE <- renderPlot({
    plot(load.image(imglist[1]))}, width="auto"
  )

  output$imgInfo <- renderText({
    paste0("Image: ", " (", imglist[1], ")")
  })

  output$plotP <- renderPlot({
    plot(load.image(imglist[1]))},
    height = function() {
      session$clientData$output_plotP_width*(450/800)
    }
  )


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

