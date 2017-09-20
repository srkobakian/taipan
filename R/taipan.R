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

ui <- navbarPage(theme = shinytheme("spacelab"), "QuestionInput",
                tabPanel("Edit", icon = icon("pencil"),
                         fluidRow(
                           column(8, textInput("dir","Directory","Path")),
                           column(2, offset = 2, actionButton("dirin", "Load Folder"))),

                         #fluidRow(plotOutput("plotE")), add preview when folder path is selected?
                         wellPanel(
                           fluidRow(
                             tabsetPanel(
                               tabPanel("Scene",
                                        wellPanel(id = "qs", style = "overflow-y:scroll; max-height: 600px;",
                                                  fluidRow(column(2, offset=1, actionButton("scene_addq", "Add a Question", icon("plus")))),
                                                  div(id = "scene_questions"))),

                               tabPanel("Selection",
                                        wellPanel(
                                          fluidRow("Add Question section when confirmed")))

                           ))
                         ),

                           fluidRow(column(1),
                                    column(3, actionButton("folderl", "Previous Folder", icon("arrow-left"))),
                                    column(4, offset=4, actionButton("save", "Save Questions", icon("check")),
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
                                  column(5, actionButton("save", "Save Answers", icon("check")),
                                         actionButton("imager","Next Image",icon("arrow-right")))),

                         fluidRow(column(1),
                                    column(6, actionButton("folderl", "Previous Folder", icon("arrow-left"))),
                                    column(5, actionButton("save", "Save Answers", icon("check")),
                                           actionButton("folderr","Next Folder",icon("arrow-right"))))
                         )

)



server <-function(input, output, session) {
  source("modules.R") # Temporary - encorporate into the package

  output$plotE <- renderPlot({
    plot(cars, type=input$plotType)
  })

  output$plotP <- renderPlot({
    plot(cars, type=input$plotType)
  })

  observeEvent(input$scene_addq, {
    insertUI("#scene_questions", ui = questionInputUI(paste0("question", input$scene_addq)))
    callModule(questionInput, paste0("question", input$scene_addq))
  })
}


shinyApp(ui, server)

