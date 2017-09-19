#' @keywords internal
"_PACKAGE"

#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

#' @import shiny
#' @import shinythemes

library(shiny)
library(shinythemes)

ui <-navbarPage(theme = shinytheme("spacelab"), "QuestionInput",
                tabPanel("Edit", icon = icon("pencil"),
                         fluidRow(
                           column(6, textInput("dir","Directory","Path")),
                           column(6, actionButton("dirin", "Load Folder"))),

                         #fluidRow(plotOutput("plotE")), add preview when folder path is selected?
                         wellPanel(
                           fluidRow(
                             tabsetPanel(
                               tabPanel("Scene",
                                        wellPanel(id = "qs", style = "overflow-y:scroll; max-height: 600px",
                                                  fluidRow(column(1),
                                                           column(11,textInput("q","New Question","Question text"),
                                                                  selectInput("qtype", "Question type", c("Check box" = "check",
                                                                                                          "Radio button" = "radio")),
                                                                  fluidRow(column(9,textInput("a","Answers", "Enter possible choice")),
                                                                           column(3, actionButton("adda",icon("plus")),
                                                                                  actionButton("removea",icon("times"))))),
                                                           fluidRow(column(11),
                                                                    column(1, actionButton("addq", icon("plus"))))
                                                  ))),

                               tabPanel("Selection",
                                        wellPanel(
                                          fluidRow("Add Question section when confirmed")))
                             )
                           ),

                           fluidRow(column(1),
                                    column(9, actionButton("back",icon("arrow-left"))),
                                    column(2, actionButton("save",icon("check")),
                                           actionButton("remove",icon("arrow-right")))))),

                tabPanel("Preview", icon = icon("search"),
                         tabPanel("Plot", plotOutput("plotP")),
                         wellPanel(
                         )

                )
)



server <-function(input, output, session) {
  output$plotE <- renderPlot({
    plot(cars, type=input$plotType)
  })

  output$plotP <- renderPlot({
    plot(cars, type=input$plotType)
  })

}


shinyApp(ui, server)

