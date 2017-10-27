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


                        uiOutput("plotUIE"),
                        verbatimTextOutput("info"),

                        wellPanel(
                           fluidRow(
                             tabsetPanel(id="areaQuestionsE", selected = "SceneE",
                               tabPanel(title ="Scene", value = "SceneE",
                                        wellPanel(style = "overflow-y:scroll; max-height: 600px;",
                                                  fluidRow(column(2, offset=1, actionButton("scene_addq", "Add a Question", icon("plus")))),
                                                  div(id = "scene_questions"))),

                               tabPanel(title = "Selection", value = "SelectionE",
                                        wellPanel(style = "overflow-y:scroll; max-height: 600px;",
                                                  fluidRow(column(2, offset=1, actionButton("selection_addq", "Add a Question", icon("plus")))),
                                                  div(id = "selection_questions")))

                           ))),

                           fluidRow(column(2, offset=10,downloadButton("saveq", "Save Questions", icon("check"))))
                        ),


                tabPanel("Preview", icon = icon("search"),

                         tabPanel("Plot", uiOutput("plotUIP")),
                         wellPanel(
                           fluidRow(
                             tabsetPanel(id="areaQuestionsP", selected = "SceneP",
                               tabPanel("Scene", value = "SceneP",
                                        wellPanel(
                                          fluidRow("Questions regarding the entire image")
                                          )),

                               tabPanel("Selection", value = "SelectionP",
                                        wellPanel(
                                          fluidRow("Questions regarding a selected area")
                                          ))
                             ))),

                         fluidRow(column(1),
                                  column(6, actionButton("imagel", "Previous Image", icon("arrow-left"))),
                                  column(5, actionButton("savei", "Save Answers", icon("check")),
                                         actionButton("imager","Next Image",icon("arrow-right")))),

                         fluidRow(column(2, offset=10, actionButton("savef", "Save Answers", icon("check"))))
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


  output$plotUIE <- renderUI({
    plotOutput(
      "plotE",
      click = "plotE_click",
      dblclick = "plotE_dblclick",
      hover = "plotE_hover",
      brush = brushOpts(
        id = "plotE_brush",
          resetOnNew = TRUE),
      #write a function to automatically find these depending on p ratio of the chosen image
      width = "80vw", height = paste0(round((hwratio*100),0), "vw")
    )
  })

  output$plotE <- renderPlot({
    plot(img1)}, width="auto"
  )

  # add title above plot
  output$imgInfo <- renderText({
    paste0("Image: ", " (", imglist[1], ")")
  })

 # switch between question sets if an area is selected by the brush
  observeEvent(input$plotE_brush, {
    if (is.null(input$plotE_brush)){
      updateTabsetPanel(session, "areaQuestionsE",
                        selected = "SceneE")}
    else if (!is.null(input$plotE_brush)){
      updateTabsetPanel(session, "areaQuestionsE",
                        selected = "SelectionE")}
  })

  # switch between question sets if an area is selected by the brush
  observeEvent(input$plotP_brush, {
    if (is.null(input$plotP_brush)){
      updateTabsetPanel(session, "areaQuestionsP",
                        selected = "SceneP")}
    else if (!is.null(input$plotE_brush)){
      updateTabsetPanel(session, "areaQuestionsP",
                        selected = "SelectionP")}
  })


    output$plotUIP <- renderUI({
    plotOutput(
      "plotP",
      click = "plotP_click",
      dblclick = "plotP_dblclick",
      hover = "plotP_hover",
      brush = "plotP_brush",
      #write a function to automatically find these depending on p ratio of the chosen image
      width = "80vw", height = paste0(round((hwratio*100),0), "vw")
    )
  })

  output$plotP <- renderPlot({
    plot(img1)}, width="auto"
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


  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
    }
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
             " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }

    paste0(
      "click: ", xy_str(input$plotE_click),
      "dblclick: ", xy_str(input$plotE_dblclick),
      "hover: ", xy_str(input$plotE_hover),
      "brush: ", xy_range_str(input$plotE_brush)
    )
  })

}



shinyApp(ui, server)

