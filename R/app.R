
#
# library(shiny)
# library(shinythemes)
# library(shinyWidgets)
# not necessary if importing pipe function
# library(tidyverse)
# library(tools) #file extension
# library(imager)

#' @importFrom shinythemes shinytheme
#' @export
launchTaipan <- function() {

  ui <- fluidPage(title = "Tapian", theme = shinythemes::shinytheme("spacelab"),
                   verbatimTextOutput("imgInfo"),
                   actionButton("debug", "debug"),
                   fluidRow("Plot", uiOutput("plotUI")),
                   actionButton("updateQs", "Update Questions"),
                   wellPanel(
                     fluidRow(
                       tabsetPanel(id="areaQuestions", selected = "Scene",
                                   tabPanel("Scene", value = "Scene",
                                            wellPanel(
                                              fluidRow(id="sceneQuestions")
                                            )),

                                   tabPanel("Selection", value = "Selection",
                                            wellPanel(
                                              fluidRow(id="selectionQuestions")
                                            ))
                       ))),

                   fluidRow(column(1),
                            column(6, actionButton("imagel", "Previous Image", icon("arrow-left"))),
                            column(5, actionButton("savei", "Save Answers", icon("check")),
                                   actionButton("imager","Next Image",icon("arrow-right"))))


  )


  server <- function(input, output, session) {
    #source("modules.R")
    #source("helpers.R")
    #source("global.R")

    #debug
    observeEvent(input$debug, {
      browser()
    })


    v2 <- reactiveValues(img_questions = list(selection = list(),
                                              scene = list()),
                         sArea = "Scene")



    # add title above plot
    output$imgInfo <- renderText({
      paste0("Image: ", " (", imglist[1], ")")
    })


    # switch between question sets if an area is selected by the brush
    observeEvent(input$plot_click, {
      v2$sArea <- "Scene"
    })
    observeEvent(input$plot_brush, {
      v2$sArea <- "Selection"
    })


    observeEvent(v2$sArea,  {
      updateTabsetPanel(session, "areaQuestions",
                        selected = v2$sArea)})


    output$plotUI <- renderUI({
      plotOutput(
        "plot",
        click = "plot_click",
        dblclick = "plot_dblclick",
        hover = "plot_hover",
        brush = "plot_brush",
        #write a function to automatically find these depending on p ratio of the chosen image
        width = "80vw", height = paste0(round((hwratio*100),0), "vw")
      )
    })

    output$plot <- renderPlot({
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
      filename =
        paste0("taipan-qs.csv"),

      content = function(con) {

        out <- buildQuestionTable(v2$img_questions)
        write.csv(out, con, row.names = FALSE)
      },
      contentType = "text/csv"
    )


    #
    #   observeEvent(input$updateQs, {
    #
    #     #Populate preview question sections
    #     #turn this into a module
    #     #take in temp file, build app around it
    #
    #       # tmp <- tempfile()
    #       # write.csv(buildQuestionTable(v2$img_questions), row.names = FALSE, tmp)
    #
    #      # Preview set of Questions, replace this with actual questions when created?
    #       read_csv("questions.csv") %>%
    #         mutate(inputID = paste(AreaType, InputType, Title,sep="_")) %>%
    #         split(.$AreaType) %>%
    #               map(~ .x %>% split(.$Title) %>%
    #                     map(~ .x %>% buildQuestionOutputs(.))) -> qs
    #
    #   })
    #
  }
  shinyApp(ui, server)
}
