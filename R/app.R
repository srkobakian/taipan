
#
# library(shiny)
# library(shinythemes)
# library(shinyWidgets)
# not necessary if importing pipe function
# library(tidyverse)
# library(tools) #file extension
# library(imager)

#' @importFrom shinythemes shinytheme
#' @importFrom purrr imap map2
#' @export
launchTaipan <- function(questions = sampleQuestions,
                         images = list.files(system.file("images", package="taipan"))) {

  ui <- fluidPage(title = "Tapian", theme = shinythemes::shinytheme("spacelab"),
                   textOutput("imgInfo", shiny::h3),
                   actionButton("debug", "debug"),
                   fluidRow(uiOutput("plotUI")),
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
                            column(6, actionButton("imagePrev", "Previous Image", icon("arrow-left"))),
                            column(5, actionButton("savei", "Save Answers", icon("check")),
                                   actionButton("imageNext","Next Image",icon("arrow-right"))))


  )


  server <- function(input, output, session) {
    #source("modules.R")
    #source("helpers.R")
    #source("global.R")

    #debug
    observeEvent(input$debug, {
      browser()
    })


    # TODO: Increment image
    # ### produce the pixel size of the first image
    #
    # img1 <- imager::load.image(imglist[2])
    # hwratio <- imager::height(img1)/imager::width(img1)
    #

    v <- reactiveValues(sArea = "Scene",
                         imageNum = 1)



    # add title above plot
    output$imgInfo <- renderText({
      paste0("Image: ", " (", basename(imglist[1]), ")")
    })


    # switch between question sets if an area is selected by the brush
    observeEvent(input$plot_click, {
      v$sArea <- "Scene"
    })
    observeEvent(input$plot_brush, {
      v$sArea <- "Selection"
    })


    observeEvent(v$sArea,  {
      updateTabsetPanel(session, "areaQuestions",
                        selected = v$sArea)})


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

        out <- buildQuestionTable(v$img_questions)
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
    #       # write.csv(buildQuestionTable(v$img_questions), row.names = FALSE, tmp)
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
