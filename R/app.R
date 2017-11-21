#' Launch Taipan
#'
#' This launches the app
#'
#' @param questions the questions
#' @param images the image folder
#' @param answers a dataset of previously created answers from the app
#' @importFrom shinythemes shinytheme
#' @importFrom purrr imap map2
#'
#' @examples
#'
#' \dontrun{
#' sampleQuestions <- list(scene = list(Q1 = list(qType = "radio",
#' label = "My First Question",
#' choices = c("a", "b", "c")),
#' Q2 = list(qType = "check",
#'           label = "My Second Question",
#'           choices = c("1", "2", "3"))),
#' selection = list(Q1 = list(qType = "check",
#'                            label = "My First Question",
#'                            choices = c("a", "b", "c")),
#'                  Q2 = list(qType = "radio",
#'                            label = "My Second Question",
#'                            choices = c("1", "2", "3"))))
#'
#' launchTaipan(sampleQuestions)
#' }
#'
#' @export
launchTaipan <- function(questions = sampleQuestions,
                         images = list.files(system.file("images", package="taipan"), full.names = TRUE),
                         answers = NULL) {

  ui <- fluidPage(title = "Tapian", theme = shinythemes::shinytheme("spacelab"),
                   textOutput("imgInfo", shiny::h3),
                   actionButton("debug", "debug"),
                   fluidRow(uiOutput("plotUI")),
                   wellPanel(
                     fluidRow(
                       uiOutput("questionTabs")
                       )
                     ),

                   # fluidRow(column(1),
                   #          column(6, actionButton("imagePrev", "Previous Image", icon("arrow-left"))),
                   #          column(5, downloadButton("savei", "Save Answers", icon("check")),
                   #                 actionButton("imageNext","Next Image",icon("arrow-right"))))
                  fluidRow(
                    column(6, offset = 3,
                    sliderInput("slide", "Image Number", min = 1, max = length(images),
                                step = 1, value=1, ticks = FALSE)
                  ))
  )


  server <- function(input, output, session) {
    #source("modules.R")
    #source("helpers.R")
    #source("global.R")

    questionIDs <- sampleQuestions %>%
      imap(~ paste0(.y, "_", names(.x)))

    #debug
    observeEvent(input$debug, {
      browser()
    })

    v <- reactiveValues(sArea = "scene",
                        imageNum = 1,
                        ansOut = if(is.null(answers)) {data.frame()} else {answers}
    )


    # add title above plot
    output$imgInfo <- renderText({
      paste0("Image ", v$imageNum, " (", basename(images[v$imageNum]), ")")
    })

    # change image
    observeEvent(input$slide,{
     v$imageNum <- input$slide
    })

    # switch between question sets if an area is selected by the brush
    observeEvent(input$plot_click, {
      v$sArea <- "scene"
    })
    observeEvent(input$plot_brush, {
      v$sArea <- "selection"
    })


    observeEvent(v$sArea,  {
      updateTabsetPanel(session, "areaQuestions",
                        selected = v$sArea)})


    observeEvent(v$imageNum, {
      ### produce the pixel size of the first image
      curImage <- imager::load.image(images[v$imageNum])
      hwratio <- imager::height(curImage)/imager::width(curImage)


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
          plot(curImage)
        },
        width="auto"
      )


      output$questionTabs <- renderUI({
        tabs <- sampleQuestions %>%
          imap(~ map2(.x, paste0(.y, "_", names(.x)),
                      ~ buildQuestionOutputs(.x, .y)) %>%
                 wellPanel %>%
                 tabPanel(tolower(.y), .))

        names(tabs) <- NULL
        tabs$id <- "areaQuestions"
        do.call(tabsetPanel, tabs)
      })
    }
    )


    # observeEvent(input$imageNext, {
    #   v$ansOut <- updateAnswers(v$ansOut, images[v$imageNum], questionIDs, input)
    #   v$imageNum <- min(v$imageNum + 1, length(images))
    #   browser()
    # }
    # )
    #
    # observeEvent(input$imagePrev, {
    # v$ansOut <- updateAnswers(v$ansOut, images[v$imageNum], questionIDs, input)
    # v$imageNum <- max(1, v$imageNum - 1)
    # }
    # )

    output$savei <- downloadHandler(
      filename = paste0("taipan-a.csv"),

      content = function(con) {
        v$ansOut <- updateAnswers(v$ansOut, images[v$imageNum], questionIDs, input)
        write.csv(v$ansOut, con, row.names = FALSE)
      },
      contentType = "text/csv"
    )
  }
  shinyApp(ui, server)
}
