#' Launch Taipan
#'
#' This launches the app
#'
#' @param questions A taipan list object containing the questions to be asked in the app.
#' @param images The image folder.
#' @param answers A dataset of previously created answers from the app
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

                  fluidRow(column(3, offset=7, actionButton("saveSelection","Save Selection Answers"))),

                  fluidRow(column(1),
                           column(6, actionButton("imagePrev", "Previous Image", icon("arrow-left"))),
                           column(5, downloadButton("savei", "Save Answers", icon("check")),
                                  actionButton("imageNext","Next Image",icon("arrow-right"))))

                  #add progress bar
                  # fluidRow(
                  #   column(6, offset = 3,
                  #   sliderInput("slide", "Image Number", min = 1, max = length(images),
                  #               step = 1, value=1, ticks = FALSE)
                  # ))
  )


  server <- function(input, output, session) {
    #source("modules.R")
    #source("helpers.R")
    #source("global.R")

    questionIDs <- questions %>%
      imap(~ paste0(.y, "_", names(.x)))

    #debug
    observeEvent(input$debug, {
      browser()
    })

    v <- reactiveValues(sArea = "scene",
                        imageNum = 1,
                        ansOut = if(is.null(answers)) {data.frame()} else {answers},
                        selectionNum = 1,
                        selAnsDf = data.frame(),
                        editSel = list(x=0, y=0)
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

    observeEvent(input$plot_dblclick, {
      v$editSel$x <- input$plot_dblclick$x
      v$editSel$y <- input$plot_dblclick$y

      v$selAnsDf %>%
        filter(path == images[v$imageNum]) %>%
        filter(v$editSel$x > xmin & v$editSel$x < xmax) %>%
        filter(v$editSel$y > ymin & v$editSel$y < ymax) %>%
        distinct(selectionNum) -> v$selectionNum

          })

    observeEvent(v$selectionNum, {

      v$selAnsDf %>%
        filter(path == images[v$imageNum]) %>%
        .$selectionNum -> imgSelections

    })

    observeEvent(v$sArea,  {
      updateTabsetPanel(session, "areaQuestions",
                        selected = v$sArea)})


    observeEvent(v$imageNum, {
      ### produce the pixel size of the first image
      curImage <- imager::load.image(images[v$imageNum])
      imgHeight <- imager::height(curImage)
      imgWidth <- imager::width(curImage)
      hwratio <- imgHeight/imgWidth


      output$plotUI <- renderUI({
        plotOutput(
          "plot",
          click = "plot_click",
          dblclick = "plot_dblclick",
          hover = "plot_hover",
          brush = "plot_brush",
          #write a function to automatically find these depending on p ratio of the chosen image
          width = "90vw", height = paste0(round((hwratio*100),0), "vw")
        )
      })

      output$plot <- renderPlot({
          plot(curImage)
        boxes <- v$selAnsDf %>% filter(path==images[v$imageNum]) %>%
          mutate(id = paste0(.$path, .$selectionNum)) %>% distinct(.$id, .keep_all = TRUE)
        for(i in 1:NROW(boxes)){lines(c(rep(boxes[i,"xmin"], 2), rep(boxes[i,"xmax"], 2), boxes[i,"xmin"]),
                c(boxes[i,"ymin"], rep(boxes[i,"ymax"], 2), rep(boxes[i,"ymin"],2)), col="green")
        }
        },
        width="auto"
      )


      output$questionTabs <- renderUI({
        tabs <- questions %>%
          imap(~ map2(.x, paste0(.y, "_", names(.x)),
                      ~ build_question_outputs(.x, .y)) %>%
                 wellPanel %>%
                 tabPanel(tolower(.y), .))

        names(tabs) <- NULL
        tabs$id <- "areaQuestions"
        do.call(tabsetPanel, tabs)
      })
    }
    )


    observeEvent(input$imageNext, {
      v$ansOut <- update_answers(v$ansOut, images[v$imageNum], questionIDs, input)
      v$imageNum <- min(v$imageNum + 1, length(images))
      v$selectionNum = 1
    }
    )

    observeEvent(input$imagePrev, {
    v$ansOut <- update_answers(v$ansOut, images[v$imageNum], questionIDs, input)
    v$imageNum <- max(1, v$imageNum - 1)
    v$selectionNum = 1
    }
    )

    observeEvent(input$saveSelection, {

      v$selAnsDf <- update_selection_answers(v$selAnsDf,images[v$imageNum],v$selectionNum, questionIDs,input)

      v$selectionNum <-  v$selAnsDf %>%
        filter(path == images[v$imageNum]) %>% distinct(.$selectionNum) %>% max() + 1
    })

    output$savei <- downloadHandler(
      filename = paste0("taipanCombinedData.csv"),

      content = function(con) {
        fullData <- combine_data()
        write.csv(fullData, con, row.names = FALSE)
      },
      contentType = "text/csv"
    )
  }
  shinyApp(ui, server)
}
