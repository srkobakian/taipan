
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
                         images = list.files(system.file("images", package="taipan"), full.names = TRUE)) {

  ui <- fluidPage(title = "Tapian", theme = shinythemes::shinytheme("spacelab"),
                   textOutput("imgInfo", shiny::h3),
                   actionButton("debug", "debug"),
                   fluidRow(uiOutput("plotUI")),
                   actionButton("updateQs", "Update Questions"),
                   wellPanel(
                     fluidRow(
                       uiOutput("questionTabs")
                       )
                     ),

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

    v <- reactiveValues(sArea = "Scene",
                         imageNum = 1)


    # add title above plot
    output$imgInfo <- renderText({
      paste0("Image ", v$imageNum, " (", basename(images[v$imageNum]), ")")
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
        do.call(tabsetPanel, tabs)
      })
    }
    )





    output$saveq <- downloadHandler(
      filename =
        paste0("taipan-qs.csv"),

      content = function(con) {

        out <- buildQuestionTable(v$img_questions)
        write.csv(out, con, row.names = FALSE)
      },
      contentType = "text/csv"
    )


    observeEvent(input$imageNext, {
      v$imageNum <- v$imageNum + 1
    }
    )

    observeEvent(input$imagePrev, {
      v$imageNum <- v$imageNum - 1
    }
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
