#' Launch Taipan
#'
#' This launches the app
#'
#' @param questions A taipan list object containing the questions to be asked in the app.
#' @param images The image folder.
#' @param answers A dataset of previously created answers from the app
#' @importFrom shinythemes shinytheme
#' @importFrom purrr imap map2 possibly
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

  ui <- fluidPage(title = "Taipan", theme = shinythemes::shinytheme("spacelab"),
                   textOutput("imgInfo", shiny::h3),
                   #actionButton("debug", "debug"),
                   fluidRow(uiOutput("plotUI")),
                   wellPanel(
                     fluidRow(
                       uiOutput("questionTabs")
                       )
                     ),

                  fluidRow(column(3, offset=7, actionButton("saveSelection","Save Selection Answers"))),

                  fluidRow(column(1),
                           column(6, actionButton("imagePrev", "Previous Image", icon("arrow-left"))),
                           column(5, actionButton("saveData", "Save All Answers", icon("check")),
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

    if("cache.Rdata" %in% list.files()){
      load("cache.Rdata")
    }
    else {
      v <- reactiveValues(sArea = "scene",
                          imageNum = 1,
                          ansOut = data.frame(),
                          selectionNum = 1,
                          selAnsDf = data.frame(),
                          editing = FALSE)
    }

    questionIDs <- questions %>%
      imap(~ paste0(.y, "_", names(.x)))

    #
    # observeEvent(input$debug, {
    #   browser()
    # })


    # add title above plot
    output$imgInfo <- renderText({
      paste0("Image ", v$imageNum, " (", basename(images[v$imageNum]), ")")
    })

    # change image
    observeEvent(input$slide,{
     v$imageNum <- input$slide
    })


    observeEvent(c(v$sArea, input$plot_brush, input$plot_click),  {
      if (!is.null(input$plot_brush)) {
        v$sArea <- "selection"
        updateTabsetPanel(session, "areaQuestions",
                          selected = v$sArea)}
      if (is.null(input$plot_brush) & v$editing == FALSE) {
        v$sArea <- "scene"
        updateTabsetPanel(session, "areaQuestions",
                          selected = v$sArea)}

    })



    areaSelected <- reactive({
      if(!is.null(input$plot_brush)){
        return(list(xmin = input$plot_brush$xmin,
                    xmax = input$plot_brush$xmax,
                    ymin = input$plot_brush$ymin,
                    ymax = input$plot_brush$ymax))
      }
      else{
        range <- v$selAnsDf %>%
          filter(path == images[v$imageNum] & selectionNum == v$selectionNum) %>%
          select("xmin",
                 "xmax",
                 "ymin",
                 "ymax") %>% distinct()

        return(list(xmin = range$xmin,
                    xmax = range$xmax,
                    ymin = range$ymin,
                    ymax = range$ymax))
      }
    })


    observeEvent(input$plot_dblclick, {
      clickedFaces <- v$selAnsDf %>%
        filter(path == images[v$imageNum]) %>%
        filter(input$plot_dblclick$x > xmin & input$plot_dblclick$x < xmax) %>%
        filter(input$plot_dblclick$y > ymin & input$plot_dblclick$y < ymax) %>%
        pull(selectionNum) %>%
        unique

      if(length(clickedFaces) > 1){
        showNotification("More than one face exists here, to edit, select a region with only one face.", type = "warning")
      }     else if(length(clickedFaces) == 1){
        v$selectionNum <- clickedFaces
        output$questionTabs <- update_questions(questions, v$selAnsDf %>% filter(path == images[v$imageNum]) %>% filter(selectionNum == clickedFaces))
        v$sArea <- "selection"
        v$editing <- TRUE
      }      else if(length(clickedFaces) == 0){
        v$editing <- FALSE
      }

      })


    observeEvent(v$selectionNum, {

      v$selAnsDf %>%
        filter(path == images[v$imageNum]) %>%
        .$selectionNum -> imgSelections

    })

    observeEvent(v$sArea,  {
      updateTabsetPanel(session, "areaQuestions",
                        selected = v$sArea)})



    observeEvent(c(v$imageNum, v$editing), {
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
          brush = brushOpts("plot_brush", delay = 1e10, delayType = "debounce", clip = TRUE, resetOnNew = TRUE),
          #write a function to automatically find these depending on p ratio of the chosen image
          width = "90vw", height = paste0(round((hwratio*100),0), "vw")
        )
      })

      output$plot <- renderPlot({
          plot(curImage)
        boxes <- v$selAnsDf %>% filter(path==images[v$imageNum]) %>%
          mutate(id = paste0(.$path, .$selectionNum)) %>% distinct(.$id, .keep_all = TRUE)
        for(i in 1:NROW(boxes)){
          lines(c(rep(boxes[i,"xmin"], 2), rep(boxes[i,"xmax"], 2), boxes[i,"xmin"]),
                c(boxes[i,"ymin"], rep(boxes[i,"ymax"], 2), rep(boxes[i,"ymin"],2)), col="green")
        }
        if (v$editing) {
        lines(c(rep(2, 2), rep(798, 2), 2),
              c(2, rep(448, 2), rep(2,2)), col="red")
          lines(c(rep(boxes[v$selectionNum,"xmin"], 2), rep(boxes[v$selectionNum,"xmax"], 2), boxes[v$selectionNum,"xmin"]),
                c(boxes[v$selectionNum,"ymin"], rep(boxes[v$selectionNum,"ymax"], 2), rep(boxes[v$selectionNum,"ymin"],2)), col="red")
        }
        },
        width="auto"
      )
    })

    observeEvent(c(v$imageNum, input$saveSelection), {
      output$questionTabs <- update_questions(questions, v$ansOut %>% filter(path == images[v$imageNum]))
      v$editing <- FALSE
      v$sArea <- "scene"
      })

    observeEvent(input$imageNext, {
      v$ansOut <- update_answers(v$ansOut, images[v$imageNum], questionIDs, input)
      v$imageNum <- min(v$imageNum + 1, length(images))

      v$selectionNum = 1
    })



    observeEvent(input$imagePrev, {
    v$ansOut <- update_answers(v$ansOut, images[v$imageNum], questionIDs, input)
    v$imageNum <- max(1, v$imageNum - 1)

    v$selectionNum = 1
    })


    observeEvent(input$saveSelection, {
      if(!is.null(input$plot_brush)){
        v$selectionNum <- as.numeric(input$saveSelection)
      }
      v$ansOut <<- update_answers(isolate(v$ansOut), images[v$imageNum], questionIDs, input)
      v$selAnsDf <<- update_selection_answers(isolate(v$selAnsDf), images[v$imageNum], v$selectionNum, questionIDs,input, v$editing,                                            range = areaSelected())

    })


    onStop(function(){
      save(v, file = "cache.Rdata")
    })


    observeEvent(input$saveData, {
      combine_data(selAnsDf = v$selAnsDf, ansOut = v$ansOut) %>% as_tibble %>%
        write.csv("temp.csv", row.names = FALSE)
      showNotification("Saved at 'temp.csv'")
    })

    #
    # output$savei <- downloadHandler(
    #   filename = paste0("taipanCombinedData.csv"),
    #
    #   content = function(con) {
    #     combine_data(selAnsDf = v$selAnsDf, ansOut = v$ansOut) %>%
    #     write.csv("temp.csv", row.names = FALSE)
    #
    #     file.copy("temp.csv", con)
    #   }
    # )

    }
  shinyApp(ui, server)
}
