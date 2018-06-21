#' Launch Taipan
#'
#' This launches the app
#'
#' @param questions A taipan structured list object containing the questions to be asked in the app, see the global environment object sampleQuestions.
#' @param loadCache If a cache.Rda object is found in the working directory and loadCache = TRUE, data from previous use of the app will be made avaiable. if there is no object the app will proceed from the beinnning of the image set. The default for this is FALSE.
#' @param images The list of images located within the image folder.
#' @importFrom shinythemes shinytheme
#' @importFrom purrr imap map2 possibly
#' @importFrom here here
#'
#' @examples
#'
#' \dontrun{
#' library(taipan)
#' tennisQuestionsSmall <- list(scene = list(
#'
#'bg = list(qType = "radio",
#'          label = "What is the background?",
#'          choices = c("Crowd", "Court", "Logo wall", "Not applicable")),
#'shotangle = list(qType = "radio",
#'                 label = "What angle was the image taken from?",
#'                 choices = c("Level with players","Birds eye", "Upward angle")),
#'situation = list(qType = "radio",
#'                 label = "What is the siutation occurring?",
#'                 choices = c("Court in play", "Court player close-up","Court close-up not player",
#'                             "Crowd", "Off court close up of player","Transition"))),
#'
#'selection = list(detect = list(qType = "radio",
#'                               label = "Who is the person selected?",
#'                               choices = c("Player","Other staff on court", "Fan")),
#'                 glasses = list(qType = "radio",
#'                                label = "Is the person wearing glasses?",
#'                                choices = c("No", "Yes")),
#'                 visorhat = list(qType = "radio",
#'                                 label = "Is the person wearing a visor or hat?",
#'                                 choices = c("No", "Yes"))))
#'
#'
#' launchTaipan(tennisQuestionsSmall)
#' }
#'
#' @export
launchTaipan <- function(questions = sampleQuestions, loadCache = FALSE,
                         images = list.files("images")) {

  ui <- fluidPage(title = "Taipan", theme = shinythemes::shinytheme("spacelab"),
                  textOutput("imgInfo", shiny::h3),
                  uiOutput("congratsUI"),
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
  )


server <- function(input, output, session) {

    if (!(file.exists("images"))) {
      stop("Could not find folder called 'images'")
    }

  observeEvent(input$imageNext, {
    output$congrat <- renderUI({
      if (length(images) < v$imageNum){
        return(wellPanel(
          h4("Congratulation, you have completed all images!")
        ))
      }
    })
  })

    # If we have already processed some images these files should exist
    if (file.exists("scene_answers.csv")) {
      #v <- try({
      scene_answers <- read_csv("scene_answers.csv")
      selection_answers <- read_csv("selection_answers.csv")
      # How many have we completed
      imageNum <- sum(images %in% scene_answers$path)+1
      #})
      v <- reactiveValues(sArea = "scene",
                          imageNum = imageNum,
                          ansOut = scene_answers,
                          selectionNum = 1,
                          selAnsDf = selection_answers,
                          editing = FALSE)
    }
    else {
      #if(class(v) == "try-error"){
      #  message("WARNING: Previously classified images not found.")
      v <- reactiveValues(sArea = "scene",
                          imageNum = 1,
                          ansOut = data.frame(path=character(),
                                              question=character(),
                                              answers=character()),
                          selectionNum = 1,
                          selAnsDf = data.frame(path=character(),
                                                selectionNum=numeric(),
                                                question=character(),
                                                answers=character(),
                                                xmin=numeric(),
                                                xmax=numeric(),
                                                ymin=numeric(),
                                                ymax=numeric()),
                          editing = FALSE)
    }

    questionIDs <- questions %>%
      imap(~ paste0(.y, "_", names(.x)))

    # add title above plot
    output$imgInfo <- renderText({
      paste0("Image ", v$imageNum, " (", basename(images[v$imageNum]), ")")
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
      #browser()
      curImage <- imager::load.image(paste(here(),"/images/", images[v$imageNum], sep=""))
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
        v$ansOut <<- update_answers(isolate(v$ansOut), images[v$imageNum], questionIDs, input)
        v$selAnsDf <<- update_selection_answers(isolate(v$selAnsDf), images[v$imageNum], v$selectionNum, questionIDs,input, v$editing, range = areaSelected())
      }
      if(is.null(input$plot_brush)){
        showNotification("No area is currently selected.")
      }
    })


    observeEvent(input$saveData, {
      combine_data(selAnsDf = v$selAnsDf, ansOut = v$ansOut) %>% as_tibble %>%
        write_csv("temp.csv")
      v$ansOut <<- update_answers(isolate(v$ansOut), images[v$imageNum], questionIDs, input)
      scene_answers <<- isolate(v$ansOut)
      write_csv(scene_answers, path ="scene_answers.csv")
      selection_answers <<- isolate(v$selAnsDf)
      write_csv(selection_answers, path ="selection_answers.csv")
      showNotification("Saved at 'scene_answers.csv' and 'selection_answers.csv'")
    })

    onStop(function(){
      # final save
      scene_answers <<- isolate(v$ansOut)
      selection_answers <<- isolate(v$selAnsDf)

      if (exists('scene_answers')) {
        write_csv(scene_answers, path ="scene_answers.csv")
        write_csv(selection_answers, path ="selection_answers.csv")
      } else {
        message("No outputs saved.")
      }

    })


  }
  shinyApp(ui, server)
}
