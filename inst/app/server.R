library(shiny)

getInputID <- function(input){
  if(!inherits(input, "shiny.tag")){
    return()
  }
  c(input$attribs$id, do.call("c", sapply(input$children, getInputID)))
}

shinyServer(
  function(input, output, session) {
    questions <- readRDS("data/questions.rds")
    image_list <- list.files("www/app_images", full.names = TRUE)

    v <- reactiveValues(
      imageNum = 1,
      current_sel = NULL,
      responses = list()
    )

    current_img <- reactive({
      image_list[v$imageNum]
    })
    current_sel <- reactive({
      v$current_sel
    })

    output$out_img <- renderImage({
      list(src = current_img())
    }, deleteFile = FALSE)
    output$out_img_info <- renderText({
      sprintf("Image: %s (%i/%i)",
              basename(current_img()),
              v$imageNum,
              length(image_list))
    })

    sceneInputs <- getInputID(questions$scene)
    selectionInputs <- getInputID(questions$selection)

    scene_vals <- reactive({
      lapply(sceneInputs, function(id){input[[id]]})
    })
    selection_vals <- reactive({
      lapply(selectionInputs, function(id){input[[id]]})
    })

    output$ui_questions <- renderUI({
      if(!is.null(input$img_brush)){
        box(
          title = "Selection",
          questions$selection,
          width = 12,
          status = "primary",
          collapsible = TRUE
        )
      }
      else{
        box(
          title = "Scene",
          questions$scene,
          width = 12,
          status = "primary",
          collapsible = TRUE
        )
      }
    })

    output$ui_saveSelection <- renderUI({
      if(!is.null(input$img_brush)){
        actionLink(
          "btn_saveSelection",
          box(
            "Save Selection",
            width = 4,
            background = "blue"
          )
        )
      } else {
        column(4)
      }
    })

    observeEvent(input$img_brush, {
      if(is.null(input$img_brush)){
        v$current_sel <- NULL
      }
      else{
        v$current_sel <- length(v$responses[[basename(current_img())]][["selection"]]) + 1
      }
    })

    observeEvent(input$img_dblclick, {
      xpos <- input$img_dblclick$x
      ypos <- input$img_dblclick$y
      match <- vapply(v$responses[[current_img()]][["selection"]],
             function(sel){
               (xpos >= sel$xmin) &&
               (xpos <= sel$xmax) &&
               (ypos >= sel$ymin) &&
               (ypos <= sel$ymax)
             }, logical(1L)
      )
      sel_match <- which(match)
      if(length(sel_match) == 1){
        v$current_sel <- sel_match
      }
      else{
        showNotification(h3("Could not find matching selection, please select a unique area of a square."),
                         type = "error")
      }
    })

    observeEvent(scene_vals(), {
      v$responses[[basename(current_img())]]["scene"] <- scene_vals()
    })

    observeEvent(input$btn_prev, {
      v$imageNum <- pmax(1, v$imageNum - 1)
      session$resetBrush("img_brush")
    })
    observeEvent(input$btn_next, {
      v$imageNum <- pmin(length(image_list), v$imageNum + 1)
      session$resetBrush("img_brush")
    })
    observeEvent(input$btn_saveSelection, {
      v$responses[[basename(current_img())]][["selection"]][[current_sel()]] <-
        list(xmin = input$img_brush$xmin,
             ymin = input$img_brush$ymin,
             xmax = input$img_brush$xmax,
             ymax = input$img_brush$ymax,
             inputs = selection_vals()
        )
      session$resetBrush("img_brush")
    })
  }
)
