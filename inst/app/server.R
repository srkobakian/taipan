library(shiny)
library(ggplot2)

getInputID <- function(input){
  if(!inherits(input, "shiny.tag")){
    return()
  }
  c(
    if(!is.null(input$attribs$id)){list(list(id=input$attribs$id, type = input$name))}else{NULL},
    do.call("c", lapply(input$children, getInputID))
  )
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
    current_area <- reactive({
      if(!is.null(input$img_brush)){
        input$img_brush[c("xmin", "xmax", "ymin", "ymax")]
      }
      else if(!is.null(current_sel())){
        sel_val <- v$responses[[basename(current_img())]][["selection"]][[current_sel()]]
        list(
          xmin = sel_val$pos$xmin,
          xmax = sel_val$pos$xmax,
          ymin = sel_val$pos$ymin,
          ymax = sel_val$pos$ymax
        )
      }
      else{
        NULL
      }
    })

    output$out_img_overlay <- renderPlot({
      browser()
      v$responses[[basename(current_img())]][["selection"]]
      ggplot(NULL, aes(x=c(0,0,400,400,0), y=c(0,400, 400,0,0))) +
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0)) +
        geom_line() +
        theme_void()
    }, bg="transparent")

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
      vals <- lapply(sceneInputs, function(id){input[[id$id]]})
      names(vals) <- vapply(sceneInputs, function(x) x$id, character(1L))
      vals
    })
    selection_vals <- reactive({
      vals <- lapply(selectionInputs, function(id){input[[id$id]]})
      names(vals) <- vapply(selectionInputs, function(x) x$id, character(1L))
      vals
    })

    output$ui_questions <- renderUI({
      if(!is.null(current_sel())){
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
      if(!is.null(current_sel())){
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

    # Additional test for removing brush
    observeEvent(input$img_click, {
      if(is.null(input$img_brush)){
        v$current_sel <- NULL
      }
    })

    observeEvent(input$img_dblclick, {
      xpos <- input$img_dblclick$x
      ypos <- input$img_dblclick$y
      match <- vapply(v$responses[[basename(current_img())]][["selection"]],
             function(sel){
               (xpos >= sel$pos$xmin) &&
               (xpos <= sel$pos$xmax) &&
               (ypos >= sel$pos$ymin) &&
               (ypos <= sel$pos$ymax)
             }, logical(1L)
      )
      sel_match <- which(match)
      if(length(sel_match) == 1){
        v$current_sel <- sel_match

        # Update inputs
        lapply(selectionInputs,
           function(io){
             val <- v$responses[[basename(current_img())]][["selection"]][[sel_match]][["inputs"]][[io$id]]
             session$sendInputMessage(
               io$id,
               list(
                 value = val,
                 selected = val
               )
             )
           }
        )
      }
      else{
        showNotification(h3("Could not find matching selection, please select a unique area of a square."),
                         type = "error")
      }
    })

    observeEvent(scene_vals(), {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
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
        list(pos = current_area(),
             inputs = selection_vals()
        )
      session$resetBrush("img_brush")
    })
  }
)
