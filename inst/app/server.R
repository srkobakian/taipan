library(shiny)

shinyServer(
  function(input, output, session) {
    questions <- readRDS("data/questions.rds")
    image_list <- list.files("www/app_images", full.names = TRUE)
    output$out_img <- renderImage({
      list(src = image_list[1])
    }, deleteFile = FALSE)

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
  }
)
