library(shiny)
library(tidyverse)
library(purrr)
library(rlang)

shinyServer(
  function(input, output, session) {
    image_list <- list.files("www/app_images", full.names = TRUE)
    output$out_img <- renderImage({
      list(src = image_list[1])
    }, deleteFile = FALSE)

    output$ui_questions <- renderUI({
      tabBox(
        id = "qbox",
        width = 12,
        tabPanel("Q1"),
        tabPanel("Q2")
      )
    })

    output$ui_saveSelection <- renderUI({
      if(TRUE){
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

    updateTabsetPanel(session, "qbox", "Q2")
  }
)
