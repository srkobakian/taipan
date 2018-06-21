library(shiny)
library(tidyverse)
library(purrr)
library(rlang)

shinyServer(
  function(input, output, session) {
    image_list <- list.files("www/app_images", full.names = TRUE)
    output$out_img <- renderImage({
      list(src = image_list[1])
    })
  }
)
