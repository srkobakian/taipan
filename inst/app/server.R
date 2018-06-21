library(shiny)
library(tidyverse)
library(purrr)
library(rlang)

shinyServer(
  function(input, output, session) {
    output$out_img <- renderImage({
      list(src = "https://raw.githubusercontent.com/tidyverts/fasster/master/man/figure/logo.png")
    })
  }
)
