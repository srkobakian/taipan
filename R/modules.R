## Folder input
questionInputUI <- function(id){
  ns <- NS(id)
  wellPanel(
    style = "background-color: #ffffff;",
    id = ns("self"),
    fluidRow(
      div(class="input-group", style="display:flex;width:100%;",
          strong("New Question"),
          textInput(ns("q"),NULL,"Question text", width = "100%"),
          span(class="input-btn-group",
               actionButton(ns("remove_self"), "Delete Question", icon = icon("times"), class="btn btn-danger")
          )
      )
    ),
    fluidRow(
      radioButtons(ns("qtype"), "Question type", c("Check box" = "check", "Radio button" = "radio"), inline=TRUE),
      answerOptUI(ns("option_0"))
    )
  )
}

questionInput <- function(input, output, session){
  observeEvent(input$remove_self, {
    removeUI(selector = paste0("#", session$ns("self")))
  })
}

answerOptUI <- function(id){
  ns <- NS(id)

  fluidRow(column(10,textInput("a",NULL, "Enter possible choice")),
           column(2,
                  # div(style= "position: relative;",
                  #  div(style= "position: absolute; bottom: 0",
                  actionButton(ns("adda"),icon("plus")),
                  actionButton(ns("removea"),icon("times")), style= "bottom:0;"
                  # ))
           ))
}
