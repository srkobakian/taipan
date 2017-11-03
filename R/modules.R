## Folder input
questionInputUI <- function(id){
  ns <- NS(id)
  wellPanel(
    style = "background-color: #ffffff;",
    id = ns("self"),


    actionButton(ns("debug"), "debug"),

    fluidRow(
      div(class="input-group",
          #strong("New Question"),
          div(class="input-group-btn",
              pickerInput(inputId = "Id059",
                          label = NULL, choices = c("a", "b", "c", "d"))
          ),
          textInput(ns("q"),NULL,placeholder = "Question text"),# width = "100%"),
          div(class="input-group-btn",
               actionButton(ns("remove_self"), "Delete Question", icon = icon("times"), class="btn btn-danger")
          )
      )
    ),
     fluidRow(
       radioButtons(ns("qtype"), "Question type", c("Check box" = "check", "Radio button" = "radio"), inline=TRUE)
     ),

    actionButton(ns("adda"),icon("plus")),
    fluidRow(
      h4("Enter your options"),
      div(id=ns("choices"),
          answerOptUI(ns("option_0")))
    )
  )
}

questionInput <- function(input, output, session){
  observeEvent(input$remove_self, {
    removeUI(selector = paste0("#", session$ns("self")))
  })

  ainputs <- reactiveValues(answers=list())

  ainputs$answers[[session$ns("option_0")]] <- callModule(answerOpt, "option_0")

  #debug
  observeEvent(input$debug, {
    browser()
  })

  observeEvent(input$adda, {
    insertUI(paste0("#",session$ns("choices")), ui = answerOptUI(session$ns(paste0("option_", input$adda))))
    ainputs$answers[[session$ns(paste0("option_", input$adda))]] <- callModule(answerOpt, paste0("option_", input$adda))
  })

  return(list(qtext = reactive({input$`q`}),
              qtype = reactive({input$`qtype`}),
              rm = reactive({input$`remove_self`}),
              opts = reactive({ainputs$answers %>%
                  map(~ if(.x$rm() > 0){NULL}else{.x$answer()}) %>%
                  compact})
              ))
}


answerOptUI <- function(id){
  ns <- NS(id)

  div(id = ns("self"),

   fluidRow(column(10,textInput(ns("answer"), NULL, placeholder = "Enter possible choice")),
           column(2,
                  # div(style= "position: relative;",
                  #  div(style= "position: absolute; bottom: 0",
                  actionButton(ns("removea"),icon("times")), style= "bottom:0;"
                  # ))
           ))
  )
}

answerOpt <- function(input, output, session){
  observeEvent(input$removea, {
    removeUI(selector = paste0("#", session$ns("self")))
  })

  return(list(answer = reactive({input$`answer`}),
              rm = reactive({input$`removea`})
  ))
}

# insertUI("#selection_questionsP", ui = questionDisplayUI(paste0("selection", input$selection_addq)))
# }
#

