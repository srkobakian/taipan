library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "taipan",
      tags$li(class = "dropdown", downloadLink("btn_export", span(icon("download"), " Export Responses"))),
      tags$li(class = "dropdown", a(href="https://github.com/srkob1/taipan", target="_blank", span(icon("github"), " GitHub")))
    ),
    dashboardSidebar(
      disable = TRUE
    ),
    dashboardBody(
      includeScript("www/img_size.js"),
      includeCSS("www/taipan.css"),
      box(
        title = textOutput("out_img_info"),
        div(class = "taipan_image_div",
            imageOutput("out_img_overlay",
                        click = clickOpts(id = "img_click"),
                        dblclick = dblclickOpts(id = "img_dblclick"),
                        brush = brushOpts(id = "img_brush"),
                        inline=TRUE),
            imageOutput("out_img",
                        inline = TRUE)
        ),
        width = 6,
        status = "primary",
        collapsible = TRUE
      ),

      column(6,
             uiOutput("ui_instructions"),
             uiOutput("ui_questions"),
             actionLink(
               "btn_prev",
               box(
                 "Previous",
                 width = 3,
                 background = "green"
               )
             ),
             uiOutput("ui_deleteSelection"),
             uiOutput("ui_saveSelection"),
             actionLink(
               "btn_next",
               box(
                 "Next",
                 width = 3,
                 background = "green",
                 offset = 1
               )
             )
      )
    )
  )
)
