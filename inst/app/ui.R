library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "taipan",
      tags$li(class = "dropdown", a(href="https://github.com/srkob1/taipan", target="_blank", icon("github", class="fa-2x")))
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
         uiOutput("ui_questions"),
         actionLink(
           "btn_prev",
           box(
             "Previous",
             width = 3,
             background = "green"
           )
         ),
         column(1),
         uiOutput("ui_saveSelection"),
         column(1),
         actionLink(
           "btn_next",
           box(
             "Next",
             width = 3,
             background = "green"
           )
         )
      )
    )
  )
)
