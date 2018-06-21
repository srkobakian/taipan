library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "taipan"
    ),
    dashboardSidebar(
      disable = TRUE
    ),
    dashboardBody(
      box(
        title = textOutput("out_img_info"),
        imageOutput("out_img",
                    click = clickOpts(id = "img_click"),
                    dblclick = dblclickOpts(id = "img_dblclick"),
                    brush = brushOpts(id = "img_brush")),
        imageOutput("out_img_overlay",
                    click = clickOpts(id = "img_click"),
                    dblclick = dblclickOpts(id = "img_dblclick"),
                    brush = brushOpts(id = "img_brush")),
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
