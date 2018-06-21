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
        title = "Image",
        imageOutput("out_img"),
        width = 12,
        status = "primary",
        collapsible = TRUE
      )
    )
  )
)
