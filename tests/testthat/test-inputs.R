context("test inputs")

test_that("check for changes to shiny inputs", {
  # To be completed, checking shiny apps built by tests in a temporary directory is difficult
  # Use this test to ensure inputs produce expected data structure
  library(shiny)

  questions <- taipanQuestions(
    scene = div(checkboxInput("checkbox", "Choice A", value = TRUE),
                checkboxGroupInput("checkGroup",
                                   h3("Checkbox group"),
                                   choices = list("Choice 1" = 1,
                                                  "Choice 2" = 2,
                                                  "Choice 3" = 3),
                                   selected = 1),
                dateRangeInput("dates", h3("Date range"))),
    selection = div(textInput("text", h3("Text input"),
                              value = "Enter text..."),
                    sliderInput("slider1", h3("Sliders"),
                                min = 0, max = 100, value = 50),
                    selectInput("select", h3("Select box"),
                                choices = list("Choice 1" = 1, "Choice 2" = 2,
                                               "Choice 3" = 3), selected = 1))
  )

  skip("Testing shiny apps is not working due to tmpdir issues")
  appdir <- file.path(tempdir(), "taipanQuestionOptions")

  images <- c("https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/1first_image.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/2second_image.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/3third_image.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/4fourth_image.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/5fifth_image.png")

  buildTaipan(
    questions = questions,
    images = images,
    appdir,
    launch = FALSE,
    overwrite = TRUE,
    skip_check = TRUE,
    ext_restricted = TRUE
  )


  # Test that accessing input of a value works
  #expect_equal(
  #    )

})
