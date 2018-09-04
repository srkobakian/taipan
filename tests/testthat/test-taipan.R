context("test-taipan.R")

test_that("building app", {
  skip("Testing shiny apps is not working due to tmpdir issues")
  # To be completed, checking apps built by tests in a temporary directory is difficult

  library(shiny)


  questions <- taipanQuestions(
    scene = div(radioButtons("graphic", label = ("2D Graphic"),
                             choices = list("Live image", "2D Graphic")),
                radioButtons("bg", label = ("Background"),
                             choices = list("Crowd",
                                            "Court", "Logo wall", "Not applicable")),
                radioButtons("person", label = ("Detectable Person"),
                             choices = list("Yes", "No"), selected = "Yes"),
                radioButtons("shotangle", label = ("Shot angle"),
                             choices = list("Level with players",
                                            "Birds eye",
                                            "Upward angle")),
                radioButtons("situation", label = ("Situation"),
                             choices = list("Court in play",
                                            "Court player close-up",
                                            "Court close-up not player",
                                            "Crowd",
                                            "Off court close up of player",
                                            "Transition"))),
    selection = div(radioButtons("detect", label = ("Detect Face"),
                                 choices = list("Player" ,
                                                "Other staff on court", "Fan", "None")),
                    radioButtons("obscured", label = ("Face obscured"),
                                 choices = list("Yes", "No"), selected = "No"),
                    radioButtons("lighting", label = ("Lighting"),
                                 choices = list("Direct sunlight", "Shaded", "Partially shaded")),
                    radioButtons("headangle", label = ("Head angle"),
                                 choices = list("Front on", "Back of head",
                                                "Profile", "Other")),
                    radioButtons("glasses", label = ("Glasses"),
                                 choices = list("Yes", "No"), selected = "No"),
                    radioButtons("visorhat", label = ("Visor/hat"),
                                 choices = list("Yes", "No")))
  )

  appdir <- file.path(tempdir(), "taipan")

  images <- c("https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/blue_player.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/pink_player.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/red_player.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/yellow_player.png",
              "https://raw.githubusercontent.com/srkob1/taipan/master/sample_images/disgruntled_player.png",
              "https://www.iconsdb.com/icons/download/black/mickey-mouse-128.ico")

  buildTaipan(
    questions = questions,
    images = images,
    appdir,
    launch = FALSE,
    overwrite = TRUE,
    skip_check = TRUE
  )



  # Test that all images have been copied to app folder
  expect_equal(
    list.files(file.path(appdir, "www", "app_images")), basename(images)[file_ext(images) %in% c("png", "jpeg", "jpg", "svg", "gif")])

})

