
<!-- README.md is generated from README.Rmd. Please edit that file -->
Taipan <img src="man/figures/taipan.png" align="right" />
======

[![Travis-CI Build Status](https://travis-ci.org/srkob1/taipan.svg?branch=master)](https://travis-ci.org/srkob1/taipan)

Taipan is a Tool for Annotating Images in Preparation for ANalysis. It functions as a Shiny web app which allows image surveys to be created from a list of questions and folder of images.

Installation
------------

You can install taipan from github with:

``` r
# install.packages("devtools")
devtools::install_github("srkob1/taipan")
```

Example
-------

This code displays several questions regarding images captured from the Channel 7 broadcast of the 2016 Australian Open.

``` r
library(taipan)

tennisQuestionsSmall <- list(
  scene = list(bg = list(qType = "radio",
                         label = "What is the background?",
                         choices = c("Crowd", "Court", "Logo wall", "Not applicable")),
               shotangle = list(qType = "radio",
                                label = "What angle was the image taken from?",
                                choices = c("Level with players","Birds eye", "Upward angle")),
               situation = list(qType = "radio",
                                label = "What is the siutation occurring?",
                                choices = c("Court in play", "Court player close-up","Court close-up not player","Crowd", "Off court close up of player","Transition"))),
  selection = list(detect = list(qType = "radio",
                               label = "Who is the person selected?",
                               choices = c("Player","Other staff on court", "Fan")),
                   glasses = list(qType = "radio",
                                label = "Is the person wearing glasses?",
                                choices = c("No", "Yes")),
                   visorhat = list(qType = "radio",
                                 label = "Is the person wearing a visor or hat?",
                                 choices = c("No", "Yes"))))

 launchTaipan(tennisQuestionsSmall)
```
