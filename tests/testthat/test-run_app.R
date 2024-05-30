library(testthat)
library(PhytosanitaryCalculator)

test_that("run_app works without errors", {
  expect_error(run_app(), NA)
})

test_that("ui is created without errors", {
  ui <- fluidPage(
    titlePanel("Phytosanitary Calculator for Inspection Plans Based on Risks"),
    sidebarLayout(
      sidebarPanel(
        selectInput("sampling_type", label = "Sampling Type", choices = c("Hypergeometric", "Binomial", "Poisson")),
        conditionalPanel(condition = 'input.sampling_type === "Hypergeometric"',
                         numericInput(inputId = "lot_size", label = "Lot Size:", value = 100, min = 1)),
        numericInput(inputId = "acceptable_sanity_level", label = "Acceptable Sanity Level:", value = 0.05, min = 0, max = 1, step = 0.001),
        numericInput(inputId = "limit_sanity_level", label = "Limit Sanity Level:", value = 0.24, min = 0, max = 1, step = 0.001),
        numericInput(inputId = "producer_risk", label = "Producer's Risk:", value = 0.05, min = 0, max = 1, step = 0.001),
        numericInput(inputId = "importer_risk", label = "Importer's Risk:", value = 0.15, min = 0, max = 1, step = 0.001)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Sampling Plan", verbatimTextOutput("sampling_plan_summary")),
          tabPanel("Operating Characteristic Curve", plotOutput("oc_curve")),
          tabPanel("Characteristics", verbatimTextOutput("characteristics_summary"))
        )
      )
    )
  )
  expect_s3_class(ui, "shiny.tag.list")
})
