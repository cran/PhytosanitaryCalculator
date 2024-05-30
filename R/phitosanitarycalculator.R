#' @title Phytosanitary Calculator
#' @description A Shiny application for calculating phytosanitary inspection plans based on risks.
#' @details The application provides sampling plans with minimal sample sizes that keep both producer and importer risks below user-defined levels.
#' @return A Shiny app object.
#' @import shiny
#' @import AcceptanceSampling
#' @importFrom graphics abline grid
#' @export
run_app <- function() {
  # Define UI
  ui <- fluidPage(
    titlePanel("Phytosanitary Calculator for Inspection Plans Based on Risks"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "sampling_type",
          label = "Sampling Type",
          choices = c("Hypergeometric", "Binomial", "Poisson")
        ),
        conditionalPanel(
          condition = 'input.sampling_type === "Hypergeometric"',
          numericInput(
            inputId = "lot_size",
            label = "Lot Size:",
            value = 100,
            min = 1
          )
        ),
        numericInput(
          inputId = "acceptable_sanity_level",
          label = "Acceptable Sanity Level:",
          value = 0.05,
          min = 0,
          max = 1,
          step = 0.001
        ),
        numericInput(
          inputId = "limit_sanity_level",
          label = "Limit Sanity Level:",
          value = 0.24,
          min = 0,
          max = 1,
          step = 0.001
        ),
        numericInput(
          inputId = "producer_risk",
          label = "Producer's Risk:",
          value = 0.05,
          min = 0,
          max = 1,
          step = 0.001
        ),
        numericInput(
          inputId = "importer_risk",
          label = "Importer's Risk:",
          value = 0.15,
          min = 0,
          max = 1,
          step = 0.001
        )
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
  
  # Define server logic
  server <- function(input, output) {
    output$sampling_plan_summary <- renderPrint({
      plan_summary <- switch(input$sampling_type,
                             "Binomial" = {
                               plan <- find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                 CRP = c(input$limit_sanity_level, input$importer_risk), type = "b")
                               summary(OC2c(plan$n, plan$c), full = TRUE)
                             },
                             "Poisson" = {
                               plan <- find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                 CRP = c(input$limit_sanity_level, input$importer_risk), type = "p")
                               summary(OC2c(plan$n, plan$c), full = TRUE)
                             },
                             "Hypergeometric" = {
                               plan <- find.plan(N = input$lot_size,
                                                 PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                 CRP = c(input$limit_sanity_level, input$importer_risk), type = "h")
                               summary(OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "hypergeom"), full = TRUE)
                             }
      )
      plan_summary
    })
    
    output$oc_curve <- renderPlot({
      plan_curve <- switch(input$sampling_type,
                           "Binomial" = {
                             plan <- find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                               CRP = c(input$limit_sanity_level, input$importer_risk), type = "b")
                             curve <- OC2c(plan$n, plan$c)
                           },
                           "Poisson" = {
                             plan <- find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                               CRP = c(input$limit_sanity_level, input$importer_risk), type = "p")
                             curve <- OC2c(plan$n, plan$c)
                           },
                           "Hypergeometric" = {
                             plan <- find.plan(N = input$lot_size,
                                               PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                               CRP = c(input$limit_sanity_level, input$importer_risk), type = "h")
                             curve <- OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "hypergeom")
                           }
      )
      plot(curve, type = "l", lwd = 2, xlim = c(0, 0.4), bg = "gray", main = "Operating Characteristic Curve")
      grid(lty = "solid")
      abline(h = 1 - input$producer_risk, col = "red", lwd = 1)
      abline(h = input$importer_risk, col = "red", lwd = 1)
      abline(v = input$acceptable_sanity_level, col = "blue", lwd = 1)
      abline(v = input$limit_sanity_level, col = "blue", lwd = 1)
    })
    
    output$characteristics_summary <- renderPrint({
      characteristics_summary <- switch(input$sampling_type,
                                        "Binomial" = {
                                          plan <- find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                            CRP = c(input$limit_sanity_level, input$importer_risk), type = "b")
                                          assess(OC2c(plan$n, plan$c), PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                 CRP = c(input$limit_sanity_level, input$importer_risk))
                                        },
                                        "Poisson" = {
                                          plan <- find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                            CRP = c(input$limit_sanity_level, input$importer_risk), type = "p")
                                          assess(OC2c(plan$n, plan$c), PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                 CRP = c(input$limit_sanity_level, input$importer_risk))
                                        },
                                        "Hypergeometric" = {
                                          plan <- find.plan(N = input$lot_size,
                                                            PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                            CRP = c(input$limit_sanity_level, input$importer_risk), type = "h")
                                          assess(OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "hypergeom"),
                                                 PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                 CRP = c(input$limit_sanity_level, input$importer_risk))
                                        }
      )
    })
  }
  
  shinyApp(ui = ui, server = server)
}
