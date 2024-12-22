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
  tags$head(
    tags$style(HTML("
      #download_report_html {
        background-color: #d0e7ff;
        color: #000;
        border: 1px solid #ccc;
        padding: 10px 20px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 14px;
        cursor: pointer;
        border-radius: 4px;
        transition-duration: 0.4s;
      }


        @media print {
         html, body {
         height: auto;  /* Prevent extra pages */
         width: auto;   /* Prevent horizontal scrolling */
         margin: 0;     /* Remove default margins */
         }
      }

      #download_report_html:hover {
        background-color: #a6c8ff;
        border: 1px solid #000;
      }

      .gray-background {
        background-color: #f2f2f2;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }


      .main-title {
        font-size: 21px;
        font-family: Arial, sans-serif;
      }
      .highlight {
        background-color: yellow !important;
      }
      @media print {
        .highlight {
          background-color: yellow !important;
        }
        .highlight div {
          background-color: yellow !important;
        }
      }
    "))
  ),
  titlePanel(tags$h1(class = "main-title", "Phytosanitary Calculator for Inspection Plans Based on Risks")),

  # Main layout with sidebar and main panel
  sidebarLayout(
    sidebarPanel(
      # Input for selecting sampling type
      selectInput(
        "sampling_type",
        label = "Sampling Type",
        choices = c("Hypergeometric", "Binomial", "Poisson")
      ),

      # Conditional panel displayed when the third tab is selected (Inspection)
      conditionalPanel(
        condition = "input.tabselected == 3 && input.sampling_type === 'Hypergeometric'",
        selectInput(
          inputId = "sampling_method",
          label = "Sampling Method:",
          choices = c("Simple Random Sampling", "Systematic Sampling with Random Start")
        )
      ),

      # Conditional panel for hypergeometric sampling type
      conditionalPanel(
        condition = 'input.sampling_type === "Hypergeometric"',
        numericInput(
          inputId = "lot_size",
          label = "Lot Size:",
          value = 144,
          min = 1,
          step = 1
        )
      ),

      # Inputs for acceptable sanity level, limit sanity level, producer and importer risks
      conditionalPanel(
        condition = "input.tabselected == 1 || input.tabselected == 2",
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

      # Additional inputs for hypergeometric sampling type in the inspection tab
      conditionalPanel(
        condition = "input.tabselected == 3 && input.sampling_type === 'Hypergeometric'",
        numericInput(
          inputId = "N_Row",
          label = "Number of rows per Pallet:",
          value = 6,
          min = 1,
          step = 1
        ),
        numericInput(
          inputId = "N_Column",
          label = "Number of columns per Pallet:",
          value = 6,
          min = 1,
          step = 1
        ),
        numericInput(
          inputId = "Height",
          label = "Height of the Pallet:",
          value = 4,
          min = 1,
          step = 1
        )#,

      ),

      # Conditional panel para mostrar el botón solo en la pestaña "Inspection Design"
      conditionalPanel(
        condition = "input.tabselected == 3",
        downloadButton("download_report_html", "Generate HTML Report")
      )
    ),

    # Main panel to display the tables and plots
    mainPanel(

      # Tabset panel for displaying content
      tabsetPanel(
        id = "tabselected",

        # Tab for displaying the sampling plan
        tabPanel("Sampling Plan", value = 1, verbatimTextOutput("sampling_plan_summary")),



        # Tab for displaying the Operating Characteristic (OC) curve
        tabPanel("Operating Characteristic Curve", value = 2, plotOutput("oc_curve", height = "400px")),

        # Tab for displaying the inspection design
        tabPanel("Inspection Design", value = 3,
                 h3(class = "inspection-title", "Phytosanitary Inspection Characteristics"),
                 textOutput("selected_distribution"),
                 textOutput("selected_method"),
                 textOutput("ASL"),
                 textOutput("LSL"),
                 textOutput("Producer_Risk"),
                 textOutput("Importer_Risk"),
                 textOutput("sample_size"),
                 textOutput("acceptance_number"),
                 textOutput("Obtained_Producer_Risk"),
                 textOutput("Obtained_Importer_Risk"),
                 textOutput("total_units"),

                 # Conditional panel to display pallet design in case of hypergeometric sampling
                 conditionalPanel(
                   condition = "input.sampling_type === 'Hypergeometric'",
                   textOutput("num_pallets"),
                   textOutput("num_rows_per_pallet"),
                   textOutput("num_columns_per_pallet"),
                   textOutput("pallet_height"),
                   h3(class = "inspection-title", "Pallet diagram"),
                   uiOutput("pallets_ui"),

                 )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output) {

  output$sampling_plan_summary <- renderPrint({
    plan_summary <- switch(input$sampling_type,
                           "Binomial" = {
                             plan <- suppressWarnings(AcceptanceSampling::find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                                CRP = c(input$limit_sanity_level, input$importer_risk), type = "b"))
                             Ps<-suppressWarnings(assess(AcceptanceSampling::OC2c(plan$n, plan$c), PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                         CRP = c(input$limit_sanity_level, input$importer_risk)))
                           },
                           "Poisson" = {
                             plan <- suppressWarnings(AcceptanceSampling::find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                                CRP = c(input$limit_sanity_level, input$importer_risk), type = "p"))
                             suppressWarnings( assess(AcceptanceSampling::OC2c(plan$n, plan$c,type = "p"), PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                      CRP = c(input$limit_sanity_level, input$importer_risk)))
                           },
                           "Hypergeometric" = {
                             plan <- suppressWarnings(AcceptanceSampling::find.plan(N = input$lot_size,
                                                                PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                                CRP = c(input$limit_sanity_level, input$importer_risk), type = "h"))
                             suppressWarnings(assess(AcceptanceSampling::OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "hypergeom"),
                                                     PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                     CRP = c(input$limit_sanity_level, input$importer_risk)))

                           }
    )

  })

  output$oc_curve <- renderPlot({
    plan_curve <- switch(input$sampling_type,
                         "Binomial" = {
                           plan <- suppressWarnings(AcceptanceSampling::find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                              CRP = c(input$limit_sanity_level, input$importer_risk), type = "b"))
                           curve <- suppressWarnings(AcceptanceSampling::OC2c(plan$n, plan$c,type = "b"))
                         },
                         "Poisson" = {
                           plan <- suppressWarnings(AcceptanceSampling::find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                              CRP = c(input$limit_sanity_level, input$importer_risk), type = "p"))
                           suppressWarnings( curve <- AcceptanceSampling::OC2c(plan$n, plan$c,, type = "p"))
                         },
                         "Hypergeometric" = {
                           plan <- suppressWarnings(AcceptanceSampling::find.plan(N = input$lot_size,
                                                              PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                                                              CRP = c(input$limit_sanity_level, input$importer_risk), type = "h"))
                           suppressWarnings(curve <- AcceptanceSampling::OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "hypergeom"))
                         }
    )
    suppressWarnings(plot(curve, type = "l", lwd = 2, xlim = c(0, 0.4), bg = "gray", main = "Operating Characteristic Curve"))
    grid(lty = "solid")
    abline(h = 1 - input$producer_risk, col = "red", lwd = 1)
    abline(h = input$importer_risk, col = "red", lwd = 1)
    abline(v = input$acceptable_sanity_level, col = "blue", lwd = 1)
    abline(v = input$limit_sanity_level, col = "blue", lwd = 1)
  })

  # Display selected sampling method
  output$selected_method <- renderText({
     if (input$sampling_type == 'Hypergeometric') {
      paste("Selected sampling method :", input$sampling_method)
    } else {
      paste("")
    }
  })
  output$total_units <- renderText({

  })
  # Display selected distribution of probabilities
  output$selected_distribution <- renderText({
    paste("Distribution of Probabilities:", input$sampling_type)
  })

  # Display Acceptable Sanity Level
  output$ASL <- renderText({
    paste("Acceptable Sanity Level:", input$acceptable_sanity_level)
  })

  # Display limit_sanity_level
  output$LSL <- renderText({
    paste("Limit Sanity Level:", input$limit_sanity_level)
  })

  # Display Producer's Risk
  output$Producer_Risk <- renderText({
    paste("Required producer's risk:", input$producer_risk)
  })

  # Display Importer's Risk
  output$Importer_Risk <- renderText({
    paste("Required importer's Risk:", input$importer_risk)
  })

  plan_reactive <- reactive({
    type <- switch(input$sampling_type,
                   "Binomial" = "binomial",
                   "Poisson" = "poisson",
                   "Hypergeometric" = "hypergeom")

    # Find optimal sampling plan based on input values
    suppressWarnings(AcceptanceSampling::find.plan(PRP = c(input$acceptable_sanity_level, 1 - input$producer_risk),
                               CRP = c(input$limit_sanity_level, input$importer_risk),
                               type = type,
                               N = if(type == "hypergeom") floor(input$lot_size) else NULL))
  })
  asses_reactive<-reactive({
    plan <- plan_reactive()
    plan_summary <- switch(input$sampling_type,
                           "Binomial" = {
                             Ps<-suppressWarnings(assess(AcceptanceSampling::OC2c(plan$n, plan$c),print=F, PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                         CRP = c(input$limit_sanity_level, input$importer_risk)))
                           },
                           "Poisson" = {

                             Ps<-suppressWarnings( assess(AcceptanceSampling::OC2c(plan$n, plan$c,type = "p"),print=F, PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                          CRP = c(input$limit_sanity_level, input$importer_risk)))
                           },
                           "Hypergeometric" = {
                             Ps<- suppressWarnings(assess(AcceptanceSampling::OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "h"),print=F,
                                                          PRP = c(input$acceptable_sanity_level, input$producer_risk),
                                                          CRP = c(input$limit_sanity_level, input$importer_risk)))

                           }
    )
  })

  # Display the Obtained importer's risk
  output$Obtained_Importer_Risk <- renderText({
    res<-asses_reactive()
    CRP<-round(res$CRP[3],4)
    paste("Obtained importer's risk:",CRP)
  })

  # Display the Obtained producer's risk
  output$Obtained_Producer_Risk <- renderText({
    res<-asses_reactive()
    PRP<-round(1-res$PRP[3],4)
    paste("Obtained producer's risk:",PRP)
  })

  # Display total number of units for hypergeometric sampling
  output$total_units <- renderText({
    if (input$sampling_type == 'Hypergeometric') {
      paste("Total number of units:", input$lot_size)
    } else {
      paste("Note: In this option, it is assumed that the number of units is too large (infinite), so a pallet diagram is not provided.")
    }
  })

  # Display the calculated acceptance number
  output$acceptance_number <- renderText({
    plan <- plan_reactive()
    paste("Acceptance Number:", plan$c)
  })

  # Display the calculated sample size
  output$sample_size <- renderText({
    plan <- plan_reactive()
    paste("Sample size:", plan$n)
  })

  output$num_pallets <- shiny::renderText({
    num_units_per_pallet <- input$N_Row * input$N_Column * input$Height
    num_pallets <- ceiling(input$lot_size / num_units_per_pallet)
    paste("Calculated number of Pallets:", num_pallets)
  })

  output$num_rows_per_pallet <- shiny::renderText({
    paste("Number of rows per Pallet:", input$N_Row)
  })

  output$num_columns_per_pallet <- shiny::renderText({
    paste("Number of columns per Pallet:      ", input$N_Column)
  })

  output$pallet_height <- shiny::renderText({
    paste("Pallet height:", input$Height)
  })


  pallets_html <- shiny::reactive({
    num_units_per_pallet <- input$N_Row * input$N_Column * input$Height
    num_pallets <- ceiling(input$lot_size / num_units_per_pallet)
    num_rows <- input$N_Row
    num_columns <- input$N_Column
    height <- input$Height
    total_units <- input$lot_size

    sampled_units <- NULL
    if (input$sampling_method == "Simple Random Sampling") {
      sampled_units <- sample(1:total_units, plan_reactive()$n)
    }
    else if (input$sampling_method == "Systematic Sampling with Random Start") {
      k <- floor(total_units / plan_reactive()$n)
      start <- sample(1:k, 1)
      sampled_units <- start + (0:(plan_reactive()$n - 1)) * k
    }

    # Function to create zigzag numbering for pallet design
    create_zigzag_numbering <- function(rows, cols) {
      mat <- matrix(1:(rows * cols), nrow = rows, byrow = TRUE)
      for (i in seq(2, nrow(mat), by = 2)) {
        mat[i, ] <- rev(mat[i, ])
      }
      return(as.vector(t(mat)))
    }


    htmlOutput <- lapply(1:num_pallets, function(pallet_num) {
      div(
        h5(class = "pallet-title", paste("Pallet", pallet_num)),
        div(
          lapply(1:height, function(h) {
            div(
              h6(class = "height-title", paste("Height", h)),
              div(
                style = paste("display: grid; grid-template-columns: repeat(", num_columns, ", 40px); grid-template-rows: repeat(", num_rows, ", 40px); gap: 10px; margin-bottom: 10px;"),
                lapply(1:(num_rows * num_columns), function(i) {
                  zigzag_num <- create_zigzag_numbering(num_rows, num_columns)
                  unit_num <- (pallet_num - 1) * num_units_per_pallet + (h - 1) * (num_rows * num_columns) + zigzag_num[i]
                  if (unit_num <= total_units) {
                    if (!is.null(sampled_units) && unit_num %in% sampled_units) {
                      div(class = "highlight", style = "border: 2px solid black; width: 40px; height: 40px; display: flex; justify-content: center; align-items: center; background-color: yellow;", unit_num)
                    } else {
                      div(style = "border: 2px solid gray; width: 40px; height: 40px; display: flex; justify-content: center; align-items: center;", unit_num)
                    }
                  } else {
                    div(style = "border: 2px solid gray; width: 40px; height: 40px;")
                  }
                })
              ),
              style = "margin-right: 20px; display: inline-block; vertical-align: top;"
            )
          }),
          style = "display: inline-block; vertical-align: top; margin-right: 40px;"
        ),
        style = "margin-bottom: 40px;"
      )
    })

    do.call(shiny::tagList, htmlOutput)
  })

  output$pallets_ui <- shiny::renderUI({
    pallets_html()
  })

  #' @importFrom grDevices png dev.off
    # Function to generate the HTML report
  generate_report <- function(file) {
    res <- asses_reactive()
    plan <- plan_reactive()

    PRP <- round(1-res$PRP[3], 4)
    CRP <- round(res$CRP[3], 4)

    # Crea la curva OC en función del tipo de muestreo
    oc_curve <- switch(
      input$sampling_type,
      "Binomial" = AcceptanceSampling::OC2c(n = plan$n, c = plan$c, type = "binomial"),
      "Poisson" = AcceptanceSampling::OC2c(n = plan$n, c = plan$c, type = "poisson"),
      "Hypergeometric" = AcceptanceSampling::OC2c(n = plan$n, c = plan$c, N = input$lot_size, type = "hypergeom")
    )

    # Genera el gráfico de la curva OC
    oc_curve_file <- tempfile(fileext = ".png")
    png(oc_curve_file, width = 1000, height = 800)
    plot(oc_curve, type = "l", lwd = 2, xlim = c(0, 0.4), bg = "gray", main = "Operating Characteristic Curve")
    grid(lty = "solid")
    abline(h = 1 - input$producer_risk, col = "red", lwd = 1)
    abline(h = input$importer_risk, col = "red", lwd = 1)
    abline(v = input$acceptable_sanity_level, col = "blue", lwd = 1)
    abline(v = input$limit_sanity_level, col = "blue", lwd = 1)
    dev.off()

    # Normaliza la ruta del archivo de la curva OC
    oc_curve_file <- gsub("\\\\", "/", normalizePath(oc_curve_file))

    # Crea la tabla base del informe
    report_data <- data.frame(
      `Field` = c(
        "Selected distribution",
        "Acceptable Sanity Level",
        "Limit Sanity Level",
        "Required producer's risk",
        "Required importer's Risk",
        "Sample size",
        "Acceptance Number",
        "Obtained importer's risk",
        "Obtained producer's risk"
      ),
      `Value` = c(
        input$sampling_type,
        input$acceptable_sanity_level,
        input$limit_sanity_level,
        input$producer_risk,
        input$importer_risk,
        plan$n,
        plan$c,
        CRP,
        PRP
      )
    )

    # Agrega la fila "Sampling method" solo si el tipo de muestreo es Hipergeométrico
    if (input$sampling_type == "Hypergeometric") {
      sampling_method_data <- data.frame(
        `Field` = "Sampling method",
        `Value` = input$sampling_method
      )
      # Se inserta la fila al inicio de report_data
      report_data <- rbind(sampling_method_data, report_data)
    }


    # Agrega información adicional si el tipo de muestreo es Hipergeométrico
    if (input$sampling_type == "Hypergeometric") {
      num_units_per_pallet <- input$N_Row * input$N_Column * input$Height
      num_pallets <- ceiling(input$lot_size / num_units_per_pallet)

      additional_data <- data.frame(
        `Field` = c(
          "Total number of units",
          "Number of Pallets",
          "Number of rows per Pallet",
          "Number of columns per Pallet",
          "Pallet height"
        ),
        `Value` = c(
          input$lot_size,
          num_pallets,
          input$N_Row,
          input$N_Column,
          input$Height
        )
      )

      report_data <- rbind(report_data, additional_data)
    } else {
      # Agrega una nota para los tipos de muestreo no hipergeométricos
      additional_data <- data.frame(
        `Field` = c("Note:"),
        `Value` = c(
          "In this option, it is assumed that the number of units is too large (infinite), so a pallet diagram is not provided."
        )
      )
      report_data <- rbind(report_data, additional_data)
    }

    # Guarda el diseño del pallet si el tipo de muestreo es Hipergeométrico
    pallets_html_file <- NULL
    if (input$sampling_type == "Hypergeometric") {
      pallets_html_file <- tempfile(fileext = ".html")
      htmltools::save_html(pallets_html(), pallets_html_file)
      pallets_html_file <- gsub("\\\\", "/", normalizePath(pallets_html_file))
    }

    # Crea el contenido del reporte R Markdown
    temp_report <- tempfile(fileext = ".Rmd")

    report_content <- paste0(
      "---\n",
      "title: '<b style=\"font-size: 24px;\">Sampling Plan Report</b>'\n",
      "output: html_document\n",
      "---\n\n",
      "<style>\n",
      "  @media print {\n",
      "    html, body {\n",
      "      height: auto;\n",
      "      width: 60;\n",
      "      margin: 0;\n",
      "    }\n",
      "    .highlight {\n",
      "      background-color: yellow !important; \n",
      "      -webkit-print-color-adjust: exact !important; \n",
      "      print-color-adjust: exact !important; \n",
      "    }\n",
      "    .kable-table th, .kable-table td { padding: 5px 60px; }\n",
      "    .kable-table tr { line-height: 1.2; }\n",
      "    h2 { font-size: 18px; font-weight: bold; }\n",
      "    #pallets_ui { \n",
      "      width: 100%; \n",
      "      height: auto; \n",
      "      overflow: visible; \n",
      "    }\n",
      "  }\n",
      "</style>\n\n",
      "<button onclick=\"window.print()\" style=\"margin-bottom: 20px; padding: 10px; background-color: #007bff; color: white; border: none; border-radius: 5px; cursor: pointer;\">Print Report</button>\n\n",
      "<h4>Sampling Plan Summary</h4>\n\n",
      "```{r echo=FALSE, results='asis'}\n",
      "knitr::kable(report_data, format = 'html', table.attr = 'class=\"kable-table\"')\n",
      "```\n\n",
      "<h4>Operating Characteristic Curve</h4>\n\n",
      "```{r echo=FALSE}\n",
      "knitr::include_graphics('", oc_curve_file, "')\n",
      "```\n\n"
    )

    # Agrega la sección del diagrama de pallets solo si el tipo de muestreo es Hipergeométrico
    if (input$sampling_type == "Hypergeometric") {
      report_content <- paste0(
        report_content,
        "<h4>Inspection Design with Pallet Diagram</h4>\n\n",
        "```{r echo=FALSE}\n",
        #  Incluir el HTML del diagrama directamente
        "HTML(paste(readLines('", pallets_html_file, "', warn = FALSE), collapse = '\n'))\n",
        "```\n"
      )
    }

    # Guarda el contenido en el archivo temporal de R Markdown
    writeLines(report_content, temp_report)

    # Renderiza el informe final
    rmarkdown::render(temp_report, output_file = file)
  }



  output$download_report_html <- shiny::downloadHandler(
    filename = function() {
      paste("report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      generate_report(file)
    }
  )
}

shinyApp(ui = ui, server = server)
}#
