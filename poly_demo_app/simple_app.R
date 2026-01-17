
library(shiny)
library(DT)
library(polypharmacyR)

# Pre-load and process data
cprd_data <- load_cprd_data("all")
processed_data <- process_cprd_data(cprd_data$drug_issue, cprd_data$patient,
                                    cprd_data$product_dict, cprd_data$practice, cprd_data$staff)
episodes <- detect_treatment_episodes(processed_data)
poly_results <- analyze_polypharmacy_episodes(episodes, show_tables = FALSE)
clinical_results <- calculate_clinical_polypharmacy(poly_results$episodes, cprd_data$problem, show_tables = FALSE)

ui <- fluidPage(
  titlePanel("CPRD Polypharmacy Analysis Results"),
  tabsetPanel(
    tabPanel("Summary", verbatimTextOutput("summary")),
    tabPanel("Treatment Episodes", DT::dataTableOutput("episodes_table")),
    tabPanel("Polypharmacy", DT::dataTableOutput("poly_table")),
    tabPanel("Clinical Table", DT::dataTableOutput("clinical_table"))
  )
)

server <- function(input, output) {
  output$summary <- renderText({
    paste("Episodes:", nrow(episodes), "\n",
          "Polypharmacy Episodes:", nrow(poly_results$episodes), "\n",
          "Patients:", poly_results$summary$unique_patients)
  })

  output$episodes_table <- DT::renderDataTable({
    DT::datatable(head(episodes, 1000), options = list(scrollX = TRUE))
  })

  output$poly_table <- DT::renderDataTable({
    DT::datatable(poly_results$episodes, options = list(scrollX = TRUE))
  })

  output$clinical_table <- DT::renderDataTable({
    DT::datatable(clinical_results$clinical_episodes, options = list(scrollX = TRUE))
  })
}

shinyApp(ui, server)
