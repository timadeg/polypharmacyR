# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(polypharmacyR)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "CPRD Polypharmacy Analysis Suite"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Overview", tabName = "overview", icon = icon("database")),
      menuItem("Core Analysis", tabName = "core", icon = icon("pills")),
      menuItem("Clinical Context", tabName = "clinical", icon = icon("stethoscope")),
      menuItem("Economic Analysis", tabName = "economic", icon = icon("pound-sign")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Patterns & Trends", tabName = "patterns", icon = icon("chart-line"))
    )
  ),

  dashboardBody(
    tabItems(
      # Data Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "CPRD Dataset Summary", status = "primary", solidHeader = TRUE, width = 12,
                    h3("Synthetic CPRD Data Overview"),
                    verbatimTextOutput("data_summary")
                )
              ),
              fluidRow(
                box(title = "Patient Demographics", status = "info", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("patient_overview")
                ),
                box(title = "Prescription Overview", status = "info", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("prescription_overview")
                )
              )
      ),

      # Core Analysis Tab
      tabItem(tabName = "core",
              fluidRow(
                box(title = "Analysis Parameters", status = "primary", solidHeader = TRUE, width = 12,
                    column(3, numericInput("default_duration", "Default Duration (days)", value = 28, min = 1, max = 365)),
                    column(3, numericInput("grace_period", "Grace Period (days)", value = 30, min = 0, max = 90)),
                    column(3, numericInput("poly_threshold", "Polypharmacy Threshold", value = 5, min = 2, max = 20)),
                    column(3, actionButton("run_core_analysis", "Run Core Analysis", class = "btn-success"))
                )
              ),
              fluidRow(
                box(title = "Analysis Progress", status = "warning", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("analysis_progress")
                ),
                box(title = "Key Results", status = "success", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("core_results_summary")
                )
              ),
              fluidRow(
                box(title = "Treatment Episodes", status = "info", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("episodes_table")
                ),
                box(title = "Polypharmacy Episodes", status = "info", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("poly_episodes_table")
                )
              )
      ),

      # Clinical Context Tab
      tabItem(tabName = "clinical",
              fluidRow(
                box(title = "Clinical Analysis", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("run_clinical", "Analyze Clinical Context", class = "btn-warning"),
                    br(), br(),
                    verbatimTextOutput("clinical_progress")
                )
              ),
              fluidRow(
                box(title = "Clinical Appropriateness", status = "warning", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("appropriateness_table")
                ),
                box(title = "Risk Distribution", status = "warning", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("risk_distribution_table")
                )
              ),
              fluidRow(
                box(title = "Patient Clinical Summary", status = "info", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("patient_clinical_table")
                )
              )
      ),

      # Economic Analysis Tab
      tabItem(tabName = "economic",
              fluidRow(
                box(title = "Cost Analysis", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("run_cost", "Analyze Economic Impact", class = "btn-info"),
                    br(), br(),
                    verbatimTextOutput("cost_progress")
                )
              ),
              fluidRow(
                box(title = "Cost Summary", status = "info", solidHeader = TRUE, width = 6,
                    verbatimTextOutput("cost_summary_stats")
                ),
                box(title = "Cost Distribution", status = "info", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("cost_distribution_table")
                )
              ),
              fluidRow(
                box(title = "High Cost Episodes", status = "warning", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("high_cost_episodes_table")
                )
              )
      ),

      # Demographics Tab
      tabItem(tabName = "demographics",
              fluidRow(
                box(title = "Demographic Analysis", status = "primary", solidHeader = TRUE, width = 12,
                    actionButton("run_demographics", "Analyze Demographics", class = "btn-primary"),
                    br(), br(),
                    verbatimTextOutput("demo_progress")
                )
              ),
              fluidRow(
                box(title = "Age Band Analysis", status = "primary", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("age_band_table")
                ),
                box(title = "Age-Gender Patterns", status = "primary", solidHeader = TRUE, width = 6,
                    DT::dataTableOutput("age_gender_table")
                )
              ),
              fluidRow(
                box(title = "Elderly Analysis (65+)", status = "info", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("elderly_analysis_table")
                )
              )
      ),

      # Patterns & Trends Tab
      tabItem(tabName = "patterns",
              fluidRow(
                box(title = "Pattern Analysis", status = "primary", solidHeader = TRUE, width = 12,
                    column(4, actionButton("run_progression", "Analyze Progression", class = "btn-success")),
                    column(4, actionButton("run_seasonal", "Analyze Seasonal Patterns", class = "btn-info")),
                    column(4, actionButton("run_practice", "Analyze Practice Variation", class = "btn-warning"))
                )
              ),
              fluidRow(
                box(title = "Analysis Results", status = "success", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("patterns_progress")
                )
              ),
              fluidRow(
                box(title = "Progression Patterns", status = "success", solidHeader = TRUE, width = 4,
                    DT::dataTableOutput("progression_table")
                ),
                box(title = "Seasonal Patterns", status = "info", solidHeader = TRUE, width = 4,
                    DT::dataTableOutput("seasonal_table")
                ),
                box(title = "Regional Variation", status = "warning", solidHeader = TRUE, width = 4,
                    DT::dataTableOutput("regional_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Load data reactively
  cprd_data <- reactive({
    tryCatch({
      load_cprd_data("all")
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
      return(NULL)
    })
  })

  # Reactive values to store analysis results
  values <- reactiveValues(
    processed_data = NULL,
    episodes = NULL,
    poly_results = NULL,
    clinical_results = NULL,
    cost_results = NULL,
    demo_results = NULL,
    progression_results = NULL,
    seasonal_results = NULL,
    practice_results = NULL
  )

  # Data Overview outputs
  output$data_summary <- renderText({
    req(cprd_data())
    data <- cprd_data()
    paste("Drug Issues:", nrow(data$drug_issue), "records\n",
          "Patients:", nrow(data$patient), "patients\n",
          "Problems:", nrow(data$problem), "problems\n",
          "Practices:", nrow(data$practice), "practices\n",
          "Referrals:", nrow(data$referral), "referrals")
  })

  output$patient_overview <- DT::renderDataTable({
    req(cprd_data())
    DT::datatable(head(cprd_data()$patient, 100), options = list(scrollX = TRUE, pageLength = 5))
  })

  output$prescription_overview <- DT::renderDataTable({
    req(cprd_data())
    DT::datatable(head(cprd_data()$drug_issue, 100), options = list(scrollX = TRUE, pageLength = 5))
  })

  # Core Analysis
  observeEvent(input$run_core_analysis, {
    req(cprd_data())

    output$analysis_progress <- renderText("Starting core analysis...")

    tryCatch({
      # Step 1: Process data
      output$analysis_progress <- renderText("Step 1/3: Processing CPRD data...")
      values$processed_data <- process_cprd_data(
        drug_issue = cprd_data()$drug_issue,
        patient = cprd_data()$patient,
        product_dict = cprd_data()$product_dict,
        practice = cprd_data()$practice,
        staff = cprd_data()$staff,
        default_duration = input$default_duration,
        grace_period = input$grace_period
      )

      # Step 2: Detect episodes
      output$analysis_progress <- renderText("Step 2/3: Detecting treatment episodes...")
      values$episodes <- detect_treatment_episodes(values$processed_data)

      # Step 3: Analyze polypharmacy
      output$analysis_progress <- renderText("Step 3/3: Analyzing polypharmacy episodes...")
      values$poly_results <- analyze_polypharmacy_episodes(
        treatment_episodes = values$episodes,
        poly_threshold = input$poly_threshold,
        show_tables = FALSE
      )

      output$analysis_progress <- renderText("Core analysis completed successfully!")

    }, error = function(e) {
      output$analysis_progress <- renderText(paste("Error:", e$message))
      showNotification(paste("Analysis failed:", e$message), type = "error")
    })
  })

  output$core_results_summary <- renderText({
    req(values$poly_results)
    summary_data <- values$poly_results$summary
    paste("Treatment Episodes:", nrow(values$episodes), "\n",
          "Polypharmacy Episodes:", summary_data$total_episodes, "\n",
          "Unique Patients:", summary_data$unique_patients, "\n",
          "Average Duration:", summary_data$avg_duration_days, "days\n",
          "Average Concurrent Drugs:", summary_data$avg_concurrent_drugs)
  })

  output$episodes_table <- DT::renderDataTable({
    req(values$episodes)
    DT::datatable(head(values$episodes, 500), options = list(scrollX = TRUE, pageLength = 10))
  })

  output$poly_episodes_table <- DT::renderDataTable({
    req(values$poly_results)
    DT::datatable(values$poly_results$episodes, options = list(scrollX = TRUE, pageLength = 10))
  })

  # Clinical Analysis
  observeEvent(input$run_clinical, {
    req(values$poly_results, cprd_data())

    output$clinical_progress <- renderText("Running clinical appropriateness analysis...")

    tryCatch({
      values$clinical_results <- calculate_clinical_polypharmacy(
        poly_episodes = values$poly_results$episodes,
        problem = cprd_data()$problem,
        show_tables = FALSE
      )

      output$clinical_progress <- renderText("Clinical analysis completed!")

    }, error = function(e) {
      output$clinical_progress <- renderText(paste("Error:", e$message))
    })
  })

  output$appropriateness_table <- DT::renderDataTable({
    req(values$clinical_results)
    DT::datatable(values$clinical_results$appropriateness_distribution,
                  options = list(pageLength = 10))
  })

  output$risk_distribution_table <- DT::renderDataTable({
    req(values$clinical_results)
    DT::datatable(values$clinical_results$risk_distribution,
                  options = list(pageLength = 10))
  })

  output$patient_clinical_table <- DT::renderDataTable({
    req(values$clinical_results)
    DT::datatable(head(values$clinical_results$patient_clinical_summary, 200),
                  options = list(scrollX = TRUE, pageLength = 10))
  })

  # Cost Analysis
  observeEvent(input$run_cost, {
    req(values$poly_results, values$processed_data)

    output$cost_progress <- renderText("Running economic impact analysis...")

    tryCatch({
      cost_data <- values$processed_data %>%
        select(patid, issuedate, estnhscost, prodcodeid)

      values$cost_results <- calculate_cost_polypharmacy(
        poly_episodes = values$poly_results$episodes,
        drug_cost_data = cost_data,
        show_tables = FALSE
      )

      output$cost_progress <- renderText("Cost analysis completed!")

    }, error = function(e) {
      output$cost_progress <- renderText(paste("Error:", e$message))
    })
  })

  output$cost_summary_stats <- renderText({
    req(values$cost_results)
    summary_data <- values$cost_results$cost_summary
    paste("Episodes Analyzed:", summary_data$total_episodes_with_cost, "\n",
          "Total Cost: £", round(summary_data$total_cost_analyzed, 2), "\n",
          "Average Episode Cost: £", summary_data$avg_episode_cost, "\n",
          "Average Daily Cost: £", summary_data$avg_daily_cost, "\n",
          "High Cost Episodes:", summary_data$high_cost_episodes)
  })

  output$cost_distribution_table <- DT::renderDataTable({
    req(values$cost_results)
    DT::datatable(values$cost_results$cost_distribution, options = list(pageLength = 10))
  })

  output$high_cost_episodes_table <- DT::renderDataTable({
    req(values$cost_results)
    DT::datatable(head(values$cost_results$high_cost_episodes, 100),
                  options = list(scrollX = TRUE, pageLength = 10))
  })

  # Demographics Analysis
  observeEvent(input$run_demographics, {
    req(values$poly_results, cprd_data())

    output$demo_progress <- renderText("Running demographic analysis...")

    tryCatch({
      values$demo_results <- analyze_demographic_polypharmacy(
        poly_episodes = values$poly_results$episodes,
        patient_data = cprd_data()$patient,
        show_tables = FALSE
      )

      output$demo_progress <- renderText("Demographic analysis completed!")

    }, error = function(e) {
      output$demo_progress <- renderText(paste("Error:", e$message))
    })
  })

  output$age_band_table <- DT::renderDataTable({
    req(values$demo_results)
    DT::datatable(values$demo_results$age_band_summary, options = list(pageLength = 10))
  })

  output$age_gender_table <- DT::renderDataTable({
    req(values$demo_results)
    DT::datatable(values$demo_results$age_gender_analysis, options = list(pageLength = 10))
  })

  output$elderly_analysis_table <- DT::renderDataTable({
    req(values$demo_results)
    DT::datatable(values$demo_results$elderly_analysis, options = list(pageLength = 10))
  })

  # Pattern Analysis
  output$patterns_progress <- renderText("")

  observeEvent(input$run_progression, {
    req(values$poly_results)
    output$patterns_progress <- renderText("Analyzing progression patterns...")

    tryCatch({
      values$progression_results <- analyze_polypharmacy_progression(
        poly_episodes = values$poly_results$episodes,
        show_tables = FALSE
      )
      output$patterns_progress <- renderText("Progression analysis completed!")
    }, error = function(e) {
      output$patterns_progress <- renderText(paste("Progression error:", e$message))
    })
  })

  observeEvent(input$run_seasonal, {
    req(values$poly_results)
    output$patterns_progress <- renderText("Analyzing seasonal patterns...")

    tryCatch({
      values$seasonal_results <- analyze_seasonal_patterns(
        poly_episodes = values$poly_results$episodes,
        show_tables = FALSE
      )
      output$patterns_progress <- renderText("Seasonal analysis completed!")
    }, error = function(e) {
      output$patterns_progress <- renderText(paste("Seasonal error:", e$message))
    })
  })

  observeEvent(input$run_practice, {
    req(values$poly_results, cprd_data())
    output$patterns_progress <- renderText("Analyzing practice variation...")

    tryCatch({
      values$practice_results <- analyze_practice_variation(
        poly_episodes = values$poly_results$episodes,
        practice_data = cprd_data()$practice,
        patient_data = cprd_data()$patient,
        show_tables = FALSE
      )
      output$patterns_progress <- renderText("Practice variation analysis completed!")
    }, error = function(e) {
      output$patterns_progress <- renderText(paste("Practice error:", e$message))
    })
  })

  output$progression_table <- DT::renderDataTable({
    req(values$progression_results)
    if (nrow(values$progression_results$progression_summary) > 0) {
      DT::datatable(head(values$progression_results$progression_summary, 100),
                    options = list(scrollX = TRUE, pageLength = 5))
    }
  })

  output$seasonal_table <- DT::renderDataTable({
    req(values$seasonal_results)
    DT::datatable(values$seasonal_results$seasonal_summary, options = list(pageLength = 5))
  })

  output$regional_table <- DT::renderDataTable({
    req(values$practice_results)
    DT::datatable(values$practice_results$regional_analysis,
                  options = list(scrollX = TRUE, pageLength = 5))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
