#' Test Script for polypharmacyR Package
#'
#' This file tests the main package functions with sample data
#' Remove this file before final package build

# Source all package functions
source("R/calculate_polypharmacy_cprd.R")
source("R/process_cprd_data.R")
source("R/detect_treatment_episodes.R")
source("R/calculate_daily_polypharmacy.R")
source("R/detect_polypharmacy_episodes.R")
source("R/add_problem_context.R")


# Test the package functions
test_polypharmacy_package <- function() {

  # Load required libraries
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(data.table)

  # Load your CPRD data (adjust file paths as needed)
  cat("Loading CPRD data...\n")
  drug_issue <- read.csv("inst/extdata/DrugIssue.csv")
  patient <- read.csv("inst/extdata/Patient.csv")
  practice <- read.csv("inst/extdata/Practice.csv")
  staff <- read.csv("inst/extdata/Staff.csv")
  problem <- read.csv("inst/extdata/Problem.csv")

  # Test the main function
  cat("Testing main polypharmacy function...\n")
  results <- calculate_polypharmacy_cprd(
    drug_issue_data = drug_issue,
    patient_data = patient,
    practice_data = practice,
    staff_data = staff,
    problem_data = problem,
    grace_period_days = 30,
    default_duration_days = 28,
    polypharmacy_threshold = 5
  )

  # Display results summary
  cat("\n=== PACKAGE TEST RESULTS ===\n")
  print(results$summary)

  cat("\nPolypharmacy Episodes (first 10):\n")
  print(head(results$polypharmacy_episodes, 10))

  cat("\nDaily Polypharmacy (first 10):\n")
  print(head(results$daily_polypharmacy, 10))

  cat("\nTreatment Episodes (first 10):\n")
  print(head(results$treatment_episodes, 10))

  cat("\n=== TEST COMPLETE ===\n")

  return(results)
}

