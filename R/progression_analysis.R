#' Analyze Polypharmacy Progression Patterns Over Time
#'
#' This function analyzes how polypharmacy patterns evolve over time for patients
#' with multiple episodes, identifying escalation, de-escalation, and stability patterns
#' in drug usage and episode characteristics.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_start, poly_end, duration, avg_concurrent_drugs, max_concurrent_drugs
#' @param show_tables Logical. Display summary tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{patient_trajectories}{Episodes with progression metrics and classifications}
#'   \item{progression_summary}{Patient-level progression summaries}
#'   \item{temporal_progression}{Time-based progression patterns}
#'   \item{progression_statistics}{Overall progression statistics}
#'
#' @examples
#' \dontrun{
#' progression_results <- analyze_polypharmacy_progression(
#'   poly_episodes = poly_results$episodes
#' )
#' }
#'
#' @importFrom dplyr arrange group_by mutate ungroup filter summarise lag case_when row_number n
#' @importFrom lubridate floor_date year
#' @export
analyze_polypharmacy_progression <- function(poly_episodes,
                                             show_tables = TRUE,
                                             n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed.")
  }

  cat("=== POLYPHARMACY PROGRESSION ANALYSIS ===\n")
  cat("Analyzing progression patterns across episodes...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }

  # Check required columns
  required_cols <- c("patid", "poly_start", "poly_end", "duration", "avg_concurrent_drugs")
  missing_cols <- setdiff(required_cols, colnames(poly_episodes))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_cols, collapse = ", "))
  }

  # Step 1: Calculate patient trajectories
  cat("STEP 1: Calculating patient trajectories and progression patterns...\n")

  patient_trajectories <- poly_episodes %>%
    dplyr::arrange(patid, poly_start) %>%
    dplyr::group_by(patid) %>%
    dplyr::mutate(
      episode_number = dplyr::row_number(),
      days_since_first_episode = as.numeric(poly_start - min(poly_start)),
      days_between_episodes = as.numeric(poly_start - dplyr::lag(poly_end)),

      # Progression patterns
      drug_count_change = avg_concurrent_drugs - dplyr::lag(avg_concurrent_drugs),
      duration_change = duration - dplyr::lag(duration),

      # Trajectory classification
      progression_pattern = dplyr::case_when(
        episode_number == 1 ~ "First Episode",
        drug_count_change > 1 ~ "Escalating",
        drug_count_change < -1 ~ "De-escalating",
        abs(drug_count_change) <= 1 ~ "Stable",
        TRUE ~ "Variable"
      )
    ) %>%
    dplyr::ungroup()

  n_multi_episode <- length(unique(patient_trajectories$patid[patient_trajectories$episode_number > 1]))
  cat("Analyzed trajectories for", length(unique(patient_trajectories$patid)), "patients\n")
  cat("Found", n_multi_episode, "patients with multiple episodes\n")

  if (show_tables) {
    cat("\nPatient Trajectories Preview:\n")
    preview_cols <- c("patid", "episode_number", "avg_concurrent_drugs",
                      "drug_count_change", "progression_pattern", "days_between_episodes")
    available_cols <- intersect(preview_cols, colnames(patient_trajectories))
    print(head(patient_trajectories[available_cols], n_preview))
    cat("...\n")
  }

  # Step 2: Overall progression summary per patient
  cat("\nSTEP 2: Generating patient-level progression summaries...\n")

  progression_summary <- patient_trajectories %>%
    dplyr::filter(episode_number > 1) %>%
    dplyr::group_by(patid) %>%
    dplyr::summarise(
      total_episodes = max(episode_number),
      total_progression_days = max(days_since_first_episode, na.rm = TRUE),
      avg_gap_between_episodes = round(mean(days_between_episodes, na.rm = TRUE), 1),
      net_drug_change = dplyr::last(avg_concurrent_drugs) - dplyr::first(avg_concurrent_drugs),
      escalating_episodes = sum(progression_pattern == "Escalating"),
      stable_episodes = sum(progression_pattern == "Stable"),
      deescalating_episodes = sum(progression_pattern == "De-escalating"),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      overall_trajectory = dplyr::case_when(
        net_drug_change >= 2 ~ "Overall Escalation",
        net_drug_change <= -2 ~ "Overall De-escalation",
        escalating_episodes > deescalating_episodes ~ "Mostly Escalating",
        deescalating_episodes > escalating_episodes ~ "Mostly De-escalating",
        TRUE ~ "Stable Pattern"
      )
    )

  cat("Generated progression summaries for", nrow(progression_summary), "patients with multiple episodes\n")

  if (show_tables && nrow(progression_summary) > 0) {
    cat("\nProgression Summary Preview:\n")
    print(head(progression_summary, n_preview))
    cat("...\n")
  }

  # Step 3: Time-based progression patterns
  cat("\nSTEP 3: Analyzing temporal progression patterns...\n")

  temporal_progression <- patient_trajectories %>%
    dplyr::mutate(
      year_month = lubridate::floor_date(poly_start, "month"),
      study_year = lubridate::year(poly_start)
    ) %>%
    dplyr::group_by(year_month) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs), 2),
      avg_duration = round(mean(duration), 1),
      .groups = "drop"
    )

  cat("Generated temporal progression analysis for", nrow(temporal_progression), "time periods\n")

  # Step 4: Overall progression statistics
  cat("\nSTEP 4: Calculating overall progression statistics...\n")

  progression_statistics <- list()

  # Pattern distribution
  if (nrow(progression_summary) > 0) {
    pattern_dist <- table(progression_summary$overall_trajectory)
    progression_statistics$pattern_distribution <- data.frame(
      trajectory_pattern = names(pattern_dist),
      n_patients = as.numeric(pattern_dist),
      percentage = round(as.numeric(pattern_dist) / sum(pattern_dist) * 100, 1)
    )

    # Overall statistics
    progression_statistics$overall_stats <- data.frame(
      patients_with_multiple_episodes = nrow(progression_summary),
      avg_episodes_per_patient = round(mean(progression_summary$total_episodes), 2),
      avg_progression_period_days = round(mean(progression_summary$total_progression_days, na.rm = TRUE), 1),
      avg_gap_between_episodes = round(mean(progression_summary$avg_gap_between_episodes, na.rm = TRUE), 1),
      patients_escalating = sum(progression_summary$overall_trajectory %in% c("Overall Escalation", "Mostly Escalating")),
      patients_deescalating = sum(progression_summary$overall_trajectory %in% c("Overall De-escalation", "Mostly De-escalating")),
      patients_stable = sum(progression_summary$overall_trajectory == "Stable Pattern")
    )
  } else {
    progression_statistics$pattern_distribution <- data.frame()
    progression_statistics$overall_stats <- data.frame(
      patients_with_multiple_episodes = 0,
      note = "No patients with multiple episodes found"
    )
  }

  cat("Generated overall progression statistics\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("PROGRESSION ANALYSIS SUMMARY\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Progression Statistics:\n")
    print(progression_statistics$overall_stats)

    if (nrow(progression_statistics$pattern_distribution) > 0) {
      cat("\nProgression Pattern Distribution:\n")
      print(progression_statistics$pattern_distribution)

      cat("\nTop 10 Patients by Episode Count:\n")
      top_patients <- progression_summary %>%
        dplyr::arrange(desc(total_episodes)) %>%
        head(10)
      print(top_patients)
    }
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("PROGRESSION ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      patient_trajectories = patient_trajectories,
      progression_summary = progression_summary,
      temporal_progression = temporal_progression,
      progression_statistics = progression_statistics
    ),
    class = "progression_polypharmacy_analysis"
  )
}
