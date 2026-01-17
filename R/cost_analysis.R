#' Calculate Cost Analysis for Polypharmacy Episodes
#'
#' This function links polypharmacy episodes with prescription cost data to assess
#' the economic burden of polypharmacy, providing episode-level and patient-level
#' cost summaries with categorized cost intensity assessments.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_group, poly_start, poly_end, duration, avg_concurrent_drugs, max_concurrent_drugs
#' @param drug_cost_data Data frame containing prescription cost data with columns:
#'   patid, issuedate, estnhscost, prodcodeid
#' @param show_tables Logical. Display intermediate data tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{cost_episodes}{Episodes with cost metrics and categorizations}
#'   \item{patient_cost_summary}{Patient-level cost summaries}
#'   \item{cost_distribution}{Distribution of cost intensity vs episode categories}
#'   \item{high_cost_episodes}{Episodes flagged as high or very high cost}
#'   \item{cost_summary}{Overall cost analysis summary statistics}
#'
#' @examples
#' \dontrun{
#' # Prepare cost data from processed CPRD data
#' cost_data <- processed_data %>%
#'   select(patid, issuedate, estnhscost, prodcodeid)
#'
#' # Run cost analysis
#' cost_results <- calculate_cost_polypharmacy(
#'   poly_episodes = poly_results$episodes,
#'   drug_cost_data = cost_data
#' )
#' }
#'
#' @importFrom dplyr left_join select filter group_by summarise mutate count arrange desc case_when n
#' @export
calculate_cost_polypharmacy <- function(poly_episodes,
                                        drug_cost_data,
                                        show_tables = TRUE,
                                        n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  cat("=== POLYPHARMACY COST ANALYSIS ===\n")
  cat("Linking episodes with prescription cost data...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }
  if (!"data.frame" %in% class(drug_cost_data)) {
    stop("drug_cost_data must be a data frame")
  }

  # Check required columns - updated to match actual column names
  required_poly_cols <- c("patid", "poly_group", "poly_start", "poly_end", "duration",
                          "avg_concurrent_drugs", "max_concurrent_drugs")
  missing_poly_cols <- setdiff(required_poly_cols, colnames(poly_episodes))
  if (length(missing_poly_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_poly_cols, collapse = ", "))
  }

  required_cost_cols <- c("patid", "issuedate", "estnhscost", "prodcodeid")
  missing_cost_cols <- setdiff(required_cost_cols, colnames(drug_cost_data))
  if (length(missing_cost_cols) > 0) {
    stop("Missing required columns in drug_cost_data: ", paste(missing_cost_cols, collapse = ", "))
  }

  # Step 1: Link polypharmacy episodes with cost data
  cat("STEP 1: Linking polypharmacy episodes with prescription cost data...\n")

  episodes_with_cost <- poly_episodes %>%
    dplyr::left_join(
      drug_cost_data %>%
        dplyr::select(patid, issuedate, estnhscost, prodcodeid),
      by = "patid"
    ) %>%
    # Filter to prescriptions within polypharmacy episode periods
    dplyr::filter(issuedate >= poly_start & issuedate <= poly_end) %>%
    # Calculate cost metrics per episode
    dplyr::group_by(patid, poly_group, poly_start, poly_end, duration,
                    avg_concurrent_drugs, max_concurrent_drugs) %>%
    dplyr::summarise(
      total_episode_cost = sum(estnhscost, na.rm = TRUE),
      avg_cost_per_prescription = mean(estnhscost, na.rm = TRUE),
      max_single_prescription_cost = max(estnhscost, na.rm = TRUE),
      n_prescriptions_in_episode = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Cost per day metrics (handle division by zero)
      cost_per_day = ifelse(duration > 0, total_episode_cost / duration, 0),
      cost_per_drug = ifelse(avg_concurrent_drugs > 0, total_episode_cost / avg_concurrent_drugs, 0),

      # Cost intensity categories
      cost_intensity = dplyr::case_when(
        cost_per_day >= 5.0 ~ "Very High Cost",
        cost_per_day >= 2.0 ~ "High Cost",
        cost_per_day >= 1.0 ~ "Moderate Cost",
        cost_per_day >= 0.5 ~ "Low Cost",
        TRUE ~ "Very Low Cost"
      ),

      # Episode cost categories
      episode_cost_category = dplyr::case_when(
        total_episode_cost >= 200 ~ "Very Expensive Episode",
        total_episode_cost >= 100 ~ "Expensive Episode",
        total_episode_cost >= 50 ~ "Moderate Cost Episode",
        total_episode_cost >= 20 ~ "Low Cost Episode",
        TRUE ~ "Very Low Cost Episode"
      ),

      # Cost efficiency assessment
      cost_efficiency = dplyr::case_when(
        cost_per_drug <= 1.0 ~ "Efficient",
        cost_per_drug <= 2.5 ~ "Reasonable",
        cost_per_drug <= 5.0 ~ "Expensive",
        TRUE ~ "Very Expensive"
      )
    )

  cat("Linked", nrow(episodes_with_cost), "episodes with cost data\n")

  if (show_tables) {
    cat("\nEpisodes with Cost Data Preview:\n")
    preview_cols <- c("patid", "duration", "total_episode_cost", "cost_per_day",
                      "cost_intensity", "episode_cost_category")
    available_cols <- intersect(preview_cols, colnames(episodes_with_cost))
    print(head(episodes_with_cost[available_cols], n_preview))
    cat("...\n")
  }

  # Step 2: Patient-level cost summary
  cat("\nSTEP 2: Creating patient-level cost summaries...\n")

  patient_cost_summary <- episodes_with_cost %>%
    dplyr::group_by(patid) %>%
    dplyr::summarise(
      total_polypharmacy_cost = sum(total_episode_cost),
      avg_episode_cost = mean(total_episode_cost),
      avg_daily_poly_cost = mean(cost_per_day),
      most_expensive_episode = max(total_episode_cost),
      n_high_cost_episodes = sum(cost_intensity %in% c("High Cost", "Very High Cost")),
      predominant_cost_level = {
        cost_table <- table(cost_intensity)
        if (length(cost_table) > 0) {
          names(sort(cost_table, decreasing = TRUE))[1]
        } else {
          "Unknown"
        }
      },
      total_prescriptions = sum(n_prescriptions_in_episode),
      .groups = "drop"
    )

  cat("Generated cost summaries for", nrow(patient_cost_summary), "patients\n")

  if (show_tables) {
    cat("\nPatient Cost Summary Preview:\n")
    print(head(patient_cost_summary, n_preview))
    cat("...\n")
  }

  # Step 3: Cost distribution analysis
  cat("\nSTEP 3: Analyzing cost distributions...\n")

  cost_distribution <- episodes_with_cost %>%
    dplyr::count(cost_intensity, episode_cost_category) %>%
    dplyr::mutate(percentage = round(n / sum(n) * 100, 1))

  cat("Generated cost distribution analysis\n")

  # Step 4: High-cost episode identification
  cat("\nSTEP 4: Identifying high-cost episodes...\n")

  high_cost_episodes <- episodes_with_cost %>%
    dplyr::filter(cost_intensity %in% c("High Cost", "Very High Cost")) %>%
    dplyr::arrange(dplyr::desc(total_episode_cost)) %>%
    dplyr::select(patid, poly_group, poly_start, poly_end, duration,
                  avg_concurrent_drugs, total_episode_cost, cost_per_day,
                  cost_intensity, episode_cost_category)

  cat("Identified", nrow(high_cost_episodes), "high-cost episodes\n")

  # Step 5: Overall cost summary statistics
  cat("\nSTEP 5: Generating overall cost summary...\n")

  cost_summary <- episodes_with_cost %>%
    dplyr::summarise(
      total_episodes_with_cost = dplyr::n(),
      total_cost_analyzed = sum(total_episode_cost),
      avg_episode_cost = round(mean(total_episode_cost), 2),
      median_episode_cost = round(median(total_episode_cost), 2),
      avg_daily_cost = round(mean(cost_per_day), 2),
      high_cost_episodes = sum(cost_intensity %in% c("High Cost", "Very High Cost")),
      very_expensive_episodes = sum(episode_cost_category == "Very Expensive Episode"),
      avg_prescriptions_per_episode = round(mean(n_prescriptions_in_episode), 2)
    )

  cat("Generated overall cost summary statistics\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("COST ANALYSIS SUMMARY TABLES\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Cost Summary:\n")
    print(as.data.frame(cost_summary))

    cat("\nCost Distribution:\n")
    print(cost_distribution)

    cat("\nTop 10 Most Expensive Episodes:\n")
    print(head(high_cost_episodes, 10))

    cat("\nTop 5 Highest Cost Patients:\n")
    top_cost_patients <- patient_cost_summary %>%
      dplyr::arrange(dplyr::desc(total_polypharmacy_cost)) %>%
      head(5)
    print(top_cost_patients)
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("COST ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      cost_episodes = episodes_with_cost,
      patient_cost_summary = patient_cost_summary,
      cost_distribution = cost_distribution,
      high_cost_episodes = high_cost_episodes,
      cost_summary = cost_summary
    ),
    class = "cost_polypharmacy_analysis"
  )
}

#' Print method for cost polypharmacy analysis results
#'
#' @param x A cost_polypharmacy_analysis object
#' @param ... Additional arguments (unused)
#' @export
print.cost_polypharmacy_analysis <- function(x, ...) {
  cat("\nPolypharmacy Cost Analysis Results\n")
  cat("=====================================\n\n")

  if (x$cost_summary$total_episodes_with_cost == 0) {
    cat("No episodes with cost data found\n")
    return(invisible(x))
  }

  cat("Key Cost Statistics:\n")
  cat("• Total episodes analyzed:", x$cost_summary$total_episodes_with_cost, "\n")
  cat("• Total cost analyzed: £", round(x$cost_summary$total_cost_analyzed, 2), "\n")
  cat("• Average episode cost: £", x$cost_summary$avg_episode_cost, "\n")
  cat("• Average daily cost: £", x$cost_summary$avg_daily_cost, "\n")
  cat("• High-cost episodes:", x$cost_summary$high_cost_episodes, "\n\n")

  cat("Cost Intensity Distribution:\n")
  cost_dist <- x$cost_distribution %>%
    dplyr::group_by(cost_intensity) %>%
    dplyr::summarise(total_episodes = sum(n), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total_episodes))

  for(i in 1:nrow(cost_dist)) {
    cat("• ", cost_dist$cost_intensity[i], ": ",
        cost_dist$total_episodes[i], " episodes\n", sep="")
  }

  cat("\nUse summary(results) for detailed cost breakdowns\n")
  invisible(x)
}

#' Summary method for cost polypharmacy analysis results
#'
#' @param object A cost_polypharmacy_analysis object
#' @param ... Additional arguments (unused)
#' @export
summary.cost_polypharmacy_analysis <- function(object, ...) {
  cat("Detailed Cost Analysis Summary\n")
  cat("=================================\n\n")

  cat("Overall Statistics:\n")
  print(as.data.frame(object$cost_summary))

  if (nrow(object$patient_cost_summary) > 0) {
    cat("\nTop 10 Highest Cost Patients:\n")
    top_patients <- object$patient_cost_summary %>%
      dplyr::arrange(dplyr::desc(total_polypharmacy_cost)) %>%
      head(10)
    print(top_patients)

    cat("\nPatient Cost Statistics:\n")
    patient_stats <- object$patient_cost_summary %>%
      dplyr::summarise(
        avg_total_cost_per_patient = round(mean(total_polypharmacy_cost), 2),
        median_total_cost_per_patient = round(median(total_polypharmacy_cost), 2),
        max_patient_cost = round(max(total_polypharmacy_cost), 2),
        patients_with_high_cost_episodes = sum(n_high_cost_episodes > 0)
      )
    print(as.data.frame(patient_stats))
  }

  invisible(object)
}
