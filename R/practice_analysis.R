#' Analyze Practice and Regional Variation in Polypharmacy Patterns
#'
#' This function analyzes variation in polypharmacy patterns across GP practices
#' and UK regions, identifying geographic patterns and practice-level differences
#' in prescribing behavior and polypharmacy management.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_start, poly_end, duration, avg_concurrent_drugs, max_concurrent_drugs
#' @param practice_data Data frame containing practice information with columns:
#'   pracid, region
#' @param patient_data Data frame containing patient data with columns:
#'   patid, pracid
#' @param show_tables Logical. Display summary tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{practice_episodes}{Episodes with practice and regional information}
#'   \item{practice_summary}{Practice-level polypharmacy summaries}
#'   \item{regional_analysis}{Regional variation analysis}
#'   \item{geographic_patterns}{Geographic polypharmacy patterns}
#'   \item{practice_ranking}{Practice rankings within regions}
#'   \item{variation_statistics}{Overall variation statistics}
#'
#' @examples
#' \dontrun{
#' practice_results <- analyze_practice_variation(
#'   poly_episodes = poly_results$episodes,
#'   practice_data = cprd_data$practice,
#'   patient_data = cprd_data$patient
#' )
#' }
#'
#' @importFrom dplyr left_join select group_by summarise mutate case_when n n_distinct arrange desc ungroup
#' @export
analyze_practice_variation <- function(poly_episodes,
                                       practice_data,
                                       patient_data,
                                       show_tables = TRUE,
                                       n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  cat("=== PRACTICE VARIATION ANALYSIS ===\n")
  cat("Analyzing geographic and practice-level polypharmacy patterns...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }
  if (!"data.frame" %in% class(practice_data)) {
    stop("practice_data must be a data frame")
  }
  if (!"data.frame" %in% class(patient_data)) {
    stop("patient_data must be a data frame")
  }

  # Check required columns
  required_poly_cols <- c("patid", "duration", "avg_concurrent_drugs", "max_concurrent_drugs")
  missing_poly_cols <- setdiff(required_poly_cols, colnames(poly_episodes))
  if (length(missing_poly_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_poly_cols, collapse = ", "))
  }

  required_practice_cols <- c("pracid", "region")
  missing_practice_cols <- setdiff(required_practice_cols, colnames(practice_data))
  if (length(missing_practice_cols) > 0) {
    stop("Missing required columns in practice_data: ", paste(missing_practice_cols, collapse = ", "))
  }

  required_patient_cols <- c("patid", "pracid")
  missing_patient_cols <- setdiff(required_patient_cols, colnames(patient_data))
  if (length(missing_patient_cols) > 0) {
    stop("Missing required columns in patient_data: ", paste(missing_patient_cols, collapse = ", "))
  }

  # Step 1: Set up regional mapping
  cat("STEP 1: Setting up regional classifications...\n")

  # Hard-coded region lookup for UK regions
  region_lookup <- data.frame(
    region = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
    region_name = c(
      "North East", "North West", "Yorkshire And The Humber",
      "East Midlands", "West Midlands", "East of England",
      "South West", "South Central", "London", "South East Coast",
      "Northern Ireland", "Scotland", "Wales"
    )
  )

  # Add region names to practice data
  practice_enhanced <- practice_data %>%
    dplyr::left_join(region_lookup, by = "region")

  cat("Enhanced practice data with regional classifications\n")

  # Step 2: Link episodes with practice and regional data
  cat("\nSTEP 2: Linking episodes with practice and regional information...\n")

  episodes_with_practice <- poly_episodes %>%
    dplyr::left_join(patient_data %>% dplyr::select(patid, pracid), by = "patid") %>%
    dplyr::left_join(practice_enhanced %>% dplyr::select(pracid, region, region_name), by = "pracid")

  n_matched <- sum(!is.na(episodes_with_practice$pracid))
  cat("Linked", nrow(episodes_with_practice), "episodes with practice data\n")
  cat("Successfully matched", n_matched, "episodes with practice information\n")

  if (show_tables) {
    cat("\nEpisodes with Practice Data Preview:\n")
    preview_cols <- c("patid", "pracid", "region_name", "avg_concurrent_drugs", "duration")
    available_cols <- intersect(preview_cols, colnames(episodes_with_practice))
    print(head(episodes_with_practice[available_cols], n_preview))
    cat("...\n")
  }

  # Step 3: Practice-level polypharmacy analysis
  cat("\nSTEP 3: Analyzing practice-level polypharmacy patterns...\n")

  practice_summary <- episodes_with_practice %>%
    dplyr::group_by(pracid, region, region_name) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_episode_duration = round(mean(duration, na.rm = TRUE), 1),
      max_concurrent_drugs = max(max_concurrent_drugs, na.rm = TRUE),
      high_complexity_episodes = sum(avg_concurrent_drugs >= 8),
      very_long_episodes = sum(duration >= 180),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      episodes_per_patient = round(n_episodes / n_patients, 2),
      high_complexity_rate = round(high_complexity_episodes / n_episodes * 100, 1)
    )

  cat("Generated analysis for", nrow(practice_summary), "practices\n")

  if (show_tables) {
    cat("\nPractice Summary Preview:\n")
    print(head(practice_summary, n_preview))
    cat("...\n")
  }

  # Step 4: Regional variation analysis
  cat("\nSTEP 4: Analyzing regional variation patterns...\n")

  regional_analysis <- episodes_with_practice %>%
    dplyr::group_by(region, region_name) %>%
    dplyr::summarise(
      n_practices = dplyr::n_distinct(pracid),
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_episode_duration = round(mean(duration, na.rm = TRUE), 1),
      high_complexity_rate = round(sum(avg_concurrent_drugs >= 8) / dplyr::n() * 100, 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      episodes_per_practice = round(n_episodes / n_practices, 1),
      patients_per_practice = round(n_patients / n_practices, 1)
    ) %>%
    dplyr::arrange(dplyr::desc(avg_concurrent_drugs))

  cat("Generated regional analysis for", nrow(regional_analysis), "regions\n")

  # Step 5: Geographic polypharmacy patterns
  cat("\nSTEP 5: Classifying geographic polypharmacy patterns...\n")

  geographic_patterns <- regional_analysis %>%
    dplyr::mutate(
      polypharmacy_level = dplyr::case_when(
        avg_concurrent_drugs >= 7.0 ~ "Very High",
        avg_concurrent_drugs >= 6.0 ~ "High",
        avg_concurrent_drugs >= 5.5 ~ "Above Average",
        avg_concurrent_drugs >= 5.0 ~ "Average",
        TRUE ~ "Below Average"
      )
    ) %>%
    dplyr::arrange(dplyr::desc(avg_concurrent_drugs))

  cat("Classified geographic patterns for all regions\n")

  # Step 6: Practice performance ranking within regions
  cat("\nSTEP 6: Ranking practice performance within regions...\n")

  practice_ranking <- practice_summary %>%
    dplyr::group_by(region_name) %>%
    dplyr::mutate(
      regional_rank = rank(-avg_concurrent_drugs),
      within_region_percentile = round((rank(-avg_concurrent_drugs) / dplyr::n()) * 100, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_name, dplyr::desc(avg_concurrent_drugs))

  cat("Generated practice rankings within regions\n")

  # Step 7: Overall variation statistics
  cat("\nSTEP 7: Calculating variation statistics...\n")

  variation_statistics <- list()

  # Overall statistics
  variation_statistics$overall_stats <- data.frame(
    total_practices_analyzed = nrow(practice_summary),
    total_regions = length(unique(regional_analysis$region_name)),
    overall_avg_concurrent_drugs = round(mean(practice_summary$avg_concurrent_drugs), 2),
    practice_variation_sd = round(sd(practice_summary$avg_concurrent_drugs), 2),
    regional_variation_sd = round(sd(regional_analysis$avg_concurrent_drugs), 2)
  )

  # Regional rankings
  variation_statistics$regional_rankings <- regional_analysis %>%
    dplyr::select(region_name, avg_concurrent_drugs, high_complexity_rate, n_practices) %>%
    dplyr::arrange(dplyr::desc(avg_concurrent_drugs))

  # Practice distribution by polypharmacy level
  if (nrow(practice_summary) > 0) {
    practice_levels <- practice_summary %>%
      dplyr::mutate(
        practice_poly_level = dplyr::case_when(
          avg_concurrent_drugs >= 7.0 ~ "Very High",
          avg_concurrent_drugs >= 6.0 ~ "High",
          avg_concurrent_drugs >= 5.5 ~ "Above Average",
          avg_concurrent_drugs >= 5.0 ~ "Average",
          TRUE ~ "Below Average"
        )
      )

    variation_statistics$practice_distribution <- table(practice_levels$practice_poly_level) %>%
      as.data.frame() %>%
      setNames(c("polypharmacy_level", "n_practices")) %>%
      dplyr::mutate(percentage = round(n_practices / sum(n_practices) * 100, 1))
  }

  cat("Generated overall variation statistics\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("PRACTICE VARIATION SUMMARY\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Variation Statistics:\n")
    print(variation_statistics$overall_stats)

    cat("\nRegional Rankings (by average concurrent drugs):\n")
    print(head(variation_statistics$regional_rankings, 10))

    cat("\nGeographic Polypharmacy Patterns:\n")
    print(geographic_patterns)

    if (!is.null(variation_statistics$practice_distribution)) {
      cat("\nPractice Distribution by Polypharmacy Level:\n")
      print(variation_statistics$practice_distribution)
    }

    cat("\nTop 10 Practices by Average Concurrent Drugs:\n")
    top_practices <- practice_summary %>%
      dplyr::arrange(dplyr::desc(avg_concurrent_drugs)) %>%
      head(10) %>%
      dplyr::select(pracid, region_name, avg_concurrent_drugs, n_patients, high_complexity_rate)
    print(top_practices)
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("PRACTICE VARIATION ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      practice_episodes = episodes_with_practice,
      practice_summary = practice_summary,
      regional_analysis = regional_analysis,
      geographic_patterns = geographic_patterns,
      practice_ranking = practice_ranking,
      variation_statistics = variation_statistics
    ),
    class = "practice_polypharmacy_analysis"
  )
}
