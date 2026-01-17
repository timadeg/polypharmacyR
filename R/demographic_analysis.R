#' Analyze Polypharmacy Episodes by Demographics (Age and Gender)
#'
#' This function analyzes polypharmacy episodes across different demographic groups,
#' focusing on age bands and gender patterns with particular attention to elderly populations.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_start, poly_end, duration, avg_concurrent_drugs, max_concurrent_drugs
#' @param patient_data Data frame containing patient demographic data with columns:
#'   patid, yob (year of birth), gender
#' @param current_year Integer. Current year for age calculations (default: 2025)
#' @param show_tables Logical. Display summary tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{age_episodes}{Episodes with calculated age and demographic categories}
#'   \item{age_band_summary}{Summary statistics by age bands}
#'   \item{elderly_analysis}{Detailed analysis of elderly patients (65+)}
#'   \item{age_gender_analysis}{Cross-tabulation of age bands and gender}
#'   \item{patient_demographic_summary}{Patient-level demographic summaries}
#'   \item{demographic_summary}{Overall demographic statistics}
#'
#' @examples
#' \dontrun{
#' demo_results <- analyze_demographic_polypharmacy(
#'   poly_episodes = poly_results$episodes,
#'   patient_data = cprd_data$patient
#' )
#' }
#'
#' @importFrom dplyr left_join select mutate group_by summarise case_when n n_distinct filter
#' @importFrom lubridate year
#' @export
analyze_demographic_polypharmacy <- function(poly_episodes,
                                             patient_data,
                                             current_year = 2025,
                                             show_tables = TRUE,
                                             n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed.")
  }

  cat("=== DEMOGRAPHIC POLYPHARMACY ANALYSIS ===\n")
  cat("Analyzing polypharmacy episodes by age and gender...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }
  if (!"data.frame" %in% class(patient_data)) {
    stop("patient_data must be a data frame")
  }

  # Check required columns
  required_poly_cols <- c("patid", "poly_start", "duration", "avg_concurrent_drugs", "max_concurrent_drugs")
  missing_poly_cols <- setdiff(required_poly_cols, colnames(poly_episodes))
  if (length(missing_poly_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_poly_cols, collapse = ", "))
  }

  required_patient_cols <- c("patid", "yob", "gender")
  missing_patient_cols <- setdiff(required_patient_cols, colnames(patient_data))
  if (length(missing_patient_cols) > 0) {
    stop("Missing required columns in patient_data: ", paste(missing_patient_cols, collapse = ", "))
  }

  # Step 1: Join episodes with demographic data
  cat("STEP 1: Linking episodes with demographic data...\n")

  poly_with_demographics <- poly_episodes %>%
    dplyr::left_join(patient_data %>% dplyr::select(patid, yob, gender), by = "patid") %>%
    dplyr::mutate(
      # Calculate age at start of polypharmacy episode
      age_at_episode = lubridate::year(poly_start) - yob,

      # UK-focused age bands
      age_band = dplyr::case_when(
        age_at_episode < 18 ~ "Under 18",
        age_at_episode < 40 ~ "18-39 (Young Adults)",
        age_at_episode < 65 ~ "40-64 (Middle Age)",
        age_at_episode < 80 ~ "65-79 (Elderly)",
        age_at_episode >= 80 ~ "80+ (Very Elderly)",
        TRUE ~ "Unknown Age"
      ),

      # Elderly focus categories
      elderly_category = dplyr::case_when(
        age_at_episode < 65 ~ "Non-Elderly",
        age_at_episode < 75 ~ "Young Elderly (65-74)",
        age_at_episode < 85 ~ "Old Elderly (75-84)",
        age_at_episode >= 85 ~ "Very Old (85+)",
        TRUE ~ "Unknown"
      ),

      # Gender categories
      gender_desc = dplyr::case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        gender == 3 ~ "Indeterminate",
        gender == 4 ~ "Unknown",
        TRUE ~ "Missing"
      )
    )

  n_matched <- sum(!is.na(poly_with_demographics$yob))
  cat("Linked", nrow(poly_with_demographics), "episodes with demographic data\n")
  cat("Successfully matched", n_matched, "episodes with patient demographics\n")

  if (show_tables) {
    cat("\nDemographic Episodes Preview:\n")
    preview_cols <- c("patid", "age_at_episode", "age_band", "gender_desc",
                      "avg_concurrent_drugs", "duration")
    available_cols <- intersect(preview_cols, colnames(poly_with_demographics))
    print(head(poly_with_demographics[available_cols], n_preview))
    cat("...\n")
  }

  # Step 2: Age band analysis
  cat("\nSTEP 2: Analyzing patterns by age bands...\n")

  age_band_summary <- poly_with_demographics %>%
    dplyr::group_by(age_band) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_episode_duration = round(mean(duration, na.rm = TRUE), 1),
      max_concurrent_drugs = max(max_concurrent_drugs, na.rm = TRUE),
      episodes_per_patient = round(dplyr::n() / dplyr::n_distinct(patid), 2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      percentage_episodes = round(n_episodes / sum(n_episodes) * 100, 1),
      percentage_patients = round(n_patients / sum(n_patients) * 100, 1)
    )

  cat("Generated age band analysis for", nrow(age_band_summary), "age groups\n")

  if (show_tables) {
    cat("\nAge Band Summary:\n")
    print(age_band_summary)
    cat("...\n")
  }

  # Step 3: Elderly-focused analysis
  cat("\nSTEP 3: Detailed analysis of elderly patients (65+)...\n")

  elderly_analysis <- poly_with_demographics %>%
    dplyr::filter(age_at_episode >= 65) %>%
    dplyr::group_by(elderly_category) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_episode_duration = round(mean(duration, na.rm = TRUE), 1),
      high_drug_episodes = sum(avg_concurrent_drugs >= 10),
      very_long_episodes = sum(duration >= 180),
      .groups = "drop"
    )

  n_elderly <- sum(elderly_analysis$n_episodes)
  cat("Analyzed", n_elderly, "episodes in elderly patients (65+)\n")

  if (show_tables && nrow(elderly_analysis) > 0) {
    cat("\nElderly Analysis:\n")
    print(elderly_analysis)
    cat("...\n")
  }

  # Step 4: Age-gender interaction analysis
  cat("\nSTEP 4: Analyzing age-gender interactions...\n")

  age_gender_analysis <- poly_with_demographics %>%
    dplyr::filter(gender %in% c(1, 2)) %>%  # Focus on Male/Female
    dplyr::group_by(age_band, gender_desc) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_duration = round(mean(duration, na.rm = TRUE), 1),
      .groups = "drop"
    )

  cat("Generated age-gender interaction analysis\n")

  if (show_tables) {
    cat("\nAge-Gender Analysis:\n")
    print(age_gender_analysis)
    cat("...\n")
  }

  # Step 5: Patient-level demographic summary
  cat("\nSTEP 5: Creating patient-level demographic summaries...\n")

  patient_demographic_summary <- poly_with_demographics %>%
    dplyr::group_by(patid, age_band, gender_desc) %>%
    dplyr::summarise(
      age_at_first_episode = min(age_at_episode, na.rm = TRUE),
      n_episodes = dplyr::n(),
      total_polypharmacy_days = sum(duration),
      max_concurrent_drugs_ever = max(max_concurrent_drugs),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs), 2),
      .groups = "drop"
    )

  cat("Generated summaries for", nrow(patient_demographic_summary), "patients\n")

  # Step 6: Overall demographic summary
  cat("\nSTEP 6: Generating overall demographic summary...\n")

  demographic_summary <- poly_with_demographics %>%
    dplyr::summarise(
      total_episodes_analyzed = dplyr::n(),
      unique_patients = dplyr::n_distinct(patid),
      avg_age_at_episode = round(mean(age_at_episode, na.rm = TRUE), 1),
      median_age_at_episode = round(median(age_at_episode, na.rm = TRUE), 1),
      elderly_episodes = sum(age_at_episode >= 65, na.rm = TRUE),
      very_elderly_episodes = sum(age_at_episode >= 80, na.rm = TRUE),
      female_episodes = sum(gender == 2, na.rm = TRUE),
      male_episodes = sum(gender == 1, na.rm = TRUE),
      elderly_percentage = round(sum(age_at_episode >= 65, na.rm = TRUE) / dplyr::n() * 100, 1)
    )

  cat("Generated overall demographic summary statistics\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("DEMOGRAPHIC SUMMARY TABLES\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Demographic Summary:\n")
    print(as.data.frame(demographic_summary))

    cat("\nTop 10 Patients by Episode Count:\n")
    top_patients <- patient_demographic_summary %>%
      dplyr::arrange(desc(n_episodes)) %>%
      head(10)
    print(top_patients)
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("DEMOGRAPHIC ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      age_episodes = poly_with_demographics,
      age_band_summary = age_band_summary,
      elderly_analysis = elderly_analysis,
      age_gender_analysis = age_gender_analysis,
      patient_demographic_summary = patient_demographic_summary,
      demographic_summary = demographic_summary
    ),
    class = "demographic_polypharmacy_analysis"
  )
}
