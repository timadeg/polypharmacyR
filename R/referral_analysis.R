#' Analyze Referral Patterns in Polypharmacy Patients
#'
#' This function analyzes referral patterns for patients with polypharmacy episodes,
#' examining referral urgency, frequency, and relationships between polypharmacy
#' complexity and healthcare utilization patterns.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_start, poly_end, duration, avg_concurrent_drugs
#' @param referral_data Data frame containing referral data with columns:
#'   patid, obsid, pracid, refurgencyid
#' @param show_tables Logical. Display summary tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{patient_referral_profile}{Patient-level referral summaries}
#'   \item{polypharmacy_referral_analysis}{Referrals linked to polypharmacy episodes}
#'   \item{urgency_distribution}{Distribution of referral urgency levels}
#'   \item{complexity_referral_relationship}{Relationship between drug complexity and referrals}
#'   \item{referral_statistics}{Overall referral statistics}
#'
#' @examples
#' \dontrun{
#' referral_results <- analyze_referral_patterns(
#'   poly_episodes = poly_results$episodes,
#'   referral_data = cprd_data$referral
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarise left_join mutate case_when n n_distinct
#' @export
analyze_referral_patterns <- function(poly_episodes,
                                      referral_data,
                                      show_tables = TRUE,
                                      n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  cat("=== REFERRAL PATTERN ANALYSIS ===\n")
  cat("Analyzing referral patterns in polypharmacy patients...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }
  if (!"data.frame" %in% class(referral_data)) {
    stop("referral_data must be a data frame")
  }

  # Check required columns
  required_poly_cols <- c("patid", "poly_start", "poly_end", "avg_concurrent_drugs")
  missing_poly_cols <- setdiff(required_poly_cols, colnames(poly_episodes))
  if (length(missing_poly_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_poly_cols, collapse = ", "))
  }

  required_referral_cols <- c("patid", "refurgencyid")
  missing_referral_cols <- setdiff(required_referral_cols, colnames(referral_data))
  if (length(missing_referral_cols) > 0) {
    stop("Missing required columns in referral_data: ", paste(missing_referral_cols, collapse = ", "))
  }

  # Step 1: Create patient referral profile
  cat("STEP 1: Creating patient referral profiles...\n")

  # Enhanced referral profile using the urgency dictionary
  patient_referral_profile <- referral_data %>%
    dplyr::filter(!is.na(refurgencyid)) %>%  # Only complete urgency data
    dplyr::mutate(
      urgency_description = dplyr::case_when(
        refurgencyid == 1 ~ "2 Week Wait",
        refurgencyid == 2 ~ "Urgent",
        refurgencyid == 3 ~ "Soon",
        refurgencyid == 4 ~ "Routine",
        refurgencyid == 5 ~ "Dated",
        TRUE ~ "Unknown"
      ),
      # Group into clinical priority categories
      priority_category = dplyr::case_when(
        refurgencyid %in% c(1, 2) ~ "High Priority",  # 2 Week Wait + Urgent
        refurgencyid == 3 ~ "Medium Priority",         # Soon
        refurgencyid %in% c(4, 5) ~ "Low Priority",   # Routine + Dated
        TRUE ~ "Unknown Priority"
      )
    ) %>%
    dplyr::group_by(patid) %>%
    dplyr::summarise(
      n_referrals = dplyr::n(),
      n_two_week_wait = sum(refurgencyid == 1, na.rm = TRUE),
      n_urgent_referrals = sum(refurgencyid == 2, na.rm = TRUE),
      n_soon_referrals = sum(refurgencyid == 3, na.rm = TRUE),
      n_routine_referrals = sum(refurgencyid == 4, na.rm = TRUE),
      n_dated_referrals = sum(refurgencyid == 5, na.rm = TRUE),
      n_high_priority = sum(priority_category == "High Priority", na.rm = TRUE),
      n_medium_priority = sum(priority_category == "Medium Priority", na.rm = TRUE),
      n_low_priority = sum(priority_category == "Low Priority", na.rm = TRUE),
      high_priority_rate = round(n_high_priority / n_referrals * 100, 1),
      urgent_referral_rate = round(n_urgent_referrals / n_referrals * 100, 1),
      .groups = "drop"
    )

  cat("Created referral profiles for", nrow(patient_referral_profile), "patients\n")

  if (show_tables) {
    cat("\nPatient Referral Profile Preview:\n")
    print(head(patient_referral_profile, n_preview))
    cat("...\n")
  }

  # Step 2: Link referrals with polypharmacy episodes
  cat("\nSTEP 2: Linking referrals with polypharmacy episodes...\n")

  polypharmacy_referral_analysis <- poly_episodes %>%
    dplyr::left_join(patient_referral_profile, by = "patid") %>%
    dplyr::mutate(
      # Handle patients with no referral data
      n_referrals = ifelse(is.na(n_referrals), 0, n_referrals),
      n_high_priority = ifelse(is.na(n_high_priority), 0, n_high_priority),
      high_priority_rate = ifelse(is.na(high_priority_rate), 0, high_priority_rate),

      # Create polypharmacy complexity categories
      complexity_category = dplyr::case_when(
        avg_concurrent_drugs >= 10 ~ "Very High Complexity",
        avg_concurrent_drugs >= 8 ~ "High Complexity",
        avg_concurrent_drugs >= 6 ~ "Moderate Complexity",
        TRUE ~ "Lower Complexity"
      ),

      # Referral intensity categories
      referral_intensity = dplyr::case_when(
        n_referrals == 0 ~ "No Referrals",
        n_referrals <= 2 ~ "Low Referral Use",
        n_referrals <= 5 ~ "Moderate Referral Use",
        TRUE ~ "High Referral Use"
      )
    )

  n_with_referrals <- sum(polypharmacy_referral_analysis$n_referrals > 0)
  cat("Linked", nrow(polypharmacy_referral_analysis), "polypharmacy episodes\n")
  cat("Found", n_with_referrals, "episodes in patients with referral data\n")

  # Step 3: Urgency distribution analysis
  cat("\nSTEP 3: Analyzing referral urgency distribution...\n")

  urgency_distribution <- referral_data %>%
    dplyr::filter(!is.na(refurgencyid)) %>%
    dplyr::mutate(
      urgency_description = dplyr::case_when(
        refurgencyid == 1 ~ "2 Week Wait",
        refurgencyid == 2 ~ "Urgent",
        refurgencyid == 3 ~ "Soon",
        refurgencyid == 4 ~ "Routine",
        refurgencyid == 5 ~ "Dated",
        TRUE ~ "Unknown"
      )
    ) %>%
    dplyr::group_by(refurgencyid, urgency_description) %>%
    dplyr::summarise(
      n_referrals = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      percentage_referrals = round(n_referrals / sum(n_referrals) * 100, 1)
    ) %>%
    dplyr::arrange(refurgencyid)

  cat("Generated urgency distribution analysis\n")

  # Step 4: Complexity-referral relationship
  cat("\nSTEP 4: Analyzing relationship between drug complexity and referrals...\n")

  complexity_referral_relationship <- polypharmacy_referral_analysis %>%
    dplyr::group_by(complexity_category) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      patients_with_referrals = sum(n_referrals > 0),
      avg_referrals_per_patient = round(mean(n_referrals, na.rm = TRUE), 2),
      avg_high_priority_rate = round(mean(high_priority_rate, na.rm = TRUE), 1),
      patients_with_high_priority = sum(n_high_priority > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      referral_patient_percentage = round(patients_with_referrals / n_patients * 100, 1)
    )

  cat("Generated complexity-referral relationship analysis\n")

  # Step 5: Overall referral statistics
  cat("\nSTEP 5: Calculating overall referral statistics...\n")

  referral_statistics <- list()

  # Overall statistics
  referral_statistics$overall_stats <- data.frame(
    total_polypharmacy_patients = length(unique(poly_episodes$patid)),
    patients_with_referrals = nrow(patient_referral_profile),
    total_referrals = sum(patient_referral_profile$n_referrals),
    patients_with_high_priority = sum(patient_referral_profile$n_high_priority > 0),
    avg_referrals_per_patient = round(mean(patient_referral_profile$n_referrals), 2),
    referral_coverage_rate = round(nrow(patient_referral_profile) / length(unique(poly_episodes$patid)) * 100, 1)
  )

  # Priority breakdown
  referral_statistics$priority_breakdown <- data.frame(
    two_week_wait = sum(patient_referral_profile$n_two_week_wait),
    urgent = sum(patient_referral_profile$n_urgent_referrals),
    soon = sum(patient_referral_profile$n_soon_referrals),
    routine = sum(patient_referral_profile$n_routine_referrals),
    dated = sum(patient_referral_profile$n_dated_referrals)
  )

  cat("Generated overall referral statistics\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("REFERRAL ANALYSIS SUMMARY\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Referral Statistics:\n")
    print(referral_statistics$overall_stats)

    cat("\nReferral Priority Breakdown:\n")
    print(referral_statistics$priority_breakdown)

    cat("\nUrgency Distribution:\n")
    print(urgency_distribution)

    cat("\nComplexity-Referral Relationship:\n")
    print(complexity_referral_relationship)

    cat("\nTop 10 Patients by Referral Count:\n")
    top_referral_patients <- patient_referral_profile %>%
      dplyr::arrange(desc(n_referrals)) %>%
      head(10)
    print(top_referral_patients)
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("REFERRAL ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      patient_referral_profile = patient_referral_profile,
      polypharmacy_referral_analysis = polypharmacy_referral_analysis,
      urgency_distribution = urgency_distribution,
      complexity_referral_relationship = complexity_referral_relationship,
      referral_statistics = referral_statistics
    ),
    class = "referral_polypharmacy_analysis"
  )
}
