#' Calculate Clinical Polypharmacy Analysis
#'
#' This function assesses the clinical appropriateness of polypharmacy episodes
#' by linking them with patient problem/diagnosis data to evaluate whether
#' the medication burden is justified by clinical complexity.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_start, poly_end, duration, avg_concurrent_drugs, max_concurrent_drugs
#' @param problem Data frame containing patient problem/diagnosis data from CPRD
#'   with columns: patid, signid, probstatusid, expduration, probenddate, parentprobrelid
#' @param show_tables Logical. Display intermediate data tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{clinical_episodes}{Episodes with clinical appropriateness assessments}
#'   \item{patient_clinical_summary}{Patient-level clinical summaries}
#'   \item{patient_problem_profile}{Patient problem profiles}
#'   \item{appropriateness_distribution}{Distribution of appropriateness flags}
#'   \item{risk_distribution}{Distribution of clinical risk scores}
#'   \item{clinical_summary}{Overall clinical summary statistics}
#'
#' @examples
#' \dontrun{
#' clinical_results <- calculate_clinical_polypharmacy(
#'   poly_episodes = poly_results$episodes,
#'   problem = problem
#' )
#' }
#'
#' @importFrom dplyr mutate filter group_by summarise left_join case_when select count arrange ungroup n all_of desc
#' @export
calculate_clinical_polypharmacy <- function(poly_episodes,
                                            problem,
                                            show_tables = TRUE,
                                            n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }

  cat("=== CLINICAL POLYPHARMACY ANALYSIS ===\n")
  cat("Assessing clinical appropriateness of polypharmacy episodes...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }
  if (!"data.frame" %in% class(problem)) {
    stop("problem must be a data frame")
  }

  # Check required columns in poly_episodes
  required_poly_cols <- c("patid", "duration", "avg_concurrent_drugs", "max_concurrent_drugs")
  missing_poly_cols <- setdiff(required_poly_cols, colnames(poly_episodes))
  if (length(missing_poly_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_poly_cols, collapse = ", "))
  }

  # Check required columns in problem
  required_problem_cols <- c("patid", "signid", "probstatusid")
  missing_problem_cols <- setdiff(required_problem_cols, colnames(problem))
  if (length(missing_problem_cols) > 0) {
    stop("Missing required columns in problem: ", paste(missing_problem_cols, collapse = ", "))
  }

  # Step 1: Create patient problem profile
  cat("STEP 1: Creating patient problem profiles...\n")

  patient_problem_profile <- problem %>%
    dplyr::mutate(
      problem_severity = dplyr::case_when(
        signid == 1 ~ "Minor",
        signid == 2 ~ "Significant",
        TRUE ~ "Unknown"
      ),
      problem_relationship = dplyr::case_when(
        !is.na(parentprobrelid) & parentprobrelid == 1 ~ "Combined",
        !is.na(parentprobrelid) & parentprobrelid == 2 ~ "Evolved",
        !is.na(parentprobrelid) & parentprobrelid == 3 ~ "Grouped",
        TRUE ~ "Independent"
      )
    ) %>%
    dplyr::filter(probstatusid == 1) %>%  # Active problems only
    dplyr::group_by(patid) %>%
    dplyr::summarise(
      n_active_problems = dplyr::n(),
      n_significant_problems = sum(signid == 2, na.rm = TRUE),
      n_minor_problems = sum(signid == 1, na.rm = TRUE),
      has_chronic_conditions = any(
        (!is.na(expduration) & expduration >= 84) |
          is.na(probenddate) |
          (is.character(probenddate) & probenddate == "") |
          (!is.na(expduration) & expduration == 0), na.rm = TRUE
      ),
      predominant_severity = {
        severity_counts <- table(problem_severity)
        if (length(severity_counts) > 0) {
          names(sort(severity_counts, decreasing = TRUE))[1]
        } else {
          "Unknown"
        }
      },
      .groups = "drop"
    )

  cat("Created problem profiles for", nrow(patient_problem_profile), "patients\n")

  if (show_tables) {
    cat("\nPatient Problem Profile Preview:\n")
    print(head(patient_problem_profile, n_preview))
    cat("...\n")
  }

  # Step 2: Link episodes with clinical problems
  cat("\nSTEP 2: Linking polypharmacy episodes with clinical problems...\n")

  episodes_with_clinical <- poly_episodes %>%
    dplyr::left_join(patient_problem_profile, by = "patid") %>%
    dplyr::mutate(
      # Handle missing problem data
      n_active_problems = ifelse(is.na(n_active_problems), 0, n_active_problems),
      n_significant_problems = ifelse(is.na(n_significant_problems), 0, n_significant_problems),
      n_minor_problems = ifelse(is.na(n_minor_problems), 0, n_minor_problems),
      has_chronic_conditions = ifelse(is.na(has_chronic_conditions), FALSE, has_chronic_conditions),

      # Clinical appropriateness metrics
      drugs_per_problem = ifelse(n_active_problems > 0,
                                 avg_concurrent_drugs / n_active_problems,
                                 avg_concurrent_drugs),
      clinical_burden_ratio = ifelse(avg_concurrent_drugs > 0,
                                     n_active_problems / avg_concurrent_drugs,
                                     0),

      # Clinical complexity categories
      clinical_complexity = dplyr::case_when(
        n_active_problems >= 5 & avg_concurrent_drugs >= 8 ~ "Very High Complexity",
        n_active_problems >= 3 & avg_concurrent_drugs >= 5 ~ "High Complexity",
        n_active_problems >= 2 & avg_concurrent_drugs >= 3 ~ "Moderate Complexity",
        TRUE ~ "Low Complexity"
      ),

      # Appropriateness assessment
      appropriateness_flag = dplyr::case_when(
        drugs_per_problem > 2.5 ~ "Potentially Excessive",
        drugs_per_problem > 1.5 ~ "High Intensity",
        drugs_per_problem >= 0.8 ~ "Appropriate",
        drugs_per_problem >= 0.5 ~ "Conservative",
        TRUE ~ "Under-treated"
      ),

      # Risk stratification
      clinical_risk_score = dplyr::case_when(
        avg_concurrent_drugs >= 10 & n_active_problems >= 6 ~ "Very High Risk",
        avg_concurrent_drugs >= 8 & n_active_problems >= 4 ~ "High Risk",
        avg_concurrent_drugs >= 6 & n_active_problems >= 3 ~ "Moderate Risk",
        TRUE ~ "Low Risk"
      )
    )

  n_matched <- sum(!is.na(episodes_with_clinical$n_active_problems) & episodes_with_clinical$n_active_problems > 0)
  cat("Linked", nrow(episodes_with_clinical), "episodes with clinical data\n")
  cat("Successfully matched", n_matched, "episodes with problem data\n")

  if (show_tables) {
    cat("\nEpisodes with Clinical Data Preview:\n")
    preview_cols <- c("patid", "duration", "avg_concurrent_drugs", "n_active_problems",
                      "clinical_complexity", "appropriateness_flag", "clinical_risk_score")
    available_cols <- intersect(preview_cols, colnames(episodes_with_clinical))
    print(head(episodes_with_clinical[available_cols], n_preview))
    cat("...\n")
  }

  # Step 3: Create clinical episodes with duration assessment
  cat("\nSTEP 3: Assessing clinical appropriateness and duration matching...\n")

  clinical_episodes <- episodes_with_clinical %>%
    dplyr::mutate(
      # Duration appropriateness for complexity
      duration_clinical_match = dplyr::case_when(
        clinical_complexity == "Very High Complexity" & duration >= 90 ~ "Appropriate Long-term",
        clinical_complexity == "High Complexity" & duration >= 30 ~ "Appropriate Medium-term",
        clinical_complexity %in% c("Low Complexity", "Moderate Complexity") & duration <= 30 ~ "Appropriate Short-term",
        TRUE ~ "Duration Mismatch"
      )
    )

  # Select available columns for final output
  final_cols <- c("patid", "poly_start", "poly_end", "duration", "avg_concurrent_drugs",
                  "max_concurrent_drugs", "n_active_problems", "has_chronic_conditions",
                  "drugs_per_problem", "clinical_burden_ratio", "clinical_complexity",
                  "appropriateness_flag", "clinical_risk_score", "duration_clinical_match")
  available_final_cols <- intersect(final_cols, colnames(clinical_episodes))
  clinical_episodes <- clinical_episodes %>% dplyr::select(dplyr::all_of(available_final_cols))

  cat("Assessed clinical appropriateness for all episodes\n")

  if (show_tables) {
    cat("\nClinical Episodes Preview:\n")
    preview_cols <- c("patid", "duration", "clinical_complexity", "appropriateness_flag",
                      "clinical_risk_score", "duration_clinical_match")
    available_preview_cols <- intersect(preview_cols, colnames(clinical_episodes))
    print(head(clinical_episodes[available_preview_cols], n_preview))
    cat("...\n")
  }

  # Step 4: Generate patient-level clinical summary
  cat("\nSTEP 4: Generating patient-level clinical summaries...\n")

  patient_clinical_summary <- episodes_with_clinical %>%
    dplyr::group_by(patid) %>%
    dplyr::summarise(
      total_clinical_episodes = dplyr::n(),
      avg_drugs_per_problem = round(mean(drugs_per_problem, na.rm = TRUE), 2),
      predominant_complexity = {
        complexity_table <- table(clinical_complexity)
        if (length(complexity_table) > 0) {
          names(sort(complexity_table, decreasing = TRUE))[1]
        } else {
          "Unknown"
        }
      },
      high_risk_episodes = sum(clinical_risk_score %in% c("High Risk", "Very High Risk")),
      potentially_excessive_episodes = sum(appropriateness_flag == "Potentially Excessive"),
      chronic_condition_patient = any(has_chronic_conditions == TRUE, na.rm = TRUE),
      max_clinical_burden = max(n_active_problems, na.rm = TRUE),
      avg_episode_duration = round(mean(duration), 2),
      total_polypharmacy_days = sum(duration),
      .groups = "drop"
    )

  cat("Generated summaries for", nrow(patient_clinical_summary), "patients\n")

  if (show_tables) {
    cat("\nPatient Clinical Summary Preview:\n")
    print(head(patient_clinical_summary, n_preview))
    cat("...\n")
  }

  # Step 5: Generate distribution summaries
  cat("\nSTEP 5: Generating clinical distribution summaries...\n")

  # Clinical appropriateness distribution
  appropriateness_summary <- episodes_with_clinical %>%
    dplyr::count(appropriateness_flag, clinical_complexity) %>%
    dplyr::group_by(clinical_complexity) %>%
    dplyr::mutate(
      percentage_within_complexity = round(n / sum(n) * 100, 1)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(clinical_complexity, appropriateness_flag)

  # Risk distribution summary
  risk_summary <- episodes_with_clinical %>%
    dplyr::count(clinical_risk_score) %>%
    dplyr::mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    dplyr::arrange(match(clinical_risk_score, c("Low Risk", "Moderate Risk", "High Risk", "Very High Risk")))

  # Overall clinical summary
  clinical_summary <- episodes_with_clinical %>%
    dplyr::summarise(
      total_clinical_episodes = dplyr::n(),
      patients_with_problems = sum(!is.na(n_active_problems) & n_active_problems > 0),
      avg_problems_per_patient = round(mean(n_active_problems, na.rm = TRUE), 2),
      avg_drugs_per_problem = round(mean(drugs_per_problem, na.rm = TRUE), 2),
      high_complexity_episodes = sum(clinical_complexity %in% c("High Complexity", "Very High Complexity")),
      potentially_excessive_episodes = sum(appropriateness_flag == "Potentially Excessive"),
      high_risk_episodes = sum(clinical_risk_score %in% c("High Risk", "Very High Risk")),
      chronic_patients = sum(has_chronic_conditions == TRUE, na.rm = TRUE)
    )

  # Add duration mismatch count from clinical_episodes
  duration_mismatches <- sum(clinical_episodes$duration_clinical_match == "Duration Mismatch")
  clinical_summary$duration_mismatches <- duration_mismatches

  cat("Generated clinical distribution summaries\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("CLINICAL SUMMARY TABLES\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Clinical Summary:\n")
    print(as.data.frame(clinical_summary))

    cat("\nClinical Risk Distribution:\n")
    print(risk_summary)

    cat("\nClinical Appropriateness by Complexity:\n")
    print(appropriateness_summary)

    cat("\nTop Patients by Clinical Burden:\n")
    top_burden <- patient_clinical_summary %>%
      dplyr::arrange(dplyr::desc(max_clinical_burden), dplyr::desc(total_clinical_episodes)) %>%
      head(n_preview)
    print(top_burden)
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("CLINICAL ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      clinical_episodes = clinical_episodes,
      patient_clinical_summary = patient_clinical_summary,
      patient_problem_profile = patient_problem_profile,
      appropriateness_distribution = appropriateness_summary,
      risk_distribution = risk_summary,
      clinical_summary = clinical_summary
    ),
    class = "clinical_polypharmacy_analysis"
  )
}
