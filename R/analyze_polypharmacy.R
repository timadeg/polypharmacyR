#' Analyze Polypharmacy Episodes from CPRD Treatment Data
#'
#' This function takes treatment episodes and identifies polypharmacy periods,
#' summarizing episode characteristics and patient-level patterns with detailed
#' step-by-step processing output.
#'
#' @param treatment_episodes A data frame with columns: patid, prodcodeid, episode_start, episode_end
#' @param poly_threshold Integer. Minimum number of concurrent drugs to define polypharmacy (default: 5)
#' @param show_tables Logical. Display intermediate data tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @importFrom data.table as.data.table setorder rleid
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate lag if_else group_by summarise n rowwise
#' @importFrom dplyr mutate filter select arrange group_by ungroup summarise count distinct left_join inner_join rowwise case_when if_else lag n n_distinct
#'
#' @return A list containing:
#'   \item{episodes}{Data frame of individual polypharmacy episodes}
#'   \item{summary}{Overall summary statistics}
#'   \item{patient_summary}{Patient-level statistics}
#'   \item{duration_dist}{Distribution of episode durations}
#'   \item{drug_count_dist}{Distribution of concurrent drug counts}
#'   \item{daily_drug_use}{Daily drug usage data (intermediate)}
#'   \item{daily_polypharmacy}{Daily polypharmacy counts (intermediate)}
#'
#' @examples
#' # Basic usage with full output
#' poly_results <- analyze_polypharmacy_episodes(treatment_episodes)
#'
#' # Quiet mode (no intermediate tables)
#' poly_results <- analyze_polypharmacy_episodes(treatment_episodes, show_tables = FALSE)
#'
#' # Custom threshold with more preview rows
#' poly_results <- analyze_polypharmacy_episodes(treatment_episodes,
#'                                              poly_threshold = 4,
#'                                              n_preview = 15)
#'
#' @export
analyze_polypharmacy_episodes <- function(treatment_episodes,
                                          poly_threshold = 5,
                                          show_tables = TRUE,
                                          n_preview = 10) {

  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required but not installed.")
  }


  cat("=== POLYPHARMACY EPISODE ANALYSIS ===\n")
  cat("Polypharmacy threshold:", poly_threshold, "concurrent drugs\n")
  cat("Starting analysis...\n\n")

  # Step 1: Expand each episode into individual dates
  cat("STEP 1: Expanding treatment episodes to daily drug use...\n")
  daily_drug_use <- treatment_episodes %>%
    rowwise() %>%
    mutate(day_seq = list(seq(episode_start, episode_end, by = "day"))) %>%
    tidyr::unnest(cols = c(day_seq)) %>%
    select(patid, prodcodeid, day_seq)

  cat("✓ Created", format(nrow(daily_drug_use), big.mark = ","), "daily drug records\n")

  if (show_tables) {
    cat("\nDaily Drug Use Preview:\n")
    print(head(daily_drug_use, n_preview))
    cat("...\n")
  }

  # Step 2: Count distinct drugs per patient per day
  cat("\nSTEP 2: Counting concurrent drugs per patient per day...\n")
  daily_polypharmacy <- daily_drug_use %>%
    distinct(patid, prodcodeid, day_seq) %>%
    group_by(patid, day_seq) %>%
    summarise(n_concurrent_drugs = n(), .groups = "drop")

  cat("✓ Processed", format(nrow(daily_polypharmacy), big.mark = ","), "unique patient-days\n")

  if (show_tables) {
    cat("\nDaily Polypharmacy Counts Preview:\n")
    print(head(daily_polypharmacy, n_preview))
    cat("...\n")
  }

  # Step 3: Identify polypharmacy days
  cat("\nSTEP 3: Identifying polypharmacy days...\n")
  poly_days <- daily_polypharmacy %>%
    mutate(is_poly = n_concurrent_drugs >= poly_threshold)

  n_poly_days <- sum(poly_days$is_poly)
  cat("✓ Found", format(n_poly_days, big.mark = ","), "polypharmacy days\n")

  if (show_tables) {
    cat("\nPolypharmacy Days Preview (showing polypharmacy = TRUE only):\n")
    poly_preview <- poly_days %>% filter(is_poly == TRUE) %>% head(n_preview)
    print(poly_preview)
    if (nrow(poly_preview) == 0) {
      cat("No polypharmacy days found with threshold", poly_threshold, "\n")
    } else {
      cat("...\n")
    }
  }

  # Step 4: Group consecutive polypharmacy days into episodes
  cat("\nSTEP 4: Grouping consecutive polypharmacy days into episodes...\n")

  # Convert to data.table for efficient run grouping
  poly_dt <- data.table::as.data.table(poly_days)
  data.table::setorder(poly_dt, patid, day_seq)
  poly_dt[, poly_group := data.table::rleid(patid, is_poly)]

  # Filter only polypharmacy periods
  poly_episode_days <- poly_dt[is_poly == TRUE]

  cat("✓ Identified consecutive polypharmacy periods\n")

  # Step 5: Summarize polypharmacy episodes
  cat("\nSTEP 5: Summarizing polypharmacy episodes...\n")

  if (nrow(poly_episode_days) == 0) {
    cat("⚠ No polypharmacy episodes found with threshold", poly_threshold, "\n")

    # Return empty results structure
    empty_episodes <- data.frame(
      patid = integer(0),
      poly_start = as.Date(character(0)),
      poly_end = as.Date(character(0)),
      duration = integer(0),
      avg_concurrent_drugs = numeric(0),
      max_concurrent_drugs = integer(0),
      min_concurrent_drugs = integer(0)
    )

    return(structure(list(
      episodes = empty_episodes,
      summary = data.frame(
        total_episodes = 0,
        unique_patients = 0,
        avg_duration_days = NA,
        median_duration_days = NA,
        min_duration = NA,
        max_duration = NA,
        avg_concurrent_drugs = NA,
        max_concurrent_drugs = NA,
        avg_episodes_per_patient = NA
      ),
      patient_summary = data.frame(),
      duration_dist = data.frame(),
      drug_count_dist = data.frame(),
      daily_drug_use = daily_drug_use,
      daily_polypharmacy = daily_polypharmacy,
      threshold = poly_threshold
    ), class = "polypharmacy_analysis"))
  }

  episodes <- poly_episode_days[, .(
    poly_start = min(day_seq),
    poly_end = max(day_seq),
    duration = as.integer(max(day_seq) - min(day_seq) + 1),
    avg_concurrent_drugs = round(mean(n_concurrent_drugs), 2),
    max_concurrent_drugs = max(n_concurrent_drugs),
    min_concurrent_drugs = min(n_concurrent_drugs)
  ), by = .(patid, poly_group)] %>%
    as.data.frame()


  cat("✓ Created", format(nrow(episodes), big.mark = ","), "polypharmacy episodes in",
      n_distinct(episodes$patid), "patients\n")

  if (show_tables) {
    cat("\nPolypharmacy Episodes Preview:\n")
    print(head(episodes, n_preview))
    if (nrow(episodes) > n_preview) cat("...\n")
  }

  # Step 6: Generate comprehensive summaries
  cat("\nSTEP 6: Generating summary statistics...\n")

  # Overall episode statistics
  summary_stats <- episodes %>%
    summarise(
      total_episodes = n(),
      unique_patients = n_distinct(patid),
      avg_duration_days = round(mean(duration), 2),
      median_duration_days = median(duration),
      min_duration = min(duration),
      max_duration = max(duration),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs), 2),
      max_concurrent_drugs = max(max_concurrent_drugs),
      avg_episodes_per_patient = round(n() / n_distinct(patid), 2)
    )

  # Patient-level summary
  patient_summary <- episodes %>%
    group_by(patid) %>%
    summarise(
      n_episodes = n(),
      total_poly_days = sum(duration),
      avg_duration = round(mean(duration), 2),
      max_concurrent_ever = max(max_concurrent_drugs),
      first_poly_date = min(poly_start),
      last_poly_date = max(poly_end),
      .groups = "drop"
    )

  # Duration distribution
  duration_dist <- episodes %>%
    mutate(duration_category = case_when(
      duration <= 14 ~ "≤2 weeks",
      duration <= 30 ~ "2-4 weeks",
      duration <= 90 ~ "1-3 months",
      duration <= 365 ~ "3-12 months",
      TRUE ~ ">1 year"
    )) %>%
    count(duration_category) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    arrange(match(duration_category, c("≤2 weeks", "2-4 weeks", "1-3 months", "3-12 months", ">1 year")))

  # Drug count distribution
  drug_count_dist <- episodes %>%
    mutate(drug_category = case_when(
      max_concurrent_drugs <= 5 ~ "5 drugs",
      max_concurrent_drugs <= 7 ~ "6-7 drugs",
      max_concurrent_drugs <= 10 ~ "8-10 drugs",
      max_concurrent_drugs <= 15 ~ "11-15 drugs",
      TRUE ~ "16+ drugs"
    )) %>%
    count(drug_category) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))

  cat("✓ Generated summary statistics and distributions\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("SUMMARY TABLES\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Overall Episode Summary:\n")
    print(as.data.frame(summary_stats))

    cat("\nPatient-Level Summary Preview:\n")
    print(head(patient_summary, n_preview))
    if (nrow(patient_summary) > n_preview) cat("...\n")

    cat("\nEpisode Duration Distribution:\n")
    print(duration_dist)

    cat("\nConcurrent Drugs Distribution:\n")
    print(drug_count_dist)
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      episodes = episodes,
      summary = summary_stats,
      patient_summary = patient_summary,
      duration_dist = duration_dist,
      drug_count_dist = drug_count_dist,
      daily_drug_use = daily_drug_use,
      daily_polypharmacy = daily_polypharmacy,
      threshold = poly_threshold
    ),
    class = "polypharmacy_analysis"
  )
}

#' Print method for polypharmacy analysis results
#'
#' @param x A polypharmacy_analysis object
#' @param ... Additional arguments (unused)
#' @export
print.polypharmacy_analysis <- function(x, ...) {
  cat("\n📊 Polypharmacy Episode Analysis Results\n")
  cat("=========================================\n")
  cat("Threshold:", x$threshold, "concurrent drugs\n\n")

  if (x$summary$total_episodes == 0) {
    cat("⚠ No polypharmacy episodes found\n")
    return(invisible(x))
  }

  cat("📈 Key Statistics:\n")
  cat("• Total episodes:", format(x$summary$total_episodes, big.mark = ","), "\n")
  cat("• Unique patients:", format(x$summary$unique_patients, big.mark = ","), "\n")
  cat("• Average duration:", x$summary$avg_duration_days, "days\n")
  cat("• Average concurrent drugs:", x$summary$avg_concurrent_drugs, "\n")
  cat("• Episodes per patient:", x$summary$avg_episodes_per_patient, "\n\n")

  cat("⏱ Duration Distribution:\n")
  for(i in 1:nrow(x$duration_dist)) {
    cat("• ", x$duration_dist$duration_category[i], ": ",
        x$duration_dist$n[i], " episodes (",
        x$duration_dist$percentage[i], "%)\n", sep="")
  }

  cat("\n💊 Drug Count Distribution:\n")
  for(i in 1:nrow(x$drug_count_dist)) {
    cat("• ", x$drug_count_dist$drug_category[i], ": ",
        x$drug_count_dist$n[i], " episodes (",
        x$drug_count_dist$percentage[i], "%)\n", sep="")
  }

  cat("\n✨ Use summary(results) for detailed patient statistics\n")
  invisible(x)
}

#' Summary method for polypharmacy analysis results
#'
#' @param object A polypharmacy_analysis object
#' @param ... Additional arguments (unused)
#' @export
summary.polypharmacy_analysis <- function(object, ...) {
  cat("📋 Detailed Polypharmacy Analysis Summary\n")
  cat("========================================\n\n")

  cat("🔢 Overall Statistics:\n")
  print(as.data.frame(object$summary))

  if (nrow(object$patient_summary) > 0) {
    cat("\n👥 Top 10 Patients by Episode Count:\n")
    top_patients <- object$patient_summary %>%
      arrange(desc(n_episodes)) %>%
      head(10)
    print(top_patients)

    cat("\n📊 Patient Summary Statistics:\n")
    patient_stats <- object$patient_summary %>%
      summarise(
        avg_episodes_per_patient = round(mean(n_episodes), 2),
        median_episodes_per_patient = median(n_episodes),
        max_episodes_per_patient = max(n_episodes),
        avg_total_poly_days = round(mean(total_poly_days), 2),
        max_concurrent_ever = max(max_concurrent_ever)
      )
    print(as.data.frame(patient_stats))
  }

  invisible(object)
}

