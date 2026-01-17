#' Analyze Seasonal and Temporal Patterns in Polypharmacy Episodes
#'
#' This function analyzes temporal patterns in polypharmacy episodes including
#' seasonal variations, winter pressure periods, monthly patterns, and year-over-year trends.
#' Includes specific analysis for UK healthcare context and COVID-19 impact.
#'
#' @param poly_episodes Data frame containing polypharmacy episodes with columns:
#'   patid, poly_start, poly_end, duration, avg_concurrent_drugs, max_concurrent_drugs
#' @param show_tables Logical. Display summary tables (default: TRUE)
#' @param n_preview Integer. Number of rows to preview in tables (default: 10)
#'
#' @return A list containing:
#'   \item{seasonal_episodes}{Episodes with temporal classifications}
#'   \item{seasonal_summary}{Summary statistics by season}
#'   \item{monthly_patterns}{Monthly variation patterns}
#'   \item{winter_analysis}{Winter pressure period analysis}
#'   \item{covid_impact}{Pre-COVID vs COVID-era comparison}
#'   \item{yearly_trends}{Year-over-year trend analysis}
#'   \item{temporal_statistics}{Overall temporal statistics}
#'
#' @examples
#' \dontrun{
#' seasonal_results <- analyze_seasonal_patterns(
#'   poly_episodes = poly_results$episodes
#' )
#' }
#'
#' @importFrom dplyr mutate group_by summarise case_when n n_distinct
#' @importFrom lubridate month year
#' @export
analyze_seasonal_patterns <- function(poly_episodes,
                                      show_tables = TRUE,
                                      n_preview = 10) {

  # Validate required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package 'lubridate' is required but not installed.")
  }

  cat("=== SEASONAL POLYPHARMACY ANALYSIS ===\n")
  cat("Analyzing temporal and seasonal patterns...\n\n")

  # Validate input data
  if (!"data.frame" %in% class(poly_episodes)) {
    stop("poly_episodes must be a data frame")
  }
  if (nrow(poly_episodes) == 0) {
    stop("poly_episodes data frame is empty")
  }

  # Check required columns
  required_cols <- c("patid", "poly_start", "duration", "avg_concurrent_drugs")
  missing_cols <- setdiff(required_cols, colnames(poly_episodes))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in poly_episodes: ", paste(missing_cols, collapse = ", "))
  }

  # Step 1: Add temporal variables
  cat("STEP 1: Adding temporal and seasonal classifications...\n")

  episodes_with_seasons <- poly_episodes %>%
    dplyr::mutate(
      start_month = lubridate::month(poly_start),
      start_season = dplyr::case_when(
        lubridate::month(poly_start) %in% c(12, 1, 2) ~ "Winter",
        lubridate::month(poly_start) %in% c(3, 4, 5) ~ "Spring",
        lubridate::month(poly_start) %in% c(6, 7, 8) ~ "Summer",
        lubridate::month(poly_start) %in% c(9, 10, 11) ~ "Autumn"
      ),
      start_year = lubridate::year(poly_start),

      # UK-specific periods
      is_winter_pressure = lubridate::month(poly_start) %in% c(12, 1, 2),
      is_holiday_period = lubridate::month(poly_start) %in% c(12, 8), # Dec + Aug

      # COVID period (2020 onwards)
      covid_period = dplyr::case_when(
        poly_start < as.Date("2020-03-01") ~ "Pre-COVID",
        poly_start >= as.Date("2020-03-01") ~ "COVID-era"
      )
    )

  cat("Added temporal classifications to", nrow(episodes_with_seasons), "episodes\n")

  if (show_tables) {
    cat("\nSeasonal Episodes Preview:\n")
    preview_cols <- c("patid", "poly_start", "start_season", "start_month",
                      "covid_period", "avg_concurrent_drugs")
    available_cols <- intersect(preview_cols, colnames(episodes_with_seasons))
    print(head(episodes_with_seasons[available_cols], n_preview))
    cat("...\n")
  }

  # Step 2: Seasonal analysis
  cat("\nSTEP 2: Analyzing patterns by season...\n")

  seasonal_summary <- episodes_with_seasons %>%
    dplyr::group_by(start_season) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_duration = round(mean(duration, na.rm = TRUE), 1),
      max_concurrent_drugs = max(max_concurrent_drugs, na.rm = TRUE),
      high_complexity_episodes = sum(avg_concurrent_drugs >= 8),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      percentage_of_episodes = round(n_episodes / sum(n_episodes) * 100, 1),
      high_complexity_rate = round(high_complexity_episodes / n_episodes * 100, 1)
    )

  cat("Generated seasonal analysis for 4 seasons\n")

  if (show_tables) {
    cat("\nSeasonal Summary:\n")
    print(seasonal_summary)
    cat("...\n")
  }

  # Step 3: Monthly patterns
  cat("\nSTEP 3: Analyzing monthly variation patterns...\n")

  monthly_patterns <- episodes_with_seasons %>%
    dplyr::group_by(start_month) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_duration = round(mean(duration, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      month_name = month.name[start_month],
      percentage_episodes = round(n_episodes / sum(n_episodes) * 100, 1)
    )

  cat("Generated monthly patterns for 12 months\n")

  if (show_tables) {
    cat("\nMonthly Patterns (Top 6 months by episode count):\n")
    top_months <- monthly_patterns %>%
      dplyr::arrange(desc(n_episodes)) %>%
      head(6)
    print(top_months)
    cat("...\n")
  }

  # Step 4: Winter pressure analysis
  cat("\nSTEP 4: Analyzing winter pressure periods...\n")

  winter_analysis <- episodes_with_seasons %>%
    dplyr::group_by(is_winter_pressure) %>%
    dplyr::summarise(
      period = ifelse(is_winter_pressure, "Winter Pressure", "Other Months"),
      n_episodes = dplyr::n(),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_duration = round(mean(duration, na.rm = TRUE), 1),
      high_complexity_episodes = sum(avg_concurrent_drugs >= 8),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      percentage_episodes = round(n_episodes / sum(n_episodes) * 100, 1),
      high_complexity_rate = round(high_complexity_episodes / n_episodes * 100, 1)
    )

  cat("Generated winter pressure analysis\n")

  if (show_tables) {
    cat("\nWinter Pressure Analysis:\n")
    print(winter_analysis)
    cat("...\n")
  }

  # Step 5: COVID impact analysis
  cat("\nSTEP 5: Analyzing COVID-19 period impact...\n")

  covid_impact <- episodes_with_seasons %>%
    dplyr::group_by(covid_period) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_duration = round(mean(duration, na.rm = TRUE), 1),
      high_complexity_rate = round(sum(avg_concurrent_drugs >= 8) / dplyr::n() * 100, 1),
      .groups = "drop"
    )

  cat("Generated COVID impact analysis\n")

  if (show_tables) {
    cat("\nCOVID Impact Analysis:\n")
    print(covid_impact)
    cat("...\n")
  }

  # Step 6: Year-over-year trends
  cat("\nSTEP 6: Analyzing year-over-year trends...\n")

  yearly_trends <- episodes_with_seasons %>%
    dplyr::group_by(start_year) %>%
    dplyr::summarise(
      n_episodes = dplyr::n(),
      n_patients = dplyr::n_distinct(patid),
      avg_concurrent_drugs = round(mean(avg_concurrent_drugs, na.rm = TRUE), 2),
      avg_duration = round(mean(duration, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(start_year)

  cat("Generated yearly trends for", nrow(yearly_trends), "years\n")

  # Step 7: Overall temporal statistics
  cat("\nSTEP 7: Calculating overall temporal statistics...\n")

  temporal_statistics <- list()

  # Date range
  temporal_statistics$study_period <- data.frame(
    earliest_episode = min(episodes_with_seasons$poly_start),
    latest_episode = max(episodes_with_seasons$poly_start),
    study_period_days = as.numeric(max(episodes_with_seasons$poly_start) - min(episodes_with_seasons$poly_start)),
    total_episodes = nrow(episodes_with_seasons)
  )

  # Seasonal distribution
  temporal_statistics$seasonal_distribution <- seasonal_summary %>%
    dplyr::select(start_season, n_episodes, percentage_of_episodes) %>%
    dplyr::arrange(desc(n_episodes))

  # Winter vs non-winter comparison
  winter_episodes <- sum(episodes_with_seasons$is_winter_pressure)
  total_episodes <- nrow(episodes_with_seasons)
  temporal_statistics$winter_pressure_stats <- data.frame(
    winter_pressure_episodes = winter_episodes,
    other_months_episodes = total_episodes - winter_episodes,
    winter_pressure_percentage = round(winter_episodes / total_episodes * 100, 1)
  )

  cat("Generated overall temporal statistics\n")

  # Display summary tables
  if (show_tables) {
    cat("\n", paste(rep("=", 50), collapse = ""), "\n")
    cat("TEMPORAL ANALYSIS SUMMARY\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")

    cat("Study Period Overview:\n")
    print(temporal_statistics$study_period)

    cat("\nSeasonal Distribution:\n")
    print(temporal_statistics$seasonal_distribution)

    cat("\nWinter Pressure Statistics:\n")
    print(temporal_statistics$winter_pressure_stats)

    if (nrow(yearly_trends) > 0) {
      cat("\nYearly Trends:\n")
      print(yearly_trends)
    }
  }

  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("SEASONAL ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")

  # Return structured results
  structure(
    list(
      seasonal_episodes = episodes_with_seasons,
      seasonal_summary = seasonal_summary,
      monthly_patterns = monthly_patterns,
      winter_analysis = winter_analysis,
      covid_impact = covid_impact,
      yearly_trends = yearly_trends,
      temporal_statistics = temporal_statistics
    ),
    class = "seasonal_polypharmacy_analysis"
  )
}
