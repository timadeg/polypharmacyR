# polypharmacyR

[![CRAN status](https://www.r-pkg.org/badges/version/polypharmacyR)](https://CRAN.R-project.org/package=polypharmacyR)
[![Build Status](https://github.com/timadeg/polypharmacyR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/timadeg/polypharmacyR/actions)
[![Codecov](https://codecov.io/gh/timadeg/polypharmacyR/branch/main/graph/badge.svg)](https://codecov.io/gh/timadeg/polypharmacyR)

polypharmacyR is an R package that provides fast, reproducible tools for measuring, summarizing, and visualizing polypharmacy from electronic health and administrative medication data. It supports person-level medication counting, burden metrics (e.g., DDD and score-based measures), overlapping-exposure detection, and timeline visualizations. This work was developed in collaboration with Keele University School of Medicine.

- Website / Docs: (add pkgdown site URL when available)
- Issues & contributions: https://github.com/timadeg/polypharmacyR/issues

Benefits
- Fast, vectorized routines for computing person-level medication counts and durations
- Standardized burden metrics (counts, cumulative defined daily dose, anticholinergic burden patterns)
- Functions for identifying simultaneous exposures and potential interactions
- Plotting utilities for timelines and cohort-level medication use

Table of contents
- Installation
- Quick start
- Core concepts & example workflow
- Functions (high-level)
- Data requirements / expected input
- Vignettes & documentation
- Development & testing
- Contributing
- License & citation
- Contact

Installation

Install the latest development version from GitHub:

```r
# install.packages("remotes") # if not already installed
remotes::install_github("timadeg/polypharmacyR")
```

If the package is published on CRAN, you can install from CRAN:

```r
install.packages("polypharmacyR")
```

Quick start

Below are minimal examples that demonstrate typical workflows. Adjust column names or formats to match your dataset.

1) Compute person-level medication counts and exposure days

```r
library(polypharmacyR)

# Example: a prescription dataset with columns:
# patient_id, drug_code, start_date, end_date, dose, ddd (defined daily dose)
head(prescriptions)

# Count distinct concurrent medications per person-day (returns tidy data.frame)
daily_counts <- count_medications(
  data = prescriptions,
  id = "patient_id",
  drug = "drug_code",
  start = "start_date",
  end = "end_date",
  by = "day"
)

# Summary statistics of polypharmacy prevalence
summary_stats <- polypharmacy_summary(daily_counts, thresholds = c(5, 10))
print(summary_stats)
```

2) Compute exposure burden (e.g., cumulative DDD or anticholinergic burden)

```r
burden <- compute_burden(
  data = prescriptions,
  id = "patient_id",
  drug = "drug_code",
  start = "start_date",
  end = "end_date",
  ddd_var = "ddd",        # optional: use if DDDs are available
  acb_map = acb_scores    # optional: a lookup table of drug -> anticholinergic score
)

head(burden)
```

3) Visualize medication timelines for selected patients

```r
plot_medication_timeline(
  data = prescriptions,
  id = "patient_id",
  drug = "drug_code",
  start = "start_date",
  end = "end_date",
  patient = c("P001", "P042"),
  color_by = "drug_class"
)
```

Core concepts & recommended workflow

1. Validate and standardize medication input: ensure dates are Date objects, drug identifiers are normalized (ATC, RxNorm, local codes).
2. Define exposure episodes (gap rules, overlapping supplies).
3. Compute person-day medication counts or burden metrics.
4. Summarize by thresholds (e.g., >=5 meds = polypharmacy; >=10 meds = severe polypharmacy) or by continuous burden measures.
5. Visualize longitudinal patterns and cohort-level summaries.

High-level functions (examples)
- count_medications(): count concurrent distinct meds per time unit
- compute_burden(): calculate DDD-based or score-based burden measures
- identify_simultaneous_exposures(): list episodes with overlapping supplies for two-or-more drugs
- find_potential_interactions(): screen for known interactions given a lookup table
- plot_medication_timeline(): plot per-patient medication timelines
- polypharmacy_summary(): cohort-level prevalence and descriptive metrics

Data requirements / expected input

The package expects a prescriptions/exposures table in tidy (long) format with at minimum:
- id: unique patient identifier
- drug: medication identifier (code or name)
- start: exposure start date
- end: exposure end date

Optional useful columns:
- dose, formulation, ddd (defined daily dose), drug_class, prescriber, source

Vignettes & documentation

- Detailed usage, reproducible examples, and recommended epidemiological coding practices are available in the package vignettes (run `browseVignettes("polypharmacyR")` after installation).
- If you maintain a pkgdown site, link it here to make docs browsable online.

Development & testing

- Unit tests using testthat: tests live in tests/testthat/.
- Run checks locally:

```r
# run R CMD check (recommended via devtools)
devtools::check()
```

- Continuous integration: Add GitHub Actions with an R-CMD-check workflow (example available in .github/workflows/).

Contributing

Contributions are welcome! Please follow these steps:
1. Fork the repo and create a branch for your feature/fix.
2. Add or update unit tests that demonstrate the bug or expected behavior.
3. Run `devtools::check()` and ensure all checks pass.
4. Open a pull request describing the change and why it helps.

Please adhere to the code style, include tests for new features, and update documentation/vignettes.

Roadmap (ideas)
- Expand interaction library and mapping to common coding systems (ATC, RxNorm)
- Add functionality for dose standardization and persistence metrics
- Implement cohort-level report generator and dashboards
- Improve performance for very large administrative datasets (data.table backend)

License

Specify the license (e.g., MIT, GPL-3). Update LICENSE file accordingly.

Citation

If you use polypharmacyR in published research, please cite:

TBA — add package DOI or publication details once available.

Contact & support

- Repo: https://github.com/timadeg/polypharmacyR
- Issues: https://github.com/timadeg/polypharmacyR/issues
- Maintainer: Timothy Adegbola (github: timadeg) — please open issues for bugs, feature requests, or questions.

Acknowledgements

Thank you to contributors and the R community. Portions of functionality are inspired by best practices in pharmacoepidemiology.

---
