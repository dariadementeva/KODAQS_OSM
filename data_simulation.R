# ============================================================================
# Simulate Fully Synthetic Survey-Context Data for the osmBias Tool
# ============================================================================
#
# Purpose:
#   Generate a fully synthetic dataset for the osmBias .
#   The script auto-calibrates from the real linked data set, then generates
#   a synthetic dataset .
#
#
# ============================================================================


# Setup ----

library(dplyr)
library(tibble)
library(tidyr)
library(MASS)

set.seed(20260304)


# Prepare calibration source ----

## Define variables ----

poi_names_clean <- c(
  "count_osm_hosp_clean", "count_official_hosp_clean",
  "count_osm_school_clean", "count_official_school_clean",
  "count_osm_super_clean", "count_official_super_clean"
)

poi_names_zero <- paste0(poi_names_clean, "_zero")

## Extract original data ----

# survey_context.RDS is created in original script with original geocoded data
# (./code/survey_context.R)

orig <- survey_context |>
  dplyr::select(
    lfdn, mun_id, service_depri,
    female, age, edu_low, edu_mid, edu_high,
    any_of(poi_names_clean), any_of(poi_names_zero)
  )


# Auto-calibrate from original data ----
# All parameters are extracted, then slightly distorted.

cat("=== Extracting calibration parameters ===\n")

## Cluster structure ----

orig_cluster <- as.numeric(table(orig$mun_id))
n_municipalities <- length(orig_cluster) 
cluster_mean     <- mean(orig_cluster)
cluster_median   <- median(orig_cluster)
cluster_max      <- max(orig_cluster)

# Slight distortion
n_municipalities <- n_municipalities - sample(2:10, 1)
cluster_mean     <- round(cluster_mean * 0.97)
cluster_median   <- round(cluster_median * 0.96)
cluster_max      <- round(cluster_max * 0.96)

gamma_shape <- 8
gamma_rate  <- gamma_shape / cluster_median

cat(sprintf("  Clusters: n=%d, mean=%d, median=%d, max=%d\n",
            n_municipalities, cluster_mean, cluster_median, cluster_max))

## Demographics ----

female_prop <- round(mean(orig$female, na.rm = TRUE) + 0.02, 2)
age_mean    <- round(mean(orig$age, na.rm = TRUE) - 2)
age_sd      <- round(sd(orig$age, na.rm = TRUE) - 1)
age_min     <- min(orig$age, na.rm = TRUE) 
age_max     <- max(orig$age, na.rm = TRUE) -15
edu_low_prop  <- round(mean(orig$edu_low, na.rm = TRUE) + 0.02, 2)
edu_high_prop <- round(mean(orig$edu_high, na.rm = TRUE) - 0.03, 2)
edu_mid_prop  <- round(1 - edu_low_prop - edu_high_prop, 2)

cat(sprintf("  female=%.2f, age=%.0f±%.0f, edu=%.2f/%.2f/%.2f\n",
            female_prop, age_mean, age_sd, edu_low_prop, edu_mid_prop, edu_high_prop))

## DV distribution ----

depri_tab <- prop.table(table(orig$service_depri))
# Slight distortion: shift by ±0.01–0.02
depri_prop <- as.numeric(depri_tab) + c(-0.02, 0.01, 0.01, 0.01, 0.01)
depri_prop <- pmax(depri_prop, 0.01)
depri_prop <- depri_prop / sum(depri_prop)
names(depri_prop) <- names(depri_tab)

cat("  depri_prop:", paste(round(depri_prop, 3), collapse = " / "), "\n")

## Missingness ----

miss_depri  <- round(mean(is.na(orig$service_depri)) + 0.003, 3)
miss_age    <- round(mean(is.na(orig$age)) + 0.006, 3)
miss_female <- round(mean(is.na(orig$female)) - 0.003, 3)

## Correlations with DV ----

cor_vars_for_dv <- c("age", "female", "edu_low", "edu_high", poi_names_zero)
target_cors <- sapply(cor_vars_for_dv, function(v) {
  if (v %in% names(orig)) {
    cor(orig$service_depri, orig[[v]], use = "pairwise.complete.obs")
  } else NA
})
# Slight distortion: round to 2 decimals
target_cors <- round(target_cors, 2)

cat("  Target correlations with DV:\n")
for (i in seq_along(target_cors)) {
  cat(sprintf("    %-45s r = %.2f\n", names(target_cors)[i], target_cors[i]))
}

## POI distributions (municipality level, _zero variants) ----

orig_mun <- orig |>
  dplyr::group_by(mun_id) |>
  dplyr::summarise(across(any_of(poi_names_zero), ~ first(.x)), .groups = "drop")

poi_params <- list()
for (v in poi_names_zero) {
  x <- orig_mun[[v]]
  if (!is.null(x)) {
    poi_params[[v]] <- list(
      mean = round(mean(x, na.rm = TRUE) * 0.95, 1),
      sd   = round(sd(x, na.rm = TRUE) * 0.95, 1),
      max  = round(max(x, na.rm = TRUE) * 0.95)
    )
  }
}

cat("  POI params (_zero, municipality level):\n")
for (v in names(poi_params)) {
  p <- poi_params[[v]]
  cat(sprintf("    %-45s mean=%.1f sd=%.1f max=%d\n", v, p$mean, p$sd, p$max))
}

## POI correlation matrix (_zero, municipality level) ----

poi_zero_in_data <- intersect(poi_names_zero, names(orig_mun))
poi_cor_orig <- cor(orig_mun[poi_zero_in_data], use = "pairwise.complete.obs")
# Slight distortion: round to 2 decimals
poi_cor_matrix <- round(poi_cor_orig, 2)

cat("\n  POI correlation matrix:\n")
print(poi_cor_matrix)

## Random intercept and noise ----

noise_sd <- 1.2
random_intercept_sd <- 0.15

cat(sprintf("\n  noise_sd=%.1f, random_intercept_sd=%.2f\n", noise_sd, random_intercept_sd))
cat(sprintf("  Implied ICC ≈ %.3f\n",
            random_intercept_sd^2 / (random_intercept_sd^2 + noise_sd^2)))


# Simulate cluster structure ----

target_total_n <- round(cluster_mean * n_municipalities)

generate_cluster_sizes <- function(n_mun, target_n, shape, rate,
                                   max_size, seed = 20250304) {
  set.seed(seed)
  raw <- round(rgamma(n_mun, shape = shape, rate = rate))
  raw <- pmax(raw, 1L)
  
  top_idx <- order(raw, decreasing = TRUE)[1:3]
  raw[top_idx] <- c(max_size,
                    round(runif(1, max_size * 0.45, max_size * 0.7)),
                    round(runif(1, max_size * 0.3, max_size * 0.5)))
  
  raw <- round(raw * (target_n / sum(raw)))
  raw <- pmax(raw, 1L)
  raw[which.max(raw)] <- raw[which.max(raw)] + (target_n - sum(raw))
  return(raw)
}

cluster_sizes <- generate_cluster_sizes(n_municipalities, target_total_n,
                                        gamma_shape, gamma_rate, cluster_max)

cat("\n=== Simulated cluster structure ===\n")
print(summary(cluster_sizes))


# Simulate municipality-level POI counts ----

simulate_poi_counts <- function(n_mun, params, cor_matrix, var_names) {
  n_vars <- length(var_names)
  
  # Log-normal parameters from target mean and sd
  log_params <- lapply(params[var_names], function(p) {
    # Handle zero or near-zero means
    m <- max(p$mean, 0.1)
    s <- max(p$sd, 0.1)
    cv <- s / m
    log_sd <- sqrt(log(1 + cv^2))
    log_mean <- log(m) - log_sd^2 / 2
    list(log_mean = log_mean, log_sd = log_sd)
  })
  
  log_sds <- sapply(log_params, function(p) p$log_sd)
  log_means <- sapply(log_params, function(p) p$log_mean)
  
  # Build log-scale covariance from correlation matrix
  log_cov <- diag(log_sds) %*% cor_matrix %*% diag(log_sds)
  
  # Ensure positive definiteness
  eig <- eigen(log_cov, symmetric = TRUE)
  eig$values <- pmax(eig$values, 1e-6)
  log_cov <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  
  # Generate and exponentiate
  z <- MASS::mvrnorm(n = n_mun, mu = log_means, Sigma = log_cov)
  counts <- round(exp(z))
  
  # Floor at 0 and cap at max
  for (i in seq_len(n_vars)) {
    counts[, i] <- pmin(pmax(counts[, i], 0L), params[[var_names[i]]]$max)
  }
  
  df <- as.data.frame(counts)
  names(df) <- var_names
  return(df)
}

poi_simulated <- simulate_poi_counts(
  n_municipalities, poi_params, poi_cor_matrix, poi_zero_in_data
)

# Also create _clean variants (set zeros back to NA)
for (v_zero in poi_zero_in_data) {
  v_clean <- sub("_zero$", "", v_zero)
  poi_simulated[[v_clean]] <- ifelse(poi_simulated[[v_zero]] == 0, NA,
                                     poi_simulated[[v_zero]])
}

cat("\n=== Simulated POI distributions (_zero, municipality level) ===\n")
for (v in poi_zero_in_data) {
  x <- poi_simulated[[v]]
  cat(sprintf("  %-45s mean=%6.1f sd=%6.1f median=%4.0f max=%5.0f\n",
              v, mean(x, na.rm = TRUE), sd(x, na.rm = TRUE),
              median(x, na.rm = TRUE), max(x, na.rm = TRUE)))
}

cat("\nSimulated POI correlations:\n")
print(round(cor(poi_simulated[poi_zero_in_data], use = "pairwise.complete.obs"), 2))


# Assign clusters and create anonymous IDs ----

poi_simulated$mun_idx <- seq_len(n_municipalities)

# Size proxy: larger POI → larger cluster
poi_simulated$size_proxy <- rowSums(poi_simulated[poi_zero_in_data], na.rm = TRUE)
poi_simulated <- poi_simulated |> dplyr::arrange(desc(size_proxy))
poi_simulated$n_respondents <- sort(cluster_sizes, decreasing = TRUE)

# Random intercept per municipality
poi_simulated$mun_random_effect <- rnorm(n_municipalities, 0, random_intercept_sd)

# Anonymized IDs (randomly shuffled)
poi_simulated$mun_id <- sample(sprintf("mun_%03d", seq_len(n_municipalities)))


# Expand to respondent level ----

# Select columns to carry over
poi_all_vars <- c(poi_zero_in_data,
                  sub("_zero$", "", poi_zero_in_data))
poi_all_vars <- intersect(poi_all_vars, names(poi_simulated))

survey_sim <- poi_simulated |>
  dplyr::select(mun_id, n_respondents, mun_random_effect,
                all_of(poi_all_vars)) |>
  tidyr::uncount(n_respondents) |>
  dplyr::mutate(lfdn = dplyr::row_number())

num_respondents <- nrow(survey_sim)
cat("\nExpanded to", num_respondents, "respondents in",
    n_municipalities, "municipalities\n")


# Simulate individual-level variables ----

survey_sim <- survey_sim |>
  dplyr::mutate(
    female = rbinom(dplyr::n(), 1, prob = female_prop),
    age = round(pmin(pmax(rnorm(dplyr::n(), mean = age_mean, sd = age_sd),
                          age_min), age_max)),
    edu_cat = sample(c("low", "mid", "high"), dplyr::n(),
                     replace = TRUE,
                     prob = c(edu_low_prop, edu_mid_prop, edu_high_prop))
  ) |>
  dplyr::mutate(
    edu_low  = as.integer(edu_cat == "low"),
    edu_mid  = as.integer(edu_cat == "mid"),
    edu_high = as.integer(edu_cat == "high")
  ) |>
  dplyr::select(-edu_cat)


# Simulate DV: service_depri ----

## Build latent score ----
# Use target correlations as weights on z-scored predictors

survey_sim <- survey_sim |>
  dplyr::mutate(
    latent_depri =
      target_cors["count_osm_hosp_clean_zero"]   * scale(count_osm_hosp_clean_zero)[,1] +
      target_cors["count_official_hosp_clean_zero"] * scale(count_official_hosp_clean_zero)[,1] +
      target_cors["count_osm_school_clean_zero"]  * scale(count_osm_school_clean_zero)[,1] +
      target_cors["count_official_school_clean_zero"] * scale(count_official_school_clean_zero)[,1] +
      target_cors["count_osm_super_clean_zero"]   * scale(count_osm_super_clean_zero)[,1] +
      target_cors["count_official_super_clean_zero"] * scale(count_official_super_clean_zero)[,1] +
      target_cors["age"]      * scale(age)[,1] +
      target_cors["female"]   * scale(female)[,1] +
      target_cors["edu_low"]  * scale(edu_low)[,1] +
      target_cors["edu_high"] * scale(edu_high)[,1] +
      mun_random_effect +
      rnorm(dplyr::n(), mean = 0, sd = noise_sd)
  )

## Discretize to 0–4 ----

cumulative_props <- cumsum(depri_prop / sum(depri_prop))
cut_points <- quantile(survey_sim$latent_depri,
                       probs = c(0, cumulative_props), na.rm = TRUE)
cut_points <- unique(cut_points)

survey_sim <- survey_sim |>
  dplyr::mutate(
    service_depri = as.integer(as.character(cut(latent_depri,
                                                breaks = c(-Inf, cut_points[-c(1, length(cut_points))], Inf),
                                                labels = 0:4,
                                                include.lowest = TRUE))),
    service_depri = pmin(pmax(service_depri, 0L), 4L)
  ) |>
  dplyr::select(-latent_depri, -mun_random_effect)


# Introduce missingness ----

n <- nrow(survey_sim)
survey_sim$service_depri[sample(n, floor(miss_depri * n))]  <- NA
survey_sim$age[sample(n, floor(miss_age * n))]              <- NA
survey_sim$female[sample(n, floor(miss_female * n))]        <- NA


# Select output columns ----

simulated_survey_context <- survey_sim |>
  dplyr::select(
    lfdn, mun_id, service_depri,
    female, age, edu_low, edu_mid, edu_high,
    any_of(poi_all_vars)
  )


# Validate ----

cat("\n=== Final Validation ===\n")
cat("N:", nrow(simulated_survey_context), "\n")
cat("N municipalities:", length(unique(simulated_survey_context$mun_id)), "\n\n")

cat("Cluster sizes:\n")
print(summary(as.numeric(table(simulated_survey_context$mun_id))))

cat("\nservice_depri distribution:\n")
cat("  Original: ", paste(round(prop.table(table(orig$service_depri)), 3), collapse = " / "), "\n")
cat("  Simulated:", paste(round(prop.table(table(simulated_survey_context$service_depri)), 3), collapse = " / "), "\n")

cat("\nDemographics:\n")
cat(sprintf("  %-15s %10s %10s\n", "", "Original", "Simulated"))
cat(sprintf("  %-15s %10.3f %10.3f\n", "female",
            mean(orig$female, na.rm = TRUE), mean(simulated_survey_context$female, na.rm = TRUE)))
cat(sprintf("  %-15s %10.1f %10.1f\n", "age",
            mean(orig$age, na.rm = TRUE), mean(simulated_survey_context$age, na.rm = TRUE)))
cat(sprintf("  %-15s %10.3f %10.3f\n", "edu_low",
            mean(orig$edu_low, na.rm = TRUE), mean(simulated_survey_context$edu_low, na.rm = TRUE)))
cat(sprintf("  %-15s %10.3f %10.3f\n", "edu_high",
            mean(orig$edu_high, na.rm = TRUE), mean(simulated_survey_context$edu_high, na.rm = TRUE)))

cat("\nPOI distributions (municipality level, _zero):\n")
sim_mun <- simulated_survey_context |>
  dplyr::group_by(mun_id) |>
  dplyr::summarise(across(any_of(poi_zero_in_data), ~ first(.x)), .groups = "drop")

cat(sprintf("  %-45s %8s %8s | %8s %8s\n", "", "O:mean", "O:sd", "S:mean", "S:sd"))
for (v in poi_zero_in_data) {
  if (v %in% names(orig_mun) & v %in% names(sim_mun)) {
    o <- orig_mun[[v]]; s <- sim_mun[[v]]
    cat(sprintf("  %-45s %8.1f %8.1f | %8.1f %8.1f\n", v,
                mean(o, na.rm = TRUE), sd(o, na.rm = TRUE),
                mean(s, na.rm = TRUE), sd(s, na.rm = TRUE)))
  }
}

cat("\nCorrelations with service_depri:\n")
cat(sprintf("  %-45s %10s %10s\n", "", "Original", "Simulated"))
for (v in cor_vars_for_dv) {
  r_o <- if (v %in% names(orig)) cor(orig$service_depri, orig[[v]], use = "pairwise.complete.obs") else NA
  r_s <- if (v %in% names(simulated_survey_context)) cor(simulated_survey_context$service_depri, simulated_survey_context[[v]], use = "pairwise.complete.obs") else NA
  cat(sprintf("  %-45s %10.4f %10.4f\n", v,
              ifelse(is.na(r_o), NA, r_o), ifelse(is.na(r_s), NA, r_s)))
}

cat("\nNo real IDs in output:",
    !any(c("mun_id8", "mun_id12") %in% names(simulated_survey_context)), "\n")
cat("Sample mun_ids:", head(unique(simulated_survey_context$mun_id), 5), "\n")


# Save Linked Data ----

saveRDS(simulated_survey_context, "./data/simulated_survey_poi.rds")
#simulated_survey_context <- readRDS("./data/simulated_survey_poi.rds")


# Load shapefile and extract real municipality IDs ----

shp <- sf::st_read("./data/municipalites_2021_epsg4326.shp", quiet = TRUE)

cat("Shapefile columns:", paste(names(shp), collapse = ", "), "\n")

# Drop geometry; work with attribute table only
shp_attr <- sf::st_drop_geometry(shp)

# Identify the municipality ID column (AGS is standard for German municipalities)
id_col <- if ("mun_id" %in% names(shp_attr)) {
  "mun_id"
} else {
  # Fall back to the first character/factor column that looks like an ID
  candidates <- names(shp_attr)[sapply(shp_attr, function(x) is.character(x) | is.factor(x))]
  cat("AGS not found; candidate ID columns:", paste(candidates, collapse = ", "), "\n")
  candidates[1]
}

cat("Using municipality ID column:", id_col, "\n\n")

real_mun_ids <- as.character(shp_attr[[id_col]])
real_mun_ids <- unique(real_mun_ids[!is.na(real_mun_ids)])

cat("Real municipality IDs available:", length(real_mun_ids), "\n")


# Replace fake mun_ids with real ones ----

fake_ids <- unique(simulated_survey_context$mun_id)
n_fake   <- length(fake_ids)

if (n_fake > length(real_mun_ids)) {
  stop(sprintf(
    "Not enough real municipality IDs (%d) to replace %d fake IDs.",
    length(real_mun_ids), n_fake
  ))
}

sampled_real_ids <- sample(real_mun_ids, size = n_fake, replace = FALSE)

id_map <- tibble::tibble(
  mun_id      = fake_ids,
  mun_id_real = sampled_real_ids
)

cat(sprintf("Replacing %d fake IDs with %d sampled real IDs\n\n", n_fake, n_fake))

simulated_survey_context <- simulated_survey_context |>
  dplyr::left_join(id_map, by = "mun_id") |>
  dplyr::mutate(mun_id = mun_id_real) |>
  dplyr::select(-mun_id_real)


# Define column groups ----

survey_vars <- c("lfdn", "mun_id", "service_depri",
                 "female", "age", "edu_low", "edu_mid", "edu_high")

osm_vars      <- c("mun_id", grep("^osm_",      names(simulated_survey_context), value = TRUE)) |> (\(x) x[!grepl("_zero$", x)])()
official_vars <- c("mun_id", grep("^official_", names(simulated_survey_context), value = TRUE)) |> (\(x) x[!grepl("_zero$", x)])()


# Create sim_survey (individual-level) ----

sim_survey <- simulated_survey_context |>
  dplyr::select(all_of(survey_vars))

cat("=== sim_survey ===\n")
cat("  Rows:", nrow(sim_survey), "\n")
cat("  Columns:", paste(names(sim_survey), collapse = ", "), "\n\n")


# Create sim_osm_poi (municipality-level, one row per municipality) ----

sim_osm_poi <- simulated_survey_context |>
  dplyr::select(all_of(osm_vars)) |>
  dplyr::distinct(mun_id, .keep_all = TRUE)

cat("=== sim_osm_poi ===\n")
cat("  Rows:", nrow(sim_osm_poi), "\n")
cat("  Columns:", paste(names(sim_osm_poi), collapse = ", "), "\n\n")


# Create sim_official_poi (municipality-level, one row per municipality) ----

sim_official_poi <- simulated_survey_context |>
  dplyr::select(all_of(official_vars)) |>
  dplyr::distinct(mun_id, .keep_all = TRUE)

cat("=== sim_official_poi ===\n")
cat("  Rows:", nrow(sim_official_poi), "\n")
cat("  Columns:", paste(names(sim_official_poi), collapse = ", "), "\n\n")


# Validate ----

cat("=== Validation ===\n")

# All survey mun_ids appear in POI datasets
survey_ids <- unique(sim_survey$mun_id)
cat("Survey mun_ids in sim_osm_poi:     ",
    all(survey_ids %in% sim_osm_poi$mun_id), "\n")
cat("Survey mun_ids in sim_official_poi:",
    all(survey_ids %in% sim_official_poi$mun_id), "\n")

# No fake IDs remain
cat("No fake IDs (mun_*) remaining:     ",
    !any(grepl("^mun_", sim_survey$mun_id)), "\n")

cat("Sample real mun_ids:", head(unique(sim_survey$mun_id), 5), "\n\n")


# Save ----

saveRDS(sim_survey,       "./data/sim_survey.rds")
saveRDS(sim_osm_poi,      "./data/sim_osm_poi.rds")
saveRDS(sim_official_poi, "./data/sim_official_poi.rds")
