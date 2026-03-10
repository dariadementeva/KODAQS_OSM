# ============================================================================
# Compare simulated vs. original survey_context
# ============================================================================

library(dplyr)
library(ggplot2)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Define variables ----

poi_names_clean <- c(
  "count_osm_hosp_clean", "count_official_hosp_clean",
  "count_osm_school_clean", "count_official_school_clean",
  "count_osm_super_clean", "count_official_super_clean"
)

poi_names_zero <- paste0(poi_names_clean, "_zero")

## Extract original data ----

# survey_context.RDS is created in original script with original geocoded data


orig <- survey_context |>
  dplyr::select(
    lfdn, mun_id, service_depri,
    female, age, edu_low, edu_mid, edu_high,
    any_of(poi_names_clean), any_of(poi_names_zero)
  )


sim_survey   <- readRDS("./data/sim_survey.rds")
sim_osm      <- readRDS("./data/sim_osm_poi.rds") # or 'counts_osm_wide'
sim_official <- readRDS("./data/sim_official_poi.rds") # or 'counts_bund_wide'

sim <- sim_survey |>
  dplyr::left_join(sim_osm,      by = "mun_id") |>
  dplyr::left_join(sim_official, by = "mun_id")


# --- 1. Sample size and cluster structure ----

cat("=== Sample structure ===\n")
cat(sprintf("%-30s %10s %10s\n", "", "Original", "Simulated"))
cat(sprintf("%-30s %10d %10d\n", "N respondents", nrow(orig), nrow(sim)))
cat(sprintf("%-30s %10d %10d\n", "N municipalities",
            length(unique(na.omit(orig$mun_id))),
            length(unique(na.omit(sim$mun_id)))))

cat("\nCluster sizes (original):\n")
print(summary(as.numeric(table(orig$mun_id))))
cat("\nCluster sizes (simulated):\n")
print(summary(as.numeric(table(sim$mun_id))))


# --- 2. DV distribution ----

cat("\n=== service_depri ===\n")
cat("Original:\n")
print(round(prop.table(table(orig$service_depri, useNA = "ifany")), 3))
cat("Simulated:\n")
print(round(prop.table(table(sim$service_depri, useNA = "ifany")), 3))


# --- 3. Demographics ----

cat("\n=== Demographics ===\n")
cat(sprintf("%-20s %12s %12s\n", "", "Original", "Simulated"))
cat(sprintf("%-20s %12.3f %12.3f\n", "female",
            mean(orig$female, na.rm = TRUE), mean(sim$female, na.rm = TRUE)))
cat(sprintf("%-20s %12.1f %12.1f\n", "age mean",
            mean(orig$age, na.rm = TRUE), mean(sim$age, na.rm = TRUE)))
cat(sprintf("%-20s %12.1f %12.1f\n", "age sd",
            sd(orig$age, na.rm = TRUE), sd(sim$age, na.rm = TRUE)))
cat(sprintf("%-20s %12.3f %12.3f\n", "edu_low",
            mean(orig$edu_low, na.rm = TRUE), mean(sim$edu_low, na.rm = TRUE)))
cat(sprintf("%-20s %12.3f %12.3f\n", "edu_mid",
            mean(orig$edu_mid, na.rm = TRUE), mean(sim$edu_mid, na.rm = TRUE)))
cat(sprintf("%-20s %12.3f %12.3f\n", "edu_high",
            mean(orig$edu_high, na.rm = TRUE), mean(sim$edu_high, na.rm = TRUE)))


# --- 4. Missingness ----

cat("\n=== Missingness rates ===\n")
miss_vars <- c("service_depri", "age", "female")
cat(sprintf("%-20s %12s %12s\n", "", "Original", "Simulated"))
for (v in miss_vars) {
  cat(sprintf("%-20s %11.1f%% %11.1f%%\n", v,
              mean(is.na(orig[[v]])) * 100,
              mean(is.na(sim[[v]])) * 100))
}


# --- 5. POI count distributions ----

cat("\n=== POI count distributions (municipality level, _zero variants) ===\n")
poi_vars <- c("count_osm_hosp_clean_zero", "count_official_hosp_clean_zero",
              "count_osm_school_clean_zero", "count_official_school_clean_zero",
              "count_osm_super_clean_zero", "count_official_super_clean_zero")

# Aggregate to municipality level for fair comparison
orig_mun <- orig |>
  dplyr::group_by(mun_id) |>
  dplyr::summarise(across(any_of(poi_vars), ~ first(.x)), .groups = "drop")

sim_mun <- sim |>
  dplyr::group_by(mun_id) |>
  dplyr::summarise(across(any_of(poi_vars), ~ first(.x)), .groups = "drop")

cat(sprintf("\n%-35s %8s %8s %8s | %8s %8s %8s\n",
            "", "O:mean", "O:sd", "O:med", "S:mean", "S:sd", "S:med"))
for (v in poi_vars) {
  if (v %in% names(orig_mun) & v %in% names(sim_mun)) {
    o <- orig_mun[[v]]; s <- sim_mun[[v]]
    cat(sprintf("%-35s %8.1f %8.1f %8.0f | %8.1f %8.1f %8.0f\n", v,
                mean(o, na.rm = TRUE), sd(o, na.rm = TRUE), median(o, na.rm = TRUE),
                mean(s, na.rm = TRUE), sd(s, na.rm = TRUE), median(s, na.rm = TRUE)))
  }
}


# --- 6. Correlations with DV ----

cat("\n=== Correlations with service_depri ===\n")
cor_vars <- c("age", "female", "edu_low", "edu_high", poi_vars)
cat(sprintf("%-45s %10s %10s\n", "", "Original", "Simulated"))
for (v in cor_vars) {
  r_orig <- if (v %in% names(orig)) cor(orig$service_depri, orig[[v]], use = "pairwise.complete.obs") else NA
  r_sim  <- if (v %in% names(sim))  cor(sim$service_depri, sim[[v]], use = "pairwise.complete.obs") else NA
  if (!is.na(r_orig) | !is.na(r_sim)) {
    cat(sprintf("%-45s %10.4f %10.4f\n", v,
                ifelse(is.na(r_orig), NA, r_orig),
                ifelse(is.na(r_sim), NA, r_sim)))
  }
}


# --- 7. Visual comparison: DV distribution ----

bind_rows(
  orig |> dplyr::transmute(service_depri, source = "Original"),
  sim  |> dplyr::transmute(service_depri, source = "Simulated")
) |>
  dplyr::filter(!is.na(service_depri)) |>
  ggplot(aes(x = factor(service_depri), fill = source)) +
  geom_bar(position = "dodge", aes(y = after_stat(prop), group = source)) +
  scale_fill_manual(values = c(Original = "#014AAD", Simulated = "#CBDDFE")) +
  labs(x = "service_depri", y = "Proportion", title = "DV Distribution: Original vs. Simulated") +
  theme_minimal()


# --- 8. Visual comparison: Age distribution ----

bind_rows(
  orig |> dplyr::transmute(age, source = "Original"),
  sim  |> dplyr::transmute(age, source = "Simulated")
) |>
  dplyr::filter(!is.na(age)) |>
  ggplot(aes(x = age, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(Original = "#014AAD", Simulated = "#CBDDFE")) +
  labs(x = "Age", y = "Density", title = "Age Distribution: Original vs. Simulated") +
  theme_minimal()


# --- 9. Visual comparison: Cluster sizes ----

bind_rows(
  tibble(cluster_size = as.numeric(table(orig$mun_id)), source = "Original"),
  tibble(cluster_size = as.numeric(table(sim$mun_id)), source = "Simulated")
) |>
  ggplot(aes(x = cluster_size, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c(Original = "#014AAD", Simulated = "#CBDDFE")) +
  labs(x = "Respondents per municipality", y = "Density",
       title = "Cluster Size Distribution: Original vs. Simulated") +
  theme_minimal()

# --- 10. Running models ---

# Define predictor pairs: OSM vs Official
predictors <- list(
  "Hospitals"    = c(osm = "count_osm_hosp_clean_zero",   official = "count_official_hosp_clean_zero"),
  "Schools"      = c(osm = "count_osm_school_clean_zero", official = "count_official_school_clean_zero"),
  "Supermarkets" = c(osm = "count_osm_super_clean_zero",  official = "count_official_super_clean_zero")
)

# Run models for both datasets
run_models <- function(data, data_label, mun_var = "mun_id") {
  purrr::imap_dfr(predictors, function(vars, label) {
    purrr::imap_dfr(vars, function(v, source) {
      formula <- as.formula(paste0(
        "service_depri ~ ", v, " + age + female + edu_low + edu_high + (1 | ", mun_var, ")"
      ))
      m <- lmer(formula, data = data, REML = FALSE)
      broom.mixed::tidy(m, conf.int = TRUE) |>
        dplyr::filter(term == v) |>
        dplyr::mutate(poi_type = label, source = source, dataset = data_label)
    })
  })
}

results <- dplyr::bind_rows(
  run_models(orig, "Original", "mun_id"),
  run_models(sim,  "Simulated", "mun_id")
)

# Plot
ggplot(results, aes(x = poi_type, y = estimate, color = source, shape = dataset)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.5), size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c(osm = "#CBDDFE", official = "#014AAD"),
    labels = c(osm = "OSM", official = "Official (POI-Bund)")
  ) +
  scale_shape_manual(values = c(Original = 16, Simulated = 17)) +
  labs(
    title = "Effect of Public Service Provision on Perceived Infrastructural Deprivation",
    subtitle = "Multilevel models: Original (circles) vs. Simulated (triangles)",
    x = NULL,
    y = "Coefficient estimate (95% CI)",
    color = "Data source",
    shape = "Dataset"
  ) +
  theme_minimal() +
  facet_wrap(~ dataset) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("./figures/coef_plot_original_vs_simulated.png",
       width = 10, height = 6, dpi = 300)
