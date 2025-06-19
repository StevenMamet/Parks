# Load required libraries
library(data.table)
library(tidyverse)
library(sf)
library(gstat)
library(sp)
library(mgcv)
library(ggridges)
library(gganimate)
library(viridis)
library(transformr)
library(gifski)
library(wesanderson)  # Nice color palettes
library(purrr)
library(sf)          # For reading spatial data
library(stringr)     # String handling (for matching site names)
library(ggspatial)  # Optional: adds scale bar and north arrow
library(viridis)    # For color scale
library(broom)
library(forcats)
library(factoextra)
library(dendextend)

df <- fread("/Users/sdmamet/Desktop/Workspace/Parks/data/parks_microclimate_20041015_20241005_filled.csv")

# ---- Wapusk NP Weather and Snowpack Analysis ----

## ---- Temperature ----

# Temperature trends with depth
df_sum <- df %>% 
  select(YEAR, contains("temp"), -station_temp) %>% 
  group_by(YEAR) %>% 
  summarise(across(mlk_temp_150_avg:rlk_temp_2p5_min, ~mean(.x, na.rm = T))) %>% 
  slice(2:(n()))# %>% 
# ggplot(aes(x = YEAR, y = rlk_temp_150_avg)) + geom_line()

# Function to extract slope and standard error from lm
extract_slope <- function(df, var) {
  model <- lm(reformulate("YEAR", response = "value"), data = df)
  coef_summary <- summary(model)$coefficients
  tibble(
    variable = var,
    slope = coef_summary["YEAR", "Estimate"],
    se = coef_summary["YEAR", "Std. Error"],
    p_value = coef_summary["YEAR", "Pr(>|t|)"]
  )
}

# Compute slopes for each column starting from mlk_temp_150_avg
slopes_df <- df_sum %>%
  select(YEAR, starts_with("mlk_temp_150_avg"):last_col()) %>%  # Adjust range as needed
  pivot_longer(-YEAR, names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  group_split() %>%  # Split data into list of tibbles
  map_dfr(~ extract_slope(.x, unique(.x$variable))) %>%  # Apply function to each tibble
  mutate(
    site = substr(variable,1,3),  # Extract site name
    depth = as.numeric(gsub("p",".",str_extract(variable, "(?<=_)(\\d+p?5?)(?=_)"))),  # Extract depth
    type = str_extract(variable, "[^_]+$")  # Extract last part (type)
  ) %>% 
  mutate(
    depth = ifelse(depth %in% c(0,150), depth, depth*-1), # Adjust sign for depth
    ci_lower = slope - 1.96 * se,  # Approx. 95% CI (assuming large sample)
    ci_upper = slope + 1.96 * se,
    significant = ifelse(ci_lower > 0 | ci_upper < 0, "Yes", "No")
  ) %>% 
  mutate(
    p_adj = p.adjust(p_value, method = "fdr"),  # or "bonferroni"
    significant = ifelse(p_adj < 0.05, "Yes", "No")
  )

# Define custom colors
cust_cols <- wes_palette("Darjeeling1")

custom_colors <- c("Mary" = cust_cols[4], "Roberge" = cust_cols[5])  # Blue & Red

setwd("~/Desktop/Workspace")
slopes_df %>%
  filter(type == "avg") %>%
  mutate(
    site = ifelse(site == "rlk", "Roberge", "Mary"),
    significant = factor(significant, levels = c("No", "Yes"))
  ) %>%
  ggplot(aes(x = depth, y = slope, ymin = ci_lower, ymax = ci_upper, color = site)) +
  geom_line(aes(group = site), alpha = 0.6) +
  geom_pointrange(aes(shape = significant), alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Depth (cm)",
    y = "Temperature change (°C per year)",
    color = "Site:",
    shape = "P < 0.05:"
  ) +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = c(1, 19)) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "italic"),
    legend.background = element_rect(fill = "white")
  )
# ggsave("./Parks/figures/temp_trends_vs_depth.png", height = 5, width = 5, units = "in", dpi = 300)

df[, month := month(date)]  # ensure month is extracted
df[, season := fifelse(month %in% 6:9, "warm", "cool")]
df[, season_year := fifelse(month %in% 10:12, YEAR + 1, YEAR)]

# Reshape temperature columns to long format
temp_cols <- grep("^mlk_temp_|^rlk_temp_", names(df), value = TRUE)

# Long format
long_temp_seasonal <- melt(
  df[, c("season_year", "season", temp_cols), with = FALSE],
  id.vars = c("season_year", "season"),
  variable.name = "var",
  value.name = "temperature"
)

# Compute mean temperature by season, year, and variable
seasonal_means <- long_temp_seasonal[
  !is.na(temperature),
  .(value = mean(temperature, na.rm = TRUE)),
  by = .(season, season_year, var)
]

# Nest by season and variable
seasonal_models <- seasonal_means %>%
  as_tibble() %>%
  group_by(season, var) %>%
  group_split()

# Fit model per group
seasonal_slopes <- map_dfr(seasonal_models, function(df) {
  model <- lm(value ~ season_year, data = df)
  s <- summary(model)$coefficients
  tibble(
    variable = unique(df$var),
    season = unique(df$season),
    slope = s["season_year", "Estimate"],
    se = s["season_year", "Std. Error"],
    p_value = s["season_year", "Pr(>|t|)"]
  )
})

seasonal_slopes <- seasonal_slopes %>%
  mutate(
    p_adj = p.adjust(p_value, method = "fdr"),
    significant = ifelse(p_adj < 0.05, "Yes", "No"),
    site = substr(variable, 1, 3),
    depth = as.numeric(gsub("p", ".", str_extract(variable, "(?<=_)(\\d+p?5?)(?=_)"))),
    type = str_extract(variable, "[^_]+$")
  ) %>%
  filter(type == "avg") %>%
  mutate(
    site = ifelse(site == "rlk", "Roberge", "Mary"),
    depth = ifelse(depth %in% c(0, 150), depth, -depth)
  )

# Add to seasonal_slopes
seasonal_slopes <- seasonal_slopes %>%
  mutate(period = str_to_title(season)) %>%   # "Cool" or "Warm"
  mutate(
    # depth = ifelse(depth %in% c(0,150), depth, depth*-1), # Adjust sign for depth
    ci_lower = slope - 1.96 * se,  # Approx. 95% CI (assuming large sample)
    ci_upper = slope + 1.96 * se,
    significant = ifelse(ci_lower > 0 | ci_upper < 0, "Yes", "No")
  ) 

# Annual trends
# Add site, period, etc., to annual slopes BEFORE binding
annual_slopes <- slopes_df %>%
  filter(type == "avg") %>%
  mutate(
    site = ifelse(substr(variable, 1, 3) == "mlk", "Mary", "Roberge"),
    period = "Annual"
  )

# Combine
combined_slopes <- bind_rows(annual_slopes, seasonal_slopes)
combined_slopes$period <- recode(combined_slopes$period,
                                 "Cool" = "Oct–May",
                                 "Warm" = "Jun–Sep",
                                 "Annual" = "Annual")
combined_slopes$period <- factor(combined_slopes$period, levels = c("Annual", "Oct–May", "Jun–Sep"))

ggplot(combined_slopes, aes(x = depth, y = slope, ymin = ci_lower, ymax = ci_upper,
                            color = site, shape = significant)) +
  geom_hline(yintercept = 0, linetype = 2, color = "gray40") +
  geom_vline(xintercept = 0, linetype = 2, color = "gray40") +
  geom_pointrange(alpha = 0.8, size = 0.4) +
  geom_line(aes(group = interaction(site, period)), alpha = 0.5) +
  facet_wrap(~ period, nrow = 1) + 
  coord_flip() +
  # scale_color_manual(values = custom_colors) +
  scale_color_manual(values = c("Mary" = cust_cols[4], "Roberge" = cust_cols[5])) +
  scale_shape_manual(values = c("No" = 1, "Yes" = 19)) +
  labs(
    x = "Depth (cm)",
    y = "Temperature Trend (°C/year)",
    color = "Site",
    shape = expression(italic(P) < 0.05)
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.background = element_blank(),  # Remove grey bar
    strip.text = element_text(face = "bold", size = 12, hjust = 0),  # Left-align facet text
    legend.position = "top",
    legend.title = element_text(face = "italic"),
    legend.background = element_rect(fill = "white", color = NA)
  )
ggsave("./Parks/figures/temp_trends_vs_depth.png", height = 5, width = 10, units = "in", dpi = 300)

# Ensure data is a data.table
setDT(df)

# Step 0: Add "annual" as an additional season
df[, season := as.character(season)]
df_annual <- copy(df)
df_annual[, season := "annual"]
df_combined <- rbindlist(list(df, df_annual))

# Step 1: Reshape into long format
temp_vars <- c(
  "mlk_temp_150_avg", "mlk_temp_150_max", "mlk_temp_150_min",
  "mlk_temp_0_avg",   "mlk_temp_0_max",   "mlk_temp_0_min",
  "mlk_temp_80_avg",  "mlk_temp_80_max",  "mlk_temp_80_min",
  "rlk_temp_150_avg", "rlk_temp_150_max", "rlk_temp_150_min",
  "rlk_temp_0_avg",   "rlk_temp_0_max",   "rlk_temp_0_min",
  "rlk_temp_80_avg",  "rlk_temp_80_max",  "rlk_temp_80_min"
)

long_temp <- melt(df_combined,
                  measure.vars = temp_vars,
                  variable.name = "var",
                  value.name = "value")

# Step 2: Extract metadata
long_temp[, c("site", "._", "depth", "stat") := tstrsplit(var, "_")]
long_temp[, ._ := NULL]

# Step 3: Reshape wide
temp_wide <- dcast(
  long_temp,
  season + season_year + site + depth + date ~ stat,
  value.var = "value"
)

# Step 4: Summarize
seasonal_summary <- temp_wide[, .(
  mean_temp = mean(avg, na.rm = TRUE),
  min_temp = min(min, na.rm = TRUE),
  max_temp = max(max, na.rm = TRUE)
), by = .(season, season_year, site, depth)]

# Step 5: Format depth as factor (top to bottom)
seasonal_summary[, depth := factor(depth,
                                   levels = c("150", "0", "80"),
                                   labels = c("150", "0", "-80"))]

# Step 6: Recode and order seasons and site names
seasonal_summary <- seasonal_summary %>%
  mutate(
    season = fct_recode(season,
                        "Oct–May" = "cool",
                        "Jun–Sep" = "warm",
                        "Annual" = "annual"),
    season = factor(season, levels = c("Annual", "Oct–May", "Jun–Sep")),
    site = fct_recode(site,
                      "Roberge Lake" = "rlk",
                      "Mary Lake" = "mlk")
  )

# Step 7: Fit linear models
lm_results <- seasonal_summary %>%
  group_by(site, depth, season) %>%
  filter(!is.na(mean_temp)) %>%
  do({
    model <- lm(mean_temp ~ season_year, data = .)
    tidy_model <- tidy(model)
    if ("season_year" %in% tidy_model$term && tidy_model$p.value[tidy_model$term == "season_year"] < 0.05) {
      tibble(
        site = unique(.$site),
        depth = unique(.$depth),
        season = unique(.$season),
        intercept = tidy_model$estimate[tidy_model$term == "(Intercept)"],
        slope = tidy_model$estimate[tidy_model$term == "season_year"]
      )
    } else {
      tibble()
    }
  }) %>% ungroup()

# Step 8: Predict regression lines
pred_lines <- seasonal_summary %>%
  select(season_year, site, depth, season) %>%
  distinct() %>%
  inner_join(lm_results, by = c("site", "depth", "season")) %>%
  mutate(mean_pred = intercept + slope * season_year)

# Step 9: Define color palette
custom_colors <- c(
  "150" = wes_palette("Darjeeling1")[4],
  "0"   = wes_palette("Cavalcanti1")[2],
  "-80" = wes_palette("Darjeeling2")[2]
)

# Step 10: Plot
seasonal_summary %>%
  filter(season_year < 2024) %>%
  ggplot(aes(x = season_year)) +
  # geom_ribbon(aes(ymin = min_temp, ymax = max_temp, fill = depth), alpha = 0.25) +
  geom_line(aes(y = mean_temp, color = depth), linewidth = 0.7, alpha = 0.3) +
  geom_line(data = pred_lines, aes(x = season_year, y = mean_pred, color = depth),
            linetype = 1, linewidth = 1, alpha = 0.7) +
  facet_grid(rows = vars(site), cols = vars(season)) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  labs(
    title = "Temperatures Through Time",
    y = "Temperature (°C)",
    x = "Year",
    fill = "Depth (cm)",
    color = "Depth (cm)"
  ) +
  theme_bw() +
  theme(
    panel.spacing = unit(1, "lines"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
ggsave("./Parks/figures/temp_time_series.png", height = 6, width = 10, units = "in", dpi = 300)

  
  
df %>%
  ggplot(aes(x = date, y = mlk_rain_tot_mm)) + geom_line()
  
df %>%
  filter(rlk_rain_tot_mm < 2000) %>% 
  ggplot(aes(x = date, y = rlk_rain_tot_mm)) + geom_line()

  
## ---- Snowpack ----

df_snow <- read_csv("~/Desktop/Workspace/Parks/data/parks_snow_2006_2024.csv")

df_snow <- df_snow %>% 
  mutate(site = case_when(
    site == "Roberge Island" ~ "Roberge Lake: Tree Island",
    site == "Roberge Crest" ~ "Roberge Lake: Peat Plateau - Polygon Center",
    site == "Roberge Wedge" ~ "Roberge Lake: Peat Plateau - Ice Wedge",
    site == "Broad Ridge" ~ "Broad: Coastal Beach Ridge",
    site == "Broad Sedge" ~ "Broad: Sedge Fen",
    site == "Broad Spruce" ~ "Broad: Spruce Forest",
    site == "Broad Riparian" ~ "Broad: Riparian Forest",
    site == "Broad Shrub" ~ "Broad: Shrub",
    site %in% c("Mary Lake", "Mary Lake Forest") ~ "Mary Lake: Forest",
    site == "Owl Ridge" ~ "Owl: Coastal Beach Ridge",
    site == "Owl Sedge" ~ "Owl: Coastal Fen",
    site == "Owl Spruce" ~ "Owl: Spruce Forest",
    TRUE ~ site
  )) %>% 
  mutate(ecosystem = case_when(
    site %in% c("Roberge Lake: Tree Island","Roberge Island") ~ "Tree island",
    site %in% c("Mary Lake: Forest","Owl: Spruce Forest",
                "Broad: Riparian Forest","Broad: Spruce Forest",
                "Mary Lake Forest","Owl Spruce",
                "Broad Spruce","Mary Lake") ~ "Forest",
    site %in% c("Rupert Ck Riparian willow","Broad: Shrub","Broad Shrub","Broad Riparian") ~ "Shrub",
    site %in% c("Broad Sedge","Owl Sedge",
                "Owl: Coastal Fen","Broad: Sedge Fen") ~ "Sedge",
    site %in% c("Broad Ridge","Owl Ridge",
                "Broad: Coastal Beach Ridge","Owl: Coastal Beach Ridge") ~ "Beach ridge",
    site %in% c("Roberge Lake: Peat Plateau - Polygon Center","Roberge Crest") ~ "Polygon centre",
    site %in% c("Roberge Lake: Peat Plateau - Ice Wedge","Roberge Wedge" ) ~ "Ice wedge",
    site %in% c("South OF Nester 1 north 1km","Wetland N2Kmwest of nester 1") ~ "other",
    .default = NA
  ),
  period = case_when(
    year %in% 2006:2015 ~ "2006-2014",
    year %in% 2016:2024 ~ "2015-2024",
    .default = NA
  )
  ) %>% 
  filter(site != "No Data")

# Helper: Bootstrap mean difference
bootstrap_diff_ci <- function(x1, x2, R = 1000, conf = 0.95) {
  diffs <- replicate(R, {
    mean(sample(x1, replace = TRUE)) - mean(sample(x2, replace = TRUE))
  })
  ci <- quantile(diffs, probs = c((1 - conf)/2, 1 - (1 - conf)/2))
  list(lower = ci[1], upper = ci[2])
}

boot_ci_df <- df_snow %>%
  filter(!is.na(ecosystem), ecosystem != "other") %>%
  group_by(ecosystem) %>%
  summarise(
    swe_ci = list(bootstrap_diff_ci(swe[period == "2015-2024"], swe[period == "2006-2014"])),
    htc_ci = list(bootstrap_diff_ci(htc[period == "2015-2024"], htc[period == "2006-2014"])),
    .groups = "drop"
  ) %>%
  mutate(
    swe_sig = map_lgl(swe_ci, ~ .x$lower > 0 | .x$upper < 0),
    htc_sig = map_lgl(htc_ci, ~ .x$lower > 0 | .x$upper < 0),
    linetype_swe = ifelse(swe_sig, "solid", "dashed"),
    linetype_htc = ifelse(htc_sig, "solid", "dashed")
  )

arrows_df <- df_snow %>%
  filter(!is.na(ecosystem), ecosystem != "other") %>%
  group_by(period, ecosystem) %>%
  summarise(across(depth:htc, ~ mean(., na.rm = T)), .groups = "drop") %>%
  pivot_wider(names_from = period, values_from = c(depth, density, swe, htc)) %>%
  rename(swe_early = `swe_2006-2014`, swe_late = `swe_2015-2024`, htc_early = `htc_2006-2014`, htc_late = `htc_2015-2024`) %>%
  filter(!is.na(swe_early) & !is.na(swe_late))  # Ensure no missing data

# Join P-values and line type info
arrows_df <- arrows_df %>%
  left_join(boot_ci_df %>% select(ecosystem, linetype_swe, linetype_htc), by = "ecosystem")


ecosystems <- df_snow %>%
  filter(!is.na(ecosystem), ecosystem != "other") %>%
  distinct(ecosystem) %>%
  pull(ecosystem)

ecosystem_colors <- setNames(wes_palette("Rushmore1", length(ecosystems)+4, type = "continuous")[c(1,4:9,11)],
                             sort(ecosystems))  # consistent ordering
# ecosystem_colors <- ecosystem_colors[c(1,4:9,11)]

ggplot(df_snow %>%
         filter(!is.na(ecosystem), ecosystem != "other") %>%
         group_by(period, ecosystem) %>%
         summarise(across(depth:htc, ~ mean(., na.rm = TRUE)), .groups = "drop") %>%
         rename(Density = density),
       aes(x = swe, y = htc, color = ecosystem, shape = period, size = Density)) +
  
  geom_point(alpha = 0.6) +
  
  geom_segment(data = arrows_df,
               aes(x = swe_early, y = htc_early,
                   xend = swe_late, yend = htc_late,
                   color = ecosystem,
                   linetype = linetype_swe),
               arrow = arrow(length = unit(0.2, "cm")),
               linewidth = 1,
               inherit.aes = FALSE) +
  
  scale_color_manual(values = ecosystem_colors) +
  scale_shape_manual(values = c("2006-2014" = 1, "2015-2024" = 16)) +
  scale_size(range = c(1, 5)) +
  
  scale_linetype_identity() +  # This suppresses the linetype legend
  guides(linetype = "none") +
  
  theme_bw() +
  labs(
    x = "Snow Water Equivalent (mm)",
    y = bquote(Heat~Transfer~Coefficient~"(W m"^-2~K^-1*")"),
    color = "Ecosystem",
    shape = "Period"
  ) +
  theme(legend.position = "right")
ggsave("./Parks/figures/snow_htc_swe.png", height = 5, width = 7, units = "in", dpi = 300)


snow_summary <- df_snow %>%
  filter(site != "Wetland N2Kmwest of nester 1") %>% 
  filter(site != "South OF Nester 1 north 1km") %>% 
  group_by(site, period, ecosystem) %>%
  summarise(
    swe = mean(swe, na.rm = TRUE),
    htc = mean(htc, na.rm = TRUE),
    density = mean(density, na.rm = TRUE),
    .groups = "drop"
  )

snow_summary1 <- df_snow %>%
  filter(site != "Wetland N2Kmwest of nester 1") %>% 
  filter(site != "South OF Nester 1 north 1km") %>% 
  filter(site != "Rupert Ck Riparian willow") %>% 
  group_by(ecosystem) %>%
  summarise(
    depth_mean = mean(depth, na.rm = TRUE),
    swe_mean = mean(swe, na.rm = TRUE),
    htc_mean = mean(htc, na.rm = TRUE),
    density_mean = mean(density, na.rm = TRUE),
    depth_sd = sd(depth, na.rm = TRUE),
    swe_sd = sd(swe, na.rm = TRUE),
    htc_sd = sd(htc, na.rm = TRUE),
    density_sd = sd(density, na.rm = TRUE),
    .groups = "drop"
  )


# Focus on depth
metric_to_plot <- "depth"

# Prep empirical data
empirical <- df_snow %>%
  filter(!is.na(ecosystem), !is.na(.data[[metric_to_plot]]), ecosystem != "other") %>%
  select(ecosystem, value = all_of(metric_to_plot)) %>%
  mutate(metric = metric_to_plot)

# Compute empirical density manually per ecosystem
empirical_densities <- empirical %>%
  group_by(ecosystem) %>%
  summarise(density_data = list(density(value, na.rm = TRUE))) %>%
  mutate(
    x = map(density_data, ~ .x$x),
    y = map(density_data, ~ .x$y)
  ) %>%
  select(ecosystem, x, y) %>%
  unnest(c(x, y))

# Now get max y per ecosystem
empirical_max <- empirical_densities %>%
  group_by(ecosystem) %>%
  summarise(max_density = max(y, na.rm = TRUE))

# Prep theoretical normal curves
normal_curves <- snow_summary1 %>%
  select(ecosystem, mean = depth_mean, sd = depth_sd) %>%
  mutate(metric = metric_to_plot) %>%
  rowwise() %>%
  mutate(x = list(seq(mean - 4*sd, mean + 4*sd, length.out = 200)),
         y = list(dnorm(x, mean, sd))) %>%
  unnest(c(x, y))

# # Prep theoretical normal curves
# normal_curves <- normal_curves %>%
#   left_join(empirical_max, by = "ecosystem") %>%
#   mutate(y_scaled = y * max_density)

# Add calculated thresholds directly to snow_summary1
snow_summary1 <- snow_summary1 %>%
  mutate(
    depth_thresh_low = pmax(0, depth_mean - 3 * depth_sd),  # never below 0
    depth_thresh_high = depth_mean + 3 * depth_sd
  )

# Plot both
ggplot() +
  geom_density(data = empirical, aes(x = value, fill = ecosystem),
               alpha = 0.3, color = NA) +
  geom_line(data = normal_curves, aes(x = x, y = y, color = ecosystem),
            linewidth = 1) +
  geom_vline(data = snow_summary1,
             aes(xintercept = depth_mean, color = ecosystem), linetype = "solid") +
  geom_vline(data = snow_summary1,
             aes(xintercept = depth_thresh_high, color = ecosystem), linetype = "dashed") +
  geom_vline(data = snow_summary1,
             aes(xintercept = depth_thresh_low, color = ecosystem), linetype = "dashed") +
  facet_wrap(~ ecosystem, scales = "free") +
  labs(
    x = "Snow Depth (cm)",
    y = "Density",
    title = "Empirical and Theoretical Snow Depth Distributions by Ecosystem",
    subtitle = "Dashed lines show ±3σ thresholds (lower capped at 0); shading = empirical density; lines = theoretical density",
    fill = "Ecosystem",
    color = "Ecosystem"
  ) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )
ggsave("./Parks/figures/snow_depth_threshold.png", height = 8, width = 10, units = "in", dpi = 300)


threshold_summary <- snow_summary1 %>%
  select(ecosystem, depth_thresh_low, depth_mean, depth_thresh_high) %>%
  arrange(ecosystem)



snow_scaled <- snow_summary %>%
  mutate(across(c(swe, htc, density), scale))

dist_matrix <- dist(snow_scaled[, c("swe", "htc", "density")])
hc <- hclust(dist_matrix, method = "ward.D2")

# Cut into 3 clusters
snow_scaled$cluster <- factor(cutree(hc, k = 3))

plot(hc, labels = snow_scaled$site)
rect.hclust(hc, k = 3, border = "red")


# Step 1: Determine best k
fviz_nbclust(snow_scaled[, c("swe", "htc", "density")], kmeans, method = "wss")

library(cluster)

set.seed(123)
gap_stat <- clusGap(
  snow_scaled[, c("swe", "htc", "density")],
  FUN = hcut,
  K.max = 10,
  B = 100
)

fviz_gap_stat(gap_stat) +
  labs(title = "Gap Statistic for Optimal Clusters")

# Step 2: Cut dendrogram with chosen k
optimal_k <- 3  # <- replace with chosen k from elbow/silhouette
snow_scaled$cluster <- factor(cutree(hc, k = optimal_k))

# Step 3: Plot dendrogram
dend <- as.dendrogram(hc)
dend %>%
  color_branches(k = optimal_k) %>%
  plot(horiz = FALSE, main = "Clustered Sites", ylab = NULL)

library(vegan)

# Option 1: Test by Period
adonis2(snow_scaled[, c("swe", "htc", "density")] ~ period,
        data = snow_scaled, method = "euclidean")

# Option 2: Test by Ecosystem
adonis2(snow_scaled[, c("swe", "htc", "density")] ~ ecosystem,
        data = snow_scaled, method = "euclidean")

# Option 3: Test interaction
adonis2(snow_scaled[, c("swe", "htc", "density")] ~ period * ecosystem,
        data = snow_scaled, method = "euclidean")

#_______________________________
# Spatial snow analyses ----
# Read KML into an sf object
kml_path <- "~/Desktop/Workspace/Parks/data/Wapusk_sites.kml"
kml_sites <- st_read(kml_path) %>% 
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) %>% 
  rename(site = Name) %>% 
  mutate(site = case_when(
    site == "Mary Lake Weather Station" ~ "Mary Lake: Forest",
    site == "Broad River Riparian Forest" ~ "Broad: Riparian Forest",
    site == "Broad River Shrub" ~ "Broad: Shrub",
    site == "Broad River Spruce Forest" ~ "Broad: Spruce Forest",
    site == "Broad River Beachridge" ~ "Broad: Coastal Beach Ridge",
    site == "Broad River Fen" ~ "Broad: Sedge Fen",
    site == "Owl River Beachridge" ~ "Owl: Coastal Beach Ridge",
    site == "Owl River Forest" ~ "Owl: Spruce Forest",
    site == "Owl River Sedge Fen" ~ "Owl: Coastal Fen",
    site == "Roberge Lake Ice Wedge" ~ "Roberge Lake: Peat Plateau - Ice Wedge",
    site == "Roberge Tree Island"          ~ "Roberge Lake: Tree Island",
    site == "Roberge Lake Polygon Centre"  ~ "Roberge Lake: Peat Plateau - Polygon Center",
    TRUE ~ site
  ))

# Join coordinates to snow data
df_snow_coords <- df_snow %>% left_join(kml_sites %>% select(site, lat, lon), by = "site") %>% filter(!is.na(lat))

# Convert to sf object
snow_sf <- st_as_sf(df_snow_coords, coords = c("lon", "lat"), crs = 4326)

# Example for SWE trend per site
swe_trends <- df_snow_coords %>%
  filter(!is.na(swe)) %>%
  group_by(site, lat, lon) %>%
  do({
    mod = lm(swe ~ year, data = .)
    tidy(mod)[2, ]  # row 2 is the slope for 'year'
  }) %>%
  ungroup() %>%
  rename(slope = estimate, p_value = p.value, std_error = std.error) %>%
  mutate(metric = "SWE")

# Repeat for HTC
htc_trends <- df_snow_coords %>%
  filter(!is.na(htc)) %>%
  group_by(site, lat, lon) %>%
  do({
    mod = lm(htc ~ year, data = .)
    tidy(mod)[2, ]
  }) %>%
  ungroup() %>%
  rename(slope = estimate, p_value = p.value, std_error = std.error) %>%
  mutate(metric = "HTC")

# Combine both
trend_df <- bind_rows(swe_trends, htc_trends) %>%
  mutate(sig = ifelse(p_value < 0.05, "Yes", "No"))

ggplot(trend_df, aes(x = lon, y = lat)) +
  borders("world", xlim = range(trend_df$lon), ylim = range(trend_df$lat), fill = "grey95") +
  geom_point(aes(size = abs(slope), fill = sig, shape = sig), 
             color = "black", alpha = 0.8) +
  scale_shape_manual(values = c("Yes" = 21, "No" = 1)) +
  scale_fill_manual(values = c("Yes" = "firebrick", "No" = "white")) +
  scale_size_continuous(name = "Slope (°C/year)", range = c(2, 10)) +
  facet_wrap(~ metric) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "Trend in Snowpack Variables (2006–2024)",
    subtitle = "Bubble size = Slope; Fill = Significance",
    x = "Longitude", y = "Latitude",
    fill = "P < 0.05", shape = "P < 0.05"
  )

# # Plot SWE with HTC as fill
# ggplot() +
#   geom_sf(data = snow_sf, aes(size = swe, fill = htc), shape = 21, color = "black", alpha = 0.8) +
#   scale_size_continuous(name = "SWE (mm)", range = c(2, 10)) +
#   scale_fill_viridis_c(name = "HTC\n(W/m²·K)", option = "plasma") +
#   coord_sf(expand = FALSE) +
#   theme_minimal(base_size = 12) +
#   annotation_scale(location = "bl", width_hint = 0.3) +
#   annotation_north_arrow(location = "bl", which_north = "true", 
#                          style = north_arrow_minimal()) +
#   labs(
#     title = "Snowpack Characteristics by Ecosystem",
#     subtitle = "Bubble size = SWE; Color = HTC",
#     x = "Longitude",
#     y = "Latitude"
#   )


library(leaflet)

leaflet(trend_df %>% filter(metric == "HTC")) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = ~scales::rescale(abs(slope), to = c(4, 10)),
    color = "black",
    fillColor = ~ifelse(sig == "Yes", "firebrick", "white"),
    fillOpacity = 0.8,
    weight = 1,
    popup = ~paste0(
      "<b>Site:</b> ", site, "<br>",
      "<b>Metric:</b> ", metric, "<br>",
      "<b>Slope:</b> ", round(slope, 3), "<br>",
      "<b>p:</b> ", signif(p_value, 3)
    )
  )
