# Set work environment
library(dplyr)
library(tidyr)
library(ggplot2)

# Path
getwd()
setwd("outputs_marea_babi_kmeans_random_forest_lulc/outputs_babiDegr1_bloco2_chuvoso_2016-12-06_a_2016-12-06/random_forest")
 
# Read data
file <- "rf34_training_history.csv"
df <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
 
# Keep only metric columns (drop ID, replicate number and geometry)
drop_cols <- c("sample_mangrove", "sample_non_mangrove", "system:index", "training", ".geo")
metrics <- df %>%
  select(-any_of(drop_cols)) %>%
  select(where(is.numeric))

# Reorder columns
metrics <- metrics %>%
  select(
    train_size, test_size, 
    tp, tn, fp, fn, precision_mangrove, precision_non_mangrove, 
    recall_mangrove, recall_non_mangrove, f1_mangrove, f1_non_mangrove, 
    train_accuracy, test_accuracy, test_kappa, mangrove_area_ha, 
    non_mangrove_area_ha
  )

# Boxplots

# Precision
# Select and rename columns
precision_df <- metrics %>%
  select(precision_mangrove, precision_non_mangrove) %>%
  rename(
    "Mangue" = precision_mangrove,
    "Não mangue" = precision_non_mangrove
  )

# Reshape to long format
precision_long <- pivot_longer(precision_df, 
  cols = c("Mangue", "Não mangue"), 
  names_to = "Variable", 
    values_to = "Value")

# Plot side-by-side boxplots for mangrove and non-mangrove
precision_box_plot <- ggplot(
  precision_long, aes(x = Variable, y = Value)
) +
  geom_boxplot(
    fill =  "gray90"
  ) +
  theme_minimal() +
  labs(
    title = "Precisões de 34 modelos Random Forest",
    subtitle = "Treinados com o mosaico 'babiDegr1_bloco2'",
    x = "", 
    y = ""
  ) +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14, color = "gray10"),
    axis.text.x = element_text(size = 10)
  )

# Display
print(precision_box_plot)

# Export
ggsave("rf34_precision.png", precision_box_plot, width = 6, height = 4, dpi = 300)

# Recall

# Select and rename columns
recall_df <- metrics %>%
  select(recall_mangrove, recall_non_mangrove) %>%
  rename(
    "Mangue" = recall_mangrove,
    "Não mangue" = recall_non_mangrove
  )

# Reshape to long format
recall_long <- pivot_longer(recall_df, 
  cols = c("Mangue", "Não mangue"), 
  names_to = "Variable", 
  values_to = "Value"
)

# Plot side-by-side boxplots for mangrove and non-mangrove
recall_box_plot <- ggplot(
  recall_long, aes(x = Variable, y = Value)
) +
  geom_boxplot(
    fill = "gray90"
  ) +
  theme_minimal() +
  labs(
    title = "Recalls de 34 modelos Random Forest",
    subtitle = "Treinados com o mosaico 'babiDegr1_bloco2'",
    x = "", 
    y = ""
  ) +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14, color = "gray10"),
    axis.text.x = element_text(size = 10)
  )

# Display
print(recall_box_plot)

# Export
ggsave("rf34_recall.png", recall_box_plot, width = 6, height = 4, dpi = 300)

# F1-Score

# Select and rename columns
f1_df <- metrics %>%
  select(f1_mangrove, f1_non_mangrove) %>%
  rename(
    "Mangue" = f1_mangrove,
    "Não mangue" = f1_non_mangrove
  )

# Reshape to long format
f1_long <- pivot_longer(f1_df, 
  cols = c("Mangue", "Não mangue"), 
  names_to = "Variable", 
  values_to = "Value"
)

# Calculate min and max bounds stepped by 0.002 to ensure the upper value is included
y_min <- floor(min(f1_long$Value) * 500) / 500
y_max <- ceiling(max(f1_long$Value) * 500) / 500

# Plot side-by-side boxplots for mangrove and non-mangrove
f1_box_plot <- ggplot(
  f1_long, aes(x = Variable, y = Value)
) +
  geom_boxplot(
    fill = "gray90"
  ) +
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 0.002),
    limits = c(y_min, y_max)
  ) +
  theme_minimal() +
  labs(
    title = "F1-scores de 34 modelos Random Forest",
    subtitle = "Treinados com o mosaico 'babiDegr1_bloco2'",
    x = "", 
    y = ""
  ) +
  theme(
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14, color = "gray10"),
    axis.text.x = element_text(size = 10)
  )

# Display
print(f1_box_plot)

# Export
ggsave("rf34_f1.png", f1_box_plot, width = 6, height = 4, dpi = 300)

# Histograms 

# Test Accuracy

# Calculate min and max bounds for the x-axis
x_min <- floor(min(metrics$test_accuracy) * 500) / 500
x_max <- ceiling(max(metrics$test_accuracy) * 500) / 500

# Plot
test_acc_plot <- ggplot(metrics, aes(x = test_accuracy)) + 
  geom_histogram(
    binwidth = 0.001,       # Adjusts bin granularity for precision data
    fill = "gray90",        # Matches your boxplot fill style
    color = "gray40",       # Clean border for the bars
    linewidth = 0.3         # Subtle border thickness
  ) + 
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 0.002),
    limits = c(x_min, x_max)
  ) + 
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)) # Anchors bars to the bottom axis line
  ) +
  theme_minimal() +
  labs(
    title = "Acurácias de teste de 34 modelos Random Forest",
    subtitle = "Treinados com o mosaico 'babiDegr1_bloco2'",
    x = "", 
    y = "" 
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "gray10"),
    axis.text.x = element_text(size = 10, hjust = 1), 
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    panel.grid.minor = element_blank() # Cleans up minor grid lines for a cleaner look
  )

# Display
print(test_acc_plot)

# Export
ggsave("rf34_test_accuracy.png", test_acc_plot, width = 7, height = 4.5, dpi = 300)

# Test Kappa

# Calculate min and max bounds for the x-axis
x_min <- floor(min(metrics$test_kappa) * 500) / 500
x_max <- ceiling(max(metrics$test_kappa) * 500) / 500

# Plot
test_kappa_plot <- ggplot(metrics, aes(x = test_kappa)) + 
  geom_histogram(
    binwidth = 0.001,       # Adjusts bin granularity for decimal scores
    fill = "gray90",        # Matches boxplot fill style
    color = "gray40",       # Clean border for the bars
    linewidth = 0.3         # Subtle border thickness
  ) + 
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 0.004),
    limits = c(x_min, x_max)
  ) + 
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)) # Anchors bars to the bottom axis line
  ) +
  theme_minimal() +
  labs(
    title = "Índices Kappa de teste de 34 modelos Random Forest",
    subtitle = "Treinados com o mosaico 'babiDegr1_bloco2'",
    x = "", 
    y = "" 
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "gray10"),
    axis.text.x = element_text(size = 10, hjust = 1), 
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    panel.grid.minor = element_blank() # Cleans up minor grid lines for a cleaner look
  )

# Display
print(test_kappa_plot)

# Export
ggsave("rf34_test_kappa.png", test_kappa_plot, width = 7, height = 4.5, dpi = 300)

# Mangrove Area (ha)

# Calculate min and max bounds for the y-axis
x_min <- floor(min(metrics$mangrove_area_ha) * 500) / 500
x_max <- ceiling(max(metrics$mangrove_area_ha) * 500) / 500

# Plot (using automatic pretty breaks for area values)
mangrove_area_plot <- ggplot(metrics, aes(x = mangrove_area_ha)) + 
  geom_histogram(
    bins = 15,              # Uses a clean bin count suitable for area distribution
    fill = "gray90",        # Matches your boxplot fill style
    color = "gray40",       # Clean border for the bars
    linewidth = 0.3         # Subtle border thickness
  ) + 
  scale_x_continuous(
    breaks = seq(x_min, x_max, by = 2.15),
    limits = c(x_min, x_max)
  ) +
  scale_y_continuous(
    breaks = seq(0, 8, by = 2),
    limits = c(0, 8)
  ) +
  theme_minimal() +
  labs(
    title = "Áreas de mangue (ha) preditas por 34 modelos Random Forest",
    subtitle = "Treinados com o mosaico 'babiDegr1_bloco2'",
    x = "", 
    y = "" 
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12, color = "gray10"),
    axis.text.x = element_text(size = 10, hjust = 1), 
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    panel.grid.minor = element_blank() # Cleans up minor grid lines for a cleaner look
  )

# Display
print(mangrove_area_plot)

# Export
ggsave("rf34_mangrove_area.png", mangrove_area_plot, width = 7, height = 4.5, dpi = 300)

# Mean + 95% interval (2.5% and 97.5% percentiles across the 34 models)
metrics_means <- metrics %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  summarise(
    n        = sum(!is.na(value)),
    mean     = mean(value, na.rm = TRUE),
    sd       = sd(value, na.rm = TRUE),
    ci_lower = quantile(value, probs = 0.025, na.rm = TRUE, names = FALSE),
    ci_upper = quantile(value, probs = 0.975, na.rm = TRUE, names = FALSE),
    .groups  = "drop"
  ) %>%
  arrange(match(metric, names(metrics)))   # keep original column order
 
print(metrics_means, n = Inf)
 
# Save
write.csv(metrics_means, "rf34_metrics_means.csv", row.names = FALSE)



