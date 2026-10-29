
library(tidyverse)

input_file <- file.path("Data", "MTG_data_2026.csv")
output_dir <- file.path("Outputs", "02_creature_power")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Run this script from the Magic the Gathering in The Classroom project folder.")
}

color_levels <- c("Black", "Blue", "Green", "Red", "White")
set_levels <- c("Core 2021", "Ikoria: Lair of Behemoths", "Zendikar Rising")

color_palette <- c(
  Black = "#45434A",
  Blue = "#3D83B8",
  Green = "#43946A",
  Red = "#CC5B4D",
  White = "#E6DCC8"
)

point_palette <- color_palette
point_palette["White"] <- "#BBA67B"

variable_power_cards <- c(
  "Ashaya, Soul of the Wild",
  "Kinetic Augur",
  "Nighthawk Scavenger",
  "Scourge of the Skyclaves"
)

cards <- read_csv(input_file, show_col_types = FALSE)

stopifnot(all(c(
  "set", "card_name", "card_type", "n_colors",
  "color", "power", "toughness", "CMC"
) %in% names(cards)))

eligible <- cards %>%
  mutate(
    set = str_squish(set),
    card_name = str_squish(card_name),
    card_type = str_squish(card_type),
    color = str_to_title(str_squish(color))
  ) %>%
  filter(
    card_type == "Creature",
    n_colors == 1,
    color %in% color_levels,
    set %in% set_levels
  ) %>%
  mutate(
    power_original = power,
    toughness_original = toughness,
    CMC_original = CMC,
    power = case_when(
      card_name == "Gloom Sower" & is.na(power) ~ 8,
      card_name == "Pyroceratops" & is.na(power) ~ 2,
      card_name == "Skyclave Geopede" & is.na(power) ~ 3,
      TRUE ~ power
    ),
    toughness = case_when(
      card_name == "Gloom Sower" & is.na(toughness) ~ 6,
      card_name == "Pyroceratops" & is.na(toughness) ~ 3,
      card_name == "Skyclave Geopede" & is.na(toughness) ~ 1,
      TRUE ~ toughness
    ),
    CMC = if_else(
      card_name == "Magmatic Channeler" & CMC == 0,
      2,
      CMC
    ),
    variable_power = card_name %in% variable_power_cards
  )

design_conflicts <- eligible %>%
  group_by(set, card_name) %>%
  summarise(
    n_color = n_distinct(color),
    n_power = n_distinct(power, na.rm = FALSE),
    n_toughness = n_distinct(toughness, na.rm = FALSE),
    n_CMC = n_distinct(CMC, na.rm = FALSE),
    .groups = "drop"
  ) %>%
  filter(
    n_color > 1 |
      n_power > 1 |
      n_toughness > 1 |
      n_CMC > 1
  )

write_csv(
  design_conflicts,
  file.path(output_dir, "02_design_conflicts.csv")
)

if (nrow(design_conflicts) > 0) {
  stop("Conflicting card attributes found. Inspect 02_design_conflicts.csv.")
}

candidate_designs <- eligible %>%
  distinct(set, card_name, .keep_all = TRUE)

data_review <- candidate_designs %>%
  filter(
    variable_power |
      (is.na(power_original) & !is.na(power)) |
      (is.na(toughness_original) & !is.na(toughness)) |
      (!is.na(CMC_original) & CMC_original != CMC)
  ) %>%
  select(
    set, card_name, color, variable_power,
    power_original, power,
    toughness_original, toughness,
    CMC_original, CMC
  )

creatures <- candidate_designs %>%
  filter(
    !variable_power,
    !is.na(power),
    !is.na(CMC)
  ) %>%
  mutate(
    set = factor(set, levels = set_levels),
    color = factor(color, levels = color_levels)
  ) %>%
  arrange(set, color, card_name)

stopifnot(
  nrow(creatures) > 0,
  all(creatures$CMC > 0)
)

qc <- tibble(
  measure = c(
    "All card observations",
    "Eligible single-color creature observations",
    "Unique candidate creature designs",
    "Designs with verified data corrections",
    "Designs excluded for variable printed power",
    "Designs in primary analysis",
    "Remaining missing power or CMC"
  ),
  n = c(
    nrow(cards),
    nrow(eligible),
    nrow(candidate_designs),
    nrow(data_review %>% filter(!variable_power)),
    sum(candidate_designs$variable_power),
    nrow(creatures),
    sum(is.na(creatures$power) | is.na(creatures$CMC))
  )
)

raw_means <- creatures %>%
  group_by(color) %>%
  summarise(
    n = n(),
    mean_power = mean(power),
    sd_power = sd(power),
    mean_toughness = mean(toughness, na.rm = TRUE),
    mean_CMC = mean(CMC),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_power / sqrt(n),
    ci_lower = mean_power - qt(0.975, n - 1) * se,
    ci_upper = mean_power + qt(0.975, n - 1) * se
  )

raw_null <- lm(power ~ 1, data = creatures)
raw_model <- lm(power ~ color, data = creatures)

base_model <- lm(
  power ~ CMC + set,
  data = creatures
)

additive_model <- lm(
  power ~ CMC + color + set,
  data = creatures
)

interaction_model <- lm(
  power ~ CMC * color + set,
  data = creatures
)

test_row <- function(name, comparison) {
  tibble(
    analysis = name,
    n = nrow(creatures),
    df_num = comparison$Df[2],
    df_den = comparison$Res.Df[2],
    F = comparison$F[2],
    p_value = comparison[["Pr(>F)"]][2],
    partial_r2 = (
      comparison$RSS[1] - comparison$RSS[2]
    ) / comparison$RSS[1]
  )
}

model_tests <- bind_rows(
  test_row(
    "Unadjusted color",
    anova(raw_null, raw_model)
  ),
  test_row(
    "Color adjusted for CMC and set",
    anova(base_model, additive_model)
  ),
  test_row(
    "Color by CMC interaction",
    anova(additive_model, interaction_model)
  ),
  test_row(
    "Color terms jointly",
    anova(base_model, interaction_model)
  )
)

average_design <- function(color_value, cmc_value) {
  reference <- tibble(
    color = factor(
      rep(color_value, length(set_levels)),
      levels = color_levels
    ),
    set = factor(
      set_levels,
      levels = set_levels
    ),
    CMC = rep(cmc_value, length(set_levels))
  )
  
  colMeans(
    model.matrix(
      ~ CMC * color + set,
      data = reference
    )
  )
}

beta <- coef(interaction_model)
variance <- vcov(interaction_model)
t_value <- qt(0.975, df.residual(interaction_model))
reference_CMC <- 3

adjusted_design <- do.call(
  rbind,
  lapply(
    color_levels,
    function(cl) average_design(cl, reference_CMC)
  )
)

rownames(adjusted_design) <- color_levels

stopifnot(identical(
  colnames(adjusted_design),
  names(beta)
))

adjusted_means <- tibble(
  color = factor(color_levels, levels = color_levels),
  CMC = reference_CMC,
  mean_power = as.numeric(adjusted_design %*% beta),
  se = sqrt(
    rowSums(
      (adjusted_design %*% variance) * adjusted_design
    )
  )
) %>%
  mutate(
    ci_lower = mean_power - t_value * se,
    ci_upper = mean_power + t_value * se
  )

pairwise_results <- map_dfr(
  combn(color_levels, 2, simplify = FALSE),
  function(pair) {
    contrast <- adjusted_design[pair[1], ] -
      adjusted_design[pair[2], ]
    
    difference <- sum(contrast * beta)
    
    se <- sqrt(as.numeric(
      t(contrast) %*% variance %*% contrast
    ))
    
    t_statistic <- difference / se
    
    tibble(
      color_1 = pair[1],
      color_2 = pair[2],
      CMC = reference_CMC,
      difference = difference,
      ci_lower = difference - t_value * se,
      ci_upper = difference + t_value * se,
      t = t_statistic,
      df = df.residual(interaction_model),
      p_value = 2 * pt(
        -abs(t_statistic),
        df.residual(interaction_model)
      )
    )
  }
) %>%
  mutate(
    p_holm = p.adjust(p_value, method = "holm")
  ) %>%
  arrange(p_holm)

slope_design <- do.call(
  rbind,
  lapply(
    color_levels,
    function(cl) {
      average_design(cl, 4) - average_design(cl, 3)
    }
  )
)

power_slopes <- tibble(
  color = factor(color_levels, levels = color_levels),
  power_per_additional_mana = as.numeric(
    slope_design %*% beta
  ),
  se = sqrt(
    rowSums(
      (slope_design %*% variance) * slope_design
    )
  )
) %>%
  mutate(
    ci_lower = power_per_additional_mana - t_value * se,
    ci_upper = power_per_additional_mana + t_value * se
  )

compare_interaction <- function(data, quadratic = FALSE) {
  if (quadratic) {
    reduced <- lm(
      power ~ CMC + I(CMC^2) + color + set,
      data = data
    )
  } else {
    reduced <- lm(
      power ~ CMC + color + set,
      data = data
    )
  }
  
  full <- update(reduced, . ~ . + CMC:color)
  comparison <- anova(reduced, full)
  
  tibble(
    n = nrow(data),
    df_num = comparison$Df[2],
    df_den = comparison$Res.Df[2],
    F = comparison$F[2],
    p_value = comparison[["Pr(>F)"]][2]
  )
}

sensitivity_tests <- bind_rows(
  compare_interaction(creatures) %>%
    mutate(model = "Primary"),
  compare_interaction(creatures, quadratic = TRUE) %>%
    mutate(model = "Quadratic CMC"),
  compare_interaction(
    filter(creatures, CMC <= 7)
  ) %>%
    mutate(model = "CMC <= 7"),
  compare_interaction(
    filter(creatures, power > 0)
  ) %>%
    mutate(model = "Power > 0")
) %>%
  select(model, everything())

model_diagnostics <- creatures %>%
  transmute(
    set,
    card_name,
    color,
    CMC,
    power,
    fitted = as.numeric(fitted(interaction_model)),
    residual = as.numeric(residuals(interaction_model)),
    standardized_residual = as.numeric(
      rstandard(interaction_model)
    ),
    cooks_distance = as.numeric(
      cooks.distance(interaction_model)
    )
  )

figure_data <- bind_rows(
  raw_means %>%
    transmute(
      color,
      mean_power,
      ci_lower,
      ci_upper,
      analysis = "Unadjusted mean"
    ),
  adjusted_means %>%
    transmute(
      color,
      mean_power,
      ci_lower,
      ci_upper,
      analysis = "Adjusted at CMC = 3"
    )
) %>%
  mutate(
    analysis = factor(
      analysis,
      levels = c(
        "Unadjusted mean",
        "Adjusted at CMC = 3"
      )
    )
  )

figure_s2 <- ggplot(
  figure_data,
  aes(x = color, y = mean_power)
) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    linewidth = 0.55,
    color = "grey25"
  ) +
  geom_point(
    aes(fill = color),
    size = 4,
    shape = 21,
    color = "black",
    stroke = 0.45
  ) +
  facet_wrap(~ analysis, nrow = 1) +
  scale_fill_manual(
    values = color_palette,
    guide = "none"
  ) +
  scale_y_continuous(
    breaks = 0:5,
    expand = expansion(mult = c(0.04, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 4)) +
  labs(
    x = "Creature color",
    y = "Mean printed creature power"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(
      fill = "grey95",
      color = NA
    ),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.spacing = unit(1.1, "lines")
  )

prediction_grid <- map_dfr(
  color_levels,
  function(cl) {
    subset <- creatures %>%
      filter(color == cl)
    
    tibble(
      color = factor(
        rep(cl, 101),
        levels = color_levels
      ),
      CMC = seq(
        min(subset$CMC),
        max(subset$CMC),
        length.out = 101
      )
    )
  }
)

prediction_design <- do.call(
  rbind,
  map2(
    as.character(prediction_grid$color),
    prediction_grid$CMC,
    average_design
  )
)

prediction_data <- prediction_grid %>%
  mutate(
    estimate = as.numeric(prediction_design %*% beta),
    se = sqrt(
      rowSums(
        (prediction_design %*% variance) *
          prediction_design
      )
    ),
    ci_lower = estimate - t_value * se,
    ci_upper = estimate + t_value * se
  )

figure_2 <- ggplot() +
  geom_ribbon(
    data = prediction_data,
    aes(
      x = CMC,
      ymin = ci_lower,
      ymax = ci_upper
    ),
    fill = "grey75",
    alpha = 0.5
  ) +
  geom_point(
    data = creatures,
    aes(
      x = CMC,
      y = power,
      fill = color
    ),
    position = position_jitter(
      width = 0.08,
      height = 0.08
    ),
    size = 1.8,
    shape = 21,
    alpha = 0.6,
    color = "grey40",
    stroke = 0.2
  ) +
  geom_line(
    data = prediction_data,
    aes(x = CMC, y = estimate),
    color = "black",
    linewidth = 0.8
  ) +
  facet_wrap(~ color, nrow = 1) +
  scale_fill_manual(
    values = point_palette,
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, 2)
  ) +
  labs(
    x = "Mana value (CMC)",
    y = "Printed creature power"
  ) +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(
      fill = "grey95",
      color = NA
    ),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.7, "lines")
  )

write_csv(
  qc,
  file.path(output_dir, "02_data_QC.csv")
)

write_csv(
  data_review,
  file.path(output_dir, "02_data_review.csv")
)

write_csv(
  creatures,
  file.path(output_dir, "02_unique_creature_designs.csv")
)

write_csv(
  raw_means,
  file.path(output_dir, "02_raw_color_means.csv")
)

write_csv(
  adjusted_means,
  file.path(output_dir, "02_adjusted_color_means.csv")
)

write_csv(
  model_tests,
  file.path(output_dir, "02_color_model_tests.csv")
)

write_csv(
  pairwise_results,
  file.path(output_dir, "02_adjusted_pairwise_comparisons.csv")
)

write_csv(
  power_slopes,
  file.path(output_dir, "02_power_CMC_slopes.csv")
)

write_csv(
  sensitivity_tests,
  file.path(output_dir, "02_sensitivity_tests.csv")
)

write_csv(
  model_diagnostics,
  file.path(output_dir, "02_model_diagnostics.csv")
)

saveRDS(
  interaction_model,
  file.path(output_dir, "02_creature_power_model.rds")
)

ggsave(
  file.path(output_dir, "Figure_2_Power_vs_CMC.png"),
  figure_2,
  width = 11,
  height = 4.2,
  dpi = 400
)

ggsave(
  file.path(output_dir, "Figure_2_Power_vs_CMC.pdf"),
  figure_2,
  width = 11,
  height = 4.2
)

ggsave(
  file.path(output_dir, "Figure_S2_Raw_vs_Adjusted_Means.png"),
  figure_s2,
  width = 9,
  height = 4.8,
  dpi = 400
)

ggsave(
  file.path(output_dir, "Figure_S2_Raw_vs_Adjusted_Means.pdf"),
  figure_s2,
  width = 9,
  height = 4.8
)

print(qc, n = Inf)
print(data_review, n = Inf)
print(raw_means, n = Inf)
print(model_tests, n = Inf)
print(adjusted_means, n = Inf)
print(pairwise_results, n = Inf)
print(power_slopes, n = Inf)
print(sensitivity_tests, n = Inf)

# Printed power differed among colors before and after adjustment for mana cost and set.
# The color-by-CMC interaction was significant (F(4, 314) = 6.66, p < 0.001).
# Green had the steepest power-CMC relationship (1.09 per mana); white the shallowest (0.53).
# At CMC = 3, red exceeded blue (Holm-adjusted p = 0.0158); other pairs were nonsignificant.
# These results concern printed power, not overall gameplay strength.
