library(tidyverse)

input_file <- file.path("Data", "MTG_data_2026.csv")
output_dir <- file.path("Outputs", "03_card_diversity")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Run this script from the Magic the Gathering in The Classroom project folder.")
}

set_levels <- c("Core 2021", "Ikoria: Lair of Behemoths", "Zendikar Rising")
set_palette <- c(
  "Core 2021" = "#4E79A7",
  "Ikoria: Lair of Behemoths" = "#59A14F",
  "Zendikar Rising" = "#E15759"
)

cards <- read_csv(input_file, show_col_types = FALSE)
stopifnot(all(c("set", "rep", "pack_rep", "card_name") %in% names(cards)))

cards <- cards %>%
  mutate(
    set = str_squish(set),
    card_name = str_squish(card_name)
  )

stopifnot(
  !anyNA(cards[c("set", "rep", "pack_rep", "card_name")]),
  all(cards$set %in% set_levels),
  all(nzchar(cards$card_name))
)

cards <- cards %>%
  mutate(set = factor(set, levels = set_levels))

packs <- cards %>%
  count(set, rep, pack_rep, name = "n_cards")

qc <- cards %>%
  count(set, card_name, name = "frequency") %>%
  group_by(set) %>%
  summarise(
    n_cards = sum(frequency),
    n_designs = n(),
    singletons = sum(frequency == 1),
    doubletons = sum(frequency == 2),
    coverage_approx = 1 - singletons / n_cards,
    .groups = "drop"
  ) %>%
  left_join(
    packs %>%
      group_by(set) %>%
      summarise(
        n_packs = n(),
        n_boxes = n_distinct(rep),
        min_pack_size = min(n_cards),
        max_pack_size = max(n_cards),
        .groups = "drop"
      ),
    by = "set"
  )

diversity_indices <- function(x) {
  counts <- as.numeric(table(x))
  p <- counts / sum(counts)
  richness <- length(counts)
  shannon <- -sum(p * log(p))
  simpson_concentration <- sum(p^2)
  
  c(
    richness = richness,
    shannon = shannon,
    inverse_simpson = 1 / simpson_concentration,
    simpson_diversity = 1 - simpson_concentration,
    pielou_evenness = shannon / log(richness)
  )
}

set_samples <- set_names(set_levels) %>%
  map(~ cards$card_name[cards$set == .x])

diversity_summary <- map_dfr(set_levels, function(s) {
  tibble(
    set = s,
    n_cards = length(set_samples[[s]]),
    !!!as.list(diversity_indices(set_samples[[s]]))
  )
})

set.seed(2026)
n_rarefaction <- 500
n_box_rarefaction <- 300
n_accumulation <- 500
n_permutations <- 9999

common_card_effort <- min(diversity_summary$n_cards)

equal_card_runs <- map_dfr(set_levels, function(s) {
  x <- set_samples[[s]]
  
  map_dfr(seq_len(n_rarefaction), function(i) {
    tibble(
      set = s,
      iteration = i,
      !!!as.list(diversity_indices(
        sample(x, common_card_effort, replace = FALSE)
      ))
    )
  })
})

equal_card_summary <- equal_card_runs %>%
  pivot_longer(
    c(richness, shannon, inverse_simpson),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(set, metric) %>%
  summarise(
    n_cards = common_card_effort,
    mean_value = mean(value),
    lower_subsample = unname(quantile(value, 0.025)),
    upper_subsample = unname(quantile(value, 0.975)),
    .groups = "drop"
  )

valid_packs <- packs %>%
  filter(between(n_cards, 13, 16))

size_sensitivity <- map_dfr(set_levels, function(s) {
  x <- cards %>%
    semi_join(valid_packs, by = c("set", "rep", "pack_rep")) %>%
    filter(set == s) %>%
    pull(card_name)
  
  tibble(
    set = s,
    n_cards = length(x),
    !!!as.list(diversity_indices(x))
  )
})

box_samples <- cards %>%
  group_by(set, rep) %>%
  summarise(
    card_names = list(card_name),
    n_cards = n(),
    .groups = "drop"
  )

common_box_effort <- min(box_samples$n_cards)

box_diversity <- box_samples %>%
  mutate(
    estimates = map(card_names, function(x) {
      sims <- replicate(
        n_box_rarefaction,
        diversity_indices(sample(x, common_box_effort, replace = FALSE))
      )
      
      tibble(
        richness = mean(sims["richness", ]),
        shannon = mean(sims["shannon", ]),
        inverse_simpson = mean(sims["inverse_simpson", ])
      )
    })
  ) %>%
  unnest(estimates) %>%
  select(-card_names) %>%
  mutate(standardized_n_cards = common_box_effort)

f_statistic <- function(values, groups) {
  groups <- as.character(groups)
  means <- tapply(values, groups, mean)
  sizes <- table(groups)
  ss_between <- sum(sizes[names(means)] * (means - mean(values))^2)
  ss_within <- sum((values - means[groups])^2)
  
  (ss_between / (length(means) - 1)) /
    (ss_within / (length(values) - length(means)))
}

permutation_tests <- map_dfr(
  c("richness", "shannon", "inverse_simpson"),
  function(metric) {
    values <- box_diversity[[metric]]
    groups <- box_diversity$set
    observed_F <- f_statistic(values, groups)
    shuffled_F <- replicate(
      n_permutations,
      f_statistic(values, sample(groups, replace = FALSE))
    )
    
    tibble(
      metric = metric,
      n_boxes = length(values),
      df_num = n_distinct(groups) - 1,
      df_den = length(values) - n_distinct(groups),
      F = observed_F,
      p_value = (1 + sum(shuffled_F >= observed_F)) / (n_permutations + 1)
    )
  }
) %>%
  mutate(p_holm = p.adjust(p_value, method = "holm"))

pack_data <- cards %>%
  group_by(set, rep, pack_rep) %>%
  summarise(card_names = list(card_name), .groups = "drop")

pack_samples <- map(set_levels, function(s) {
  pack_data %>%
    filter(set == s) %>%
    pull(card_names)
})
names(pack_samples) <- set_levels

accumulate_packs <- function(pack_ids, n_designs) {
  order <- sample.int(length(pack_ids))
  seen <- rep(FALSE, n_designs)
  result <- integer(length(pack_ids))
  
  for (i in seq_along(order)) {
    seen[pack_ids[[order[i]]]] <- TRUE
    result[i] <- sum(seen)
  }
  
  result
}

accumulation_summary <- map_dfr(set_levels, function(s) {
  pack_list <- pack_samples[[s]]
  card_levels <- unique(unlist(pack_list, use.names = FALSE))
  pack_ids <- map(pack_list, ~ match(.x, card_levels))
  simulations <- replicate(
    n_accumulation,
    accumulate_packs(pack_ids, length(card_levels))
  )
  
  tibble(
    set = s,
    packs_sampled = seq_len(nrow(simulations)),
    mean_richness = rowMeans(simulations),
    lower_random_order = apply(simulations, 1, quantile, probs = 0.025),
    upper_random_order = apply(simulations, 1, quantile, probs = 0.975)
  )
})

figure_data <- diversity_summary %>%
  select(set, richness, shannon, inverse_simpson) %>%
  pivot_longer(-set, names_to = "metric", values_to = "value") %>%
  mutate(
    metric = factor(
      metric,
      levels = c("richness", "shannon", "inverse_simpson"),
      labels = c("Card-design richness", "Shannon diversity (H')", "Inverse Simpson diversity")
    ),
    set_label = factor(
      case_when(
        set == "Core 2021" ~ "Core 2021",
        set == "Ikoria: Lair of Behemoths" ~ "Ikoria",
        TRUE ~ "Zendikar Rising"
      ),
      levels = c("Core 2021", "Ikoria", "Zendikar Rising")
    ),
    value_label = case_when(
      metric == "Card-design richness" ~ sprintf("%.0f", value),
      metric == "Shannon diversity (H')" ~ sprintf("%.3f", value),
      TRUE ~ sprintf("%.1f", value)
    )
  )

figure_3 <- ggplot(
  figure_data,
  aes(x = set_label, y = value, fill = set)
) +
  geom_col(width = 0.68, color = "black", linewidth = 0.25) +
  geom_text(aes(label = value_label), vjust = -0.5, size = 3.6) +
  facet_wrap(~ metric, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = set_palette, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Diversity index") +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.spacing = unit(1, "lines")
  )

final_accumulation <- accumulation_summary %>%
  group_by(set) %>%
  slice_max(packs_sampled, n = 1, with_ties = FALSE) %>%
  ungroup()

figure_s3 <- ggplot(
  accumulation_summary,
  aes(x = packs_sampled, y = mean_richness, color = set, fill = set)
) +
  geom_ribbon(
    aes(ymin = lower_random_order, ymax = upper_random_order),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(data = final_accumulation, size = 2.5) +
  facet_wrap(~ set, nrow = 1) +
  scale_color_manual(values = set_palette, guide = "none") +
  scale_fill_manual(values = set_palette, guide = "none") +
  scale_x_continuous(breaks = seq(0, 180, by = 30)) +
  scale_y_continuous(breaks = seq(0, 270, by = 50), limits = c(0, 280)) +
  labs(x = "Booster packs sampled", y = "Cumulative distinct card designs") +
  theme_classic(base_size = 12) +
  theme(
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(0.7, "lines")
  )

write_csv(qc, file.path(output_dir, "03_data_QC.csv"))
write_csv(
  cards %>% count(set, card_name, name = "n_observed"),
  file.path(output_dir, "03_card_frequencies.csv")
)
write_csv(packs, file.path(output_dir, "03_pack_summary.csv"))
write_csv(diversity_summary, file.path(output_dir, "03_diversity_summary.csv"))
write_csv(equal_card_summary, file.path(output_dir, "03_equal_card_rarefaction.csv"))
write_csv(size_sensitivity, file.path(output_dir, "03_size_sensitivity.csv"))
write_csv(box_diversity, file.path(output_dir, "03_box_standardized_diversity.csv"))
write_csv(permutation_tests, file.path(output_dir, "03_box_permutation_tests.csv"))
write_csv(accumulation_summary, file.path(output_dir, "03_pack_accumulation.csv"))

ggsave(
  file.path(output_dir, "Figure_3_Richness_Shannon_Simpson.png"),
  figure_3, width = 11, height = 5.2, dpi = 400
)
ggsave(
  file.path(output_dir, "Figure_3_Richness_Shannon_Simpson.pdf"),
  figure_3, width = 11, height = 5.2
)
ggsave(
  file.path(output_dir, "Figure_S3_Pack_Accumulation.png"),
  figure_s3, width = 11, height = 4.4, dpi = 400
)
ggsave(
  file.path(output_dir, "Figure_S3_Pack_Accumulation.pdf"),
  figure_s3, width = 11, height = 4.4
)

print(qc, n = Inf)
print(diversity_summary, n = Inf)
print(equal_card_summary, n = Inf)
print(size_sensitivity, n = Inf)
print(permutation_tests, n = Inf)

# Zendikar Rising had the highest observed richness (264 versus 256 in each other set).
# Core 2021 and Ikoria had slightly higher Shannon and inverse Simpson diversity.
# The results illustrate how diversity rankings depend on the chosen metric.
# Box-level permutation tests assess between-set variation; pack curves show order effects.
