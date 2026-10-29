library(tidyverse)

input_file <- file.path("Data", "MTG_data_2026.csv")
output_dir <- file.path("Outputs", "01_card_rarity")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop("Run this script from the Magic the Gathering in The Classroom project folder.")
}

rarity_levels <- c("Common", "Uncommon", "Rare", "Mythic")
set_levels <- c("Core 2021", "Ikoria: Lair of Behemoths", "Zendikar Rising")
pct_lab <- function(x) paste0(round(100 * x, 1), "%")

cards <- read_csv(input_file, show_col_types = FALSE) %>%
  mutate(
    set = factor(str_squish(set), levels = set_levels),
    rarity = factor(str_to_title(str_squish(rarity)), levels = rarity_levels)
  )

stopifnot(
  all(c("set", "rep", "pack_rep", "rarity") %in% names(cards)),
  !anyNA(cards[c("set", "rep", "pack_rep", "rarity")])
)

packs <- cards %>%
  group_by(set, rep, pack_rep) %>%
  summarise(
    n_cards = n(),
    n_rare = sum(rarity == "Rare"),
    n_mythic = sum(rarity == "Mythic"),
    has_rare = n_rare > 0,
    has_mythic = n_mythic > 0,
    high_rarity_class = case_when(
      has_mythic ~ "Mythic present",
      has_rare ~ "Rare only",
      TRUE ~ "Neither recorded"
    ),
    .groups = "drop"
  ) %>%
  mutate(
    unusual_size = n_cards < 13 | n_cards > 16,
    missing_rare_or_mythic = high_rarity_class == "Neither recorded"
  )

set.seed(2026)
n_boot <- 2000

bootstrap_rate <- function(df) {
  estimates <- replicate(n_boot, {
    i <- sample.int(nrow(df), size = nrow(df), replace = TRUE)
    sum(df$events[i]) / sum(df$total[i])
  })
  
  tibble(
    n_events = sum(df$events),
    n_total = sum(df$total),
    n_boxes = nrow(df),
    proportion = sum(df$events) / sum(df$total),
    ci_lower = unname(quantile(estimates, 0.025)),
    ci_upper = unname(quantile(estimates, 0.975))
  )
}

card_box_counts <- cards %>%
  filter(rarity %in% c("Rare", "Mythic")) %>%
  count(set, rep, rarity, name = "events") %>%
  complete(nesting(set, rep), rarity = c("Rare", "Mythic"), fill = list(events = 0)) %>%
  left_join(cards %>% count(set, rep, name = "total"), by = c("set", "rep"))

card_summary <- bind_rows(
  card_box_counts,
  card_box_counts %>% mutate(set = factor("All sets", levels = c(set_levels, "All sets")))
) %>%
  group_by(set, rarity) %>%
  group_modify(~ bootstrap_rate(.x)) %>%
  ungroup() %>%
  mutate(sampling_unit = "Individual cards")

pack_box_counts <- packs %>%
  count(set, rep, high_rarity_class, name = "events") %>%
  complete(
    nesting(set, rep),
    high_rarity_class = c("Rare only", "Mythic present", "Neither recorded"),
    fill = list(events = 0)
  ) %>%
  left_join(packs %>% count(set, rep, name = "total"), by = c("set", "rep"))

pack_summary <- bind_rows(
  pack_box_counts,
  pack_box_counts %>% mutate(set = factor("All sets", levels = c(set_levels, "All sets")))
) %>%
  group_by(set, high_rarity_class) %>%
  group_modify(~ bootstrap_rate(.x)) %>%
  ungroup() %>%
  mutate(sampling_unit = "Booster packs") %>%
  rename(rarity = high_rarity_class)

summary_table <- bind_rows(card_summary, pack_summary) %>%
  arrange(sampling_unit, set, rarity)

comparison_events <- bind_rows(
  cards %>%
    filter(rarity %in% c("Rare", "Mythic")) %>%
    transmute(
      set = as.character(set),
      sampling_unit = "Individual cards",
      outcome = as.character(rarity)
    ),
  packs %>%
    filter(high_rarity_class != "Neither recorded") %>%
    transmute(
      set = as.character(set),
      sampling_unit = "Booster packs",
      outcome = if_else(has_mythic, "Mythic", "Rare")
    )
)

comparison_counts <- bind_rows(
  comparison_events,
  comparison_events %>% mutate(set = "All sets")
) %>%
  count(sampling_unit, set, outcome, name = "n") %>%
  pivot_wider(names_from = outcome, values_from = n, values_fill = 0)

binomial_results <- comparison_counts %>%
  rowwise() %>%
  mutate(
    n_compared = Rare + Mythic,
    mythic_proportion = Mythic / n_compared,
    rare_to_mythic_ratio = Rare / Mythic,
    ci_lower = binom.test(Mythic, n_compared)$conf.int[1],
    ci_upper = binom.test(Mythic, n_compared)$conf.int[2],
    p_value = binom.test(
      Mythic, n_compared,
      p = 0.5,
      alternative = "less"
    )$p.value
  ) %>%
  ungroup() %>%
  group_by(sampling_unit) %>%
  mutate(
    p_holm = p.adjust(
      if_else(set == "All sets", NA_real_, p_value),
      method = "holm"
    )
  ) %>%
  ungroup() %>%
  arrange(sampling_unit, set)

rare_mythic_table <- cards %>%
  filter(rarity %in% c("Rare", "Mythic")) %>%
  mutate(rarity = fct_drop(rarity)) %>%
  with(table(set, rarity))

rare_mythic_set_test <- chisq.test(rare_mythic_table, correct = FALSE)
mythic_pack_set_test <- chisq.test(table(packs$set, packs$has_mythic), correct = FALSE)

box_table <- packs %>%
  count(set, rep, has_mythic, name = "events") %>%
  complete(nesting(set, rep), has_mythic = c(FALSE, TRUE), fill = list(events = 0)) %>%
  pivot_wider(names_from = has_mythic, values_from = events, values_fill = 0) %>%
  arrange(set, rep)

box_matrix <- as.matrix(box_table %>% select(`FALSE`, `TRUE`))
box_labels <- box_table$set
observed_statistic <- as.numeric(mythic_pack_set_test$statistic)

n_perm <- 9999
permuted_statistics <- replicate(n_perm, {
  shuffled <- sample(box_labels, replace = FALSE)
  shuffled_table <- rowsum(box_matrix, shuffled, reorder = TRUE)
  as.numeric(chisq.test(shuffled_table, correct = FALSE)$statistic)
})

permutation_p <- (1 + sum(permuted_statistics >= observed_statistic)) / (n_perm + 1)

set_test_results <- tibble(
  analysis = c(
    "Rare versus mythic cards by set",
    "Mythic presence in packs by set",
    "Mythic presence in packs by set (box-label permutation)"
  ),
  statistic = c(
    as.numeric(rare_mythic_set_test$statistic),
    observed_statistic,
    observed_statistic
  ),
  df = c(
    as.numeric(rare_mythic_set_test$parameter),
    as.numeric(mythic_pack_set_test$parameter),
    NA_real_
  ),
  p_value = c(
    rare_mythic_set_test$p.value,
    mythic_pack_set_test$p.value,
    permutation_p
  )
)

main_figure_data <- summary_table %>%
  filter(set != "All sets") %>%
  filter(
    (sampling_unit == "Individual cards" & rarity %in% c("Rare", "Mythic")) |
      (sampling_unit == "Booster packs" & rarity %in% c("Rare only", "Mythic present"))
  ) %>%
  mutate(
    display_class = case_when(
      rarity %in% c("Rare", "Rare only") ~ "Rare",
      rarity %in% c("Mythic", "Mythic present") ~ "Mythic"
    ),
    display_class = factor(display_class, levels = c("Rare", "Mythic")),
    sampling_unit = factor(
      sampling_unit,
      levels = c("Individual cards", "Booster packs")
    )
  )

figure_1 <- ggplot(main_figure_data, aes(x = set, y = proportion, fill = display_class)) +
  geom_col(
    position = position_dodge(width = 0.74),
    width = 0.64,
    color = "black",
    linewidth = 0.25
  ) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.74),
    width = 0.15,
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = pct_lab(proportion)),
    position = position_dodge(width = 0.74),
    vjust = -0.5,
    size = 3.5
  ) +
  facet_wrap(~ sampling_unit, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Rare" = "#D4AF37", "Mythic" = "#D95F02")) +
  scale_y_continuous(
    labels = pct_lab,
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(x = NULL, y = "Observed proportion", fill = NULL) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = "top",
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1),
    panel.spacing = unit(0.8, "lines")
  )

supp_card_data <- cards %>%
  count(set, rarity, name = "events") %>%
  group_by(set) %>%
  mutate(proportion = events / sum(events)) %>%
  ungroup()

figure_s1 <- ggplot(supp_card_data, aes(x = rarity, y = proportion, fill = set)) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.64,
    color = "black",
    linewidth = 0.2
  ) +
  geom_text(
    aes(label = pct_lab(proportion)),
    position = position_dodge(width = 0.72),
    vjust = -0.4,
    size = 3.2
  ) +
  scale_fill_manual(values = c(
    "Core 2021" = "#4E79A7",
    "Ikoria: Lair of Behemoths" = "#59A14F",
    "Zendikar Rising" = "#E15759"
  )) +
  scale_y_continuous(
    labels = pct_lab,
    limits = c(0, 0.8),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(x = "Card rarity", y = "Proportion of cards", fill = "Set") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top")

write_csv(summary_table, file.path(output_dir, "01_rarity_probabilities.csv"))
write_csv(binomial_results, file.path(output_dir, "01_rare_mythic_binomial_tests.csv"))
write_csv(
  binomial_results %>%
    filter(sampling_unit == "Individual cards") %>%
    transmute(
      set, Rare, Mythic, rare_to_mythic_ratio,
      mythic_share_among_high_rarity_cards = mythic_proportion
    ),
  file.path(output_dir, "01_rare_mythic_summary.csv")
)
write_csv(set_test_results, file.path(output_dir, "01_rarity_tests.csv"))
write_csv(packs, file.path(output_dir, "01_pack_summary.csv"))
write_csv(
  packs %>% filter(unusual_size | missing_rare_or_mythic),
  file.path(output_dir, "01_pack_QC_flags.csv")
)
write_csv(
  tibble(permuted_chisq = permuted_statistics),
  file.path(output_dir, "01_box_permutation_statistics.csv")
)

ggsave(
  file.path(output_dir, "Figure_1_Rare_vs_Mythic.png"),
  figure_1, width = 8.5, height = 7.5, dpi = 400
)
ggsave(
  file.path(output_dir, "Figure_1_Rare_vs_Mythic.pdf"),
  figure_1, width = 8.5, height = 7.5
)
ggsave(
  file.path(output_dir, "Figure_S1_All_Rarity_Classes.png"),
  figure_s1, width = 8, height = 5.5, dpi = 400
)
ggsave(
  file.path(output_dir, "Figure_S1_All_Rarity_Classes.pdf"),
  figure_s1, width = 8, height = 5.5
)

print(summary_table %>% filter(set == "All sets"), n = Inf)
print(binomial_results, n = Inf, width = Inf)
print(set_test_results, n = Inf)
print(packs %>% count(unusual_size, missing_rare_or_mythic))



# Summary of findings
# Across 7,713 cards, rares accounted for 6.6% and mythics for 0.9%.
# Rares were approximately 7.2 times more frequent than mythics.
# Mythics were significantly less frequent than rares overall and
# within each set under an equal-probability binomial null (p < 0.001).
# At the pack level, 87.0% were rare-only and 12.8% contained mythics.
# This difference was also significant overall and by set (p < 0.001).
# However, the relative frequency of rare and mythic outcomes did
# not differ significantly between the three sets.
# These results illustrate how rarity estimates depend on sampling
# units and why the choice of null hypothesis matters.
