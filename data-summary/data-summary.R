library(tidyverse)

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("data-summary/{name}.pdf"), plot, units = "cm", ...
  )
}

data <- read_csv("data/data.csv", col_types = cols()) %>%
  mutate(
    virus_short = fct_reorder(virus_short, virus_year),
    dose = factor(dose)
  )

discrete_placement <- function(vec, step) {
  (as.integer(vec) - 1 - (length(unique(vec)) - 1) / 2) * step
}

find_x_position <- function(virus_short, dose, day) {
  as.integer(virus_short) +
    discrete_placement(dose, 1 / 4) +
    discrete_placement(day, 1 / 7)
}

virus_short_unique <- data$virus_short %>%
  unique() %>%
  sort()

titre_plot <- data %>%
  group_by(experiment) %>%
  mutate(
    x_position = find_x_position(virus_short, dose, factor(day))
  ) %>%
  ungroup() %>%
  ggplot(aes(x_position, titre)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    panel.spacing = unit(0, "lines"),
    panel.grid.minor = element_blank(),
    legend.box.spacing = unit(0, "null"),
    legend.position = "bottom",
  ) +
  facet_wrap(
    ~experiment,
    ncol = 1,
    labeller = as_labeller(~ paste("Experiment", .x)),
    strip.position = "right",
  ) +
  scale_x_continuous(
    "Virus",
    breaks = as.integer(virus_short_unique),
    labels = virus_short_unique
  ) +
  scale_y_log10("Titre", breaks = 5 * 2^(0:15)) +
  scale_color_discrete("Dose") +
  scale_linetype_discrete("Dose") +
  scale_shape_discrete("Day") +
  geom_point(
    aes(col = dose, shape = as.factor(day)),
  ) +
  geom_line(
    aes(group = paste0(ferret, virus_short), col = dose, lty = dose),
    alpha = 0.5
  )

save_plot(titre_plot, "titres", width = 25, height = 18)

titre_range_plot <- data %>%
  mutate(log2titre = log2(titre)) %>%
  group_by(experiment, day, dose, virus_short, virus_year) %>%
  summarise(
    .groups = "drop",
    n_obs = n(),
    log2range = max(log2titre) - min(log2titre)
  ) %>%
  ggplot(aes(log2range)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    strip.background = element_blank(),
    panel.spacing = unit(0, "null")
  ) +
  scale_y_continuous("Count", expand = expansion(c(0, 0.1))) +
  scale_x_continuous("Virus log2 range (max-min)") +
  facet_grid(
    dose ~ experiment + day,
    labeller = function(labs) {
      if ("day" %in% names(labs)) {
        labs$day <- paste("Day", labs$day)
      }
      if ("experiment" %in% names(labs)) {
        labs$experiment <- paste("Experiment", labs$experiment)
      }
      if ("dose" %in% names(labs)) {
        labs$dose <- as.character(labs$dose)
      }
      labs
    }
  ) +
  geom_histogram(binwidth = 0.1, col = "gray25")

save_plot(titre_range_plot, "ranges", width = 15, height = 15)
