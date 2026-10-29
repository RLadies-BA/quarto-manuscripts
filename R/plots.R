# Article figures

library(ggplot2)

# Fig 1: mean success rate by years of research 
plot_success_by_experience <- function(participants) {
  by_years <- participants |>
    dplyr::filter(!is.na(inv_anios)) |>
    dplyr::mutate(
      rate  = obt / pres,
      years = cut(inv_anios,
                  breaks = c(-Inf, 5, 10, 15, 20, 25, 30, Inf),
                  labels = c("0-5", "6-10", "11-15", "16-20",
                             "21-25", "26-30", "> 30"))
    ) |>
    dplyr::group_by(years) |>
    dplyr::summarise(
      # NOTE: n counts everyone in the bracket, including those who did not
      # apply to international calls (rate = NA). To count only those with
      # a rate: n = sum(!is.na(rate))
      n       = dplyr::n(),
      M       = mean(rate, na.rm = TRUE),
      SD      = sd(rate, na.rm = TRUE),
      ci_low  = M - qt(0.975, df = n - 1) * SD / sqrt(n),
      ci_high = M + qt(0.975, df = n - 1) * SD / sqrt(n)
    )

  ggplot(by_years, aes(x = years, y = M)) +
    geom_point(color = "#440154FF") +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  color = "#440154FF", width = 0.2) +
    geom_text(aes(y = ci_high + 0.03, label = paste0("n = ", n))) +
    labs(x = "Years of research", y = "Mean success rate") +
    theme_minimal(base_size = 12) +
    ylim(0, 1)
}

# Fig 2 and Fig 3: stacked bars for Likert-type questions ------------------------
# levels: responses ordered from most positive to most negative.
# top:    responses used to order the items (higher up = selected more often).
plot_likert <- function(data, item, response, levels, top) {
  data <- dplyr::filter(data, !is.na({{ response }}))

  item_order <- data |>
    dplyr::group_by({{ item }}) |>
    dplyr::summarise(p_top = mean({{ response }} %in% top)) |>
    dplyr::arrange(p_top) |>
    dplyr::pull({{ item }})

  data |>
    dplyr::mutate(
      response = factor({{ response }}, levels = rev(levels)),
      response = forcats::fct_relabel(response, ~ stringr::str_wrap(.x, width = 20)),
      item     = factor({{ item }}, levels = item_order)
    ) |>
    ggplot(aes(x = item, fill = response)) +
    geom_bar(position = "fill") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_viridis_d(direction = -1) +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(x = "", y = "Percentage", fill = "") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom",
          legend.justification = "right",
          axis.text = element_text(color = "black"))
}

# Save in the format required by PLOS (TIFF, 300 dpi, LZW compression)
# bg = "white": theme_minimal() leaves the background transparent, which can show up as black in TIFF.
save_plos_figure <- function(plot, file, width = 5.2, height = 3) {
  ggsave(file, plot, width = width, height = height, units = "in",
         dpi = 300, compression = "lzw", bg = "white")
  invisible(file)
}
