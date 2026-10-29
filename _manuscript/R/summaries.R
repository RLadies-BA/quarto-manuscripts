# Summaries and number formatting for the text

# Frequency table
# Counts each category of `x` and computes its percentage of `total`.
# By default, `total` is the number of elements in `x` (including NAs).
count_table <- function(x, total = length(x)) {
  tibble::tibble(category = x) |>
    dplyr::count(category) |>
    dplyr::mutate(total = total, pct = 100 * n / total) |>
    dplyr::arrange(dplyr::desc(n))
}

# Mean, SD and 95% confidence interval (Student's t) -----------------------------
mean_ci <- function(x) {
  x  <- x[!is.na(x)]
  n  <- length(x)
  se <- sd(x) / sqrt(n)
  tibble::tibble(
    n       = n,
    M       = mean(x),
    SD      = sd(x),
    ci_low  = M - qt(0.975, df = n - 1) * se,
    ci_high = M + qt(0.975, df = n - 1) * se
  )
}

# Formatting for the text
# Number with a fixed number of decimals. `lead_zero = FALSE` writes .41 instead of 0.41.
num <- function(x, digits = 2, lead_zero = TRUE) {
  out <- formatC(x, format = "f", digits = digits)
  if (!lead_zero) out <- sub("^(-?)0\\.", "\\1.", out)
  out
}

# Count and percentage in the two formats used in the article:
# n_pct(126, 202) -> "n = 126, 62.38%"
# pct_n(126, 202) -> "62.38% (n = 126)"
n_pct <- function(n, total) {
  sprintf("n = %d, %s%%", as.integer(n), num(100 * n / total))
}
pct_n <- function(n, total) {
  sprintf("%s%% (n = %d)", num(100 * n / total), as.integer(n))
}

# Same as above, for one or more categories of a count_table() table
report <- function(tbl, category) {
  rows <- tbl[tbl$category %in% category, ]
  n_pct(sum(rows$n), tbl$total[1])
}
report_pct <- function(tbl, category) {
  rows <- tbl[tbl$category %in% category, ]
  pct_n(sum(rows$n), tbl$total[1])
}

# "95% CI [13.36, 15.75]" from mean_ci() output
ci95 <- function(s, digits = 2, lead_zero = TRUE) {
  sprintf("95%% CI [%s, %s]",
          num(s$ci_low, digits, lead_zero),
          num(s$ci_high, digits, lead_zero))
}
