# ============================================================
# Program: figure_01_weight_change.R
#
# Purpose:
#   Plot mean change in body weight from baseline by visit
#   and actual treatment for the ABC101 safety population.
#
# Input:
#   data/adam/advs.csv
#
# Outputs:
#   output/figures/figure_01_weight_change.csv
#   output/figures/figure_01_weight_change.png
#
# Run from the project root.
# ============================================================

advs <- read.csv(
  "data/adam/advs.csv",
  stringsAsFactors = FALSE,
  colClasses = "character"
)

required <- c("STUDYID", "USUBJID", "ASEQ", "SAFFL", "TRT01A",
              "PARAMCD", "AVISIT", "AVISITN", "AVALU", "CHG")
stopifnot(all(required %in% names(advs)))
stopifnot(!anyNA(advs[required]))
stopifnot(!anyDuplicated(advs[c("USUBJID", "ASEQ")]))
stopifnot(all(advs$STUDYID == "ABC101"))
stopifnot(all(advs$SAFFL %in% c("Y", "N")))

arm_order <- c("Placebo", "Drug 10 mg", "Drug 20 mg")
visit_numbers <- 2:5
weeks <- c(2L, 4L, 8L, 12L)
visit_labels <- paste("Week", weeks)

weight <- advs[
  advs$SAFFL == "Y" & advs$PARAMCD == "WEIGHT" &
    advs$AVISITN %in% as.character(visit_numbers),
]

stopifnot(nrow(weight) > 0L)
stopifnot(all(weight$TRT01A %in% arm_order))
stopifnot(all(weight$AVALU == "kg"))
stopifnot(!any(weight$CHG == ""))
stopifnot(!anyDuplicated(weight[c("USUBJID", "AVISITN")]))

change <- suppressWarnings(as.numeric(weight$CHG))
stopifnot(!anyNA(change), all(is.finite(change)))

expected_labels <- visit_labels[match(as.integer(weight$AVISITN),
                                      visit_numbers)]
stopifnot(all(weight$AVISIT == expected_labels))

# Calculate the exact values used by the figure -----------------------------

summary_rows <- list()
for (visit_index in seq_along(visit_numbers)) {
  for (arm in arm_order) {
    indices <- which(weight$AVISITN == as.character(visit_numbers[visit_index]) &
                       weight$TRT01A == arm)
    n <- length(unique(weight$USUBJID[indices]))
    stopifnot(n > 0L, n == length(indices))

    summary_rows[[length(summary_rows) + 1L]] <- data.frame(
      AVISITN = visit_numbers[visit_index],
      AVISIT = visit_labels[visit_index],
      WEEK = weeks[visit_index],
      TRT01A = arm,
      N = n,
      MEAN_CHG = round(mean(change[indices]), 4),
      UNIT = "kg",
      stringsAsFactors = FALSE
    )
  }
}

summary <- do.call(rbind, summary_rows)
row.names(summary) <- NULL
stopifnot(nrow(summary) == length(visit_numbers) * length(arm_order))
stopifnot(sum(summary$N) == nrow(weight))

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
write.csv(summary, "output/figures/figure_01_weight_change.csv",
          row.names = FALSE, na = "")

# Draw a line chart and a count grid beneath it -----------------------------

colors <- c("Placebo" = "#444444", "Drug 10 mg" = "#0072B2",
            "Drug 20 mg" = "#D55E00")
symbols <- c("Placebo" = 16, "Drug 10 mg" = 17,
             "Drug 20 mg" = 15)

mean_range <- range(c(0, summary$MEAN_CHG))
padding <- max(0.5, diff(mean_range) * 0.35)
y_limits <- mean_range + c(-padding, padding)

png("output/figures/figure_01_weight_change.png",
    width = 1800, height = 1100, res = 180, type = "cairo")
layout(matrix(c(1, 2), nrow = 2), heights = c(3.9, 1.3))

par(mar = c(3.3, 5.3, 4.3, 1.6), family = "sans")
plot(NA, xlim = c(1.5, 12.5), ylim = y_limits,
     xaxt = "n", yaxt = "n", xlab = "", 
     ylab = "Mean change from baseline (kg)",
     main = "Mean change in body weight from baseline",
     cex.main = 1.25, cex.lab = 1.05, bty = "l")
abline(h = pretty(y_limits), col = "#E6E9ED", lwd = 1)
abline(h = 0, col = "#777777", lwd = 1.3, lty = 2)
axis(1, at = weeks, labels = visit_labels, cex.axis = 0.95)
axis(2, las = 1, cex.axis = 0.9)

for (arm in arm_order) {
  points_for_arm <- summary[summary$TRT01A == arm, ]
  lines(points_for_arm$WEEK, points_for_arm$MEAN_CHG,
        col = colors[arm], lwd = 2.7)
  points(points_for_arm$WEEK, points_for_arm$MEAN_CHG,
         col = colors[arm], pch = symbols[arm], cex = 1.35)
}

legend("topright", legend = arm_order, col = colors[arm_order],
       pch = symbols[arm_order], lty = 1, lwd = 2.7,
       bty = "n", cex = 0.88)
mtext("ABC101 safety population; actual treatment", side = 3,
      line = 0.25, cex = 0.82, col = "#555555")

par(mar = c(1.0, 1.5, 0.3, 1.5))
plot.new()
plot.window(xlim = c(0, 5.2), ylim = c(0, 4))
x_positions <- c(2.0, 2.9, 3.8, 4.7)
text(0.1, 3.45, "Contributing subjects (n)", pos = 4,
     font = 2, cex = 0.9)
text(x_positions, 3.45, visit_labels, cex = 0.82)
segments(0.1, 3.14, 5.05, 3.14, col = "#CED3D8")

for (arm_index in seq_along(arm_order)) {
  arm <- arm_order[arm_index]
  y <- 3.1 - arm_index * 0.8
  text(0.1, y, arm, pos = 4, col = colors[arm],
       font = 2, cex = 0.88)
  arm_counts <- summary$N[summary$TRT01A == arm]
  text(x_positions, y, arm_counts, cex = 0.88)
}

dev.off()
