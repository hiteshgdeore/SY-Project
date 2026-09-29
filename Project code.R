# Statistical Analysis of Monthly Expenses of Hostelers
# B.Sc. (Mathematical Sciences) II Field Project 2025-26, KBCNMU Jalgaon
#
# Chi-square test of independence (Gender vs each variable), grouped bar charts,
# and frequency tables. Base R only, no packages needed.
#
# Usage:  Rscript analysis.R      (or source("analysis.R") in RStudio)

ALPHA <- 0.05
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# ---- Contingency tables from the report (Chapter 4). Rows: Female, Male ----
tables <- list(
  "Years in hostel" = list(
    cols = c("1 yr", "2 yr", "3 yr", "4 yr", "5 yr"),
    F = c(7, 12, 11, 8, 9),  M = c(10, 19, 11, 10, 3)),
  "Home area" = list(
    cols = c("Rural", "Urban"),
    F = c(29, 18),  M = c(34, 19)),
  "Education status" = list(
    cols = c("PG 1st", "PG 2nd", "UG 1st", "UG 4th", "UG 2nd", "UG 3rd"),
    F = c(2, 8, 11, 8, 5, 13),  M = c(4, 8, 13, 9, 6, 13)),
  "Room type" = list(
    cols = c("Double", "Single", "Triple"),
    F = c(14, 15, 18),  M = c(21, 9, 23)),
  "Travel spending" = list(
    cols = c("0-250", "250-500", "Above 500"),
    F = c(10, 23, 14),  M = c(10, 33, 10)),
  "Food spending" = list(
    cols = c("2000-3000", "3000-4000", "Above 4000"),
    F = c(15, 18, 14),  M = c(26, 23, 4)),
  "Entertainment spending" = list(
    cols = c("0-500", "500-1000", "Above 1000"),
    F = c(17, 26, 4),  M = c(24, 24, 5)),
  "Study material spending" = list(
    cols = c("0-100", "100-200", "Above 200"),
    F = c(13, 23, 11),  M = c(17, 25, 11)),
  "Additional income" = list(
    cols = c("No", "Yes"),
    F = c(29, 18),  M = c(30, 23)),
  "Rent" = list(
    cols = c("4500", "5000", "5500"),
    F = c(19, 21, 7),  M = c(28, 22, 3)),
  "Personal/health care spending" = list(
    cols = c("0-500", "600-1000", "Above 1000"),
    F = c(12, 23, 13),  M = c(26, 24, 3)),
  "WiFi should be provided" = list(
    cols = c("No", "Yes"),
    F = c(16, 31),  M = c(19, 34)),
  "Fathers occupation" = list(
    cols = c("Businessman", "Farmer", "Gov. job", "Other", "Pvt. job", "Teacher"),
    F = c(4, 9, 7, 4, 11, 12),  M = c(8, 20, 8, 2, 9, 6)),
  "Mode of payment" = list(
    cols = c("Cash", "Online"),
    F = c(12, 35),  M = c(15, 38)),
  "Average monthly expenses" = list(
    cols = c("Upto 2000", "2000-4000", "Above 4000"),
    F = c(21, 8, 18),  M = c(26, 7, 20))
)

make_matrix <- function(s) {
  m <- rbind(Female = s$F, Male = s$M)
  colnames(m) <- s$cols
  m
}

# Pool columns with expected frequency < 5 into a neighbouring column (report sec. 5.1.3)
pool_small_columns <- function(m, min_expected = 5) {
  while (ncol(m) > 2) {
    e <- suppressWarnings(chisq.test(m, correct = FALSE)$expected)
    low <- which(apply(e < min_expected, 2, any))
    if (length(low) == 0) break
    i <- low[1]
    j <- if (i < ncol(m)) i + 1 else i - 1
    a <- min(i, j); b <- max(i, j)
    left   <- m[, seq_len(a - 1), drop = FALSE]
    right  <- if (b < ncol(m)) m[, (b + 1):ncol(m), drop = FALSE] else m[, 0, drop = FALSE]
    merged <- matrix(m[, a] + m[, b], ncol = 1)
    new_m  <- cbind(left, merged, right)
    colnames(new_m)[a] <- paste(colnames(m)[a], colnames(m)[b], sep = " + ")
    m <- new_m
  }
  m
}

safe_name <- function(x) gsub("[^A-Za-z0-9]+", "_", x)

# ---- Chi-square tests + charts ----
results <- data.frame()
freq <- data.frame()

for (name in names(tables)) {
  m <- make_matrix(tables[[name]])

  # Grouped bar chart
  png(file.path("figures", paste0(safe_name(name), ".png")), width = 1000, height = 650, res = 150)
  barplot(m, beside = TRUE, col = c("#e8743b", "#4472c4"),
          main = paste("Gender vs", name), ylab = "Number of students",
          legend.text = rownames(m), args.legend = list(title = "Gender", x = "topright"))
  dev.off()

  # Chi-square test
  test <- suppressWarnings(chisq.test(pool_small_columns(m), correct = FALSE))
  reject <- test$p.value < ALPHA
  results <- rbind(results, data.frame(
    Variable   = name,
    Chi_square = round(unname(test$statistic), 3),
    DF         = unname(test$parameter),
    p_value    = round(test$p.value, 3),
    Decision   = if (reject) "Reject H0" else "Fail to reject H0",
    Conclusion = paste0("Gender is ", if (reject) "" else "NOT ", "associated with ", tolower(name))
  ))

  # Overall frequency table
  tot <- colSums(m)
  freq <- rbind(freq, data.frame(
    Variable = name, Category = names(tot),
    Frequency = as.integer(tot), Percent = round(100 * tot / sum(tot), 1),
    row.names = NULL))
}

print(results, row.names = FALSE)
write.csv(results, "results/chi_square_results.csv", row.names = FALSE)
write.csv(freq, "results/frequency_tables.csv", row.names = FALSE)
