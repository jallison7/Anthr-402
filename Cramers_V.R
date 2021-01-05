
Cramers_V <- function(Table) {
  rows <- nrow(Table)
  cols <- ncol(Table)
  chisq <- chisq.test(Table)
  chisq <- as.numeric(chisq$statistic)
  Table_with_Totals <- addmargins(Table)
  Grand_Total <- Table_with_Totals[rows+1,cols+1]
  Cramers_V <- sqrt(chisq/(Grand_Total*min(rows-1,cols-1)))
}
