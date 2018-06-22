pick.obs <- function(diffs, limit) {
  keep <- c(T, rep(F, length(diffs)))
  acc <- 0
  for (i in seq_along(diffs)) {
    acc <- acc + diffs[i]
    if (acc > limit) {
      keep[i+1] <- TRUE
      acc <- 0
    }
  }
  return(keep)
}