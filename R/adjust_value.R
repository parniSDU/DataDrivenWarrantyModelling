adjust_value <- function(x) {
  ifelse(x < 0, 0, ifelse(x > 1, 1, x))
}