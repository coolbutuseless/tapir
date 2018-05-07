

#-----------------------------------------------------------------------------
#' Generate a an arbitrary integer as a text string
#'
#' @param digits integer. number of digits
#'
#' @return A character string containing an integer with the specified number of digits.
#' @export
#-----------------------------------------------------------------------------
rbigint <- function(digits) {
  digits <- as.integer(digits)
  stopifnot(!is.na(digits) && digits > 0)
  if (digits == 1) {
    as.character(sample(0:9, size=1))
  } else {
    paste(c(
      sample(1:9, size = 1),
      sample(0:9, size = digits-1, replace = TRUE)
    ), collapse='')
  }
}
