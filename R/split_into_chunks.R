

#-----------------------------------------------------------------------------
#' Split numbers A and B into chunks of N digits
#'
#' @param a first number
#' @param b second number
#' @param N number of digits in chunk
#'
#' @importFrom stringr str_sub
#' @export
#-----------------------------------------------------------------------------
split_into_chunks <- function(a, b, N) {

  # Pad shorter number with zeros
  zeros <- paste(rep('0', abs(nchar(a) - nchar(b))), collapse='')
  if (nchar(a) < nchar(b)) {
    a <- paste0(zeros, a)
  } else {
    b <- paste0(zeros, b)
  }

  n <- seq(1, nchar(a), N)

  # split into chunks
  A <- rev(as.integer(stringr::str_sub(a, -n-N+1, -n)))
  B <- rev(as.integer(stringr::str_sub(b, -n-N+1, -n)))

  list(A=A, B=B)
}