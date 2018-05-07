


max_uint64_ <- '18,446,744,073,709,551,615'
max_uint64  <- '18446744073709551615'



#-----------------------------------------------------------------------------
#' Add aribtrarily long integers
#'
#' Split each number into chunks of add digits. Add these chunks in sequence
#' using R's built in interger addition (as the result is still within
#' R's 32bit integer type).
#'
#' Manually handle the carry from this addition.
#'
#' @param a lhs. A big integer as a character string.
#' @param b rhs. A big integer as a character string.
#'
#' @return a + b (as a bit integer in a character string)
#'
#' @importFrom utils tail
#' @export
#-----------------------------------------------------------------------------
'%add%' <- function(a, b) {
  stopifnot(is.character(a), is.character(b))

  # Split original numbers into chunks
  N             <- 8
  max_chunk_int <- as.integer(10^N)
  chunks        <- split_into_chunks(a, b, N)

  # vectorised addition of chunks and calculation of carry
  digits <- chunks$A + chunks$B
  carry  <- digits %/% max_chunk_int
  digits <- digits %%  max_chunk_int
  digits <- c(0L, digits)

  # If there's any carry, then add it in
  while (any(carry > 0L)) {
    carry  <- tail(c(carry, 0L), length(chunks$A) + 1)
    digits <- digits + carry
    carry  <- digits %/% max_chunk_int
    digits <- digits %%  max_chunk_int
  }

  # Turn integer vector back into a string
  result <- paste(sprintf("%08i", digits), collapse='')
  result <- sub("^0+", '', result) # Remove leading zeros for neatness
  if (result == '') {
    "0"
  } else {
    result
  }
}




#-----------------------------------------------------------------------------
#' Subtract aribtrarily long integers
#'
#' Split each number into chunks of add digits. Add these chunks in sequence
#' using R's built in interger addition (as the result is still within
#' R's 32bit integer type).
#'
#' Manually handle the carry from this addition.
#'
#' @param a lhs. A big integer as a character string.
#' @param b rhs. A big integer as a character string.
#'
#' @return a - b (as a bit integer in a character string)
#'
#' @export
#-----------------------------------------------------------------------------
"%sub%" <- function(a, b) {
  stopifnot(is.character(a), is.character(b))

  if (a %lt% b) { stop("Can't subtract a larger integer from a smaller one") }

  # Split original numbers into chunks
  N             <- 8
  max_chunk_int <- as.integer(10^N)
  chunks        <- split_into_chunks(a, b, N)

  # vectorised addition of chunks and calculation of carry
  digits <- chunks$A - chunks$B
  carry  <- digits %/% max_chunk_int
  digits <- digits %%  max_chunk_int
  digits <- c(0L, digits)

  # If there's any carry, then add it in
  while (any(carry < 0L)) {
    carry  <- tail(c(carry, 0L), length(chunks$A) + 1)
    digits <- digits + carry
    carry  <- digits %/% max_chunk_int
    digits <- digits %%  max_chunk_int
  }

  # Turn integer vector back into a string
  result <- paste(sprintf("%08i", digits), collapse='')
  result <- sub("^0+", '', result) # Remove leading zeros for neatness
  if (result == '') {
    "0"
  } else {
    result
  }
}







