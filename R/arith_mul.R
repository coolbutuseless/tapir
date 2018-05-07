#-----------------------------------------------------------------------------
#' Multiply 2 aribtrarily long integers
#'
#' Split each number into chunks of 4 digits and then multiple these chunks
#' using R's built-in integer multiply - the result of this chunk multiply
#' will always fit into R's 32 bit signed integer type.
#'
#' @param a lhs. A big integer as a character string.
#' @param b rhs. A big integer as a character string.
#'
#' @return a * b (as a bit integer in a character string)
#'
#' @export
#-----------------------------------------------------------------------------
'%mul%' <- function(a, b) {
  stopifnot(is.character(a), is.character(b))

  # Split 'a' and 'b' into chunked representations 'A' and 'B'
  N             <- 4
  max_chunk_int <- as.integer(10^N)
  chunks        <- split_into_chunks(a, b, N)
  A             <- rev(chunks$A)
  B             <- rev(chunks$B)

  # Allocate space for the output digits and the
  # intermediate carry
  digits <- integer(2 * length(A))
  carry  <- integer(2 * length(A))

  count  <- 1
  for (j in rev(seq_along(B))) {
    count <- count + 1
    row <- integer(2 * length(A))
    for (i in rev(seq_along(A))) {
      row[i + j - 1] <- as.integer(B[j]) * as.integer(A[i])
    }
    digits <- digits + rev(row)

    # Avoid overflow when accumulating lots of chunk multiplications
    # when the numbers are large. Do this by calculating the
    # carry every 20 iterations.
    # Note: you can still overflow for *really* large digits
    # by overflowing the 'carry'
    if (count %% 20 == 0) {
      carry  <- carry + (digits %/% max_chunk_int)
      digits <- digits %%  max_chunk_int
    }
  }

  # Calculate any overall carry
  carry  <- carry + (digits %/% max_chunk_int)
  digits <- digits %%  max_chunk_int
  digits <- c(0L, digits)

  # If there's any carry, then add it in
  while (any(carry > 0L)) {
    carry  <- tail(c(carry, 0L), length(digits))
    digits <- digits + carry
    carry  <- digits %/% max_chunk_int
    digits <- digits %%  max_chunk_int
  }

  # Turn integer vector back into a string
  result <- paste(sprintf("%04i", digits), collapse='')
  result <- sub("^0+", '', result) # Remove leading zeros for neatness
  if (result == '') {
    "0"
  } else {
    result
  }
}


#-----------------------------------------------------------------------------
#' Integer division of 2 long integers
#'
#' This is quite slow as it uses the technique of 'long division'
#' \url{https://en.wikipedia.org/wiki/Division_algorithm}
#'
#' @param a lhs. A big integer as a character string.
#' @param b rhs. A big integer as a character string.
#'
#' @return a %/% b (as a bit integer in a character string)
#'
#' @export
#-----------------------------------------------------------------------------
'%div%' <- function(a, b) {
  stopifnot(is.character(a), is.character(b))

  # Catch some conditions early
  if (a %lt% b)   { return("0") }
  if (a %eq% b)   { return("1") }
  if (b %eq% "0") { stop("Divide by zero error") }

  # allocate space for the output
  digits <- integer(nchar(a))

  # Set the conditions of the start of the loop
  start  <- 1
  end    <- start + nchar(b) - 1
  remain <- ''
  ia     <- '0'

  while (end <= nchar(a)) {
    # Carry through the last remainder
    ia <- paste0(remain, str_sub(a, start, end))

    # consume more digits from 'a' until 'ia' >= 'b'
    while ((ia %lt% b) && (end < nchar(a))) {
      end <- end + 1
      ia  <- paste0(remain, str_sub(a, start, end))
    }

    # Count how many times we can subtract 'b' from 'a'
    this_div <- 0
    while(ia %gte% b) {
      this_div <- this_div + 1
      ia       <- ia %sub% b
    }

    # The number of subtractions is the integer division of this segment
    digits[end] <- this_div
    remain      <- ia

    start  <- end + 1
    end    <- start
  }

  # Turn integer vector back into a string
  result <- paste(digits, collapse='')
  result <- sub("^0+", '', result) # Remove leading zeros for neatness
  if (result == '') {
    "0"
  } else {
    result
  }

}



#-----------------------------------------------------------------------------
#' Calcualte the modulo of the integer division of 2 long integers
#'
#' This is quite slow as it uses the technique of 'long division'
#' \url{https://en.wikipedia.org/wiki/Division_algorithm}
#'
#' @param a lhs. A big integer as a character string.
#' @param b rhs. A big integer as a character string.
#'
#' @return a %% b (as a bit integer in a character string)
#'
#' @export
#-----------------------------------------------------------------------------
'%mod%' <- function(a, b) {
  stopifnot(is.character(a), is.character(b))

  n   <- a %div% b
  i   <- n %mul% b
  rem <- a %sub% i

  rem
}















