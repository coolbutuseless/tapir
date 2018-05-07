

#-----------------------------------------------------------------------------
#' Greater than for TAPIR
#'
#' @param lhs A big integer as a character string.
#' @param rhs A big integer as a character string.
#'
#' @return boolean
#' @export
#-----------------------------------------------------------------------------
'%gt%' <- function(lhs, rhs) {
  lhs <- gsub('^0+', '', lhs); if (lhs == '') { lhs <- '0' }
  rhs <- gsub('^0+', '', rhs); if (rhs == '') { rhs <- '0' }


  if (nchar(lhs) > nchar(rhs)) {
    TRUE
  } else if (nchar(lhs) < nchar(rhs)) {
    FALSE
  } else {
    # equal number of digits
    lhs > rhs
  }
}



#-----------------------------------------------------------------------------
#' Less than for TAPIR
#'
#' @param lhs A big integer as a character string.
#' @param rhs A big integer as a character string.
#'
#' @return boolean
#' @export
#-----------------------------------------------------------------------------
'%lt%' <- function(lhs, rhs) {
  lhs <- gsub('^0+', '', lhs); if (lhs == '') { lhs <- '0' }
  rhs <- gsub('^0+', '', rhs); if (rhs == '') { rhs <- '0' }


  if (nchar(lhs) < nchar(rhs)) {
    TRUE
  } else if (nchar(lhs) > nchar(rhs)) {
    FALSE
  } else {
    # equal number of digits
    lhs < rhs
  }
}

#-----------------------------------------------------------------------------
#' Greater than for TAPIR
#'
#' @param lhs A big integer as a character string.
#' @param rhs A big integer as a character string.
#'
#' @return boolean
#' @export
#-----------------------------------------------------------------------------
'%gte%' <- function(lhs, rhs) {
  lhs <- gsub('^0+', '', lhs); if (lhs == '') { lhs <- '0' }
  rhs <- gsub('^0+', '', rhs); if (rhs == '') { rhs <- '0' }


  if (nchar(lhs) > nchar(rhs)) {
    TRUE
  } else if (nchar(lhs) < nchar(rhs)) {
    FALSE
  } else {
    # equal number of digits
    lhs >= rhs
  }
}



#-----------------------------------------------------------------------------
#' Less than for TAPIR
#'
#' @param lhs A big integer as a character string.
#' @param rhs A big integer as a character string.
#'
#' @return boolean
#' @export
#-----------------------------------------------------------------------------
'%lte%' <- function(lhs, rhs) {
  lhs <- gsub('^0+', '', lhs); if (lhs == '') { lhs <- '0' }
  rhs <- gsub('^0+', '', rhs); if (rhs == '') { rhs <- '0' }


  if (nchar(lhs) < nchar(rhs)) {
    TRUE
  } else if (nchar(lhs) > nchar(rhs)) {
    FALSE
  } else {
    # equal number of digits
    lhs <= rhs
  }
}



#-----------------------------------------------------------------------------
#' Less than for TAPIR
#'
#' @param lhs A big integer as a character string.
#' @param rhs A big integer as a character string.
#'
#' @return boolean
#' @export
#-----------------------------------------------------------------------------
'%eq%' <- function(lhs, rhs) {
  lhs <- gsub('^0+', '', lhs); if (lhs == '') { lhs <- '0' }
  rhs <- gsub('^0+', '', rhs); if (rhs == '') { rhs <- '0' }

  lhs == rhs
}


