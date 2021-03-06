% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arith_mul.R
\name{\%mul\%}
\alias{\%mul\%}
\title{Multiply 2 aribtrarily long integers}
\usage{
a \%mul\% b
}
\arguments{
\item{a}{lhs. A big integer as a character string.}

\item{b}{rhs. A big integer as a character string.}
}
\value{
a * b (as a bit integer in a character string)
}
\description{
Split each number into chunks of 4 digits and then multiple these chunks
using R's built-in integer multiply - the result of this chunk multiply
will always fit into R's 32 bit signed integer type.
}
