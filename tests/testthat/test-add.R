context("add")

library(gmp)



test_that("chunked addition works", {

  a = '1111'
  b =  '222'
  expect_identical(a %add% b, as.character(as.integer(a) + as.integer(b)))

  a = '1111'
  b =  '999'
  expect_identical(a %add% b, as.character(as.integer(a) + as.integer(b)))

  a <- '2398429374923874298374928374923743982'
  b <- '9127309128730192873109283710923781'
  expect_identical(a %add% b, as.character(gmp::as.bigz(a) + gmp::as.bigz(b)))

  for (i in seq_len(100)) {
    n1 <- as.integer(runif(1, 1, 100))
    n2 <- as.integer(runif(1, 1, 100))
    a  <- rbigint(n1)
    b  <- rbigint(n2)

    expect_identical(a %add% b, as.character(gmp::as.bigz(a) + gmp::as.bigz(b)))

    if (a %gt% b) {
      expect_identical(a %sub% b, as.character(gmp::as.bigz(a) - gmp::as.bigz(b)))
    } else {
      expect_identical(b %sub% a, as.character(gmp::as.bigz(b) - gmp::as.bigz(a)))
    }
  }

})
