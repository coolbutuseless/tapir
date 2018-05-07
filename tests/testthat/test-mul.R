context("mul")

test_that("multiplication works", {


  a <- '9127309128730192873109283710923781'
  b <- '2398429374923874298374928374923743982'
  expect_identical(a %mul% b, as.character(gmp::as.bigz(a) * gmp::as.bigz(b)))


  a <-   '123'
  b <-    '45'
  expect_identical(a %mul% b, as.character(as.integer(a) * as.integer(b)))


  for (i in seq_len(100)) {
    n1 <- as.integer(runif(1, 1, 100))
    n2 <- as.integer(runif(1, 1, 100))
    a  <- rbigint(n1)
    b  <- rbigint(n2)

    expect_identical(a %mul% b, as.character(gmp::as.bigz(a) * gmp::as.bigz(b)))
  }

})



test_that("division works", {


  a <- '2398429374923874298374928374923743982'
  b <- '9127309128730192873109283710923781'
  expect_identical(a %div% b, as.character(gmp::as.bigz(a) %/% gmp::as.bigz(b)))


  a <-   '123'
  b <-    '45'
  expect_identical(a %div% b, as.character(as.integer(a) %/% as.integer(b)))

  set.seed(1)
  for (i in seq_len(100)) {
    n1 <- as.integer(runif(1, 2, 100))
    n2 <- as.integer(runif(1, 2, 100))
    a  <- rbigint(n1)
    b  <- rbigint(n2)

    if (a %gt% b) {
      expect_identical(a %div% b, as.character(gmp::as.bigz(a) %/% gmp::as.bigz(b)))
    } else {
      expect_identical(b %div% a, as.character(gmp::as.bigz(b) %/% gmp::as.bigz(a)))
    }
  }

})





test_that("modulo works", {


  a <- '2398429374923874298374928374923743982'
  b <- '9127309128730192873109283710923781'
  expect_identical(a %mod% b, as.character(gmp::as.bigz(a) %% gmp::as.bigz(b)))


  a <-   '123'
  b <-    '45'
  expect_identical(a %mod% b, as.character(as.integer(a) %% as.integer(b)))

  set.seed(1)
  for (i in seq_len(100)) {
    n1 <- as.integer(runif(1, 2, 100))
    n2 <- as.integer(runif(1, 2, 100))
    a  <- rbigint(n1)
    b  <- rbigint(n2)

   expect_identical(a %mod% b, as.character(gmp::as.bigz(a) %% gmp::as.bigz(b)))

  }

})




