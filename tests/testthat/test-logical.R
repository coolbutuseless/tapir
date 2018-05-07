context("logical")

test_that("multiplication works", {

  expect_true ("123" %gt%  "34")
  expect_false("34"  %gt% "123")
  expect_true ("34"  %gt%  "33")
  expect_false("34"  %gt%  "34")
  expect_false("34"  %gt%  "35")

  expect_true ("123" %gte%  "34")
  expect_false("34"  %gte% "123")
  expect_true ("34"  %gte%  "33")
  expect_true ("34"  %gte%  "34")
  expect_false("34"  %gte%  "35")

  expect_false("123" %lt%  "34")
  expect_true ("34"  %lt% "123")
  expect_false("34"  %lt%  "33")
  expect_false("34"  %lt%  "34")
  expect_true ("34"  %lt%  "35")

  expect_false("123" %lte%  "34")
  expect_true ("34"  %lte% "123")
  expect_false("34"  %lte%  "33")
  expect_true ("34"  %lte%  "34")
  expect_true ("34"  %lte%  "35")

  expect_false("123" %eq%  "34")
  expect_false("34"  %eq% "123")
  expect_false("34"  %eq%  "33")
  expect_true ("34"  %eq%  "34")
  expect_false("34"  %eq%  "35")

})
