#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a4_df <- data.frame(
  Systolic      = c(161,   161, 160, NA,    NA, 160,   141,   141,   140, NA),
  Diastolic     = c( NA,    NA,  90, 91,    91,  90,    NA,    NA,    90, NA),
  Comorbidity_1 = c( NA,    NA,  NA, NA,    NA,  NA, "E10", "E10", "E10", NA),
  Drug_1        = c( NA, "C07",  NA, NA, "C07",  NA,    NA, "C07",    NA, NA))

test_that("start_a4 works", {
  expect_equal(
    start_a4(start_a4_df),
    c("START-A4", "Appropriate", "Not Relevant",
      "START-A4", "Appropriate", "Not Relevant",
      "START-A4", "Appropriate", "Not Relevant",
      NA))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a4_comorbs <- c("E10", "E11", "E12", "E13", "E14")
start_a4_drugs   <- c("C07", "C08", "C09", "C03A", "C03EA")

start_a4_trigger <- data.frame(
  Systolic      = c(161, NA, rep(141, length(start_a4_comorbs))),
  Diastolic     = c( NA, 91, rep(NA, length(start_a4_comorbs))),
  Comorbidity_1 = c( NA, NA, start_a4_comorbs),
  Drug_1        = NA)

start_a4_appropriate1 <- data.frame(
  Systolic      = 161,
  Diastolic     = NA,
  Comorbidity_1 = NA,
  Drug_1        = start_a4_drugs)

start_a4_appropriate2 <- data.frame(
  Systolic      = NA,
  Diastolic     = 91,
  Comorbidity_1 = NA,
  Drug_1        = start_a4_drugs)

start_a4_appropriate3 <- data.frame(
  Systolic      = 141,
  Diastolic     = NA,
  Comorbidity_1 = expand.grid(start_a4_comorbs, start_a4_drugs)[, 1],
  Drug_1        = expand.grid(start_a4_comorbs, start_a4_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_a4(start_a4_trigger),
    rep("START-A4", 2 + length(start_a4_comorbs)))
})

test_that("all appropriate 1", {
  expect_equal(
    start_a4(start_a4_appropriate1),
    rep("Appropriate", length(start_a4_drugs)))
})

test_that("all appropriate 2", {
  expect_equal(
    start_a4(start_a4_appropriate2),
    rep("Appropriate", length(start_a4_drugs)))
})

test_that("all appropriate 3", {
  expect_equal(
    start_a4(start_a4_appropriate3),
    rep("Appropriate", length(start_a4_comorbs)*length(start_a4_drugs)))
})

