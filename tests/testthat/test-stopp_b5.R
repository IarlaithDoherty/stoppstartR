#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b5_df <- data.frame(
  Comorbidity_1 = c("I47.1", "I47.1", NA),
  Drug_1        = c("C01BD01",      NA, NA))

test_that("stopp_b5 works", {
  expect_equal(
    stopp_b5(stopp_b5_df),
    c("STOPP-B5", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b5_comorbs <- c("I47.1", "I48")
stopp_b5_drugs   <- c("C01BD01")

stopp_b5_trigger <- data.frame(
  Comorbidity_1 = expand.grid(stopp_b5_comorbs, stopp_b5_drugs)[, 1],
  Drug_1        = expand.grid(stopp_b5_comorbs, stopp_b5_drugs)[, 2])

stopp_b5_appropriate <- data.frame(
  Comorbidity_1 = stopp_b5_comorbs,
  Drug_1        = NA)

test_that("all triggered", {
  expect_equal(
    stopp_b5(stopp_b5_trigger),
    rep("STOPP-B5", length(stopp_b5_comorbs) * length(stopp_b5_drugs)))
})

test_that("all appropriate", {
  expect_equal(
    stopp_b5(stopp_b5_appropriate),
    rep("Appropriate", length(stopp_b5_comorbs)))
})
