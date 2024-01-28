#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e6_df <- data.frame(
  Comorbidity_1 = c("M10",   "M10", NA),
  Drug_1        = c(NA, "M04AA", NA))

test_that("start_e6 works", {
  expect_equal(
    start_e6(start_e6_df),
    c("START-E6", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e6_comorbs <- c("M10")
start_e6_drugs   <- c("M04AA")

start_e6_trigger <- data.frame(
  Comorbidity_1 = start_e6_comorbs,
  Drug_1        = NA)

start_e6_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_e6_comorbs, start_e6_drugs)[, 1],
  Drug_1        = expand.grid(start_e6_comorbs, start_e6_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_e6(start_e6_trigger),
    rep("START-E6", length(start_e6_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_e6(start_e6_appropriate),
    rep("Appropriate", length(start_e6_comorbs) * length(start_e6_drugs)))
})
