#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_g1_df <- data.frame(
  Gender        = c(  "M",     "M", "M",   "F",      NA),
  Comorbidity_1 = c("N40",   "N40",  NA, "N40",   "N40"),
  Drug_1        = c(   NA, "G04CA",  NA,    NA, "G04CA"))

test_that("start_g1 works", {
  expect_equal(
    start_g1(start_g1_df),
    c("START-G1", "Appropriate", "Not Relevant", "Not Relevant", NA))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_g1_comorbs <- c("N40", "R33")
start_g1_drugs   <- c("G04CA")

start_g1_trigger <- data.frame(
  Gender        = "M",
  Comorbidity_1 = start_g1_comorbs,
  Drug_1        = NA)

start_g1_appropriate <- data.frame(
  Gender        = "M",
  Comorbidity_1 = expand.grid(start_g1_comorbs, start_g1_drugs)[, 1],
  Drug_1        = expand.grid(start_g1_comorbs, start_g1_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_g1(start_g1_trigger),
    rep("START-G1", length(start_g1_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_g1(start_g1_appropriate),
    rep("Appropriate", length(start_g1_comorbs) * length(start_g1_drugs)))
})
