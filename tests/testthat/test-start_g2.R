#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_g2_df <- data.frame(
  Gender        = c(  "M",     "M", "M",   "F",      NA),
  Comorbidity_1 = c("N40",   "N40",  NA, "N40",   "N40"),
  Drug_1        = c(   NA, "G04CB",  NA,    NA, "G04CB"))

test_that("start_g2 works", {
  expect_equal(
    start_g2(start_g2_df),
    c("START-G2", "Appropriate", "Not Relevant", "Not Relevant", NA))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_g2_comorbs <- c("N40", "R33")
start_g2_drugs   <- c("G04CB")

start_g2_trigger <- data.frame(
  Gender        = "M",
  Comorbidity_1 = start_g2_comorbs,
  Drug_1        = NA)

start_g2_appropriate <- data.frame(
  Gender        = "M",
  Comorbidity_1 = expand.grid(start_g2_comorbs, start_g2_drugs)[, 1],
  Drug_1        = expand.grid(start_g2_comorbs, start_g2_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_g2(start_g2_trigger),
    rep("START-G2", length(start_g2_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_g2(start_g2_appropriate),
    rep("Appropriate", length(start_g2_comorbs) * length(start_g2_drugs)))
})
