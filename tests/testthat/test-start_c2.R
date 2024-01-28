#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c2_df <- data.frame(
  Comorbidity_1 = c("F32",   "F32", NA),
  Drug_1        = c(NA, "N06AB", NA))

test_that("start_c2 works", {
  expect_equal(
    start_c2(start_c2_df),
    c("START-C2", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c2_comorbs <- c("F32", "F33")
start_c2_drugs   <- c("N06AB", "N06AF", "N06AG", "N06AX", "N06CA03")

start_c2_trigger <- data.frame(
  Comorbidity_1 = start_c2_comorbs,
  Drug_1        = NA)

start_c2_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_c2_comorbs, start_c2_drugs)[, 1],
  Drug_1        = expand.grid(start_c2_comorbs, start_c2_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_c2(start_c2_trigger),
    rep("START-C2", length(start_c2_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_c2(start_c2_appropriate),
    rep("Appropriate", length(start_c2_comorbs) * length(start_c2_drugs)))
})
