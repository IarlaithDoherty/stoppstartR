#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_d1_df <- data.frame(
  Comorbidity_1 = c("K21",   "K21", NA),
  Drug_1        = c(   NA, "A02BC", NA))

test_that("start_d1 works", {
  expect_equal(
    start_d1(start_d1_df),
    c("START-D1", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_d1_comorbs <- c("K21", "K22.2", "K22.7")
start_d1_drugs   <- c("A02BC")

start_d1_trigger <- data.frame(
  Comorbidity_1 = start_d1_comorbs,
  Drug_1        = NA)

start_d1_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_d1_comorbs, start_d1_drugs)[, 1],
  Drug_1        = expand.grid(start_d1_comorbs, start_d1_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_d1(start_d1_trigger),
    rep("START-D1", length(start_d1_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_d1(start_d1_appropriate),
    rep("Appropriate", length(start_d1_comorbs)*length(start_d1_drugs)))
})
