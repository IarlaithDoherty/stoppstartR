#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e1_df <- data.frame(
  Comorbidity_1 = c("M05",     "M05", NA),
  Drug_1        = c(   NA, "L04AX01", NA))

test_that("start_e1 works", {
  expect_equal(
    start_e1(start_e1_df),
    c("START-E1", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e1_comorbs <- c("M05", "M06")
start_e1_drugs   <- c("L04AX01", "L04AX03", "L04AA13", "L04AD01", "A07EC01",
                      "P01BA", "M01CB", "M01CC")

start_e1_trigger <- data.frame(
  Comorbidity_1 = start_e1_comorbs,
  Drug_1        = NA)

start_e1_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_e1_comorbs, start_e1_drugs)[, 1],
  Drug_1        = expand.grid(start_e1_comorbs, start_e1_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_e1(start_e1_trigger),
    rep("START-E1", length(start_e1_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_e1(start_e1_appropriate),
    rep("Appropriate", length(start_e1_comorbs)*length(start_e1_drugs)))
})
