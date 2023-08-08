#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e4_df <- data.frame(
  Comorbidity_1 = c("M80",  "M80", NA),
  Drug_1        = c(   NA, "M05B", NA))

test_that("start_e4 works", {
  expect_equal(
    start_e4(start_e4_df),
    c("START-E4", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e4_comorbs <- c("M80", "M81",
                      "S12", "S22", "S32", "S42", "S52",
                      "S62", "S72", "S82", "S92",
                      "T02", "T08", "T10", "T12", "T14.2")
start_e4_drugs   <- c("M05B", "H05AA02")

start_e4_trigger <- data.frame(
  Comorbidity_1 = start_e4_comorbs,
  Drug_1        = NA)

start_e4_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_e4_comorbs, start_e4_drugs)[, 1],
  Drug_1        = expand.grid(start_e4_comorbs, start_e4_drugs)[, 2])

test_that("all triggered", {
  expect_equal(
    start_e4(start_e4_trigger),
    rep("START-E4", length(start_e4_comorbs)))
})

test_that("all appropriate", {
  expect_equal(
    start_e4(start_e4_appropriate),
    rep("Appropriate", length(start_e4_comorbs)*length(start_e4_drugs)))
})
