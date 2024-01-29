#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a5_df <- data.frame(
  Age           = c(84,      84,    85, 84),
  Comorbidity_1 = c("I20",   "I20", "I20", NA),
  Drug_1        = c(NA, "C10AA",    NA, NA)
)

test_that("start_a5 works", {
  expect_equal(
    start_a5(start_a5_df),
    c("START-A5", "Appropriate", "Not Relevant", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a5_comorbs <- c("I20", "I21", "I22", "I24", "I25",
                      "I63", "I64", "I65", "I66",
                      "I73.9", "I74", "G45",
                      "Z95.1", "Z95.5", "Z95.8")
start_a5_drugs   <- c("C10AA")

start_a5_trigger <- data.frame(
  Age           = 84,
  Comorbidity_1 = start_a5_comorbs,
  Drug_1        = NA
)

start_a5_appropriate <- data.frame(
  Age           = 84,
  Comorbidity_1 = expand.grid(start_a5_comorbs, start_a5_drugs)[, 1],
  Drug_1        = expand.grid(start_a5_comorbs, start_a5_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_a5(start_a5_trigger),
    rep("START-A5", length(start_a5_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_a5(start_a5_appropriate),
    rep("Appropriate", length(start_a5_comorbs) * length(start_a5_drugs))
  )
})
