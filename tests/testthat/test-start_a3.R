#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a3_df <- data.frame(
  Comorbidity_1 = c("I20",   "I20", "I20", NA),
  Comorbidity_2 = c(NA, NA, "I48", NA),
  Drug_1        = c(NA, "B01AC", NA, NA)
)

test_that("start_a3 works", {
  expect_equal(
    start_a3(start_a3_df),
    c("START-A3", "Appropriate", "Not Relevant", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_a3_comorbs <- c("I20", "I21", "I22", "I24", "I25",
                      "I63", "I64", "I65", "I66",
                      "I73.9", "I74", "G45",
                      "Z95.1", "Z95.5", "Z95.8")
start_a3_drugs   <- c("B01AC")

start_a3_trigger <- data.frame(
  Comorbidity_1 = start_a3_comorbs,
  Drug_1        = NA
)

start_a3_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_a3_comorbs, start_a3_drugs)[, 1],
  Drug_1        = expand.grid(start_a3_comorbs, start_a3_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_a3(start_a3_trigger),
    rep("START-A3", length(start_a3_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_a3(start_a3_appropriate),
    rep("Appropriate", length(start_a3_comorbs) * length(start_a3_drugs))
  )
})
