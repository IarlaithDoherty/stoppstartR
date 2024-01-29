#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c5_df <- data.frame(
  Comorbidity_1 = c("F40",   "F40", NA),
  Drug_1        = c(NA, "N06AB", NA)
)

test_that("start_c5 works", {
  expect_equal(
    start_c5(start_c5_df),
    c("START-C5", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_c5_comorbs <- c("F40", "F41")
start_c5_drugs   <- c("N06AB", "N06AX16", "N06AX21", "N03AX16", "N06CA03")

start_c5_trigger <- data.frame(
  Comorbidity_1 = start_c5_comorbs,
  Drug_1        = NA
)

start_c5_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_c5_comorbs, start_c5_drugs)[, 1],
  Drug_1        = expand.grid(start_c5_comorbs, start_c5_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_c5(start_c5_trigger),
    rep("START-C5", length(start_c5_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_c5(start_c5_appropriate),
    rep("Appropriate", length(start_c5_comorbs) * length(start_c5_drugs))
  )
})
