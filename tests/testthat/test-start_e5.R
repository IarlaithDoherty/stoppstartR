#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e5_df <- data.frame(
  Comorbidity_1 = c("R29.6", "R29.6", NA),
  Drug_1        = c(NA, "A11CC", NA)
)

test_that("start_e5 works", {
  expect_equal(
    start_e5(start_e5_df),
    c("START-E5", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e5_comorbs <- c("R29.6", "R26.3",
                      "W01", "W05", "W06", "W07",
                      "W08", "W10", "W18", "W19")
start_e5_drugs   <- c("A11CB", "A12AX",
                      "M05BB03", "M05BB04", "M05BB05",
                      "M05BB06", "M05BB07", "M05BB08")

start_e5_trigger <- data.frame(
  Comorbidity_1 = start_e5_comorbs,
  Drug_1        = NA
)

start_e5_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_e5_comorbs, start_e5_drugs)[, 1],
  Drug_1        = expand.grid(start_e5_comorbs, start_e5_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_e5(start_e5_trigger),
    rep("START-E5", length(start_e5_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_e5(start_e5_appropriate),
    rep("Appropriate", length(start_e5_comorbs) * length(start_e5_drugs))
  )
})
