#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_b1_df <- data.frame(
  Comorbidity_1 = c("J40",     "J40", NA),
  Drug_1        = c(NA, "R03AK04", NA)
)

test_that("start_b1 works", {
  expect_equal(
    start_b1(start_b1_df),
    c("START-B1", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_b1_comorbs <- c("J40", "J41", "J42", "J43",
                      "J44", "J45", "J46")
start_b1_drugs   <- c("R03AK04", "R03AK05", "R03AK06", "R03AK07",
                      "R03AK08", "R03AK09", "R03AK10", "R03AK11",
                      "R03AK12", "R03AK13",
                      "R03AC", "R03AL", "R03BB")

start_b1_trigger <- data.frame(
  Comorbidity_1 = start_b1_comorbs,
  Drug_1        = NA
)

start_b1_appropriate <- data.frame(
  Comorbidity_1 = expand.grid(start_b1_comorbs, start_b1_drugs)[, 1],
  Drug_1        = expand.grid(start_b1_comorbs, start_b1_drugs)[, 2]
)

test_that("all triggered", {
  expect_equal(
    start_b1(start_b1_trigger),
    rep("START-B1", length(start_b1_comorbs))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_b1(start_b1_appropriate),
    rep("Appropriate", length(start_b1_comorbs) * length(start_b1_drugs))
  )
})
