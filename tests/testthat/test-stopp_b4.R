#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b4_df <- data.frame(
  Heart_Rate     = c(50,    49,      50, 49, 50),
  Comorbidity_1  = c("I49.5",    NA, "I49.5", NA, NA),
  Drug_1         = c("C07", "C07",      NA, NA, NA)
)
colnames(stopp_b4_df)[1] <- "Lab Values: Heart Rate"

test_that("stopp_b4 works", {
  expect_equal(
    stopp_b4(stopp_b4_df),
    c("STOPP-B4", "STOPP-B4", "Appropriate", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

stopp_b4_comorbs <- c("I49.5", "R00.1", "I44.2", "I44.3",
                      "I45.5", "I45.9", "Q24.6")
stopp_b4_drugs   <- c("C07")

stopp_b4_trigger1 <- data.frame(
  Heart_Rate     = 50,
  Comorbidity_1  = expand.grid(stopp_b4_comorbs, stopp_b4_drugs)[, 1],
  Drug_1         = expand.grid(stopp_b4_comorbs, stopp_b4_drugs)[, 2]
)
colnames(stopp_b4_trigger1)[1] <- "Lab Values: Heart Rate"

stopp_b4_trigger2 <- data.frame(
  Heart_Rate     = 49,
  Comorbidity_1  = NA,
  Drug_1         = stopp_b4_drugs
)
colnames(stopp_b4_trigger2)[1] <- "Lab Values: Heart Rate"

stopp_b4_appropriate1 <- data.frame(
  Heart_Rate    = 49,
  Comorbidity_1 = NA,
  Drug_1        = NA
)
colnames(stopp_b4_appropriate1)[1] <- "Lab Values: Heart Rate"

stopp_b4_appropriate2 <- data.frame(
  Heart_Rate    = 50,
  Comorbidity_1 = stopp_b4_comorbs,
  Drug_1        = NA
)
colnames(stopp_b4_appropriate2)[1] <- "Lab Values: Heart Rate"


test_that("all triggered 1", {
  expect_equal(
    stopp_b4(stopp_b4_trigger1),
    rep("STOPP-B4", length(stopp_b4_comorbs) * length(stopp_b4_drugs))
  )
})

test_that("all triggered 2", {
  expect_equal(
    stopp_b4(stopp_b4_trigger2),
    rep("STOPP-B4", length(stopp_b4_drugs))
  )
})

test_that("all appropriate 1", {
  expect_equal(
    stopp_b4(stopp_b4_appropriate1),
    "Appropriate"
  )
})

test_that("all appropriate 2", {
  expect_equal(
    stopp_b4(stopp_b4_appropriate2),
    rep("Appropriate", length(stopp_b4_comorbs))
  )
})
