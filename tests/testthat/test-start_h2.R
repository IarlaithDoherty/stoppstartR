#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_h2_df <- data.frame(
  Drug_1 = c("N02A", "N02A", NA),
  Drug_2 = c(NA, "A06A", NA))

test_that("start_h2 works", {
  expect_equal(
    start_h2(start_h2_df),
    c("START-H2", "Appropriate", "Not Relevant"))
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_h2_drugs1 <- c("N02A",    "N07BC05", "N07BC06",
                     "N07BC02", "N07BC01", "R05DA04")
start_h2_drugs2 <- c("A06A", "A02AA04")

start_h2_trigger <- data.frame(
  Drug_1 = start_h2_drugs1,
  Drug_2 = NA)

start_h2_appropriate <- data.frame(
  Drug_1 = expand.grid(start_h2_drugs1, start_h2_drugs2)[, 1],
  Drug_2 = expand.grid(start_h2_drugs1, start_h2_drugs2)[, 2])

test_that("all triggered", {
  expect_equal(
    start_h2(start_h2_trigger),
    rep("START-H2", length(start_h2_drugs1)))
})

test_that("all appropriate", {
  expect_equal(
    start_h2(start_h2_appropriate),
    rep("Appropriate", length(start_h2_drugs1) * length(start_h2_drugs2)))
})
