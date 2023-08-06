# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Basic Test -------------------------------------------------------------------
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# start_c6_df <- data.frame(
#   Comorbidity_1 = c("G25.8",   "G25.8", NA),
#   Drug_1        = c(     NA, "N04BC04", NA))
#
# test_that("start_c6 works", {
#   expect_equal(
#     start_c6(start_c6_df),
#     c("START-C6", "Appropriate", "Not Relevant"))
# })
#
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Comprehensive Test -----------------------------------------------------------
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# start_c6_comorbs <- c("G25.8")
# start_c6_drugs   <- c("N04BC04", "N04BC05", "N04BC09")
#
# start_c6_trigger <- data.frame(
#   Comorbidity_1 = start_c6_comorbs,
#   Drug_1        = NA)
#
# start_c6_appropriate <- data.frame(
#   Comorbidity_1 = expand.grid(start_c6_comorbs, start_c6_drugs)[, 1],
#   Drug_1        = expand.grid(start_c6_comorbs, start_c6_drugs)[, 2])
#
# test_that("all triggered", {
#   expect_equal(
#     start_c6(start_c6_trigger),
#     rep("START-C6", length(start_c6_comorbs)))
# })
#
# test_that("all appropriate", {
#   expect_equal(
#     start_c6(start_c6_appropriate),
#     rep("Appropriate", length(start_c6_comorbs)*length(start_c6_drugs)))
# })
