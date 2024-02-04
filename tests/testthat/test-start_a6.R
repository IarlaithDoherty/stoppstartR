test_that("Not Relevant", {
  start_a6_irrelevant <- expand.grid(
    Comorbidity_1 = c(NA),
    Drug_1 = c(NA, "C09")
  )

  expect_setequal(
    start_a6(start_a6_irrelevant),
    "Not Relevant"
  )
})

test_that("Appropriate", {
  start_a6_appropriate <- expand.grid(
    Comorbidity_1 = c("I20", "I21", "I22", "I24", "I25", "Z95.1", "Z95.5"),
    Drug_1 = c("C09")
  )

  expect_setequal(
    start_a6(start_a6_appropriate),
    "Appropriate"
  )
})

test_that("Triggered", {
  start_a6_triggered <- expand.grid(
    Comorbidity_1 = c("I20", "I21", "I22", "I24", "I25", "Z95.1", "Z95.5"),
    Drug_1 = c(NA)
  )

  expect_setequal(
    start_a6(start_a6_triggered),
    "START-A6"
  )
})
