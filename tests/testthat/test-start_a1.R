test_that("Not Relevant", {
  start_a1_irrelevant <- expand.grid(
    Comorbidity_1 = c(NA),
    Drug_1 = c(NA, "B01AA", "B01AE", "B01AF")
  )

  expect_setequal(
    start_a1(start_a1_irrelevant),
    "Not Relevant"
  )
})

test_that("Appropriate", {
  start_a1_appropriate <- expand.grid(
    Comorbidity_1 = c("I48.2"),
    Drug_1 = c("B01AA", "B01AE", "B01AF")
  )

  expect_setequal(
    start_a1(start_a1_appropriate),
    "Appropriate"
  )
})

test_that("Triggered", {
  start_a1_triggered <- expand.grid(
    Comorbidity_1 = c("I48.2"),
    Drug_1 = c(NA)
  )

  expect_setequal(
    start_a1(start_a1_triggered),
    "START-A1"
  )
})
