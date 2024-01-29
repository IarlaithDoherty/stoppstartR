#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic Test -------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e2_df <- data.frame(
  Drug_1 = c("H02AB", "H02AB", "H02AB", "H02AB", NA),
  Drug_2 = c("M05BA", "M05BA",      NA, "M05BA", "M05BA"),
  Drug_3 = c("A12AA",      NA, "A12AA", "A12AA", "A12AA"),
  Drug_4 = c(NA, "A11CC", "A11CC", "A11CC", "A11CC")
)

test_that("start_e2 works", {
  expect_equal(
    start_e2(start_e2_df),
    c("START-E2", "START-E2", "START-E2", "Appropriate", "Not Relevant")
  )
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comprehensive Test -----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

start_e2_drugs1  <- c("H02AB")
start_e2_drugs2  <- c("M05BB", "M05BA")
start_e2_drugs3 <- c("A11GB01", "A11AA02", "A12AX",
                     "M05BB01", "M05BB02", "M05BB04", "M05BB05", "M05BB08")
start_e2_drugs3b <- c("A11GB01", "A11AA02", "A12AX")
start_e2_drugs3c <- c("A11GB01", "A11AA02", "M05BB01", "M05BB02")
start_e2_drugs4 <- c("A11CB", "A12AX",
                     "M05BB03", "M05BB04", "M05BB05",
                     "M05BB06", "M05BB07", "M05BB08")
start_e2_drugs4b <- c("A11CB", "A12AX")
start_e2_drugs4c <- c("A11CB", "M05BB03", "M05BB06", "M05BB07")



start_e2_trigger1 <- data.frame(
  Drug_1 = expand.grid(start_e2_drugs1, NA,
                       start_e2_drugs3b, start_e2_drugs4b)[, 1],
  Drug_2 = expand.grid(start_e2_drugs1, NA,
                       start_e2_drugs3b, start_e2_drugs4b)[, 2],
  Drug_3 = expand.grid(start_e2_drugs1, NA,
                       start_e2_drugs3b, start_e2_drugs4b)[, 3],
  Drug_4 = expand.grid(start_e2_drugs1, NA,
                       start_e2_drugs3b, start_e2_drugs4b)[, 4]
)

start_e2_trigger2 <- data.frame(
  Drug_1 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       NA, start_e2_drugs4c)[, 1],
  Drug_2 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       NA, start_e2_drugs4c)[, 2],
  Drug_3 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       NA, start_e2_drugs4c)[, 3],
  Drug_4 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       NA, start_e2_drugs4c)[, 4]
)

start_e2_trigger3 <- data.frame(
  Drug_1 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3c, NA)[, 1],
  Drug_2 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3c, NA)[, 2],
  Drug_3 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3c, NA)[, 3],
  Drug_4 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3c, NA)[, 4]
)

start_e2_appropriate <- data.frame(
  Drug_1 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3, start_e2_drugs4)[, 1],
  Drug_2 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3, start_e2_drugs4)[, 2],
  Drug_3 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3, start_e2_drugs4)[, 3],
  Drug_4 = expand.grid(start_e2_drugs1, start_e2_drugs2,
                       start_e2_drugs3, start_e2_drugs4)[, 4]
)

test_that("all triggered 1", {
  expect_equal(
    start_e2(start_e2_trigger1),
    rep("START-E2", length(start_e2_drugs3b) * length(start_e2_drugs4b))
  )
})

test_that("all triggered 2", {
  expect_equal(
    start_e2(start_e2_trigger2),
    rep("START-E2", length(start_e2_drugs2) * length(start_e2_drugs4c))
  )
})

test_that("all triggered 3", {
  expect_equal(
    start_e2(start_e2_trigger3),
    rep("START-E2", length(start_e2_drugs2) * length(start_e2_drugs3c))
  )
})

test_that("all appropriate", {
  expect_equal(
    start_e2(start_e2_appropriate),
    rep("Appropriate",
        length(start_e2_drugs1) * length(start_e2_drugs2) *
          length(start_e2_drugs3) * length(start_e2_drugs4))
  )
})
