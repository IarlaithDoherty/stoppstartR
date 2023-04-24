test_that("start_a functions are true for corresponding rows of
          trigger_start_a", {
  expect_equal(
    diag(rbind(
      start_a1(trigger_start_a),
      start_a3(trigger_start_a),
      start_a4(trigger_start_a),
      start_a5(trigger_start_a),
      start_a6(trigger_start_a),
      start_a7(trigger_start_a),
      start_a8(trigger_start_a))),
    trigger_start_a[, 1])
})
