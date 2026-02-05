test_that("basic calculation works", {
  result <- sample_size(
    sample = 'two-sample',
    design = 'parallel',
    outcome = 'mean',
    type = 'non-inferiority',
    x1 = 5.0,
    x2 = 5.0,
    SD = 0.1,
    delta = -0.05
  )

  expect_s3_class(result, "sample_size")
  expect_true(result$total > 0)
})
