test_that('checkX', {
  expect_error(checkX(x = c('aaa', 2)))
})

test_that('checkSingleNum', {
  expect_error(checkSingleNum(z = c('aaa', 2), lower = 3, inclusive = TRUE))
})

test_that('checkTime', {
  expect_error(checkTime(x = c('aaa', 2)))
})
