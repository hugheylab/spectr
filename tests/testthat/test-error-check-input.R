context('check_input Error Tests')


test_that('checkX', {
    
    expect_silent(checkX(x = c('aaa',2)))

})

test_that('checkSingleNum', {
    
    expect_silent(checkSingleNum(z = c('aaa',2), lower = 3, inclusive = TRUE))

})

test_that('checkTime', {
    
    expect_silent(checkTime(x = c('aaa',2)))

})