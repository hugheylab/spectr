context('check_input Error Tests')


test_that('checkx', {
    
    expect_silent(checkX(x = c('aaa',2)))

})