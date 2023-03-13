context('dt2Track')


x = data.frame(pesa56511) %>% data.table
o = dt2Track(x, projection = sp::proj4string(pesa56511) )

# full function is working
test_that('dt2Track is track', {

  expect_s4_class( o, 'Track' )

})

