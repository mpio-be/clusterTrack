context('as_tdbscan')


data(pesa56511)
o = as_tdbscan(pesa56511)

# full function is working
test_that('as_tdbscan returns sf', {

  expect_s3_class( o, 'sf' )

})
