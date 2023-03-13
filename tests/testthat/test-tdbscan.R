context('tdbscan')

z = tdbscan(pesa56511, eps = 6600, minPts = 8, maxLag = 6, borderPoints = TRUE)

# full function is working
test_that('tdbscan is list', {

  expect_type( z,  'list' )

})

