context('tdbscan')

data(pesa56511)
x = as_tdbscan(pesa56511)

z = tdbscan(x, eps = 6600, minPts = 8, maxLag = 6, borderPoints = TRUE)

test_that('tdbscan is sf', {

  expect_s3_class( z, 'sf' )

})
