context('tdbscan')



test_that('tdbscan is sf', {
  
  data(pesa56511)
  x = as_tdbscan(pesa56511, time = "locationDate")

  z = tdbscan(x, eps = 6600, minPts = 8, maxLag = 6, borderPoints = TRUE)

  expect_s3_class( z, 'sf' )

})
