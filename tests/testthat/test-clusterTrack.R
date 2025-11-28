test_that("cluster_track works using defaults", {
  data(toy_ctdf_k3)
  x = as_ctdf(toy_ctdf_k3, s_srs = 4326, t_srs = "+proj=eqearth")
  cluster_track(x)

  expect_s3_class(x, "ctdf")
})
