test_that("cluster_track works using defaults", {
  data(mini_ruff)
  x = as_ctdf(mini_ruff, s_srs = 4326, t_srs = "+proj=eqearth")
  cluster_track(x)

  expect_s3_class(x, "ctdf")
})
