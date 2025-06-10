test_that("cluster_track example from documentation works", {
  data(toy_ctdf_k2)
  x = as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth") |> 
      cluster_track()


  expect_s3_class(x, "ctdf")


  expect_true("cluster" %in% colnames(x))


  expect_error(
    as_ctdf(toy_ctdf_k2, crs = 4326, project_to = "+proj=eqearth") |> cluster_track(),
    NA
  )
})