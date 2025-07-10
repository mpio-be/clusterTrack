

# Test .check_ctdf

test_that(".check_ctdf errors on non-ctdf input", {
  expect_error(.check_ctdf(toy_ctdf_k2) )
})


test_that(".check_ctdf errors on unsorted timestamp", {
  ctdf = as_ctdf(toy_ctdf_k2)

  x = ctdf[sample(.N)]

  expect_error(.check_ctdf(x))
})


test_that(".check_ctdf errors when required columns missing", {
  ctdf = as_ctdf(toy_ctdf_k2)
  x = copy(ctdf)[, .id := NULL]
  expect_error(.check_ctdf(x))


})


# Test as_ctdf

test_that("as_ctdf errors on duplicated points", {
  x = rbind(toy_ctdf_k2, toy_ctdf_k2[1,])
  expect_error(as_ctdf(x))
})


test_that("as_ctdf warns on reserved columns", {
  toy_ctdf_k2 = copy(toy_ctdf_k2)
  toy_ctdf_k2[, .id := 1]
  expect_warning(as_ctdf(toy_ctdf_k2))
})

# Test as_ctdf_track

test_that("as_ctdf_track creates LINESTRING segments", {
  ctdf = as_ctdf(toy_ctdf_k2)
  track = as_ctdf_track(ctdf)
  expect_true(nrow(track) == nrow(ctdf) - 1)
  geom_types = st_geometry_type(track$track)
  expect_true(all(geom_types == "LINESTRING"))
})

# Test summary.ctdf

test_that("summary.ctdf returns correct summary", {
  ctdf = as_ctdf(toy_ctdf_k2)
  ctdf[c(1:2, 4:5), let(cluster = rep(c(1, 2), each = 2))][, .segment := cluster]
  sum_tbl = summary(ctdf)
  expect_s3_class(sum_tbl, c("summary_ctdf", "data.table", "data.frame"))
  expect_equal(nrow(sum_tbl), 2)
  expect_true(all(c(
    "cluster", "start", "stop", "geometry", "segments", "N", "tenure",
    "dist_to_next"
  ) %in% names(sum_tbl)))

})

# Test plot.ctdf

test_that("plot.ctdf runs without error", {
  ctdf = as_ctdf(toy_ctdf_k2)
  expect_silent(plot(ctdf))
})
