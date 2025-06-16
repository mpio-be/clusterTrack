
data(toy_ctdf_k2)
ctdf = as_ctdf(toy_ctdf_k2)

# Test .check_ctdf

test_that(".check_ctdf errors on non-ctdf input", {
  expect_error(.check_ctdf(toy_ctdf_k2) )
})


test_that(".check_ctdf errors on unsorted timestamp", {
  x = ctdf[sample(.N)]

  expect_error(.check_ctdf(x))
})


test_that(".check_ctdf errors when required columns missing", {

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
  ctdf = as_ctdf(test_dt)
  track = as_ctdf_track(ctdf)
  expect_true(nrow(track) == nrow(ctdf) - 1)
  geom_types = st_geometry_type(track)
  expect_true(all(geom_types == "LINESTRING"))
})

# Test summary.ctdf

test_that("summary.ctdf returns correct summary", {
  ctdf = as_ctdf(test_dt)
  ctdf[, cluster := c(1, 1, 2)]
  sum_tbl = summary(ctdf)
  expect_s3_class(sum_tbl, c("summary_ctdf", "data.table", "data.frame"))
  expect_equal(nrow(sum_tbl), 2)
  # check columns
  expect_true(all(c('start', 'stop', 'tenure', 'geometry', 'segment', 'N') %in% names(sum_tbl)))
  # N counts
  expect_equal(sum_tbl[N == 2, N], 2)
  expect_equal(sum_tbl[N == 1, N], 1)
})

# Test plot.ctdf

test_that("plot.ctdf runs without error", {
  ctdf = as_ctdf(test_dt)
  expect_silent(plot(ctdf))
})
