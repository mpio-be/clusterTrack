require(dbo)
require(sf)
require(mapview)
mapviewOptions(fgb = FALSE)
require(tracktools)

# pesa 56511
d = dbq(
  q = 'SELECT distinct tagID, latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "56511"'
)
d = tracktools::argos_prepare(d)
d = unique(d, by = c("latitude", "longitude"))

pesa56511 = copy(d)[, let(tagID = NULL)]
usethis::use_data(pesa56511, overwrite = TRUE)


# lbdo66862
d = dbq(
  q = 'SELECT distinct tagID, latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "66862"'
)[, pk := .I]
d = d[!pk %in% c(97, 174, 626, 690, 1004, 1087, 1105, 1464, 2074)]
d = tracktools::argos_prepare(d)
d = unique(d, by = c("latitude", "longitude"))

# check tracktools::flagpts(d)

lbdo66862 = copy(d)[, let(pk = NULL, tagID = NULL)]
usethis::use_data(lbdo66862, overwrite = TRUE)


# ruff143789
d = dbq(
  q = 'SELECT distinct tagID, latitude,longitude,locationDate,locationClass, pk FROM ARGOS.2015_RUFF
        where tagID = "143789" '
)
d = d[
  locationDate < as.POSIXct('2015-07-15 00:00:00') &
    locationDate > as.POSIXct('2015-04-15 00:00:00')
]
#' ds = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)
#' mapview(ds)
#' tracktools::flagpts(d)

d = d[!pk %in% c(275128, 275129, 275130, 275134, 275268)]
d = tracktools::argos_prepare(d)
d = unique(d, by = c("latitude", "longitude"))


ruff143789 = d[, .(latitude, longitude, locationDate, locationClass)]
usethis::use_data(ruff143789, overwrite = TRUE)


# toy_ctdf_k3: 3 clusters, movement from a to b and back.
require(leaflet)
require(sf)
require(data.table)
require(mapedit)
require(mapview)
require(clusterTrack)
mapviewOptions(fgb = FALSE)

drawn_points = drawFeatures(leaflet() |> addTiles())
drawn_points$id = 1:nrow(drawn_points)

x = data.table(st_coordinates(drawn_points), id = drawn_points$id)
# cluster 1
sq = seq(
  from = as.POSIXct("2025-06-01 00:00:00", tz = "UTC"),
  to = as.POSIXct("2025-06-02 00:00:00", tz = "UTC"),
  length.out = 10
) +
  runif(10, min = -4 * 3600, max = 4 * 3600) |> sort()
# move to cluster 2
sq2 = seq(from = max(sq), by = "5 mins", length.out = 7) +
  runif(1, min = -4 * 3600, max = 4 * 3600) |> sort()
#cluster 2
sq3 = seq(from = max(sq2), by = "3 hours", length.out = 8) +
  runif(8, min = -10 * 3600, max = 10 * 3600) |> sort()
# move back to cluster 1
sq4 = seq(from = max(sq), by = "5 mins", length.out = 7) +
  runif(1, min = -4 * 3600, max = 4 * 3600) |> sort()
#cluster 1b
sq4 = seq(from = max(sq2), by = "3 hours", length.out = 19) +
  runif(19, min = -10 * 3600, max = 10 * 3600) |> sort()

ssq = c(sq, sq2, sq3, sq4) |> sort()

x[, datetime := ssq]
x[, id := NULL]

ctdf = as_ctdf(
  x,
  time = 'datetime',
  coords = c("X", "Y"),
  s_srs = 4326,
  t_srs = '+proj=eqearth'
)

plot(ctdf, by = 'filter')

o = cluster_segments(ctdf, sd = 1)

cc = st_as_sf(o)
tt = as_ctdf_track(ctdf)
mapview(cc) + mapview(tt)

toy_ctdf_k3 = x
setnames(toy_ctdf_k3, c("longitude", "latitude", "time"))

usethis::use_data(toy_ctdf_k3, overwrite = TRUE)
