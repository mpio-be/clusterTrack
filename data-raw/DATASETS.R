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


# ruff_test
data(ruff143789)
mini_ruff = ruff143789[1225:1500][, locationClass := NULL]

setnames(mini_ruff, "locationDate", "time")

usethis::use_data(mini_ruff, overwrite = TRUE)
