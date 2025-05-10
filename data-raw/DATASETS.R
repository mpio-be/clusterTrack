
require(dbo)
require(sf)
require(mapview)
mapviewOptions(fgb = FALSE)
require(tracktools)

# pesa 56511
  d = dbq(q = 'SELECT distinct tagID, latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "56511"')
  d = tracktools::argos_prepare(d)
  d = unique(d, by = c("latitude", "longitude"))
  
  pesa56511 = copy(d)
  usethis::use_data(pesa56511, overwrite = TRUE)

# lbdo66867
  d = dbq(q = 'SELECT distinct tagID, latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "66867"')[, i := .I]
  d = d[!i%in% c(128,138,265,331,334)]
  d = tracktools::argos_prepare(d)
  d = unique(d, by = c("latitude", "longitude"))

  # check
  x = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)
  mapview(x, zcol = "i")

  lbdo66867 = copy(d)[, i := NULL]
  usethis::use_data(lbdo66867, overwrite = TRUE)
