
require(dbo)
require(sf)
require(mapview)

pesa56511 = dbq(q = 'SELECT latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "56511"')
usethis::use_data(pesa56511, overwrite = TRUE)


d = dbq(q = 'SELECT latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "66867"')[, i := .I]
d = d[!i%in% c(128,138,265,331,334)]
x = st_as_sf(d, coords = c("longitude", "latitude"), crs = 4326)
mapview(x, zcol = "i")
tr =  as_ctdf(d, time = 'locationDate') |> track_segments() |> st_set_geometry('segment')

mapviewOptions(fgb = FALSE)
mapview(x, zcol = "i") + mapview(tr, zcol = "i")

lbdo66867 = copy(d)[, i := NULL]
usethis::use_data(lbdo66867, overwrite = TRUE)
