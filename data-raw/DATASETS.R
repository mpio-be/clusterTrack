
require(dbo)
require(tracktools)
require(mapview)


x = dbq(q = 'SELECT * FROM ARGOS.ARGOS_ALL where tagID = "56511"')

o = as_tdbscan(x, time = "locationClass")
o$day = as.numeric(o$locationDate) |>
  scale() |>
  as.numeric()
ll = st_lines(o)
mapview(o, zcol = 'day') + mapview(ll)


usethis::use_data(DATASET, overwrite = TRUE)
