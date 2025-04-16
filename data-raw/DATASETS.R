
require(dbo)
require(sf)
require(mapview)


pesa56511 = dbq(q = 'SELECT latitude,longitude,locationDate,locationClass FROM ARGOS.ARGOS_ALL where tagID = "56511"')
usethis::use_data(pesa56511, overwrite = TRUE)
