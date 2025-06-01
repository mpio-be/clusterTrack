  
  # ! remove before release
  
  m = function(x,z) {

    mapview::mapviewOptions(fgb = FALSE)

    if(!missing(z)){
    x[, (z) := as.factor(get(z))]  
    x= st_as_sf(x)
    mapview::mapview(x, zcol = z, lwd = 4)

    }  else mapview::mapview(st_as_sf(x))
  }