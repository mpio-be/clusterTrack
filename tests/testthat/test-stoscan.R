context('stoscan')


require(data.table)

z = tdbscan(zbird, eps = 12, minPts   = 5, maxLag = 5, borderPoints = TRUE )
z = z[, clustID := factor(clustID)]

d = data.frame(zbird) %>% data.table
d = merge(z, d, by.x = 'id', by.y = 'sp.ID')

d = rbindlist(list(copy(d[, tagID := 'bird1']), copy(d[, tagID := 'bird2'])), use.names = TRUE)
d[tagID == 'bird2', x := x + 5]
d[tagID == 'bird2', x := x + 5]
d[!is.na(clustID), ID := paste0(tagID, '_', clustID)]

dp = dt2Convexhull(d, pid = 'ID', projection = '+proj=utm +zone=4 +datum=WGS84')

s = stoscan(dp)


# full function is working
test_that('stoscan is list', {

  expect_type( s,  'list' )

})


















