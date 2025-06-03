  
# ! remove before release
  
  m = function(x,z) {

    mapview::mapviewOptions(fgb = FALSE)

    if(!missing(z)){
    x = data.table(x)  
    x[, (z) := as.factor(get(z))]  
    x= st_as_sf(x)
    mapview::mapview(x, zcol = z, lwd = 4)

    }  else mapview::mapview(st_as_sf(x))
  }

#+ in-work

s = function(clust) {

  o =
    clust[cluster > 0, .(
      sta = min(timestamp),
      sto = max(timestamp),
      ten = difftime(max(timestamp), min(timestamp))
    ),
    by = .(cluster, .segment)
    ]
  setorder(o, sta, sto)
  o

}

ss = function(ctdf, segs) {
  x = ctdf[.segment %in% segs, range(.id)]
  o = ctdf[between(.id, x[1], x[2])]
  class(o) <- class(ctdf)
  o


}

#' p1(clust)
p1 = function(x) {
  require("ggplot2")
  require("forcats")



  s(x) |>
  ggplot(
    aes(
      y     = fct_reorder(cluster |> factor(), sta),
      x     = sta,
      xend  = sto
    )
  ) +
    geom_segment(size = 3) +
    geom_label(
      aes(
        x     = sta + (sto - sta) / 2,
        y     = fct_reorder(factor(cluster), sta),
        label = .segment,
        color = factor(.segment)
      ),
      nudge_y = 0.2,
      size = 3
    ) +
    scale_x_datetime(name = "Time") +
    scale_y_discrete(name = "Cluster") +
    labs(color = "Segment") +
    theme_minimal()
}

p2 = function(ctdf) {

  require("ggplot2")

  x = ctdf[ , .(
    sta = min(timestamp),
    sto = max(timestamp),
    dt = difftime(max(timestamp), min(timestamp))
  ),
  by = .(.segment)
  ]

  ggplot(
    x,
    aes(
      y     = .segment,
      x     = sta,
      xend  = sto
    )
  ) +
  geom_segment(size = 3) +

  geom_label(
  aes(
  x     = sta + (sto - sta) / 2,
  y     = .segment,
  label = .segment
  ),
  nudge_y = 0.2,
  size = 3
  ) +

  scale_x_datetime(name = "Time") +
  scale_y_discrete(name = "Segment") +
  labs(color = "Filter") +
  theme_minimal() +
  theme(legend.position = 'top')

}

map <- function(clust) {

require(mapview)
require(leaflet)
require(glue)

mapview::mapviewOptions(fgb = FALSE)

if (Sys.info()[["sysname"]] == "Linux")  mapviewOptions(fgb = FALSE)

x = copy(clust)
clus   = st_as_sf(x[cluster>0])  
nonclus = st_as_sf(x[cluster ==0])  
x[, cluster := factor(cluster)]

cluster_centroids = data.table(clus)
cluster_centroids =
  cluster_centroids[, .(
    start = min(timestamp),
    stop  = max(timestamp),
    tenure   = difftime(max(timestamp), min(timestamp), units = "days"),
    geometry = st_union(location) |> st_convex_hull() |> st_centroid(),
    segment  = unique(.segment),
    N        = .N

  ), by = cluster]
cluster_centroids[, lab := glue_data(
  .SD,
  "tenure:{round(tenure,1)}[d]      <br>
  start:{format(start, '%d-%b-%y')} <br>
  stop:{format(stop, '%d-%b-%y')}   <br>
  segment:{segment}                 <br>
  N:{N}"
)]
  
cluster_centroids = 
  cluster_centroids |>
  st_as_sf() |>
  st_transform(4326)
  
  


tr = as_ctdf_track(x) |> setDT()
tr[, let(segement = factor(.segment) )]
tr = st_as_sf(tr)




o = 
  mapview(map.types = c("CartoDB", "Esri.WorldImagery")) +
  mapview(nonclus, color = "#7e7f81cc", cex = 3,  legend = FALSE) +
  mapview(tr, legend = FALSE, color = "#7e7f81cc") +
  mapview(clus, zcol = "cluster", layer.name = "cluster")

clust_ico = awesomeIcons(
  icon       = NULL,
  text       = as.character(cluster_centroids$cluster),
  iconColor  = "black",
  library    = "fa",
  markerColor = "white"
)

o@map |>
  addAwesomeMarkers(
    data = cluster_centroids,
    icon = clust_ico, 
    popup = ~lab
  )


}
