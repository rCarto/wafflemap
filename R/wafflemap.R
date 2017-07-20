#' #' @title Waffle Map
#' #' @name wafflemap
#' #' @description Plot a waffle map.
#' #' @param x a data.frame, two first column must be longitudes and latitudes of
#' #' gridded data.
#' #' @param var name of the variables to plot (at least 2).
#' #' @export
#'
#'

waf <- function(v, lab, nw, nh, xy, size){
  coordX <- seq(from = xy[1], to = xy[1] + size * (nw - 1), by = size)
  coordY <- seq(from = xy[2], to = xy[2] + size * (nh - 1), by = size)
  spatGrid <- expand.grid(coordX, coordY)
  spatGrid$id <- seq(1, nrow(spatGrid), 1)
  coordinates(spatGrid) <- spatGrid[,1:2]
  gridded(spatGrid) <- TRUE
  g <-  methods::as(spatGrid, "SpatialPolygonsDataFrame")
  type <- character(length(g))
  x <- character(0)
  for (i in 1:length(lab)){
    x <- c(x,rep(lab[i],v[i]))
  }
  g@data <- data.frame(id = g@data$id, type, stringsAsFactors = FALSE)
  g$type[1:length(x)] <- x
  g <- g[g$type!="",]
  row.names(g) <- as.character(g$id)
  return(g)
}

library(cartography)
library(mapinsetr)
library(sf)
library(sp)

size = 200
nw = 10
mtq <- st_read(system.file("shape/martinique.shp", package="cartography"))
mtq_extract <- mtq[mtq$INSEE_COM %in% c(97201,97225, 97219, 97211, 97215, 97203, 97214, 97218),]
lab = c("C13_CS1", "C13_CS2", "C13_CS3", "C13_CS4", "C13_CS5", "C13_CS6")
df <- cbind(st_coordinates(st_centroid(mtq_extract)), mtq_extract[,lab, drop=T])


waf_l <- list()
i = 1
for (i in 1:nrow(df)){
  v <- df[i,lab] / 10

  nc <- sum(v)
  mod <- nc%%nw
  if(mod>0){nh <- ceiling(nc/nw)}else{nh <- floor(nc/nw)}
  c1 <- c(df[i,"X"], df[i,"Y"])
  x <- c1[1] - (nw * size / 2)
  y <- c1[2] - (nh * size / 2)
  xy <- c(x,y)
  waf_l[[i]] <- waf(v = v, lab = lab, nw = nw, nh = nh, xy = xy, size = size)
}

xx <- inset_rbinder(waf_l)
plot(mtq_extract$geometry,bg = "lightblue2")
plot(mtq$geometry, add=T, lwd = 0.5, col = "lightblue3")
plot(mtq_extract$geometry,add=T)
typoLayer(spdf = xx, var = "type", colNA = "white", border = "white",
          lwd = 0.5, add=T, legend.values.order = lab, legend.pos = "n",
          col = carto.pal("multi.pal", 6), legend.nodata = F)
lab2 <- rev(c("Agriculteurs", "Artisans", "Cadres","Prof. intermédiaires", "Employés", "Ouvriers"))
legendTypo(col = rev(carto.pal("multi.pal", 6)), categ = lab2, nodata = F, title.txt = "Catégories\nsocio professionnelles", pos = "bottomleft")
layoutLayer(title = "Les CSP en Martinique", sources = "",author = "", frame = T, theme = "blue.pal")
