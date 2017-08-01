library(wafflemap)
wafcells <- c(A = 2, B = 3, C = 7, D = 6, E = 4)
mywaf <- waffle(v = wafcells, ncol = 5, xy = c(0,0), size = 2)
plot(mywaf)


mtq <- sf::st_read(system.file("shape/martinique.shp", package="cartography"))
k =  10
mtq$CS1 = mtq$C13_CS1 / k
mtq$CS2 = mtq$C13_CS2 / k
mtq$CS3 = mtq$C13_CS3 / k
mtq$CS4 = mtq$C13_CS4 / k
mtq$CS5 = mtq$C13_CS5 / k
mtq$CS6 = mtq$C13_CS6 / k
mtq_extract <- mtq[mtq$INSEE_COM %in% c(97201,97225, 97219, 97211, 97215, 97203, 97214, 97218),]
var = c("CS1", "CS2", "CS3", "CS4", "CS5", "CS6")


par(mar = c(0,0,0,0))
plot(mtq_extract$geometry,bg = "lightblue2")
plot(mtq$geometry, add=T, lwd = 0.5, col = "lightblue3")
plot(mtq_extract$geometry,add=T)
wafflemap(x = mtq_extract, var = var, ncol = 3, size = 500, add = T,
          border = "lightblue3", legend.pos = "topleft", legend.frame = T,
          lwd = 2,
          col = cartography::carto.pal("multi.pal", 6), legend.nodata = F)
cartography::legendTypo(col = cartography::carto.pal("multi.pal", 6),
           categ = var, nodata = F,
           title.txt = "Catégories\nsocio professionnelles", pos = "bottomleft")

legend(legend = "10 persons",cex = 2, text.col = "green",
       pch=20 ,pt.cex=2, x="topright", box.col = "blue",
       bg = "grey")


200

?legend
?xinch

load(file = "/home/tg/Documents/blg/bond/bond.RData")


library(sf)
world <- st_as_sf(world)

head(loc)
library(reshape2)
loc$actor <- as.character(loc$actor)

summary(loc)

ag <- aggregate(loc[3], by = list(ISO2 = loc$ISO2, actor007 = loc$actor), FUN = length)
head(ag)
bd <- dcast(data = ag, formula = ISO2~actor007, value.var = 'actor', fill = 0)
world <- merge(world, bd, by.x = "ISO2", by.y  = "ISO2", all.x = T)
for (i in 4:9){
  world[is.na(world[,i]),i] <- 0
}



truec <- function(x){
  x <- world[7,]
  x1 <- st_cast(x = x, to = "POLYGON", warn = FALSE )
  st_centroid(x1[which.max(st_area(x1)),])

}
wc <- do.call(rbind,apply(world, 1, truec))
par(mar = c(0,0,0,0))

library(sf)
st_centroid(world)
world2 <- st_cast(world, to = "POLYGON", warn = F)
row.names(world2)
world2


?st_cast
plot(world[world$ISO3%in%c("FRA"), "geometry"])
plot(st_centroid(world[world$ISO3%in%c("FRA"),]), add=T)
st_cast(world, to = "POLYGON")
?st_centroid

# library(wafflemap)
plot(world$geometry, bg = "ivory1", col = "ivory3", border = "ivory2", lwd = 0.5, add=T)
wafflemap(x = world, var = names(world)[4:9], ncol = 3, size = 100000, border = 'ivory3',
          col = cartography::carto.pal("multi.pal", 6), add=T)
replace


plot(world$geometry)

worldptpol <- st_geometry(mtq[1,])
pt <- st_centroid(pol)
xy <- st_coordinates(pt)
lab = c("A", "B", "C", "D", "E")
v = c(A = 2, B = 3, C = 2, D = 6, E = 1)
ncol = 5
nrow = 10
size = 100
w = size * (ncol) / 2
h = size * (nrow) / 2




plot(mtq_extract$geometry)
plot(xx, add=T)


# waf <- function(v, lab, nw, nh, xy, size){
#   coordX <- seq(from = xy[1], to = xy[1] + size * (nw - 1), by = size)
#   coordY <- seq(from = xy[2], to = xy[2] + size * (nh - 1), by = size)
#   spatGrid <- expand.grid(coordX, coordY)
#   spatGrid$id <- seq(1, nrow(spatGrid), 1)
#   coordinates(spatGrid) <- spatGrid[,1:2]
#   gridded(spatGrid) <- TRUE
#   g <-  methods::as(spatGrid, "SpatialPolygonsDataFrame")
#   type <- character(length(g))
#   x <- character(0)
#   for (i in 1:length(lab)){
#     x <- c(x,rep(lab[i],v[i]))
#   }
#   g@data <- data.frame(id = g@data$id, type, stringsAsFactors = FALSE)
#   g$type[1:length(x)] <- x
#   g <- g[g$type!="",]
#   row.names(g) <- as.character(g$id)
#   return(g)
# }


plot(mtq_extract$geometry,bg = "lightblue2")
plot(mtq$geometry, add=T, lwd = 0.5, col = "lightblue3")
plot(mtq_extract$geometry,add=T)
typoLayer(spdf = xx, var = "type", colNA = "white", border = "white",
          lwd = 0.5, add=T, legend.values.order = lab, legend.pos = "n",
          col = carto.pal("multi.pal", 6), legend.nodata = F)

lab2 <- rev(c("Agriculteurs", "Artisans", "Cadres","Prof. intermédiaires", "Employés", "Ouvriers"))
legendTypo(col = rev(carto.pal("multi.pal", 6)), categ = lab2, nodata = F, title.txt = "Catégories\nsocio professionnelles", pos = "bottomleft")
layoutLayer(title = "Les CSP en Martinique", sources = "",author = "", frame = T, theme = "blue.pal")

