
x <- world[1:8,]
x$iddd = 1:nrow(x)
x1 <- st_cast(x = x, to = "POLYGON", warn = F)
x1$area <- st_area(x1)
st_cast
for(i in x$iddd){
  x2 <- x1[x1$iddd%in%i,]
  st_geometry(x[i,]) <- st_geometry(st_centroid(x2[which.max(x2$area),]))
}


  x1 <- x1[which.max(x1$area),"geometry"]
  x1
x1


ff <- function(x){
x1$iddd
    which.max(x$area)
}

apply(X = x, MARGIN = 1, FUN = mc)

test <- function(x){
  x <- x[which.max(st_area(x)),]
}





?coordinates

truec(world)

  wc <- do.call(rbind,truec(x))
