#' @title Waffle
#' @name waffle
#' @description Create a waffle.
#' @param v a named vector of numeric values. It indicates the number of cells for each modalities.
#' @param ncol number of columns.
#' @param xy coordinates of the center of the waffle.
#' @param size size of a cell side.
#' @return An sf object of polygons is returned.
#' @examples
#' wafcells <- c(A = 2, B = 3, C = 7, D = 6, E = 4)
#' mywaf <- waffle(v = wafcells, ncol = 5, xy = c(0,0), size = 2)
#' plot(mywaf)
#' @export
waffle <- function(v, ncol, xy, size){
  nc <- sum(v)
  if(nc==0){return()}
  mod <- nc%%ncol
  if(mod>0){
    nrow <- ceiling(nc/ncol)
  }else{
    nrow <- floor(nc/ncol)
  }
  w = size * (ncol) / 2
  h = size * (nrow) / 2
  wcontainer <- sf::st_sfc(sf::st_polygon(list(cbind(
    c(xy[1] - w, xy[1] + w, xy[1] + w, xy[1] - w, xy[1] - w),
    c(xy[2] - h, xy[2] - h, xy[2] + h, xy[2] + h, xy[2] - h)))))
  wp <- sf::st_make_grid(x = wcontainer, cellsize = size, what = "polygons")
  type <- character(length(wp))
  x <- character(0)
  var <- names(v)
  for (i in 1:length(var)){
    x <- c(x,rep(var[i],floor(v[i])))
  }
  wp <- sf::st_sf(type = type, geometry = wp, stringsAsFactors = F)

  if(length(x)>0){wp$type[1:length(x)] <- x}
  wp <- wp[wp$type!="",]
  return(wp)
}


#' @title Waffle Map
#' @name wafflemap
#' @description Plot a waffle map.
#' @param x either an sf object or a data.frame with coordinates fields named "X" and "Y".
#' @param var names of the field to use in x.
#' @param ncol number of columns.
#' @param size size of a cell side.
#' @param ... other arguments from cartography::typoLayer.
#' @return An sf object of polygons is returned.
#' @examples
#' wafcells <- c(A = 2, B = 3, C = 7, D = 6, E = 4)
#' mywaf <- waffle(v = wafcells, ncol = 5, xy = c(0,0), size = 2)
#' plot(mywaf)
#' @export
#'
#
# x = world
# var = names(world)[4:9]
# ncol = 5
# size = 100000
wafflemap <- function(x, var, ncol, size, ...){
  if(methods::is(object = x, class2 = "sf")){
    mycentro <- function(x){
      truec <- function(x){
        x1 <- st_cast(x = x, to = "POLYGON", warn = FALSE )
        st_centroid(x1[which.max(st_area(x1)),])

      }
      wc <- do.call(rbind,truec(x))
      x
      world
      class(wc)
    }



    x <- cbind(sf::st_coordinates(mycentro(x)), x[,var, drop=T])
  }
  waf_l <- list()
  i = 1
  for (i in 1:nrow(x)){
    v <- as.vector(x[i,var])
    xy <- c(x[i,"X"], x[i,"Y"])
    waf_l[[i]] <- waffle(v = v, ncol = ncol, xy = xy, size = size)
  }
  waf <- do.call(rbind, waf_l)
  cartography::typoLayer(x = waf[order(waf$type),], var = "type", ...)
  return(invisible(waffle))
}


