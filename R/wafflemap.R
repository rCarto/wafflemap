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
  if(nc==0){return(NULL)}
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
  wp <- sf::st_make_grid(x = wcontainer, cellsize = size, n = c(ncol, nrow),
                         what = "polygons")
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
#' library(wafflemap)
#' library(cartography)
#' data(bond)
#' plot(st_geometry(bond[bond$ISO2 %in% c("IS", "TR"), ]), bg = "ivory3", border = NA)
#' plot(bond$geometry, border = "ivory2", bg = "ivory3", col = "ivory1", add=TRUE)
#' wafflemap(x = bond, var = c("Sean.Connery", "Roger.Moore", "Pierce.Brosnan",  "Daniel.Craig"),
#'           ncol = 3, size = 100000, add=TRUE, col = carto.pal(pal1 = "multi.pal", n1 = 4))
#' @export
wafflemap <- function(x, var, ncol, size, ...){
  # x = world
  # var = c("Roger.Moore", "Sean.Connery")
  # ncol = 6
  # size = 200000



  if(methods::is(object = x, class2 = "sf")){
    sfcoord <- function(x){
      x0 <- sf::st_sf(id = 1:nrow(x), geometry = sf::st_geometry(x))
      x1 <- sf::st_cast(x = x0, to = "POLYGON", warn = F)
      x1$area <- sf::st_area(x1)
      x2 <- stats::aggregate(x1[,c("id", "area"), drop = T],
                             by = list(x1$id),
                             FUN = which.max)
      ind <- match(x0$id, x1$id) + x2$area - 1
      sf::st_geometry(x) <- sf::st_geometry(x1[ind,])
      sf::st_centroid(x)
    }


    x <- cbind(sf::st_coordinates(sfcoord(x)), x[,var, drop=T])
  }
  waf_l <- list()

  for (i in 1:nrow(x)){
    v <- as.vector(x[i,var])
    xy <- c(x[i,"X"], x[i,"Y"])
    waf_l[[length(waf_l)+1]] <- waffle(v = v, ncol = ncol, xy = xy, size = size)
  }
  waf <- do.call(rbind, waf_l)
  cartography::typoLayer(x = waf[order(waf$type),], var = "type", ...)
  return(invisible(waffle))
}



