#' function for creating the mcp of telemetry data
#' @param xy sf data
#' @param percent percentage of mcp. default 100%
#' @return mcps for all individuals
#' @export
#' @examples
#' \dontrun{
#' calc_mcp()
#' }

calc_mcp <- function (xy, percent = 100) {

  id      <- xy[[1]]
  id      <- factor(id)
  xy      <- as.data.frame(sf::st_coordinates(xy))
  r       <- split(xy, id)
  est.cdg <- function(xy) apply(xy, 2, mean)
  cdg     <- lapply(r, est.cdg)
  levid   <- levels(id)

  res     <- lapply(1:length(r), function(i) {
    k        <- levid[i]
    df.t     <- r[[levid[i]]]
    cdg.t    <- cdg[[levid[i]]]
    dist.cdg <- function(xyt) {
      d <- sqrt(((xyt[1] - cdg.t[1])^2) + ((xyt[2] - cdg.t[2])^2))
      return(d)
    }
    di       <- apply(df.t, 1, dist.cdg)
    key      <- c(1:length(di))
    acons    <- key[di <= stats::quantile(di, percent/100)]
    xy.t     <- df.t[acons, ]
    coords.t <- grDevices::chull(xy.t[, 1], xy.t[, 2])
    xy.bord  <- xy.t[coords.t, ]
    xy.bord  <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
    while(nrow(xy.bord) < 4) xy.bord  <- rbind(xy.bord[nrow(xy.bord), ], xy.bord)
    so       <- sf::st_as_sf(xy.bord, coords = c("X", "Y"), crs = 4326)
    so       <- sf::st_combine(so)
    so       <- sf::st_cast(so, "POLYGON")
    so       <- sf::st_sf(so)
    so       <- sf::st_set_geometry(so, value = "geometry")

    return(so)
  })

  df <- do.call(rbind, res)
  df$id <- levels(id)

  return(df)
}
