#' function for creating an interactive leaflet plot. works for vector data only
#' @param x data
#' @param id_col the name of the id column
#' @return an interactive leaflet plot where you can click on the different ids to show
#' @export
#' @examples
#' \dontrun{
#' map_layers()
#' }

map_layers <- function(x, id_col) {

  x <- x |> dplyr::rename(id = id_col)

  x <- x |> sf::st_transform(4326)

  #number of groups
  k <- unique(x$id)

  # get geometry type
  type <- unique(sf::st_geometry_type(x)) |> droplevels()

  #base map
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  #loop through all groups and add a layer one at a time

  # polygons
  if(type %in% c("POLYGON", "MULTIPOLYGON")){
    for (i in k) {
      map <- map |>
        leaflet::addPolygons(
          data = x |> dplyr::filter(id == i), group = as.character(i)
        )
    }
  }

  # lines
  if(type %in% c("LINESTRING", "MULTILINESTRING")){
    for (i in k) {
      map <- map  |>
        leaflet::addPolylines(
          data = x |> dplyr::filter(id == i), group = as.character(i)
        )
    }
  }

  # points
  if(type %in% c("POINT", "MULTIPOINT")){
    for (i in k) {
      map <- map |>
        leaflet::addCircleMarkers(
          data = x |> dplyr::filter(id == i), group = as.character(i)
        )
    }
  }

  #create layer control
  map |>
    leaflet::addLayersControl(
      overlayGroups = k,
      options = leaflet::layersControlOptions(collapsed = FALSE)) |>
    leaflet:: hideGroup(k[-1]) #hide all groups except the 1st one

}
