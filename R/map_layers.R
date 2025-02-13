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

  # get geometry type
  type <- unique(sf::st_geometry_type(x)) |> droplevels()

  #base map
  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  #loop through all groups and add a layer one at a time

  # polygons
  if(type %in% c("POLYGON", "MULTIPOLYGON")){
    map <- map |>
      leaflet::addPolygons(
        data = x,
        group = "All",
        popup = leafpop::popupTable(x)
      )

    for (i in unique(x$id)) {
      map <- map |>
        leaflet::addPolygons(
          data = x |> dplyr::filter(id == i),
          group = as.character(i),
          popup = leafpop::popupTable(x |> filter(id == i))
        )
    }
  }

  # lines
  if(type %in% c("LINESTRING", "MULTILINESTRING")){
    map <- map |>
      leaflet::addPolylines(
        data = x,
        group = "All",
        popup = leafpop::popupTable(x)
      )


    for (i in unique(x$id)) {
      map <- map |>
        leaflet::addPolylines(
          data = x |> filter(id == i),
          group = as.character(i),
          popup = leafpop::popupTable(x |> filter(id == i))
        )
    }
  }

  # points
  if(type %in% c("POINT", "MULTIPOINT")){
    map <- map |>
      leaflet::addCircleMarkers(
        data = x,
        group = "All",
        popup = leafpop::popupTable(x)
      )

    for (i in unique(x$id)) {
      map <- map |>
        leaflet::addCircleMarkers(
          data = x |> filter(id == i),
          group = as.character(i),
          popup = leafpop::popupTable(x |> filter(id == i))
        )
    }
  }


  # Add layers control with both individual groups and the "all groups" option
  map <- map |>
    leaflet::addLayersControl(
      overlayGroups = c("All", as.character(unique(x$id_col))),
      options = leaflet::layersControlOptions(collapsed = TRUE)
    ) |>
    leaflet::hideGroup(as.character(unique(x$id_col)))

  # Add custom JavaScript to handle the toggling and move the "All" button to the top
  map <- map |>
    htmlwidgets::onRender("
    function(el, x) {
      // Move the 'All' checkbox to the top of the layers control
      var controlContainer = document.querySelector('.leaflet-control-layers-overlays');
      var allGroupDiv = Array.from(controlContainer.children).find(function(div) {
        return div.innerText.includes('All');
      });

      if (allGroupDiv) {
        controlContainer.insertBefore(allGroupDiv, controlContainer.firstChild);
      }

      // Find the 'All' checkbox and all individual checkboxes
      var allCheckbox = document.querySelector('input[name=\"All\"]');
      var individualCheckboxes = [];

      document.querySelectorAll('input[type=\"checkbox\"]').forEach(function(checkbox) {
        if (checkbox.name !== 'All') {
          individualCheckboxes.push(checkbox);
        }
      });

      // Add change event to 'All' checkbox to toggle all individual checkboxes
      allCheckbox.addEventListener('change', function() {
        var checked = this.checked;
        individualCheckboxes.forEach(function(checkbox) {
          checkbox.checked = checked;
          var event = new Event('input', { bubbles: true });
          checkbox.dispatchEvent(event);
        });
      });

      // Add change event to individual checkboxes to sync 'All' checkbox
      individualCheckboxes.forEach(function(checkbox) {
        checkbox.addEventListener('change', function() {
          var allChecked = individualCheckboxes.every(function(checkbox) {
            return checkbox.checked;
          });
          allCheckbox.checked = allChecked;
        });
      });
    }
  ")

  # Display the map
  map

}
