#' automatic plots for the projdata
#' @param path The path where the data lays
#' @return save the data to the original location
#' @export
#' @examples
#' \dontrun{
#'
#' }

library(ggplot2)

library(sf)
library(tidyverse)

auto_plot <- function(path){
  ger_fed <- st_read("C:/Users/wenzler/PopDynIZW Dropbox/GeoData/data-raw/germany/FederalStates_germany_03035_XXX_gpkg/germany_bl_3035.gpkg") %>%
    st_transform(4326)

  data_sf <- xlsx::read.xlsx(path, sheetIndex = 1) %>%
    st_as_sf(coords = c("x", "y"),
             crs = 4326,
             sf_column_name = "geometry",
             remove = FALSE)

  geom_sf(data = data_sf,
          size = 2, shape = 18) +
    # scale_color_brewer(palette = "PuOr",
    #                    name = "ID_new2",
    #                    na.value = "grey60") +
    guides(color = guide_legend(#title = "Boar ID",
      direction = "horizontal",
      title.position = "top",
      title.hjust = .5)) +
    theme(#legend.title = element_text("Species", colour = "Black"),
      #legend.position = c(0.8, 0.0),
      legend.position="bottom",
      legend.box = "vertical",
      legend.direction = "horizontal")


  ggsave(here("data-raw", "2017_aves_de_b_abundance_planillo", "2017_aves_de_b_abundance_planillo.pdf"), plot = last_plot(), device = "pdf",
         width = 20, height = 20, units = "cm")

}


# test
library(ows4R)
wfs_client  <- ows4R::WFSClient$new("http://sg.geodatenzentrum.de/web_download/downloadservice.atom.xml", serviceVersion = "2.0.0")

layer       <- wfs_client$ # layer name (incl. prefix, e. g.: "fis:")
  getCapabilities()$
  getFeatureTypes() %>%
  purrr::map_chr(function(x){x$getName()})

if(length(layer) > 1) stop(paste0("This function is not suited for WFS-sets with multiple layers. First layer here: ", layer[1]))

typename <- unlist(strsplit(layer, ":"))[2] # layer name without prefix

name     <- title_engl # takes as surrogate for option at top of function (name <- single_row[...])

title    <- wfs_client$ # layer title in German
  getCapabilities()$
  findFeatureTypeByName(layer)$
  getTitle()

crs      <- wfs_client$ # CRS
  getCapabilities()$
  findFeatureTypeByName(layer)$
  getDefaultCRS()[1]$input

link2       <- httr::parse_url(single_link)
link2$query <- list(service   = "wfs",
                    version   = "2.0.0",
                    request   = "GetFeature",
                    typenames = typename,
                    srsName   = paste0("EPSG:", unlist(strsplit(crs, ":"))[2])) # applies CRS to shapefile for download
request     <- httr::build_url(link2)
request2    <- sf::st_read(request)







# old

# library(rworldmap)
# library(rworldxtra)
#
# worldMap <- getMap("high")
# worldMap_sf <- as(worldMap, "sf")
#
# # Member States of the European Union
# europe <- c("Austria","Belgium",
#             "Czech Rep.","Denmark","France","Luxembourg","Netherlands","Poland", "Sweden", "Switzerland")
# # Select only the index of states member of the E.U.
# indEU <- worldMap_sf %>%
#   dplyr::select("NAME") %>%
#   filter(NAME %in% europe) %>%
  st_transform(4326)
