#' automatic plots for the projdata
#' @param data_sf the data that has to be plotted
#' @param out_path path where the plot has to be stored
#' @param out_name name of the plot
#' @param add_germany  A Boolean. If TRUE a germany map is added to the map
#' @return save the data to the original location
#' @export
#' @examples
#' \dontrun{
#'
#' }

auto_plot <- function(data_sf,
                      out_path,
                      out_name,
                      add_germany = FALSE){

  data_sf <- data_sf
  germany_sf <- fun_dl_ger_bor()

  if(add_germany == TRUE){
    ggplot2::ggplot() +
      geom_sf(data = germany_sf, fill = NA) +
      geom_sf(data = fun_inter_data_ger(pts_data = data_sf, germany_sf = germany_sf)) +
      geom_sf(data = data_sf,
              size = 2, shape = 18) +
      guides(color = guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = .5)) +
      theme(
        legend.position="bottom",
        legend.box = "vertical",
        legend.direction = "horizontal")
  } else{
  ggplot2::ggplot() +
    geom_sf(data = fun_inter_data_ger(pts_data = data_sf, germany_sf = germany_sf)) +
    geom_sf(data = data_sf,
          size = 2, shape = 18) +
    guides(color = guide_legend(
      direction = "horizontal",
      title.position = "top",
      title.hjust = .5)) +
    theme(
      legend.position="bottom",
      legend.box = "vertical",
      legend.direction = "horizontal")
  }

  ggsave(here(out_path, paste0(out_name,".pdf")), plot = last_plot(), device = "pdf",
         width = 20, height = 20, units = "cm")

}

## BKG DATA ------------------------------------------------------------------
## Germany bkg data utm32 (changes to wgs84 lonlat)
## Source: https://gdz.bkg.bund.de

#' download data from the bkg database
#' @return a shapefile of germany
#' @export
#' @examples
#' \dontrun{
#' fun_dl_ger_bor()
#' }


fun_dl_ger_bor <- function(){
url_list <- base::list("https://daten.gdz.bkg.bund.de/produkte/vg/nuts250_0101/aktuell/nuts250_01-01.utm32s.shape.zip",
                       "https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/vg2500_01-01.utm32s.shape.zip")

base::names(url_list) <- c("states", "regions")

chooser <- c("states", "regions")[utils::menu(c("states", "regions"), title = "decide between if you want to plot states or regions as background")]

zip_file <- here::here("data-raw", "geo-raw", base::paste0(chooser, ".zip"))
shp_path <- here::here("data-raw", "geo-raw", chooser)
if(!base::file.exists(shp_path)){
link <- url_list[[chooser]]

curl::curl_download(link, destfile = zip_file)
utils::unzip(zipfile = zip_file, exdir = shp_path)
} else{"path exists"}
germany_sf <- sf::st_read(base::list.files(shp_path,
                                           full.names = TRUE,
                                           recursive = TRUE,
                                           pattern = ".shp")[1]) %>%
  dplyr::select(base::ifelse(chooser == "states", "NUTS_NAME", "GEN")) %>%
  sf::st_transform(4326)

return(germany_sf)
}

# intersect data
#' dintersect point data with one of the germany shapefiles
#' @return a shapefile with part of germany
#' @param pts_data the point data
#' @export
#' @examples
#' \dontrun{
#' fun_inter_data_ger()
#' }

fun_inter_data_ger <- function(pts_data, germany_sf){
  data_pts <- pts_data
  data_ger <- germany_sf
  inter <- base::data.frame(inter = base::do.call(base::rbind,
                                                  base::lapply(sf::st_intersects(data_ger,
                                                                                 data_pts %>%
                                                                                   sf::st_transform(4326) %>%
                                                                                   sf::st_combine(),
                                                                                 sparse = TRUE),
                                                               function(x){ifelse(length(x) != 0, 1, 0)})))
  data_inter <- base::cbind(data_ger, inter) %>%
    dplyr::filter(inter %in% 1)
  return(data_inter)
}



