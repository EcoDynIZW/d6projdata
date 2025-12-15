#' standardise the raw data.
#' @param data_path The path where the data has to be stored
#' @param taxon_col name of the taxon
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' build_data_files()
#' }

std_raw_data <- function(taxon_col, data_path){
  stud_spec <- base::gsub(pattern = " ",
                          replacement = "_",
                          x = base::tolower(taxon_col))

  dir <- here::here("data",
                    base::list.files(path = here::here("data"),
                                     pattern = stud_spec), "_archive")

  files <- lapply(list.files(dir, pattern = ".csv", recursive = TRUE, full.names = TRUE), utils::read.csv)

  files_new_gps <- lapply(files, function(x){
    data <- x %>%
      tidyr::drop_na(as.character(names(x)[utils::menu(names(x),
                                                title = "choose x")])) %>%
      sf::st_as_sf(coords = c(as.character(names(x)[utils::menu(names(x),
                                                            title = "choose x")]),
                          as.character(names(x)[utils::menu(names(x),
                                                            title = "choose y")])),
               crs = as.numeric(fun_epsg()),
               sf_column_name = "geometry") %>% # add search for x and y column
      sf::st_transform(4326) %>%
      dplyr::mutate(x_4326 = sf::st_coordinates()[,1],
                    y_4326 = sf::st_coordinates()[,2]) %>%
      sf::st_transform(3035) %>%
      dplyr::mutate(x_3035 = sf::st_coordinates()[,1],
                    y_3035 = sf::st_coordinates()[,2]) %>%
      sf::st_transform(25833) %>%
      dplyr::mutate(x_25833 = sf::st_coordinates()[,1],
                    y_25833 = sf::st_coordinates()[,2])

  })

}
