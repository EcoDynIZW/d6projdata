#' function for rendering an markdown to get an html of the project data
#' @param path The path where the data has to be stored
#' @param data_name The name of the data
#' @param data_ras spatRaster object
#' @param proj_data meta data file
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' render_projdata_ras()
#' }

render_projdata_ras <-
  function(path, data_name, data_ras, proj_data) {
    rmarkdown::render(here::here("R", "projdata_template_ras_html.Rmd"),
                      output_format = "html_document",
                      output_dir = path,
                      output_file = unlist(stringi::stri_split(path, regex = "/"))[length(unlist(stringi::stri_split(path, regex = "/")))])
  }
