#' function for rendering an markdown to get an html of the project data
#' @param path The path where the data has to be stored
#' @param data_name The name of the data
#' @param data_sf sf object
#' @param proj_data meta data file
#' @param data_plot data for plotting
#' @param id_col id column for plotting
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' render_projdata()
#' }

render_projdata <-
  function(path, data_name, data_sf, data_plot, proj_data, id_col) {
    rmarkdown::render(here::here("R", "projdata_template_html.Rmd"),
                      output_format = "html_document",
                      output_dir = path,
                      output_file = unlist(stringi::stri_split(path, regex = "/"))[length(unlist(stringi::stri_split(path, regex = "/")))])
  }
