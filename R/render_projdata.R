#' function for rendering an markdown to get an html of the project data
#' @param path The path where the data has to be stored
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' render_projdata()
#' }

render_projdata <-
  function(path, data_name, data_sf, proj_data) {
    rmarkdown::render(here::here("R", "projdata_template_html.Rmd"),
                      output_format = "html_document",
                      output_dir = path,
                      output_file = unlist(stringi::stri_split(path, regex = "/"))[length(unlist(stringi::stri_split(path, regex = "/")))])
  }
