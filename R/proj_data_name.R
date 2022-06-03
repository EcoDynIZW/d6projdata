#' create name of proj data files
#' @data dataframe or sf object
#' @return a data name in a given format
#' @export
#' @examples
#' \dontrun{
#' proj_data_name()
#' }

proj_data_name <- function(data){
  base::paste(lubridate::year(min(data$timestamp, na.rm = TRUE)),
    base::gsub(" ", "-", base::readline("species:")),
    base::readline("country short:"),
    base::readline("type of data:"),
    base::readline("project editor:"),
    sep = "_")
}



