#' create name of proj data files
#' @param x dataframe or sf object
#' @return a data name in a given format
#' @export
#' @examples
#' \dontrun{
#' proj_data_name(data)
#' }

proj_data_name <- function(x){
  base::paste(lubridate::year(min(x$timestamp, na.rm = TRUE)),
    base::gsub(" ", "-", base::readline("species:")),
    base::readline("country short:"),
    base::readline("type of data:"),
    base::readline("project editor:"),
    sep = "_")
}



