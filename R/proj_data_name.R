#' create name of proj data files
#' @return a data name in a given format
#' @export
#' @examples
#' \dontrun{
#' proj_data_name()
#' }

proj_data_name <- function(){
  base::paste(base::readline("year of the data:"),
    base::gsub(" ", "-", base::readline("species:")),
    base::readline("country short:"),
    base::readline("region/city:"),
    base::readline("type of data:"),
    base::readline("project editor:"),
    sep = "_")
}



