#' move raw data from any directoy to the _archive folder.
#' @param data_name column of the taxon name
#' @param data_path complete path to the raw data with the file itself
#' @export
#' @examples
#' \dontrun{
#' fun_move_raw(taxon_col = "taxon_col", data_path = "path")
#' }

fun_move_raw <- function(data_name, data_path){

  if(!base::dir.exists(paths = here::here("data-raw", data_name, "/_archive")))
    base::dir.create(path = here::here("data-raw", data_name, "/_archive"),
                     showWarnings = TRUE,
                     recursive = TRUE)

  base::file.copy(from = data_path,
                  to = here::here("data-raw", data_name, "/_archive"))

}
