#' move raw data from any directoy to the _archive folder.
#' @param taxon_col column of the taxon name
#' @param data_path complete path to the raw data with the file itself
#' @export
#' @examples
#' \dontrun{
#' fun_move_raw(taxon_col = "taxon_col", data_path = "path")
#' }

fun_move_raw <- function(taxon_col, data_path){

  stud_spec <- base::gsub(pattern = " ",
                    replacement = "_",
                    x = base::tolower(taxon_col))

  dir <- here::here("data-raw",
                    base::list.files(path = here::here("data-raw"),
                         pattern = stud_spec))

  if (!base::dir.exists(paths = base::paste0(dir, "/_archive")))
    base::dir.create(path = base::paste0(dir, "/_archive"),
                     showWarnings = TRUE,
                     recursive = TRUE)

  base::file.copy(from = data_path,
                  to = base::paste0(dir, "/_archive"))

}
