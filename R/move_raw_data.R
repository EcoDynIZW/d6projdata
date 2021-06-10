#' move raw data from any directoy to the _archive folder.
#' @param taxon_col column of the taxon name
#' @export
#' @examples
#' \dontrun{
#' fun_move_raw(taxon_col = "taxon_col")
#' }

fun_move_raw <- function(taxon_col){

  stud_spec <- base::gsub(pattern = " ",
                    replacement = "_",
                    x = tolower(taxon_col))

  dir <- here::here("data-raw",
                    base::list.files(path = here::here("data-raw"),
                         pattern = stud_spec))

  if (!base::dir.exists(base::paste0(dir, "/_archive")))
    base::dir.create(base::paste0(dir, "/_archive"),
                     showWarnings = TRUE,
                     recursive = TRUE)

  base::file.copy(data_path,
                  base::paste0(dir, "/_archive"))

}

#devtools::install()

#dat <- d6projdata::fun_move_raw(path = "C:/Users/wenzler/PopDynIZW Dropbox/Lab_Orga/D6_PopDynTeam/ProjectData/data-raw/2008_vulpes_vulpes_de_b_individual_gras/2008_vulpes_vulpes_de_b_individual_gras.xlsx")
