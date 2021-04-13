#' create xlsx for the project data.
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' build_data_files()
#' }


build_data_files <- function(){

  fun_date <- function(x){
    date_in <- base::readline(base::paste0("enter date ", x,": "))
    if(base::class(try(assertthat::is.date(base::as.Date(date_in)), silent = TRUE)) == "try-error"){
      print("wrong format. Enter date as following: 2017-01-01")
      date_in <- base::readline(base::paste0("enter date ", x,": "))
    }
    return(date_in)
  }

  fun_num <- function(x){
    num_in <- base::readline(paste0(x, ": "))
    if(is.na(suppressWarnings(base::as.numeric(num_in)))){
      print("wrong format. Enter a numeric value")
      num_in <- base::readline(paste0(x, ": "))
    }
    return(num_in)
  }

  data <- dplyr::tibble(proj_id = base::readline("project id:"),
                        Dep = fun_num("Department"),
                        sup_vis = base::readline("super visor:"),
                        data_name = base::readline("names of data files:"),
                        data_name_archive = base::readline("original names of data files in archive:"),
                        edited_unique_identifier = base::readline("edited unique identifier:"),
                        original_unique_identifier = base::readline("original unique identifier:"),
                        storage_folder = "data-raw",
                        study_country = base::readline("study country:"),
                        study_area = base::tolower(base::readline("study area:")),
                        species = base::tolower(base::readline("species:")),
                        type_of_sample = base::readline("type of sample:"),
                        type_of_data = base::readline("type of data:"),
                        type_of_measure = base::readline("type of measure:"),
                        repeated_measure = c("yes", "no")[utils::menu(c("yes", "no"), title = "repeated measure?")],
                        no_spec = fun_num("number of species"),
                        no_ind = fun_num("number of individuals"),
                        data_from = fun_date("from"),
                        data_to = fun_date("to"),
                        additional_info = base::readline("additional information:"),
                        publication = base::readline("publication name:"),
                        publication_doi = base::readline("publication doi:"),
                        metadata_description = base::readline("metadata description:"),
                        temporal_resolution_sec = fun_num("temporal resolution sec"),
                        temporal_resolution_min = fun_num("temporal resolution min"),
                        IZW_archive_path = base::readline("IZW archive path:"),
                        IZW_data_path = base::readline("IZW data path:"),
                        external_archives = base::readline("external archives:"),
                        sample_data = base::readline("sample data:"),
                        proc_data = base::readline("processed data:"),
                        cooperation = base::readline("cooperation:"),
                        data_owner = base::readline("data owner:"),
                        projection = base::readline("projection name:"),
                        EPSG_code = fun_num("epsg code"),
                        data_type = base::readline("data type ending:"))

  data2 <- dplyr::mutate(data, proj_name = base::paste(lubridate::year(data_from), species, study_country, study_area, type_of_data, sep = "_"),
    )
  data3 <- data2 %>% dplyr::mutate_if(base::nzchar, NA)

  return(data2)

}

#test <- d6projdata::build_data_files()

#devtools::install()





