#' create xlsx for the project data.
#' @param path The path where the data has to be stored
#' @return a tibble where you have to set the parameters by hand.
#' @export
#' @examples
#' \dontrun{
#' build_data_files()
#' }


build_data_files <- function(path = "."){
  # function for dates
  fun_date <- function(x){
    date_in <- base::readline(base::paste0("enter date ", x,": "))
    while(base::class(try(assertthat::is.date(base::as.Date(date_in)), silent = TRUE)) == "try-error"){
      print("wrong format. Enter date as following: 2017-01-01")
      date_in <- base::readline(base::paste0("enter date ", x,": "))
    }
    return(date_in)
  }
  # function for numeric
  fun_num <- function(x){
    num_in <- base::readline(paste0(x, ": "))
    while(is.na(suppressWarnings(base::as.numeric(num_in)))){
      print("wrong format. Enter a numeric value")
      num_in <- base::readline(paste0(x, ": "))
    }
    return(num_in)
  }
  # function for empty cols
  empty_as_na <- function(x){
    base::ifelse(base::as.character(x)!="", x, NA)
  }

  # function for epsg code
  fun_epsg <- function(){
    epsg_in <- c("4326", "3035", "25833", "other")[utils::menu(c("4326", "3035", "25833", "other"), title = "choose epsg?")]
    if(epsg_in %in% "other"){
      epsg_in <- base::readline("enter epsg code: ")
      while(is.na(suppressWarnings(base::as.numeric(epsg_in)))){
        print("wrong format. Enter a numeric value")
        epsg_in <- base::readline("enter epsg code: ")
      }
    }
    return(epsg_in)
  }


  data <- dplyr::tibble(proj_id = base::readline("project id:"),
                        proj_editor = base::readline("project editor:"),
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
                        EPSG_code = fun_epsg(),
                        data_type = base::readline("data type ending:"))

  data2 <- dplyr::mutate(data,
                         proj_name = base::paste(lubridate::year(data_from),
                                                 species,
                                                 study_country,
                                                 study_area,
                                                 type_of_data,
                                                 proj_editor,
                                                 sep = "_")) %>%
    dplyr::mutate_each(dplyr::funs(empty_as_na))

  if(!base::file.exists(base::paste(path, data2$proj_name, sep = "/"))){ # creates new folder per layer
    base::dir.create(base::paste(path, data2$proj_name, sep = "/"))
  }

xlsx::write.xlsx(data2, base::paste0(path, "/", data2$proj_name, "/", data2$proj_name, ".xlsx"),
                 row.names = FALSE)

}

#test <- d6projdata::build_data_files()

#devtools::install()

#d6projdata::build_data_files(path = "C:/Users/wenzler/PopDynIZW Dropbox/Lab_Orga/D6_PopDynTeam/ProjectData/data-raw")



