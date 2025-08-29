#' Retrieve Watershed Stressor Data
#'
#' @param State
#'
#' @return
#' @export
#'
#' @examples

retrieve_stressor_data <- function(State){
  stateAbb <- state.abb[which(state.name == State)]

  files <- data(package = "CASToolWSStressorPckg", verbose = FALSE)$results[,3] %>%
    stringr::str_subset(paste0(stateAbb, "_"))

  # Use pblapply for parallel processing with progress bar
  ret_data_list <- pbapply::pblapply(files, function(file) {
    data(list = file, package = "CASToolWSStressorPckg", envir = environment())
    get(file, envir = environment())
  })

  # Bind rows of all data frames in the list
  ret_data <- dplyr::bind_rows(ret_data_list)

  return(ret_data)

}
