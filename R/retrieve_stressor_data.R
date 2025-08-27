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

  ret_data <- NULL

  for(i in 1:length(files)){
    data(list = files[i], package = "CASToolWSStressorPckg", envir = environment())
    temp_data <- get(files[i], envir = environment())

    ret_data <- ret_data %>% bind_rows(temp_data)
  }

  return(ret_data)

}
