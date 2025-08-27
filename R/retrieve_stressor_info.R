#' Retrieve Watershed Stressor Metadata
#'
#' @param State
#'
#' @return
#' @export
#'
#' @examples

retrieve_stressor_info <- function(State){
  stateAbb <- state.abb[which(state.name == State)]

  data(list = paste0("WSStressorInfo_", stateAbb), package = "CASToolWSStressorPckg", envir = environment())
  temp_data <- get(paste0("WSStressorInfo_", stateAbb), envir = environment())

  return(temp_data)
}
