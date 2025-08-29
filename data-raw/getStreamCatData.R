#  Copyright statement here
#  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  R v4.4.3
#
#' @title Get StreamCat data
#' @description
#' Returns StreamCat watershed summary data for reaches in the specified state
#' and, if provided in the cluster data file, in the 300 m buffer around the
#' state outline.
#'
#' @details Uses API to query StreamCat data for available parameter names,
#' which are matched to previously identified anthropogenically-influenced
#' parameters. Next uses the API to aquire data corresponding to each of these
#' parameters within each reach's watershed.
#'
#' Uses the library dplyr, readxl, StreamCatTools, stringr, and tidyr.
#'
#' @param localdir Directory containing CASTool metadata
#' @param dir_results Directory containing cluster data
#' @param state Two letter state abbreviation of region of interest
#'
#' Will need to change this function if extrapolating to a region beyond a state
#'
#' @result Writes StreamCat_data_region.csv and StreamCat_stressor-info_region.csv
#' to the local directory if the download is successful. If unsuccessful returns NULL.

getStreamCatData <- function(state = ""){

  library(tidyverse)
  library(usethis)

  tryCatch({

    stateAbb <- state.abb[which(state.name == state)]

    data_bkginfo <- read.csv(file.path("data-raw", "Input", "SelectedStreamCatStressors.csv"))

    # Get all SC parameter names
    SCmetrics <- StreamCatTools::sc_get_params(param = 'metric_names')
    data_stressorinfoWS <- data.frame(SCmetrics)

    data_stressorinfoWS <- data_stressorinfoWS %>%
      dplyr::mutate(Year = suppressWarnings(
        dplyr::case_when(
          SCmetrics == "popden2010" ~ NA_integer_,
          grepl("^\\w*\\d{4}$", SCmetrics) ~ as.integer(sub("^\\w*(\\d{4})$",
                                                            "\\1", SCmetrics)),
          TRUE ~ NA_integer_))) %>%
      dplyr::mutate(StreamCatVar = suppressWarnings(
        dplyr::case_when(SCmetrics == "popden2010" ~ "popden2010",
                         grepl("\\d{4}$", SCmetrics) ~ sub("\\d{4}", "####", SCmetrics),
                         TRUE ~ SCmetrics))) %>%
      dplyr::select(SCmetrics, StreamCatVar, Year)

    # Match metric names in StreamCat to metric names desired
    data_stressorinfoWS <- merge(data_stressorinfoWS, data_bkginfo,
                                 by.x = "StreamCatVar", by.y = "variable",
                                 all.y = TRUE)
    data_stressorinfoWS <- data_stressorinfoWS %>%
      dplyr::mutate(StreamCatVar = sub("####", "", StreamCatVar),
                    Label = dplyr::case_when(grepl("^NABD", description) ~ description,
                                             grepl("^NPDES", description) ~ description,
                                             TRUE ~ stringr::str_to_sentence(description)))



    # Obtain actual data
    SCmetrics_nni <- data_stressorinfoWS %>%
      dplyr::filter(stringr::str_detect(SCmetrics, "_")) %>%
      dplyr::pull(SCmetrics) %>%
      paste(collapse = ",")
    SCmetrics_other <- data_stressorinfoWS %>%
      dplyr::filter(stringr::str_detect(SCmetrics, "_")== FALSE) %>%
      dplyr::pull(SCmetrics) %>%
      paste(collapse = ",")

    message("Downloading StreamCat data")

    data_stressorWS_nni <- StreamCatTools::sc_get_data(metric = SCmetrics_nni,
                                                       aoi = 'watershed',
                                                       state = stateAbb)
    data_stressorWS_other <- StreamCatTools::sc_get_data(metric = SCmetrics_other,
                                                         aoi = 'watershed',
                                                         state = stateAbb)

    data_stressorWS <- dplyr::full_join(data_stressorWS_nni,
                                        data_stressorWS_other,
                                        by = "comid")



    # Deal with buffer COMIDS
    STATE.shp <- sf::read_sf(file.path("data-raw", "Input","gadm41_USA_shp/gadm41_USA_1.shp")) %>% filter(NAME_1 == state) %>%
      sf::st_transform(crs = 5070) %>% sf::st_buffer(300)

    NHD.STATE <- nhdplusTools::get_nhdplus(AOI = STATE.shp) %>%
      dplyr::filter(ftype %in% c("Connector", "CanalDitch", "StreamRiver", "Drainageway", "ArtificialPath"))

    buffer <- setdiff(NHD.STATE$comid, data_stressorWS$comid)

    buffer_nni <- NULL
    buffer_other <- NULL


    for(k in 1:ceiling(length(buffer)/100)){
      start_ind <- ((k-1)*100) + 1
      end_ind <- 100 * k
      print(paste0("downloading buffer ", start_ind, ":", end_ind))

      temp_buffer <- buffer[start_ind:end_ind] %>% paste(collapse = ",")

      tryCatch({
        temp_nni <- StreamCatTools::sc_get_data(metric = SCmetrics_nni,
                                                aoi = 'watershed',
                                                comid = temp_buffer)
        temp_other <- StreamCatTools::sc_get_data(metric = SCmetrics_other,
                                                  aoi = 'watershed',
                                                  comid = temp_buffer)
      }, error = function(msg){
        print("Encountered error")
      })


      buffer_nni <- buffer_nni %>% bind_rows(temp_nni)
      buffer_other <- buffer_other %>% bind_rows(temp_other)
    }


    buffer_WS <- dplyr::full_join(buffer_nni %>% distinct(), buffer_other %>% distinct(), by = "comid")

    data_stressorWS <- data_stressorWS %>%
        dplyr::bind_rows(buffer_WS)

    data_stressorWS <- data_stressorWS %>%
      dplyr::rename_with(~sub("ws$", "", .)) %>%
      tidyr::pivot_longer(cols = !c(comid),
                          names_to = "StreamCatVar",
                          values_to = "WatershedValue")

    data_stressorWS <- data_stressorWS %>%
      dplyr::mutate(Year = suppressWarnings(dplyr::case_when(
        StreamCatVar == "popden2010" ~ NA_integer_,
        grepl("^\\w*\\d{4}$", StreamCatVar) ~
          as.integer(sub("^\\w*(\\d{4})$", "\\1", StreamCatVar)),
        TRUE ~ NA_integer_)),
        StreamCatVar = suppressWarnings(dplyr::case_when(
          StreamCatVar == "popden2010" ~ "popden2010",
          grepl("\\d{4}$", StreamCatVar) ~ sub("\\d{4}", "", StreamCatVar),
          TRUE ~ StreamCatVar)))

    # Trim data_stressorinfoWS to StreamCatVar and Label only
    data_stressorinfoWS <- dplyr::distinct(data_stressorinfoWS, StreamCatVar,
                                           SCmetrics, Year, Label)


    rda.name <- paste0("WSStressorInfo_", stateAbb)
    assign(rda.name, data_stressorinfoWS)
    do.call("use_data", list(as.name(rda.name), overwrite = TRUE))

    options(scipen = 100)

    for(q in 1:ceiling(nrow(data_stressorWS)/100000)){
      start_ind <- ((q-1)*100000) + 1
      end_ind <- 100000 * q
      print(paste0("zipping ", start_ind, ":", end_ind))

      temp_df <- data_stressorWS %>% slice(start_ind:end_ind)

      rda.name <- paste0(stateAbb, "_WSStressor_", q)
      assign(rda.name, temp_df)

      do.call("use_data", list(as.name(rda.name), overwrite = TRUE))
      }
  beepr::beep(8)

    if (nrow(data_stressorWS) != 0 & nrow(data_stressorinfoWS) != 0) {
      message("StreamCat data successfully written to localdir")
    }

  }, error = function(err) {
    message(paste0("Error downloading StreamCat data: ", err))
    return(NULL)
  })
  beepr::beep(8)
}
