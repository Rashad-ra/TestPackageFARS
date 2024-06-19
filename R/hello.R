library(usethis)
library(devtools)

use_github_action()
usethis::use_vignette("intro")
#' Read a FARS data file
#'
#' This function reads a CSV file containing FARS data and returns it as a tibble.
#'
#' @param filename A character string giving the path to the file.
#' @return A tibble containing the FARS data from the file.
#' @details If the file does not exist, the function will stop with an error message.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_data <- fars_read("AER.csv")
#' }
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a FARS filename
#'
#' This function creates a FARS filename for a given year.
#'
#' @param year An integer or a value that can be coerced to an integer, representing the year.
#' @return A character string with the FARS filename for the given year.
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for multiple years
#'
#' This function reads FARS data for multiple years and returns a list of tibbles.
#'
#' @param years A vector of integers representing the years.
#' @return A list of tibbles containing the FARS data for each year.
#' @details If a year does not correspond to a valid file, a warning is issued and NULL is returned for that year.
#' @importFrom dplyr mutate select
#' @examples
#' \dontrun{
#' data_list <- fars_read_years(c(2013, 2014, 2015))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize FARS data by year and month
#'
#' This function summarizes FARS data by year and month.
#'
#' @param years A vector of integers representing the years.
#' @return A tibble summarizing the number of accidents by year and month.
#' @details The function binds the yearly data into a single data frame, groups by year and month, and then summarizes the number of accidents.
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#' summary <- fars_summarize_years(c(2013, 2014, 2015))
#' }
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot FARS data on a map
#'
#' This function plots FARS data on a map for a given state and year.
#'
#' @param state.num An integer representing the state number.
#' @param year An integer representing the year.
#' @return A map with the plotted FARS data for the given state and year, or an invisible NULL if there are no accidents to plot.
#' @details If the state number is not valid, the function stops with an error message. If there are no accidents to plot, a message is displayed and NULL is returned.
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}



