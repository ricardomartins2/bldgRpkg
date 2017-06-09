#' Reads a CSV file into a data frame.
#'
#' @description
#' Reads a CSV file from \code{filename} using readr::read_csv and converts
#' the resulting tibble into a data frame.
#'
#' @param filename File path of a CSV file (character)
#'
#' @return Returns a (data.frame) based on the CSV file.
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("data/accident_2013.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' Create filename with variable year
#'
#' @description
#' Create a filename in the format "accident_XXXX.csv.bz2 where XXXX is
#' replaced by a year supplied to the function.
#'
#' @param year Specified year of data to be used in the filename.
#'
#' @return This function returns a filename to be used for a specific year
#' of accident data.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename("2013")
#' }
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' Get Months and Year from Accident Data Files
#'
#' @description
#' Extracts Month and Year data from Accident datafiles for specified years.
#'
#' @param year Years of data to be retrieved.
#'
#' @return Returns a list of (data.frames) representing the month and year
#' data from available Accident data files.  Provides and error message if
#' years requested are not available.
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years("2013")
#' fars_read_years(2013:2015)
#' }
#'
#' @export
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

#' Calculates a summary of accidents per month for given years
#'
#' @description
#' Accepts a list of years \code{years} and provides a summary of total
#' accidents for each month in those years.
#'
#' @param years Years of data to be summarized.
#'
#' @return Returns a (data.frame) with number of accidents per month for
#' the specified years.
#'
#' importFrom dplyr bind_rows group_by summarize
#' importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years("2013")
#' fars_summarize_years(2013:2015)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' Creates a visual Map of Accidents
#'
#' @description
#' Creates a visual Map of Accident data for specified year \code{year}
#' occuring in a specified state \code{state.num}.
#'
#' @param state.num A number corresponding to a US state based on FARS data.
#' @param year A year for which to summarize data.
#'
#' @return Returns a map plot of the US with accidents for a specified state
#' and year.  Returns an error if the state or year provided are invalid.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(1,2013)
#' }
#'
#' @export
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
