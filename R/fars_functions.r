#' Read FARS data
#'
#' @description
#' Loads FARS data from provided file in CSV format.
#'
#' If a file does not exist, an execution is stopped.
#'
#' @param filename A name of file to load FARS data from. File must be in CSV format.
#'
#' @importFrom readr "read_csv"
#'
#' @return A data frame containing FARS data
#'
#'
#' @seealso  \code{\link{make_filename}}, \code{\link{fars_read_years}}
#'
#' @examples
#' df <- fars_read(make_filename(2015))
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

#' Make a name of file containing FARS data for given year
#'
#' Constructs a file name of FARS data file for specified year
#'
#' @param year A year to load data for
#'
#' @return A file name (without PATH) to be used with fars_read function
#'
#' @seealso  \code{\link{fars_read}}
#'
#' @examples
#' df <- fars_read(make_filename(2015))
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for multiple years
#'
#' @description
#' Loads FARS data for multiple years. Data files must be in current working directory.
#'
#' If a file for given year does not exist or cannot be loaded, a warning is logged.
#'
#' @param years An integer vector of years to load data for
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "mutate"
#' @importFrom dplyr "select"
#'
#' @return A list containing FARS data for given years
#'
#' @seealso  \code{\link{fars_read}}
#'
#' @examples
#' df_list <- fars_read_years(c(2013, 2014))
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

#' Provides a number of FARS incidents grouped by year and month
#'
#' @description
#' Loads FARS data for multiple years and groups them by year and month.
#'
#' If a file for given year does not exist or cannot be loaded, a warning is logged.
#'
#' @param years An integer vector of years to load data for
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "group_by"
#' @importFrom dplyr "summarize"
#' @importFrom tidyr "spread"
#'
#' @return A data frame containing number of incidents per year and month
#'
#' @seealso  \code{\link{fars_read}}, \code{\link{fars_read_years}}
#'
#' @examples
#' df_grp <- fars_summarize_years(c(2013, 2014))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Makes a geographical map of FARS incidents for given state
#'
#' @description
#' Loads FARS data for specified year and plots incidents happened in given state on a map
#' using incident's geographical position.
#'
#' If a wrong state number is provided, the execution is stopped.
#' If a file for given year does not exist or cannot be loaded, a warning is logged.
#'
#' @param state.num A state number
#'
#' @importFrom dplyr "filter"
#' @importFrom maps "map"
#' @importFrom graphics "points"
#'
#' @param year An year
#'
#' @seealso  \code{\link{fars_summarize_years}}

#' @examples
#' fars_map_state(1, 2014)
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
