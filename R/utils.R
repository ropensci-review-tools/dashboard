#' Get time elapsed to current time of a date-time vector, in integer units of
#' largest interval.
#'
#' @param tvec Date-time vector.
#' @return Time elapsed in integer units of largest interval.
#' @noRd
get_elapsed_time <- function (tvec) {

    d0 <- lubridate::ymd_hms (Sys.time ())
    d1 <- lubridate::ymd_hms (tvec)
    dtime_days <- as.numeric (lubridate::interval (d1, d0)) / (24 * 3600)
    dtime_days [dtime_days < 1] <- 1 # Mimimum 1 day
    dtime <- cbind (
        round (dtime_days),
        round (dtime_days * 52 / 365),
        round (dtime_days * 12 / 365)
    )
    dtime [is.na (dtime)] <- 1 # just to suppress NA warnings; removed below

    units <- apply (dtime, 1, function (i) {
        ifelse (all (i <= 1L), 1L, max (which (i > 1L)))
    })
    units <- c ("days", "weeks", "months") [units]
    dtime <- apply (dtime, 1, function (i) {
        ilim <- ifelse (any (i > 1), 1L, 0L)
        utils::tail (i [which (i > ilim)], 1)
    })
    units [which (dtime == 1)] <- gsub ("s$", "", units [which (dtime == 1)])

    dtime <- paste0 (dtime, " ", units)
    dtime [which (is.na (d1))] <- NA

    return (list (dtime_days = round (dtime_days), dtime = dtime))
}
