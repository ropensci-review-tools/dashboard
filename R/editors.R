#' Get current status of all rOpenSci editors.
#'
#' @return (Invisibly) A `data.frame` with one row per editor and some key
#' statistics.
#' @noRd
editor_gh_data <- function () {

    q <- gh_editors_team_qry (stats = FALSE)
    editors <- gh::gh_gql (query = q)
    editors <- editors$data$organization$team$members$nodes
    editors <- vapply (editors, function (i) i$login, character (1L))

    q <- gh_editors_team_qry (stats = TRUE)
    editors_stats <- gh::gh_gql (query = q)
    editors_stats <- editors_stats$data$organization$team$members$nodes
    editors_stats <- vapply (editors_stats, function (i) i$login, character (1L))

    editors <- data.frame (
        login = editors,
        stats = editors %in% editors_stats
    )

    has_next_page <- TRUE
    end_cursor <- NULL

    number <- state <- assignees <- updated_at <- closed_at <- titles <- NULL

    page_count <- 0L

    while (has_next_page) {

        q <- gh_issue_assignees_qry (
            org = "ropensci",
            repo = "software-review",
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        nodes <- dat$data$repository$issues$nodes

        number <- c (
            number,
            vapply (nodes, function (i) i$number, integer (1L))
        )
        state <- c (
            state,
            vapply (nodes, function (i) i$state, character (1L))
        )
        assignees <- c (
            assignees,
            lapply (nodes, function (i) {
                vapply (i$assignees$nodes, function (j) j$login, character (1L))
            })
        )
        updated_at <- c (
            updated_at,
            vapply (nodes, function (i) i$updatedAt, character (1L))
        )
        # closedAt is 'null' for open issues:
        closed_at <- c (
            closed_at,
            vapply (nodes, function (i) {
                ifelse (is.null (i$closedAt), "", i$closedAt)
            }, character (1L))
        )
        titles <- c (
            titles,
            vapply (nodes, function (i) i$title, character (1L))
        )

        page_count <- page_count + 1L
        message (
            "Retrieved page [", page_count, "] to issue number [",
            max (number), "]"
        )
    }

    # Reduce only to issues assigned to "editors" team members:
    index <- which (vapply (assignees, function (i) any (i %in% editors$login), logical (1L)))
    number <- number [index]
    state <- state [index]
    assignees <- assignees [index]
    updated_at <- updated_at [index]
    closed_at <- closed_at [index]
    titles <- titles [index]

    # Prefer closedAt over updatedAt for closed issues:
    index <- which (nzchar (closed_at))
    updated_at [index] <- closed_at [index]

    # Then find latest issue for each editor:
    ed_index <- vapply (editors$login, function (i) {
        index <- which (vapply (assignees, function (j) any (j == i), logical (1L)))
        ifelse (
            length (index) == 0L,
            NA_integer_,
            which (updated_at == max (updated_at [index]))
        )
    }, integer (1L))

    # And return data on those issues only:
    data.frame (
        editor = editors$login,
        stats = editors$stats,
        number = number [ed_index],
        state = state [ed_index],
        updated_at = updated_at [ed_index]
    )
}

#' Generate a summary report of current state of all rOpenSci editors
#'
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export

editor_status <- function () {
    dat <- editor_gh_data ()

    dat$status <- "FREE"
    dat$status [dat$state == "OPEN"] <- "BUSY"

    dat$inactive_for <- get_elapsed_time (dat$updated_at)

    # Suppress no visible binding notes:\
    stats <- status <- updated_at <- editor <- state <- inactive_for <-
        number <- NULL

    # desc status so "Free" before "Busy"
    dplyr::arrange (dat, stats, dplyr::desc (status), dplyr::desc (updated_at)) |>
        dplyr::select (-updated_at) |>
        dplyr::relocate (editor, status, state, inactive_for, number, stats)
}

#' Get time elapsed to current time of a date-time vector, in integer units of
#' largest interval.
#'
#' @param tvec Date-time vector.
#' @return Time elapsed in integer units of largest interval.
#' @noRd
get_elapsed_time <- function (tvec) {

    d0 <- lubridate::ymd_hms (Sys.time ())
    d1 <- lubridate::ymd_hms (tvec)
    dtime <- as.numeric (lubridate::interval (d1, d0)) / (24 * 3600) # in days
    dtime [dtime < 1] <- 1 # Mimimum 1 day
    dtime <- cbind (
        round (dtime),
        round (dtime * 52 / 365),
        round (dtime * 12 / 365)
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

    return (dtime)
}
