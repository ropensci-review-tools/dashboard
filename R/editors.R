#' Get current status of all rOpenSci editors.
#'
#' @param quiet If `FALSE`, display progress information on screen.
#' @return (Invisibly) A `data.frame` with one row per editor and some key
#' statistics.
#' @noRd
editor_gh_data <- function (quiet = FALSE) {

    q <- gh_editors_team_qry (stats = FALSE)
    stopifnot (nchar (q) == 267L)
    editors <- gh::gh_gql (query = q)
    editors <- editors$data$organization$team$members$nodes
    stopifnot (length (editors) > 0L)
    editors <- vapply (editors, function (i) i$login, character (1L))
    stopifnot (length (editors) > 0L)

    q <- gh_editors_team_qry (stats = TRUE)
    editors_stats <- gh::gh_gql (query = q)
    editors_stats <- editors_stats$data$organization$team$members$nodes
    editors_stats <-
        vapply (editors_stats, function (i) i$login, character (1L))
    stopifnot (length (editors_stats) > 0L)

    editors <- data.frame (
        login = editors,
        stats = editors %in% editors_stats
    )

    has_next_page <- TRUE
    end_cursor <- NULL

    number <- state <- assignees <- updated_at <-
        opened_at <- closed_at <- titles <- NULL

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
        opened_at <- c (
            opened_at,
            vapply (nodes, function (i) i$createdAt, character (1L))
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
        if (!quiet) {
            message (
                "Retrieved page [", page_count, "] to issue number [",
                max (number), "]"
            )
        }
    }

    # Reduce only to issues assigned to "editors" team members:
    index <- which (vapply (
        assignees,
        function (i) any (i %in% editors$login),
        logical (1L)
    ))
    stopifnot (length (index) > 0L)
    number <- number [index]
    state <- state [index]
    assignees <- clean_assignees (assignees [index], editors)
    updated_at <- updated_at [index]
    opened_at <- opened_at [index]
    closed_at <- closed_at [index]
    titles <- titles [index]

    # Prefer closedAt over updatedAt for closed issues:
    index <- which (nzchar (closed_at))
    updated_at [index] <- closed_at [index]

    list (
        latest = editor_latest_issue (
            editors, assignees, number, state, updated_at
        ),
        timeline = editor_timeline (
            editors, assignees, state, opened_at, updated_at, closed_at
        ),
        reviews = editor_reviews (
            assignees, number, titles, state,
            opened_at, updated_at, closed_at
        )
    )
}

m_editor_gh_data <- memoise::memoise (editor_gh_data)

#' Extract data on latest issue for each editor
#'
#' @param editors The `data.frame` of editors constructed at start of
#' `editor_gh_data()` function.
#' @param assignees List of (potentially multiple) issue assignees
#' @param number Vector of issue numbers
#' @param state Vector of issue states
#' @param updated_at Vector of issue updated times
#' @return A `data.frame` with one row per editor and some key statistics.
#' @noRd
editor_latest_issue <- function (editors, assignees, number,
                                 state, updated_at) {

    ed_index <- vapply (editors$login, function (i) {
        index <- which (vapply (
            assignees,
            function (j) any (j == i),
            logical (1L)
        ))
        ifelse (
            length (index) == 0L,
            NA_integer_,
            which (updated_at == max (updated_at [index]))
        )
    }, integer (1L))

    # Reduce data to those issues only:
    data.frame (
        editor = editors$login,
        stats = editors$stats,
        number = number [ed_index],
        state = state [ed_index],
        updated_at = updated_at [ed_index]
    )
}

#' Clean assignees to only ed team. The first element is always correct here.
#'
#' @noRd
clean_assignees <- function (assignees, editors) {
    vapply (assignees, function (i) {
        i [which (i %in% editors$login) [1]]
    }, character (1L))
}

#' Extract timelines of historical editorial load
#'
#' @noRd
editor_timeline <- function (editors, assignees, state,
                             opened_at, updated_at, closed_at) {

    end_date <- closed_at
    index <- which (!nzchar (end_date))
    end_date [index] <- updated_at [index]
    start_date <- lubridate::date (lubridate::ymd_hms (opened_at))
    end_date <- lubridate::date (end_date)

    # Create matrix of editorial load:
    start_months <- format (start_date, "%Y-%m")
    end_months <- format (end_date, "%Y-%m")
    date0 <- min (start_date)
    lubridate::day (date0) <- 1L
    all_months <- format (seq (date0, Sys.Date (), by = "1 month"), "%Y-%m")

    timelines <- matrix (
        0L,
        nrow = length (all_months),
        ncol = length (editors$login),
        dimnames = list (all_months, editors$login)
    )
    for (i in seq_along (editors$login)) {
        index <- which (assignees == editors$login [i])
        for (j in index) {
            seq_j <- seq (start_date [j], end_date [j], by = "1 month")
            index_j <- match (format (seq_j, "%Y-%m"), all_months)
            timelines [index_j, i] <- timelines [index_j, i] + 1L
        }
    }

    data.frame (timelines)
}

#' Collate reviews by editor
#'
#' @noRd
editor_reviews <- function (assignees, number, titles, state,
                            opened_at, updated_at, closed_at) {

    end_date <- closed_at
    index <- which (!nzchar (end_date))
    end_date [index] <- updated_at [index]
    start_date <- lubridate::date (lubridate::ymd_hms (opened_at))
    end_date <- lubridate::date (end_date)

    data.frame (
        editor = assignees,
        number = number,
        title = titles,
        state = state,
        opened_at = start_date,
        closed_at = end_date
    ) |>
        dplyr::arrange (tolower (assignees), number)
}

#' Generate a summary report of current state of all rOpenSci editors
#'
#' @param quiet If `FALSE`, display progress information on screen.
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export

editor_status <- function (quiet = FALSE) {
    dat <- m_editor_gh_data (quiet = quiet)

    dat$latest$status <- "FREE"
    dat$latest$status [dat$latest$state == "OPEN"] <- "BUSY"

    dtime <- get_elapsed_time (dat$latest$updated_at)
    dat$latest$inactive_days <- dtime$dtime_days
    dat$latest$inactive_for <- dtime$dtime

    # Suppress no visible binding notes:\
    stats <- status <- updated_at <- editor <- state <- inactive_for <-
        number <- inactive_days <- NULL

    # desc status so "Free" before "Busy"
    status <- dplyr::arrange (
        dat$latest, stats, dplyr::desc (status), dplyr::desc (updated_at)
    ) |>
        dplyr::select (-updated_at) |>
        dplyr::relocate (editor, status, stats, inactive_for, inactive_days) |>
        dplyr::group_by (stats) |>
        dplyr::arrange (dplyr::desc (inactive_days), .by_group = TRUE) |>
        dplyr::ungroup ()

    # Then editor timeline
    month <- name <- NULL # Suppress no visible binding notes
    timeline <- dat$timeline
    timeline$month <- lubridate::ymd (paste0 (rownames (dat$timeline), "-01"))
    timeline <- tidyr::pivot_longer (timeline, cols = -month) |>
        dplyr::arrange (tolower (name), month)
    eds <- unique (timeline$name)
    timeline$y <- match (timeline$name, eds)

    return (list (status = status, timeline = timeline))
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
