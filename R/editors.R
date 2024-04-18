#' Get current status of all rOpenSci editors.
#'
#' @param aggregation_period The time period for aggregation of timeline for
#' editorial loads. Must be one of "month", "quarter", or "semester".
#' @param quiet If `FALSE`, display progress information on screen.
#' @return (Invisibly) A `data.frame` with one row per editor and some key
#' statistics.
#' @noRd
editor_gh_data <- function (aggregation_period = "quarter", quiet = FALSE) {

    aggregation_period <- match.arg (
        aggregation_period,
        c ("month", "quarter", "semester")
    )

    q <- gh_editors_team_qry (stats = FALSE)
    editors <- gh::gh_gql (query = q)
    editors <- editors$data$organization$team$members$nodes
    editors <- vapply (editors, function (i) i$login, character (1L))

    q <- gh_editors_team_qry (stats = TRUE)
    editors_stats <- gh::gh_gql (query = q)
    editors_stats <- editors_stats$data$organization$team$members$nodes
    editors_stats <-
        vapply (editors_stats, function (i) i$login, character (1L))

    editors <- data.frame (
        login = editors,
        stats = editors %in% editors_stats
    )
    editors_stats <- editors_stats [which (!editors_stats %in% editors$login)]
    editors <- rbind (editors, data.frame (login = editors_stats, stats = TRUE))

    editors$general <- !editors$stats
    also_gen <- c ("adamhsparks", "mpadge")
    editors$general [editors$login %in% also_gen] <- TRUE

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

    # Extract timelines from full editorial data, include those no longer part
    # of current "editors" team:
    timeline <- editor_timeline (
        assignees, state, opened_at, updated_at, closed_at,
        aggregation_period = aggregation_period
    )

    # Then reduce data to only issues assigned to "editors" team members:
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
        timeline = timeline,
        reviews = editor_reviews (
            assignees, number, titles, state,
            opened_at, updated_at, closed_at
        )
    )
}

m_editor_gh_data <- memoise::memoise (editor_gh_data)

eds_to_remove <- c ("haozhu233", "mpadge")

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
        general = editors$general,
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
#' Editorial loads can be aggregated for different time periods of month,
#' quarter, or semester.
#'
#' @param assignees List of (potentially multiple) issue assignees
#' @param number Vector of issue numbers
#' @param state Vector of issue states
#' @param opened_at Vector of issue opening times
#' @param updated_at Vector of issue updated times
#' @param closed_at Vector of issue closing times
#' @param aggregation_period The time period for aggregation. Must be one of
#' "month", "quarter", or "semester".
#' @return A list of two `data.frame` objects, each with one column for each
#' editor, and rows tallying reviews for each time period, as defined by the row
#' names. The first list item is "issues_total", which tallies the total number
#' of concurrent issues, while the second is "issues_new", which tallies the
#' number of new issues assigned to each editor in each of the defined periods.
#' @noRd
editor_timeline <- function (assignees, state,
                             opened_at, updated_at, closed_at,
                             aggregation_period = "quarter") {

    aggregation_period <- match.arg (
        aggregation_period,
        c ("month", "quarter", "semester")
    )

    end_date <- closed_at
    index <- which (!nzchar (end_date))
    end_date [index] <- updated_at [index]
    start_date <- lubridate::date (lubridate::ymd_hms (opened_at))
    end_date <- lubridate::date (end_date)

    # Create matrix of editorial load, with hard-coded aggregation period.
    if (aggregation_period == "month") {
        period_txt <- "1 month"
        start_periods <- start_date
        end_periods <- end_date
        first_date <- min (start_date)
        lubridate::day (first_date) <- 1L
        all_periods <- seq (first_date, Sys.Date (), by = "1 month")
    } else if (aggregation_period == "quarter") {
        period_txt <- "quarter"
        start_periods <- lubridate::quarter (start_date, type = "date_first")
        end_periods <- lubridate::quarter (end_date, type = "date_last")
        all_periods <- seq (min (start_periods), Sys.Date (), by = "quarter")
    } else if (aggregation_period == "semester") {
        period_txt <- "6 months"
        start_periods <- lubridate::semester (start_date, with_year = FALSE)
        end_periods <- lubridate::semester (end_date, with_year = FALSE)
        start_periods <- lubridate::ymd (paste0 (
            lubridate::year (start_date), c ("-01-01", "-07-01") [start_periods]
        ))
        end_periods <- lubridate::ymd (paste0 (
            lubridate::year (end_date), c ("-01-01", "-07-01") [end_periods]
        ))
        # `seq.Date` only allows quarter or year:
        all_periods <- seq (min (start_periods), Sys.Date (), by = "quarter")
        all_periods <-
            as.Date (grep (("\\-0(1|7)\\-"), all_periods, value = TRUE))
    }
    all_periods <- format (all_periods, "%Y-%m")

    all_editors <- unique (unlist (assignees))
    issues_total <- issues_new <- matrix (
        0L,
        nrow = length (all_periods),
        ncol = length (all_editors),
        dimnames = list (all_periods, all_editors)
    )
    for (i in seq_along (all_editors)) {
        index <- which (assignees == all_editors [i])
        for (j in index) {
            seq_j <- seq (start_periods [j], end_periods [j], by = period_txt)
            index_j <- match (format (seq_j, "%Y-%m"), all_periods)
            issues_total [index_j, i] <- issues_total [index_j, i] + 1L

            index_j <- match (format (start_periods [j], "%Y-%m"), all_periods)
            issues_new [index_j, i] <- issues_new [index_j, i] + 1L
        }
    }

    list (
        issues_total = data.frame (issues_total),
        issues_new = data.frame (issues_new)
    )
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

#' Generate summary data of current and historical activities of all rOpenSci
#' editors
#'
#' @param aggregation_period The time period for aggregation of timeline for
#' editorial loads. Must be one of "month", "quarter", or "semester".
#' @param quiet If `FALSE`, display progress information on screen.
#' @return A list of four items:
#' \itemize{
#' \item{'status', a `data.frame` of current editor status, with one row per
#' editor and some key statistics.}
#' \item{'timeline_total', a `data.frame` representing the timelines for each
#' editor of total concurrent editorial load.}
#' \item{'timeline_new', a `data.frame` representing the timelines for each
#' editor of editorial load of new issues opened in each time period (default of
#' quarter-year.}
#' \item{'reviews', a `data.frame` with one row per issue and some key
#' statistics.} }
#' @export

editor_status <- function (quiet = FALSE, aggregation_period = "quarter") {

    aggregation_period <- match.arg (
        aggregation_period,
        c ("month", "quarter", "semester")
    )

    dat <- m_editor_gh_data (
        aggregation_period = aggregation_period,
        quiet = quiet
    )

    dat$latest$status <- "FREE"
    dat$latest$status [dat$latest$state == "OPEN"] <- "BUSY"

    dtime <- get_elapsed_time (dat$latest$updated_at)
    dat$latest$inactive_days <- dtime$dtime_days
    dat$latest$inactive_for <- dtime$dtime

    # Suppress no visible binding notes:\
    stats <- status <- updated_at <- editor <- inactive_for <-
        inactive_days <- NULL

    # desc status so "Free" before "Busy"
    status <- dplyr::arrange (
        dat$latest, stats, dplyr::desc (status), dplyr::desc (updated_at)
    ) |>
        dplyr::select (-updated_at) |>
        dplyr::relocate (editor, status, stats, inactive_for, inactive_days) |>
        dplyr::group_by (stats) |>
        dplyr::arrange (dplyr::desc (inactive_days), .by_group = TRUE) |>
        dplyr::ungroup ()

    edvac <- editor_vacation_status () # in editors-airtable.R
    # Note that next line presumes slack 'name' == GitHub handle:
    editors_on_vacation <- edvac$name [which (edvac$away)]
    status$status [status$editor %in% editors_on_vacation] <- "ON LEAVE"

    # Add additional columns from airtable data:
    status <- add_editor_airtable_data (status)

    # Then editor timelines
    month <- name <- NULL # Suppress no visible binding notes
    process_timeline <- function (dat, what = "issues_total") {
        what <- match.arg (what, c ("issues_total", "issues_new"))
        timeline <- dat$timeline [[what]]
        nms <- rownames (timeline)
        timeline$month <- lubridate::ymd (paste0 (nms, "-01"))
        timeline <- tidyr::pivot_longer (timeline, cols = -month) |>
            dplyr::arrange (tolower (name), month)
        dplyr::filter (timeline, !name %in% eds_to_remove)
    }
    timeline_total <- process_timeline (dat, what = "issues_total")
    timeline_new <- process_timeline (dat, what = "issues_new")

    # Finally, reviews for each editor
    # ed_rev <- split (dat$reviews, f = as.factor (dat$reviews$editor))
    # ed_rev <- lapply (ed_rev, function (i) {
    #     i$editor <- NULL
    #     i$closed_at [i$state == "OPEN"] <- NA
    #     return (i)
    # })
    ed_rev <- dat$reviews
    ed_rev$closed_at [ed_rev$state == "OPEN"] <- NA

    return (list (
        status = status,
        timeline_total = timeline_total,
        timeline_new = timeline_new,
        reviews = ed_rev
    ))
}

#' Extract average duration of reviews for each editor
#'
#' Note that is extracts data for all editors, including those who may not be
#' part of current editorial team. Current team members can be identified from
#' data returned from the \link{editor_status} function.
#'
#' @param nyears Length of time prior to current date over which average
#' durations should be calculated.
#' @return A `data.frame` with two columns of 'editor' and 'duration' in days.
#' @export
ed_rev_durations <- function (nyears = 2) {

    editor <- duration_days <- NULL # suppress no visible binding notes

    dat <- review_history (quiet = FALSE)

    t_from_closed_dat <- Sys.Date () - dat$closed_at
    index <- which (!is.na (t_from_closed_dat) & t_from_closed_dat <= (nyears * 365))
    dat [index, ] |>
        dplyr::group_by (editor) |>
        dplyr::summarise (duration = mean (duration_days))
}
