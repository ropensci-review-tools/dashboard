#' Generate historical data on software reviews.
#'
#' This is a reduced version of the `reviews_gh_data()` function, which returns
#' data only on dates of issue opening and closing, to be used to generate
#' historical patterns.
#'
#' @param quiet If `FALSE`, display progress information on screen.
#' @return (Invisibly) A `data.frame` with one row per issue and some key
#' statistics.
#' @export
review_history <- function (quiet = FALSE) {

    has_next_page <- TRUE
    end_cursor <- NULL

    # Suppress no visible binding notes:
    number <- opened_at <- closed_at <- submission_type <- state <-
        assignees <- NULL
    labels <- list ()

    page_count <- 0L

    while (has_next_page) {

        q <- gh_issues_qry_dates_states (
            org = "ropensci",
            repo = "software-review",
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        edges <- dat$data$repository$issues$edges

        number <- c (
            number,
            vapply (edges, function (i) i$node$number, integer (1L))
        )
        state <- c (
            state,
            vapply (edges, function (i) i$node$state, character (1L))
        )
        submission_type <- c (
            submission_type, submission_type_from_body (edges)
        )
        labels <- c (
            labels,
            lapply (edges, function (i) unname (unlist (i$node$labels)))
        )
        opened_at <- c (
            opened_at,
            vapply (edges, function (i) i$node$createdAt, character (1L))
        )
        closed_at <- c (
            closed_at,
            vapply (edges, function (i) {
                closed <- i$node$closedAt
                ifelse (is.null (closed), NA_character_, closed)
            }, character (1L))
        )
        assignees <- c (
            assignees,
            lapply (edges, function (i) {
                assignee <- i$node$assignees$nodes
                res <- NA_character_
                if (length (assignee) > 0) {
                    res <- vapply (assignee, function (i) i$login, character (1L))
                }
                return (res)
            })
        )

        page_count <- page_count + 1L
        if (!quiet) {
            message (
                "Retrieved page [", page_count, "] to issue number [",
                max (number), "]"
            )
        }
    }

    # Index of all historical full reviews + currently open submissions can be
    # identified with by "6/approved" labels, or more recent 'submission_type'
    # values for open issues:
    approved <- vapply (
        labels,
        function (i) any (grepl ("^6", i)),
        logical (1L)
    )
    std_stats <- submission_type %in% c ("Standard", "Stats") &
        is.na (closed_at)
    # There is also a "legacy" label on #137 that should be excluded:
    legacy <- vapply (labels, function (i) "legacy" %in% i, logical (1L))
    pkg_index <- which ((approved | std_stats) & !legacy)

    number <- number [pkg_index]
    opened_at <- opened_at [pkg_index]
    closed_at <- closed_at [pkg_index]
    submission_type <- submission_type [pkg_index]
    state <- state [pkg_index]
    labels <- labels [pkg_index]
    assignees <- assignees [pkg_index]

    assignee <- vapply (assignees, function (i) i [1], character (1L))

    # labels are then only used to identify stats submissions:
    stats <- vapply (labels, function (i) "stats" %in% i, logical (1L))


    res <- data.frame (
        number = number,
        state = state,
        stats = stats,
        editor = assignee,
        opened_at = lubridate::date (lubridate::ymd_hms (opened_at)),
        closed_at = lubridate::date (lubridate::ymd_hms (closed_at))
    ) |> dplyr::arrange (number)

    dtime <- lubridate::interval (res$opened_at, res$closed_at)
    res$duration_days <- as.numeric (dtime) / (24 * 3600)

    return (res)
}

m_review_history <- memoise::memoise (review_history)

#' Generate historical data on logged review times in hours from airtable database.
#'
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export
rev_hours_airtable <- function () {

    rev_hours <- airtabler::airtable (
        base = airtable_base_id, table = "reviews"
    )
    fields <- list ("id_no", "packages", "review_hours", "onboarding_url")
    rev_hours <-
        rev_hours$`reviews`$select_all (fields = fields)
    # airtable 'id_no' fields do not all equal actual issue numbers
    # (for example, id_no = 217 is for issue #214).
    rev_hours$number <- as.integer (gsub ("^.*\\/", "", rev_hours$onboarding_url))
    rev_hours$review_hours <- as.numeric (rev_hours$review_hours)
    rev_hours <- rev_hours [which (!is.na (rev_hours$number) & !is.na (rev_hours$review_hours)), ]

    rev_hist <- m_review_history (quiet = TRUE)
    dplyr::left_join (rev_hours, rev_hist, by = "number") |>
        dplyr::select (number, review_hours, state, stats, editor, opened_at, closed_at, duration_days)
}
