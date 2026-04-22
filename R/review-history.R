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
    m_review_history (quiet = quiet)
}

review_history_internal <- function (quiet = FALSE) {

    has_next_page <- TRUE
    end_cursor <- NULL

    # Suppress no visible binding notes:
    number <- opened_at <- closed_at <- submission_type <- state <-
        assignees <- opened_by <- author <- reviewers <- NULL
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
        opened_by <- c (
            opened_by,
            vapply (edges, function (i) {
                ifelse (
                    "login" %in% names (i$node$author),
                    i$node$author$login,
                    NA_character_
                )
            }, character (1L))
        )
        author <- c (
            author,
            lapply (edges, function (i) pkg_author_from_body (i$node$body))
        )
        reviewers <- c (
            reviewers,
            lapply (edges, function (i) reviewers_from_body (i$node$body))
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
    opened_by <- opened_by [pkg_index]
    author <- author [pkg_index]
    aut_name <- vapply (author, function (a) {
        ifelse (is.null (a$name), NA_character_, a$name)
    }, character (1L))
    aut_gh <- vapply (author, function (a) {
        ifelse (is.null (a$gh_handle), NA_character_, a$gh_handle)
    }, character (1L))
    # Remove HTML var delims:
    index <- grep ("@.*<", aut_gh)
    aut_gh [index] <- gsub (
        "<$", "",
        regmatches (aut_gh [index], regexpr ("@.*<", aut_gh [index]))
    )
    aut_gh <- gsub ("^@", "", aut_gh)
    index <- which (is.na (aut_gh))
    aut_gh [index] <- opened_by [index]
    # No check done for aut_gh == opened_by!

    reviewers <- reviewers [pkg_index]
    rev1 <- vapply (reviewers, function (i) i [1L], character (1L))
    rev2 <- vapply (reviewers, function (i) {
        ifelse (length (i) > 1L, i [2L], NA_character_)
    }, character (1L))

    assignee <- vapply (assignees, function (i) i [1], character (1L))

    # labels are then only used to identify stats submissions:
    stats <- vapply (labels, function (i) "stats" %in% i, logical (1L))
    index <- which (submission_type == "Stats" & !stats)
    if (length (index) > 0L) {
        n_txt <- paste0 (number [index], collapse = ", ")
        warning (
            "The following stats issues are missing 'stats' labels: ", n_txt
        )
        stats <- stats | submission_type == "Stats"
    }

    res <- data.frame (
        number = number,
        state = state,
        stats = stats,
        aut_gh = aut_gh,
        author_name = aut_name,
        editor = assignee,
        reviewer1 = gsub ("^@", "", rev1),
        reviewer2 = gsub ("^@", "", rev2),
        opened_at = lubridate::date (lubridate::ymd_hms (opened_at)),
        closed_at = lubridate::date (lubridate::ymd_hms (closed_at))
    ) |> dplyr::arrange (number)

    dtime <- lubridate::interval (res$opened_at, res$closed_at)
    res$duration_days <- as.numeric (dtime) / (24 * 3600)

    return (res)
}

m_review_history <- memoise::memoise (review_history_internal)

pkg_author_from_body <- function (body) {

    body <- strsplit (body, "\\n") [[1]]

    yaml_delim <- grep ("^\\-\\-\\-(\\r?)$", body)
    author <- list ()
    if (length (yaml_delim) > 0L) {
        y <- fake_yaml_parse (body, yaml_delim)

        fields <- c (
            "Submitting Author Name",
            "Submitting Author Github Handle",
            "Other Package Authors Github handles"
        )
        if (all (fields [1:2] %in% names (y))) {
            # These 'gh_handle' vals include HTML variable delimiters:
            author <- list (
                name = y [[fields [1]]],
                gh_handle = y [[fields [2]]]
            )
        } else if ("Submitting Author" %in% names (y)) {
            aut <- y [["Submitting Author"]]
            if (grepl ("@", aut)) {
                aut_gh <- regmatches (aut, regexpr ("@\\S+", aut))
                # Some issues have HTML pars with older field names:
                aut_gh <- gsub ("<\\!.*$", "", aut_gh)
                aut_gh <- gsub ("(\\)|\\])$", "", aut_gh)
                aut_nm <- regmatches (aut, regexpr ("^.*@", aut))
                aut_nm <- gsub ("^.*\\->", "", aut_nm)
                sp <- gregexpr ("\\s+", aut_nm) [[1]]
                aut_nm <- substring (aut_nm, 1L, max (sp) - 1L)
                author <- list (name = aut_nm, gh_handle = aut_gh)
            } else {
                author <- list (name = aut, gh_handle = NA_character_)
            }
        }
    }

    return (author)
}

reviewers_from_body <- function (body) {

    body <- strsplit (body, "\\n") [[1]]

    yaml_delim <- grep ("^\\-\\-\\-(\\r?)$", body)
    rev_out <- NA_character_
    if (length (yaml_delim) > 0L) {
        y <- fake_yaml_parse (body, yaml_delim)
        if ("Reviewers" %in% names (y)) {
            rev <- y [["Reviewers"]]
            ptns <- c ("<!--reviewers-list-->", "<!--end-reviewers-list-->")
            rev <- gsub (ptns [1], "", rev, fixed = TRUE)
            rev <- gsub (ptns [2], "", rev, fixed = TRUE)
            rev <- gsub ("^\\s*|\\s*$", "", rev)
            if (rev != "TBD") {
                rev_out <- strsplit (rev, "\\,\\s*") [[1]]
            }
        }
    }
    return (rev_out)
}

# Can't use yaml pkg because old-school bodies are too messy and fragile.
fake_yaml_parse <- function (body, yaml_delim) {

    y <- gsub ("\\s*(\\r?)$", "", body [seq_len (min (yaml_delim) - 1L)])
    y <- y [which (nzchar (y))]
    these_fields <- gsub ("\\:.*$", "", y)
    y <- gsub ("^\\s*", "", gsub ("^.*\\:(\\s*?)", "", y))
    names (y) <- these_fields

    return (y)
}

#' Generate historical data on logged review times in hours from airtable database.
#'
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export
rev_hours_airtable <- function () {

    m_rev_hours_airtable ()
}

rev_hours_airtable_internal <- function () {

    rev_hours <- airtabler::airtable (
        base = airtable_base_id, table = "reviews"
    )
    fields <- list ("id_no", "packages", "review_hours", "onboarding_url")
    rev_hours <-
        rev_hours$`reviews`$select_all (fields = fields)
    # airtable 'id_no' fields do not all equal actual issue numbers
    # (for example, id_no = 217 is for issue #214).
    rev_hours$number <- suppressWarnings (
        as.integer (gsub ("^.*\\/", "", rev_hours$onboarding_url))
    )
    rev_hours$review_hours <- suppressWarnings (
        as.numeric (rev_hours$review_hours)
    )
    rev_hours <- rev_hours [which (!is.na (rev_hours$number) & !is.na (rev_hours$review_hours)), ]

    # Suppress 'no visible binding' notes:
    number <- review_hours <- state <- stats <- editor <- opened_at <-
        closed_at <- duration_days <- NULL

    rev_hist <- m_review_history (quiet = TRUE)
    dplyr::left_join (rev_hours, rev_hist, by = "number") |>
        dplyr::select (number, review_hours, state, stats, editor, opened_at, closed_at, duration_days)
}

m_rev_hours_airtable <- memoise::memoise (rev_hours_airtable_internal)
