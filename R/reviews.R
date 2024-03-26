#' Apply the 'gh_issues_qry' query to extract data from GitHub GraphQL API, and
#' post-process into a `data.frame`.
#'
#' @param quiet If `FALSE`, display progress information on screen.
#' @param open_only Include only open issues?
#' @return (Invisibly) A `data.frame` with one row per issue and some key
#' statistics.
#' @noRd
reviews_gh_data <- function (open_only = TRUE, quiet = FALSE) {

    has_next_page <- TRUE
    end_cursor <- NULL

    # Suppress no visible binding notes:
    number <- assignees <- created_at <- last_edited_at <-
        updated_at <- stage <- stage_date <- state <- titles <-
        submission_type <- NULL
    # The "event_" field come from the timeline data, and include data on all
    # events, both addition and removal of labels. "labels" holds the current
    # labels only.
    labels <- event_labels <- event_dates <- event_actors <- comments <- list ()

    page_count <- 0L

    while (has_next_page) {

        q <- gh_issues_qry (
            org = "ropensci",
            repo = "software-review",
            open_only = open_only,
            end_cursor = end_cursor
        )
        dat <- gh::gh_gql (query = q)

        has_next_page <- dat$data$repository$issues$pageInfo$hasNextPage
        end_cursor <- dat$data$repository$issues$pageInfo$endCursor

        edges <- dat$data$repository$issues$edges

        submission_type <- c (
            submission_type, submission_type_from_body (edges)
        )
        number <- c (
            number,
            vapply (edges, function (i) i$node$number, integer (1L))
        )
        state <- c (
            state,
            vapply (edges, function (i) i$node$state, character (1L))
        )
        assignees <- c (
            assignees,
            lapply (edges, function (i) unlist (i$node$assignees$nodes))
        )
        created_at <- c (
            created_at,
            vapply (edges, function (i) i$node$createdAt, character (1L))
        )
        last_edited_at <- c (
            last_edited_at,
            vapply (edges, function (i) {
                res <- i$node$lastEditedAt
                if (is.null (res)) res <- ""
                return (res)
            }, character (1L))
        )
        updated_at <- c (
            updated_at,
            vapply (edges, function (i) i$node$updatedAt, character (1L))
        )
        titles <- c (
            titles,
            vapply (edges, function (i) i$node$title, character (1L))
        )
        labels <- c (
            labels,
            lapply (edges, function (i) unname (unlist (i$node$labels)))
        )
        # Dates for labels are in separate "timeline" data:
        event_labels <- c (
            event_labels, extract_event_timeline_data (edges, "labels")
        )
        event_dates <- c (
            event_dates, extract_event_timeline_data (edges, "dates")
        )
        event_actors <- c (
            event_actors, extract_event_timeline_data (edges, "actors")
        )
        comments <- c (
            comments,
            lapply (edges, function (i) unname (unlist (i$node$comments$nodes)))
        )

        page_count <- page_count + 1L
        if (!quiet) {
            message (
                "Retrieved page [", page_count, "] to issue number [",
                max (number), "]"
            )
        }
    }

    holding_date <- extract_holding_events (event_labels, event_dates)

    # Reduce "event" data down to current labels only, and sort by labels so
    # that "official" 'N/' ones come first, which happens to be done perfectly
    # by standard sort:
    for (i in seq_along (labels)) {
        index <- which (event_labels [[i]] %in% labels [[i]])
        if (length (index) > 0L) {
            index <- index [order (event_labels [[i]] [index])]
            event_labels [[i]] <- event_labels [[i]] [index]
            event_dates [[i]] <- event_dates [[i]] [index]
            event_actors [[i]] <- event_actors [[i]] [index]
        }
    }
    # neither "labels" nor "event_actors" are used from that point on.

    # Replace NULL with empty label
    event_labels [which (vapply (event_labels, is.null, logical (1L)))] <- ""
    event_dates [which (vapply (event_dates, is.null, logical (1L)))] <- ""

    # Then extract latest stage and associated dates
    stages <- vapply (seq_along (labels), function (i) {
        ret <- rep (NA_character_, 2L)
        st_i <- grep ("^[0-9]\\/", event_labels [[i]])
        if (length (st_i) > 0) {
            st_i <- max (st_i) # in case multiple stage labels
            ret <- c (
                event_labels [[i]] [st_i],
                as.character (event_dates [[i]] [st_i])
            )
        }
        return (ret)
    }, character (2L))
    stages <- data.frame (t (stages))
    names (stages) <- c ("label", "date")
    stages$date <- as.Date (stages$date)

    stats <- vapply (labels, function (i) "stats" %in% i, logical (1L))

    # Also identify any issues with multiple stages:
    multiple_stages <- vapply (event_labels, function (i) {
        n_total <- length (grep ("^[0-9]\\/", unique (i)))
        n_approved <- length (grep ("^6\\/", unique (i)))
        n_total > 1L && n_total != n_approved
    }, logical (1L))

    # Finally, reduce labels to the non-stage values, ignoring
    # `labels_created_at` and `labels_updated_at` from here.
    event_labels <- lapply (event_labels, function (i) {
        st_i <- grep ("^[0-9]\\/", i)
        if (length (st_i) > 0) {
            i <- i [-st_i]
            if (length (i) == 0) {
                i <- ""
            }
        }
        return (unique (i))
    })

    # And remove `pkgcheck` results:
    comments <- lapply (comments, function (i) {
        i [grep ("^## Checks for", i)] <- "## pkgcheck check results"
        return (i)  })

    res <- data.frame (
        number = number,
        title = titles,
        submission_type = submission_type,
        state = state,
        stats = stats,
        stage = stages$label,
        stage_date = stages$date,
        has_multiple_stages = multiple_stages,
        labels = I (event_labels),
        assignees = I (assignees),
        created_at = lubridate::date (lubridate::ymd_hms (created_at)),
        last_edited_at = lubridate::date (lubridate::ymd_hms (last_edited_at)),
        updated_at = lubridate::date (lubridate::ymd_hms (updated_at)),
        holding_date = holding_date,
        comments = I (comments)
    ) |> dplyr::arrange (stage, stage_date)

    # That puts "0/editorial-team-prep" before "0/presubmission" - reverse
    # these:
    index <- grep ("^0\\/", res$stage)
    if (length (index) > 0) {
        res0 <- res [index, ]
        res <- res [-index, ]
        index_pre <- grep ("presubmission", res0$stage)
        index_edi <- grep ("editorial", res0$stage)
        res <- rbind (
            res0 [index_pre, ],
            res0 [index_edi, ],
            res
        )
    }

    return (res)
}

#' Parse issue body to extract submission type
#'
#' @param edges The GraphQL 'edges' data as a list with one item for each issue,
#' and each item including the issue body.
#' @noRd
submission_type_from_body <- function (edges) {

    vapply (edges, function (i) {

        b <- strsplit (i$node$body, "\\n") [[1]]
        stype <- grep ("^Submission\\stype\\:", b, value = TRUE)
        if (length (stype) == 0L) {
            return (NA_character_)
        }
        if (grepl (">", stype)) {
            stype <- regmatches (stype, regexpr (">.*<", stype))
            stype <- gsub ("^>|<$", "", stype)
        } else if (grepl ("\\:", stype)) {
            stype <- gsub ("^.*\\:(\\s?)|\\\r$|\\\n$", "", stype)
        } else {
            stype <- NA_character_
        }
        return (stype)

    }, character (1L))
}

#' Extract holding events
#'
#' @noRd
extract_holding_events <- function (event_labels, event_dates) {
    vapply (seq_along (event_labels), function (i) {
        if (any (event_labels [[i]] == "holding")) {
            h_index <- grep ("holding", event_labels [[i]])
            h_dates <- lubridate::ymd_hms (event_dates [[i]] [h_index])
            return (as.character (max (h_dates)))
        } else {
            return (NA_character_)
        }
    }, character (1L))
}

extract_event_timeline_data <- function (edges, what = "labels") {

    stopifnot (what %in% c ("labels", "dates", "actors"))

    lapply (edges, function (i) {
        unlist (lapply (
            i$node$timelineItems$nodes,
            function (j) {
                if (what == "labels") {
                    return (j$label$name)
                } else if (what == "dates") {
                    return (j$createdAt)
                } else if (what == "actors") {
                    return (j$actor$login)
                }
            }
        ))
    })
}

#' Generate a summary report for incoming Editor-in-Charge of current state of
#' all open software-review issues.
#'
#' @param open_only If `TRUE` (default), only extract information for currently
#' open issues.
#' @param browse If `TRUE` (default), open the results as a \pkg{DT} `datatable`
#' HTML page in default browser.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return A `data.frame` with one row per issue and some key statistics.
#' @export

review_status <- function (open_only = TRUE, browse = TRUE, quiet = FALSE) {

    # Suppress no visible binding notes:
    editor <- editor_date <- NULL

    dat <- reviews_gh_data (open_only, quiet = quiet)

    cmt_data <- extract_comment_info (dat)
    dat$comments <- NULL

    dat <- dplyr::bind_cols (dat, cmt_data) |>
        dplyr::relocate (editor, .after = labels) |>
        dplyr::relocate (editor_date, .after = editor)

    has_multiple_stages <- any (dat$has_multiple_stages)
    if (has_multiple_stages) {
        multiple_stages <- dat$number [which (dat$has_multiple_stages)]
        if (!quiet) {
            txt <- ifelse (
                length (multiple_stages) == 1,
                "issue currently has",
                "issues currently have"
            )
            warning (
                "The following ", txt, " multiple 'stage' labels:\n   ",
                paste0 (multiple_stages, collapse = ", ")
            )
        }
    }

    # Collapse list columns:
    listcols <- c ("assignees", "labels")
    for (lc in listcols) {
        dat [[lc]] <- vapply (dat [[lc]], function (i) {
            paste0 (i, collapse = ", ")
        }, character (1L))
    }

    # Convert update cols to elapsed time:
    elapsed <- get_elapsed_time (paste0 (dat$updated_at, "00:00:00"))
    elapsed_days <- elapsed$dtime_days
    elapsed <- elapsed$dtime
    dat$last_edited_at <- dat$updated_at <- NULL
    dat <- dplyr::bind_cols (
        dat,
        elapsed = elapsed,
        elapsed_days = elapsed_days
    )

    if (browse) {
        print (open_gt_table (dat))
    }

    if (has_multiple_stages) {
        attr (dat, "multiple_stages") <- multiple_stages
    }

    return (dat)
}

extract_comment_info <- function (dat) {

    extract_one <- function (x) {
        index <- 3 * seq_len (length (x) / 3) - 2

        dates <- x [index]
        actors <- x [index + 1]
        comments <- x [index + 2]

        index <- which (
            actors == "ropensci-review-bot" &
                grepl ("assigned|added", comments, ignore.case = TRUE)
        )
        dates <- dates [index]
        comments <- comments [index]

        editor <- editor_date <-
            rev1 <- rev1_assigned <- rev1_due <-
            rev2 <- rev2_assigned <- rev2_due <- ""

        index <- grep ("editor$", comments)
        if (length (index) > 0) {
            index <- max (index) # in case multiple editors re-assigned
            editor <- gsub ("^Assigned\\!\\s", "", comments [index])
            editor <- gsub ("\\s.*$", "", editor)
            editor_date <- lubridate::date (lubridate::ymd_hms (dates [index]))
        }

        extract_rev <- function (comments, dates, index) {
            rev1 <- gsub ("\\s.*$", "", comments [index [1]])
            rev1_assigned <-
                lubridate::date (lubridate::ymd_hms (dates [index [1]]))
            g <- regexpr (
                "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",
                comments [index [1]]
            )
            rev1_due <- regmatches (comments [index [1]], g)
            c (rev1, paste0 (rev1_assigned), rev1_due)
        }
        index <- grep ("reviewers list", comments)
        if (length (index) > 0) {
            rd1 <- extract_rev (comments, dates, index)
            rev1 <- rd1 [1]
            rev1_assigned <- rd1 [2]
            rev1_due <- rd1 [3]
            index <- index [-1]
            if (length (index) > 0) {
                rd2 <- extract_rev (comments, dates, index)
                rev2 <- rd2 [1]
                rev2_assigned <- rd2 [2]
                rev2_due <- rd2 [3]
            }
        }

        c (
            editor = editor,
            editor_date = paste0 (editor_date),
            rev1 = rev1,
            rev1_assigned = rev1_assigned,
            rev1_due = rev1_due,
            rev2 = rev2,
            rev2_assigned = rev2_assigned,
            rev2_due = rev2_due
        )
    }

    res <- lapply (dat$comments, extract_one)

    editor <- vapply (res, function (i) i [["editor"]], character (1L))
    editor_date <- vapply (res, function (i) {
        i [["editor_date"]]
    }, character (1L))
    rev1 <- vapply (res, function (i) i [["rev1"]], character (1L))
    rev1_assigned <- vapply (res, function (i) {
        i [["rev1_assigned"]]
    }, character (1L))
    rev1_due <- vapply (res, function (i) i [["rev1_due"]], character (1L))
    rev2 <- vapply (res, function (i) i [["rev2"]], character (1L))
    rev2_assigned <- vapply (res, function (i) {
        i [["rev2_assigned"]]
    }, character (1L))
    rev2_due <- vapply (res, function (i) i [["rev2_due"]], character (1L))

    return (data.frame (
        editor = editor,
        editor_date = editor_date,
        rev1 = rev1,
        rev1_assigned = rev1_assigned,
        rev1_due = rev1_due,
        rev2 = rev2,
        rev2_assigned = rev2_assigned,
        rev2_due = rev2_due
    ))
}
