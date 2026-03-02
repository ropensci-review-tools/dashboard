# ---------------------------------------------------------------------------
# Helpers: minimal fake GraphQL 'edges' structures
# ---------------------------------------------------------------------------

make_body_edge <- function (body) {
    list (node = list (body = body))
}

make_timeline_edge <- function (labels, dates, actors) {
    nodes <- mapply (function (lbl, dt, act) {
        list (
            label = list (name = lbl),
            createdAt = dt,
            actor = list (login = act)
        )
    }, labels, dates, actors, SIMPLIFY = FALSE)
    list (node = list (timelineItems = list (nodes = nodes)))
}

# ---------------------------------------------------------------------------
# submission_type_from_body()
# ---------------------------------------------------------------------------

test_that ("submission_type_from_body returns NA when no Submission type line", {

    edges <- list (make_body_edge ("Just a normal issue body\nWith multiple lines"))
    expect_identical (submission_type_from_body (edges), NA_character_)
})

test_that ("submission_type_from_body parses HTML-style >Type< markup", {

    body <- "Preamble\nSubmission type: >Standard<\nOther text"
    edges <- list (make_body_edge (body))
    expect_equal (submission_type_from_body (edges), "Standard")
})

test_that ("submission_type_from_body parses plain colon format", {

    body <- "Preamble\nSubmission type: Stats\nOther text"
    edges <- list (make_body_edge (body))
    expect_equal (submission_type_from_body (edges), "Stats")
})

test_that ("submission_type_from_body handles multi-line body correctly", {

    body <- paste0 (
        "Title\n",
        "- [x] I have read ...\n",
        "Submission type: Standard\n",
        "Package name: foo\n",
        "More text"
    )
    edges <- list (make_body_edge (body))
    expect_equal (submission_type_from_body (edges), "Standard")
})

test_that ("submission_type_from_body vectorises over multiple edges", {

    edges <- list (
        make_body_edge ("Submission type: Standard\n"),
        make_body_edge ("No submission type here"),
        make_body_edge ("Submission type: Stats\n")
    )
    res <- submission_type_from_body (edges)

    expect_length (res, 3L)
    expect_equal (res [1], "Standard")
    expect_true (is.na (res [2]))
    expect_equal (res [3], "Stats")
})

# ---------------------------------------------------------------------------
# extract_event_timeline_data()
# ---------------------------------------------------------------------------

test_that ("extract_event_timeline_data returns list same length as edges", {

    edges <- list (
        make_timeline_edge ("1/editor-checks", "2024-01-01T00:00:00Z", "userA"),
        make_timeline_edge (character (0), character (0), character (0))
    )

    for (what in c ("labels", "dates", "actors")) {
        res <- extract_event_timeline_data (edges, what)
        expect_type (res, "list")
        expect_length (res, 2L)
    }
})

test_that ("extract_event_timeline_data extracts labels", {

    edges <- list (
        make_timeline_edge (
            c ("1/editor-checks", "2/seeking-reviewers"),
            c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
            c ("userA", "userB")
        )
    )
    res <- extract_event_timeline_data (edges, "labels")
    expect_equal (res [[1]], c ("1/editor-checks", "2/seeking-reviewers"),
        ignore_attr = TRUE
    )
})

test_that ("extract_event_timeline_data extracts dates", {

    edges <- list (
        make_timeline_edge (
            c ("1/editor-checks", "2/seeking-reviewers"),
            c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
            c ("userA", "userB")
        )
    )
    res <- extract_event_timeline_data (edges, "dates")
    expect_equal (res [[1]], c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
        ignore_attr = TRUE
    )
})

test_that ("extract_event_timeline_data extracts actors", {

    edges <- list (
        make_timeline_edge (
            c ("1/editor-checks", "2/seeking-reviewers"),
            c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"),
            c ("userA", "userB")
        )
    )
    res <- extract_event_timeline_data (edges, "actors")
    expect_equal (res [[1]], c ("userA", "userB"), ignore_attr = TRUE)
})

test_that ("extract_event_timeline_data errors on invalid 'what'", {

    edges <- list (
        make_timeline_edge ("lbl", "2024-01-01T00:00:00Z", "usr")
    )
    expect_error (extract_event_timeline_data (edges, "invalid"))
})

test_that ("extract_event_timeline_data returns NULL for edge with no nodes", {

    edges <- list (
        make_timeline_edge (character (0), character (0), character (0))
    )
    res <- extract_event_timeline_data (edges, "labels")
    expect_null (res [[1]])
})

# ---------------------------------------------------------------------------
# extract_holding_events()
# ---------------------------------------------------------------------------

test_that ("extract_holding_events returns NA when no holding label", {

    event_labels <- list (c ("1/editor-checks", "2/seeking-reviewers"))
    event_dates <- list (c ("2024-01-01T00:00:00Z", "2024-02-01T00:00:00Z"))

    res <- extract_holding_events (event_labels, event_dates)
    expect_length (res, 1L)
    expect_true (is.na (res))
})

test_that ("extract_holding_events returns date string for single holding event", {

    event_labels <- list (c ("1/editor-checks", "holding"))
    event_dates <- list (c ("2024-01-01T00:00:00Z", "2024-03-15T12:00:00Z"))

    res <- extract_holding_events (event_labels, event_dates)
    expect_type (res, "character")
    expect_match (res, "2024-03-15")
})

test_that ("extract_holding_events returns maximum date for multiple holding events", {

    event_labels <- list (c ("holding", "1/editor-checks", "holding"))
    event_dates <- list (c (
        "2024-02-01T00:00:00Z",
        "2024-01-15T00:00:00Z",
        "2024-03-01T00:00:00Z"
    ))

    res <- extract_holding_events (event_labels, event_dates)
    # Later date (March) must win
    expect_match (res, "2024-03-01")
    expect_false (grepl ("2024-02-01", res))
})

test_that ("extract_holding_events vectorises over issues", {

    event_labels <- list (
        c ("holding"),
        c ("1/editor-checks")
    )
    event_dates <- list (
        c ("2024-06-01T00:00:00Z"),
        c ("2024-01-01T00:00:00Z")
    )

    res <- extract_holding_events (event_labels, event_dates)
    expect_length (res, 2L)
    expect_match (res [1], "2024-06-01")
    expect_true (is.na (res [2]))
})

# ---------------------------------------------------------------------------
# extract_comment_info()
# ---------------------------------------------------------------------------

# Comments are stored as flat character vectors of triplets:
# c(date1, actor1, body1, date2, actor2, body2, ...)
BOT <- "ropensci-review-bot"

test_that ("extract_comment_info returns 8-column data.frame", {

    dat <- list (comments = list (character (0)))
    res <- extract_comment_info (dat)

    expect_s3_class (res, "data.frame")
    expect_named (res, c (
        "editor", "editor_date",
        "rev1", "rev1_assigned", "rev1_due",
        "rev2", "rev2_assigned", "rev2_due"
    ))
    expect_equal (nrow (res), 1L)
})

test_that ("extract_comment_info returns empty strings when no bot comments", {

    # Non-bot comment only
    comments <- c ("2024-01-10T09:00:00Z", "someuser", "Great package!")
    dat <- list (comments = list (comments))
    res <- extract_comment_info (dat)

    expect_equal (res$editor, "")
    expect_equal (res$rev1, "")
    expect_equal (res$rev2, "")
})

test_that ("extract_comment_info extracts editor correctly", {

    comments <- c (
        "2024-01-15T10:00:00Z", BOT, "Assigned! mpadge as editor",
        "2024-01-10T09:00:00Z", "someuser", "Nice!"
    )
    dat <- list (comments = list (comments))
    res <- extract_comment_info (dat)

    expect_equal (res$editor, "mpadge")
    expect_equal (res$editor_date, "2024-01-15")
})

test_that ("extract_comment_info extracts reviewer 1", {

    comments <- c (
        "2024-01-15T10:00:00Z", BOT, "Assigned! mpadge as editor",
        "2024-02-01T10:00:00Z", BOT,
        "reviewer1 added to the reviewers list. Due date: 2024-03-15."
    )
    dat <- list (comments = list (comments))
    res <- extract_comment_info (dat)

    expect_equal (res$rev1, "reviewer1")
    expect_equal (res$rev1_assigned, "2024-02-01")
    expect_equal (res$rev1_due, "2024-03-15")
    expect_equal (res$rev2, "")
})

test_that ("extract_comment_info extracts reviewer 2 from second reviewers-list comment", {

    comments <- c (
        "2024-01-15T10:00:00Z", BOT, "Assigned! mpadge as editor",
        "2024-02-01T10:00:00Z", BOT,
        "reviewer1 added to the reviewers list. Due date: 2024-03-15.",
        "2024-02-05T10:00:00Z", BOT,
        "reviewer2 added to the reviewers list. Due date: 2024-04-15."
    )
    dat <- list (comments = list (comments))
    res <- extract_comment_info (dat)

    expect_equal (res$rev1, "reviewer1")
    expect_equal (res$rev1_due, "2024-03-15")
    expect_equal (res$rev2, "reviewer2")
    expect_equal (res$rev2_assigned, "2024-02-05")
    expect_equal (res$rev2_due, "2024-04-15")
})

test_that ("extract_comment_info handles multiple rows", {

    empty <- character (0)
    with_ed <- c ("2024-01-15T10:00:00Z", BOT, "Assigned! editorA as editor")
    dat <- list (comments = list (empty, with_ed))
    res <- extract_comment_info (dat)

    expect_equal (nrow (res), 2L)
    expect_equal (res$editor [1], "")
    expect_equal (res$editor [2], "editorA")
})
